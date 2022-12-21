use rug::{
    float::Constant, float::OrdFloat, float::Round, float::Special, ops::AssignRound,
    ops::DivAssignRound, ops::PowAssignRound, Float,
};

use float_next_after::NextAfter;


const F64_PREC: u32 = 53;

enum IntervalClassification {
    StrictlyPos,
    StrictlyNeg,
    Mixed,
}

/// The `lo` field represents whether the computation must error.
/// The `hi` field represents whether the computation may error.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct ErrorInterval {
    pub lo: bool,
    // the
    pub hi: bool,
}

impl ErrorInterval {
    pub fn union(&self, other: &ErrorInterval) -> ErrorInterval {
        ErrorInterval {
            lo: self.lo || other.lo,
            hi: self.hi || other.hi,
        }
    }

    pub fn is_valid(&self) -> bool {
        !self.lo || self.hi
    }

    pub fn is_guaranteed(&self) -> bool {
        self.lo
    }

    pub fn is_possible(&self) -> bool {
        self.hi
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BooleanInterval {
    pub lo: bool,
    pub hi: bool,
    pub err: ErrorInterval,
}

impl BooleanInterval {
    pub fn true_interval() -> BooleanInterval {
        BooleanInterval {
            lo: true,
            hi: true,
            err: ErrorInterval::default(),
        }
    }

    pub fn is_valid(&self) -> bool {
        (!self.lo) || self.hi && self.err.is_valid()
    }

    pub fn false_interval() -> BooleanInterval {
        BooleanInterval {
            lo: false,
            hi: false,
            err: ErrorInterval::default(),
        }
    }

    pub fn unknown_interval() -> BooleanInterval {
        BooleanInterval {
            lo: false,
            hi: true,
            err: ErrorInterval::default(),
        }
    }

    pub fn and(&self, other: &BooleanInterval) -> BooleanInterval {
        BooleanInterval {
            lo: self.lo && other.lo,
            hi: self.hi && other.hi,
            err: self.err.union(&other.err),
        }
    }

    pub fn or(&self, other: &BooleanInterval) -> BooleanInterval {
        BooleanInterval {
            lo: self.lo || other.lo,
            hi: self.hi || other.hi,
            err: self.err.union(&other.err),
        }
    }

    pub fn not(&self) -> BooleanInterval {
        BooleanInterval {
            lo: !self.hi,
            hi: !self.lo,
            err: self.err.clone(),
        }
    }

    pub fn if_real_result(&self, other: &Interval, third: &Interval) -> Interval {
        if self.lo {
            other.with_error(self.err.union(&other.err))
        } else if !self.hi {
            third.with_error(self.err.union(&third.err))
        } else {
            other.union(third)
        }
    }

    pub fn union(&self, other: &BooleanInterval) -> BooleanInterval {
        BooleanInterval {
            lo: self.lo || other.lo,
            hi: self.hi || other.hi,
            err: self.err.union(&other.err),
        }
    }

    fn with_error(&self, err: ErrorInterval) -> BooleanInterval {
        BooleanInterval {
            lo: self.lo,
            hi: self.hi,
            err,
        }
    }

    pub fn if_boolean_result(
        &self,
        other: &BooleanInterval,
        third: &BooleanInterval,
    ) -> BooleanInterval {
        if self.lo {
            other.with_error(self.err.union(&other.err))
        } else if !self.hi {
            third.with_error(self.err.union(&third.err))
        } else {
            other.union(third)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Interval {
    pub lo: OrdFloat,
    pub hi: OrdFloat,
    pub err: ErrorInterval,
}

pub(crate) fn is_even(val: &Float) -> bool {
    if let Some(int) = val.to_integer() {
        int.is_even()
    } else {
        false
    }
}

pub(crate) fn is_odd(val: &Float) -> bool {
    if let Some(int) = val.to_integer() {
        int.is_odd()
    } else {
        false
    }
}

pub(crate) fn bf(prec: u32, val: f64) -> Float {
    Float::with_val(prec, val)
}

pub(crate) fn add_round(a: &Float, b: &Float, round: Round) -> Float {
    let mut tmp = a.clone();
    tmp.mul_add_round(&bf(a.prec(), 1.0), b, round);
    tmp
}

pub(crate) fn sub_round(a: &Float, b: &Float, round: Round) -> Float {
    let mut tmp = a.clone();
    tmp.mul_add_round(&bf(a.prec(), 1.0), b, round);
    tmp
}

pub(crate) fn div_round(a: &Float, b: &Float, round: Round) -> Float {
    let mut tmp = a.clone();
    tmp.div_assign_round(b, round);
    tmp
}

pub(crate) fn mul_round(a: &Float, b: &Float, round: Round) -> Float {
    let mut tmp = a.clone();
    tmp.mul_add_round(b, &bf(a.prec(), 0.0), round);
    tmp
}

impl Interval {
    pub fn is_valid(&self) -> bool {
        // ordered and signes ordered, and not just infinite values
        self.lo <= self.hi && self.err.is_valid() && !(self.hi().is_infinite() && self.hi == self.lo && !self.err.lo)
    }
    
    fn lo(&self) -> Float {
        self.lo.clone().into()
    }

    fn hi(&self) -> Float {
        self.hi.clone().into()
    }

    pub fn new(prec: u32, lo: f64, hi: f64) -> Interval {
        Interval {
            lo: Interval::to_ord(bf(prec, lo)),
            hi: Interval::to_ord(bf(prec, hi)),
            err: ErrorInterval {
                lo: false,
                hi: false,
            },
        }
    }

    fn to_ord(float: Float) -> OrdFloat {
        if float.is_zero() {
            OrdFloat::from(bf(float.prec(), 0.0))
        } else {
            OrdFloat::from(float)
        }
    }

    pub fn make(lo: Float, hi: Float, err: ErrorInterval) -> Interval {
        Interval {
            lo: Interval::to_ord(lo),
            hi: Interval::to_ord(hi),
            err,
        }
    }

    fn classify_with(&self, val: &Float) -> IntervalClassification {
        if &self.lo() > val {
            IntervalClassification::StrictlyPos
        } else if &self.hi() < val {
            IntervalClassification::StrictlyNeg
        } else {
            IntervalClassification::Mixed
        }
    }

    fn classify(&self) -> IntervalClassification {
        self.classify_with(&bf(F64_PREC, 0.0))
    }

    pub fn union(&self, other: &Interval) -> Interval {
        Interval::make(
            self.lo().min(&other.lo()),
            self.hi().max(&other.hi()),
            self.err.union(&other.err),
        )
    }

    pub fn neg(&self) -> Interval {
        Interval::make(-self.hi(), -self.lo(), self.err.clone())
    }

    pub fn pi(prec: u32) -> Interval {
        let mut lopi = Float::new(prec);
        lopi.assign_round(Constant::Pi, Round::Down);
        let mut hipi = Float::new(prec);
        hipi.assign_round(Constant::Pi, Round::Up);
        Interval::make(lopi, hipi, ErrorInterval::default())
    }

    pub fn e(prec: u32) -> Interval {
        let mut loe = Float::with_val(prec, 1.0);
        loe.exp_round(Round::Down);
        let mut hie = Float::with_val(prec, 1.0);
        hie.exp_round(Round::Up);
        Interval::make(loe, hie, ErrorInterval::default())
    }

    pub fn inf(prec: u32) -> Interval {
        Interval::make(
            Float::with_val(prec, Special::Infinity),
            Float::with_val(prec, Special::Infinity),
            ErrorInterval::default(),
        )
    }

    pub fn add(&self, other: &Interval) -> Interval {
        let mut lo = self.lo();
        lo.mul_add_round(&bf(other.lo().prec(), 1.0), &other.lo(), Round::Down);
        let mut hi = self.hi();
        hi.mul_add_round(&bf(other.lo().prec(), 1.0), &other.hi(), Round::Up);
        Interval::make(lo, hi, self.err.union(&other.err))
    }

    pub fn sub(&self, other: &Interval) -> Interval {
        let mut lo = self.lo();
        lo.mul_sub_round(&bf(other.lo().prec(), 1.0), &other.hi(), Round::Down);
        let mut hi = self.hi();
        hi.mul_sub_round(&bf(other.lo().prec(), 1.0), &other.lo(), Round::Up);
        Interval::make(lo, hi, self.err.union(&other.err))
    }

    pub fn mul(&self, other: &Interval) -> Interval {
        let perform_mult = |lo1: &Float, lo2: &Float, hi1: &Float, hi2: &Float| {
            let mut lo = lo1.clone();
            lo.mul_add_round(lo2, &bf(lo1.prec(), 0.0), Round::Down);
            let mut hi = hi1.clone();
            hi.mul_add_round(hi2, &bf(hi1.prec(), 0.0), Round::Up);
            Interval::make(lo, hi, self.err.union(&other.err))
        };

        use IntervalClassification::*;
        match (self.classify(), other.classify()) {
            (StrictlyPos, StrictlyPos) => {
                perform_mult(&self.lo(), &other.lo(), &self.hi(), &other.hi())
            }

            (StrictlyPos, StrictlyNeg) => {
                perform_mult(&self.hi(), &other.lo(), &self.lo(), &other.hi())
            }

            (StrictlyPos, Mixed) => perform_mult(&self.hi(), &other.lo(), &self.hi(), &other.hi()),

            (StrictlyNeg, Mixed) => perform_mult(&self.lo(), &other.hi(), &self.lo(), &other.lo()),

            (StrictlyNeg, StrictlyPos) => {
                perform_mult(&self.lo(), &other.hi(), &self.hi(), &other.lo())
            }

            (StrictlyNeg, StrictlyNeg) => {
                perform_mult(&self.hi(), &other.hi(), &self.lo(), &other.lo())
            }

            (Mixed, StrictlyPos) => perform_mult(&self.lo(), &other.hi(), &self.hi(), &other.hi()),

            (Mixed, StrictlyNeg) => perform_mult(&self.hi(), &other.lo(), &self.lo(), &other.lo()),

            (Mixed, Mixed) => perform_mult(&self.hi(), &other.lo(), &self.lo(), &other.lo()).union(
                &perform_mult(&self.lo(), &other.hi(), &self.hi(), &other.hi()),
            ),
        }
    }

    pub fn get_const(&self) -> Option<Float> {
        if self.lo() == self.hi() {
            Some(self.lo())
        } else {
            None
        }
    }

    pub fn div(&self, other: &Interval) -> Interval {
        let zero = bf(other.lo().prec(), 0.0);
        let error = ErrorInterval {
            lo: self.err.lo || other.err.lo || other.get_const() == Some(zero.clone()),
            hi: self.err.hi || other.err.hi || (other.lo() <= zero && other.hi() >= zero),
        };

        let perform_div = |lo1: &Float, lo2: &Float, hi1: &Float, hi2: &Float| {
            let mut lo = lo1.clone();
            lo.div_assign_round(lo2, Round::Down);
            let mut hi = hi1.clone();
            hi.div_assign_round(hi2, Round::Up);

            Interval::make(lo, hi, error.clone())
        };

        use IntervalClassification::*;
        match (self.classify(), other.classify()) {
            (_any, Mixed) => Interval::make(
                bf(self.lo().prec(), std::f64::NEG_INFINITY),
                bf(self.lo().prec(), std::f64::INFINITY),
                error,
            ),
            (StrictlyPos, StrictlyPos) => {
                perform_div(&self.lo(), &other.hi(), &self.hi(), &other.lo())
            }
            (StrictlyPos, StrictlyNeg) => {
                perform_div(&self.hi(), &other.hi(), &self.lo(), &other.lo())
            }
            (StrictlyNeg, StrictlyPos) => {
                perform_div(&self.lo(), &other.lo(), &self.hi(), &other.hi())
            }
            (StrictlyNeg, StrictlyNeg) => {
                perform_div(&self.hi(), &other.lo(), &self.lo(), &other.hi())
            }
            (Mixed, StrictlyPos) => perform_div(&self.lo(), &other.lo(), &self.hi(), &other.lo()),
            (Mixed, StrictlyNeg) => perform_div(&self.hi(), &other.hi(), &self.lo(), &other.hi()),
        }
    }

    fn monotonic_mut(&self, fun: fn(&mut Float, Round) -> std::cmp::Ordering) -> Interval {
        let mut tmplo = self.lo();
        let mut tmphi = self.hi();
        fun(&mut tmplo, Round::Down);
        fun(&mut tmphi, Round::Up);
        if tmplo.is_nan() || tmphi.is_nan() {
            assert!(self.err.lo);
        }
        Interval::make(tmplo, tmphi, self.err.clone())
    }

    fn comonotonic_mut(&self, fun: fn(&mut Float, Round) -> std::cmp::Ordering) -> Interval {
        let mut tmplo = self.hi();
        let mut tmphi = self.lo();
        fun(&mut tmplo, Round::Down);
        fun(&mut tmphi, Round::Up);
        if tmplo.is_nan() {
            panic!("comonotonic_mut: lo is NaN");
        }
        if tmphi.is_nan() {
            panic!("comonotonic_mut: hi is NaN");
        }
        Interval::make(tmplo, tmphi, self.err.clone())
    }

    fn clamp(&self, lo: &Float, hi: &Float) -> Interval {
        Interval::make(
            self.lo().max(lo).min(hi),
            self.hi().min(hi).max(lo),
            ErrorInterval {
                lo: self.err.lo || &self.hi() < lo || &self.lo() > hi,
                hi: self.err.hi || &self.lo() < lo || &self.hi() > hi,
            },
        )
    }

    fn clamp_strict(&self, lo: &Float, hi: &Float) -> Interval {
        Interval::make(
            self.lo().max(lo).min(hi),
            self.hi().min(hi).max(lo),
            ErrorInterval {
                lo: self.err.lo || &self.hi() <= lo || &self.lo() >= hi,
                hi: self.err.hi || &self.lo() <= lo || &self.hi() >= hi,
            },
        )
    }

    pub fn round(&self) -> Interval {
        Interval::make(self.lo().floor(), self.hi().ceil(), self.err.clone())
    }

    pub fn ceil(&self) -> Interval {
        Interval::make(self.lo().floor(), self.hi().ceil(), self.err.clone())
    }

    pub fn floor(&self) -> Interval {
        Interval::make(self.lo().floor(), self.hi().ceil(), self.err.clone())
    }

    pub fn trunc(&self) -> Interval {
        Interval::make(self.lo().floor(), self.hi().ceil(), self.err.clone())
    }

    pub fn fabs(&self) -> Interval {
        let zero = bf(self.lo().prec(), 0.0);
        if self.lo() >= zero {
            self.clone()
        } else if self.hi() <= zero {
            self.neg()
        } else {
            Interval::make(zero, self.hi().max(&-self.lo()), self.err.clone())
        }
    }

    pub fn exp(&self) -> Interval {
        self.monotonic_mut(Float::exp_round)
    }

    pub fn exp_m1(&self) -> Interval {
        self.monotonic_mut(Float::exp_m1_round)
    }

    pub fn exp2(&self) -> Interval {
        self.monotonic_mut(Float::exp2_round)
    }

    pub fn ln(&self) -> Interval {
        self.clamp_strict(
            &bf(self.lo().prec(), 0.0),
            &bf(self.lo().prec(), f64::INFINITY),
        )
        .monotonic_mut(Float::ln_round)
    }

    pub fn log10(&self) -> Interval {
        self.clamp_strict(
            &bf(self.lo().prec(), 0.0),
            &bf(self.lo().prec(), f64::INFINITY),
        )
        .monotonic_mut(Float::log10_round)
    }

    pub fn log2(&self) -> Interval {
        self.clamp_strict(
            &bf(self.lo().prec(), 0.0),
            &bf(self.lo().prec(), f64::INFINITY),
        )
        .monotonic_mut(Float::log2_round)
    }

    pub fn ln_1p(&self) -> Interval {
        self.clamp_strict(
            &bf(self.lo().prec(), -1.0),
            &bf(self.lo().prec(), f64::INFINITY),
        )
        .monotonic_mut(Float::ln_1p_round)
    }

    pub fn sqrt(&self) -> Interval {
        self.clamp(
            &bf(self.lo().prec(), 0.0),
            &bf(self.lo().prec(), f64::INFINITY),
        )
        .monotonic_mut(Float::sqrt_round)
    }

    pub fn cbrt(&self) -> Interval {
        self.monotonic_mut(Float::cbrt_round)
    }

    pub fn hypot(&self, other: &Interval) -> Interval {
        let self_pos = self.fabs();
        let other_pos = other.fabs();
        let mut tmp_lo = self_pos.lo();
        tmp_lo.hypot_round(&other_pos.lo(), Round::Down);
        let mut tmp_hi = self_pos.hi();
        tmp_hi.hypot_round(&other_pos.hi(), Round::Up);
        Interval::make(tmp_lo, tmp_hi, self.err.union(&other.err))
    }

    // assumes self is positive or zero
    fn pow_pos(&self, other: &Interval) -> Interval {
        let perform_pow = |lo1: &Float, lo2: &Float, hi1: &Float, hi2: &Float| {
            let err = ErrorInterval {
                lo: self.err.lo
                    || other.err.lo
                    || (self.hi().is_zero() && other.hi() <= bf(other.hi().prec(), 0.0)),
                hi: self.err.hi
                    || other.err.hi
                    || (self.lo().is_zero() && other.lo() <= bf(other.hi().prec(), 0.0)),
            };
            let mut tmp_lo = lo1.clone();
            tmp_lo.pow_assign_round(lo2, Round::Down);
            let mut tmp_hi = hi1.clone();
            tmp_hi.pow_assign_round(hi2, Round::Up);
            Interval::make(tmp_lo, tmp_hi, err)
        };

        // copied from mult (just replaced name with perform_pow)
        use IntervalClassification::*;
        match (
            self.classify_with(&bf(other.lo().prec(), 1.0)),
            other.classify(),
        ) {
            (StrictlyPos, StrictlyPos) => {
                perform_pow(&self.lo(), &other.lo(), &self.hi(), &other.hi())
            }

            (StrictlyPos, StrictlyNeg) => {
                perform_pow(&self.hi(), &other.lo(), &self.lo(), &other.hi())
            }

            (StrictlyPos, Mixed) => perform_pow(&self.hi(), &other.lo(), &self.hi(), &other.hi()),

            (StrictlyNeg, Mixed) => perform_pow(&self.lo(), &other.hi(), &self.lo(), &other.lo()),

            (StrictlyNeg, StrictlyPos) => {
                perform_pow(&self.lo(), &other.hi(), &self.hi(), &other.lo())
            }

            (StrictlyNeg, StrictlyNeg) => {
                perform_pow(&self.hi(), &other.hi(), &self.lo(), &other.lo())
            }

            (Mixed, StrictlyPos) => perform_pow(&self.lo(), &other.hi(), &self.hi(), &other.hi()),

            (Mixed, StrictlyNeg) => perform_pow(&self.hi(), &other.lo(), &self.lo(), &other.lo()),

            (Mixed, Mixed) => perform_pow(&self.hi(), &other.lo(), &self.lo(), &other.lo()).union(
                &perform_pow(&self.lo(), &other.hi(), &self.hi(), &other.hi()),
            ),
        }
    }

    // assumes x is negative or zero
    fn pow_neg(&self, other: &Interval) -> Interval {
        let mut err_possible = other.lo() < other.hi();
        if !err_possible {
            if let Some(rat) = other.lo().to_rational() {
                err_possible = rat.denom().is_even();
            }
        }
        let error = ErrorInterval {
            lo: false,
            hi: err_possible,
        };

        let x_pos = self.fabs();
        let positive_ans = x_pos.pow_pos(other);
        let mut res = positive_ans.union(&positive_ans.neg());
        res.err = res.err.union(&error);
        res
    }

    pub fn contains(&self, value: &Float) -> bool {
        &self.lo() <= value && value <= &self.hi()
    }

    pub fn split(&self, along: &Float) -> Option<(Interval, Interval)> {
        if self.contains(along) {
            Some((
                Interval::make(self.lo(), along.clone(), self.err.clone()),
                Interval::make(along.clone(), self.hi(), self.err.clone()),
            ))
        } else {
            None
        }
    }

    pub fn pow(&self, other: &Interval) -> Interval {
        if self.hi() < bf(self.lo().prec(), 0.0) {
            self.pow_neg(other)
        } else if self.lo() >= bf(self.lo().prec(), 0.0) {
            self.pow_pos(other)
        } else if let Some((neg, pos)) = self.split(&bf(self.lo().prec(), 0.0)) {
            neg.pow_neg(other).union(&pos.pow_pos(other))
        } else {
            assert!(self.err.hi);
            Interval::make(
                bf(self.lo().prec(), f64::NAN),
                bf(self.lo().prec(), f64::NAN),
                ErrorInterval { lo: true, hi: true },
            )
        }
    }

    pub fn fma(&self, other: &Interval, third: &Interval) -> Interval {
        self.mul(other).add(third)
    }

    fn period_lower(&self, is_even: bool) -> Float {
        let mut lopi = Float::new(self.lo().prec());
        lopi.assign_round(Constant::Pi, Round::Down);
        let mut hipi = Float::new(self.lo().prec());
        hipi.assign_round(Constant::Pi, Round::Up);
        let zero = bf(self.lo().prec(), 0.0);

        let mut afactor = self.lo();
        afactor.div_assign_round(if self.lo() < zero { lopi } else { hipi }, Round::Down);
        if is_even {
            afactor.mul_sub_round(
                &bf(self.lo().prec(), 1 as f64),
                &bf(self.lo().prec(), 0.5 as f64),
                Round::Down,
            );
        }
        afactor = afactor.floor();

        afactor
    }

    fn period_higher(&self, is_even: bool) -> Float {
        let mut lopi = Float::new(self.lo().prec());
        lopi.assign_round(Constant::Pi, Round::Down);
        let mut hipi = Float::new(self.lo().prec());
        hipi.assign_round(Constant::Pi, Round::Up);
        let zero = bf(self.lo().prec(), 0.0);

        let mut bfactor = self.hi();
        bfactor.div_assign_round(if self.hi() < zero { hipi } else { lopi }, Round::Up);
        if is_even {
            bfactor.mul_sub_round(
                &bf(self.hi().prec(), 1 as f64),
                &bf(self.hi().prec(), 0.5 as f64),
                Round::Up,
            );
        }
        bfactor = bfactor.ceil();
        bfactor
    }

    pub fn cos(&self) -> Interval {
        let mut lopi = Float::new(self.lo().prec());
        lopi.assign_round(Constant::Pi, Round::Down);
        let mut hipi = Float::new(self.lo().prec());
        hipi.assign_round(Constant::Pi, Round::Up);

        let afactor = self.period_lower(false);
        let bfactor = self.period_higher(false);

        if afactor == bfactor && is_even(&afactor) {
            let mut hitmp = self.hi();
            let mut lotmp = self.lo();
            hitmp.cos_round(Round::Down);
            lotmp.cos_round(Round::Up);
            Interval::make(hitmp, lotmp, self.err.clone())
        } else if afactor == bfactor && is_odd(&afactor) {
            let mut hitmp = self.hi();
            let mut lotmp = self.lo();
            hitmp.cos_round(Round::Up);
            lotmp.cos_round(Round::Down);
            Interval::make(lotmp, hitmp, self.err.clone())
        } else if (bfactor.clone() - afactor.clone()) == (1.0) && is_even(&afactor) {
            let mut lotmp = self.lo();
            lotmp.cos_round(Round::Up);
            let mut hitmp = self.hi();
            hitmp.cos_round(Round::Up);
            Interval::make(
                bf(self.lo().prec(), -1.0),
                lotmp.max(&hitmp),
                self.err.clone(),
            )
        } else if (bfactor.clone() - afactor.clone()) == (1.0) && is_odd(&afactor) {
            let mut lotmp = self.lo();
            lotmp.cos_round(Round::Down);
            let mut hitmp = self.hi();
            hitmp.cos_round(Round::Down);
            Interval::make(
                lotmp.min(&hitmp),
                bf(self.lo().prec(), 1.0),
                self.err.clone(),
            )
        } else {
            return Interval::make(
                bf(self.lo().prec(), -1.0),
                bf(self.lo().prec(), 1.0),
                self.err.clone(),
            );
        }
    }

    pub fn sin(&self) -> Interval {
        let mut lopi = Float::new(self.lo().prec());
        lopi.assign_round(Constant::Pi, Round::Down);
        let mut hipi = Float::new(self.lo().prec());
        hipi.assign_round(Constant::Pi, Round::Up);

        let afactor = self.period_lower(true);
        let bfactor = self.period_higher(true);

        if afactor == bfactor && is_even(&afactor) {
            let mut hitmp = self.hi();
            let mut lotmp = self.lo();
            hitmp.sin_round(Round::Down);
            lotmp.sin_round(Round::Up);
            Interval::make(hitmp, lotmp, self.err.clone())
        } else if afactor == bfactor && is_odd(&afactor) {
            let mut hitmp = self.hi();
            let mut lotmp = self.lo();
            hitmp.sin_round(Round::Up);
            lotmp.sin_round(Round::Down);
            Interval::make(lotmp, hitmp, self.err.clone())
        } else if (bfactor.clone() - afactor.clone()) == (1.0) && is_even(&afactor) {
            let mut lotmp = self.lo();
            lotmp.sin_round(Round::Up);
            let mut hitmp = self.hi();
            hitmp.sin_round(Round::Up);
            Interval::make(
                bf(self.lo().prec(), -1.0),
                lotmp.max(&hitmp),
                self.err.clone(),
            )
        } else if (bfactor.clone() - afactor.clone()) == (1.0) && is_odd(&afactor) {
            let mut lotmp = self.lo();
            lotmp.sin_round(Round::Down);
            let mut hitmp = self.hi();
            hitmp.sin_round(Round::Down);
            Interval::make(
                lotmp.min(&hitmp),
                bf(self.lo().prec(), 1.0),
                self.err.clone(),
            )
        } else {
            return Interval::make(
                bf(self.lo().prec(), -1.0),
                bf(self.lo().prec(), 1.0),
                self.err.clone(),
            );
        }
    }

    pub fn tan(&self) -> Interval {
        let mut lopi = Float::new(self.lo().prec());
        lopi.assign_round(Constant::Pi, Round::Down);
        let mut hipi = Float::new(self.lo().prec());
        hipi.assign_round(Constant::Pi, Round::Up);

        let afactor = self.period_lower(true);
        let bfactor = self.period_higher(true);

        if afactor == bfactor {
            let mut hitmp = self.hi();
            let mut lotmp = self.lo();
            lotmp.tan_round(Round::Down);
            hitmp.tan_round(Round::Up);
            Interval::make(lotmp, hitmp, self.err.clone())
        } else {
            return Interval::make(
                bf(self.lo().prec(), f64::NEG_INFINITY),
                bf(self.lo().prec(), f64::INFINITY),
                self.err.clone(),
            );
        }
    }

    pub fn atan2(&self, other: &Interval) -> Interval {
        let mkatan = |lo1: &Float, lo2: &Float, hi1: &Float, hi2: &Float| {
            let mut lotmp = lo1.clone();
            let mut hitmp = hi1.clone();
            lotmp.atan2_round(lo2, Round::Down);
            hitmp.atan2_round(hi2, Round::Up);
            Interval::make(lotmp, hitmp, self.err.union(&other.err))
        };

        use IntervalClassification::*;
        match (other.classify(), self.classify()) {
            (StrictlyNeg, StrictlyNeg) => mkatan(&self.hi(), &other.lo(), &self.lo(), &other.hi()),
            (Mixed, StrictlyNeg) => mkatan(&self.hi(), &other.lo(), &self.hi(), &other.hi()),
            (StrictlyPos, StrictlyNeg) => mkatan(&self.lo(), &other.lo(), &self.hi(), &other.hi()),
            (StrictlyPos, Mixed) => mkatan(&self.lo(), &other.lo(), &self.hi(), &other.lo()),
            (StrictlyPos, StrictlyPos) => mkatan(&self.lo(), &other.hi(), &self.hi(), &other.lo()),
            (Mixed, StrictlyPos) => mkatan(&self.lo(), &other.hi(), &self.lo(), &other.lo()),
            (StrictlyNeg, StrictlyPos) => mkatan(&self.hi(), &other.hi(), &self.lo(), &other.lo()),
            (_, Mixed) => {
                let mut hipi = Float::new(self.lo().prec());
                hipi.assign_round(Constant::Pi, Round::Up);
                let zero = bf(self.lo().prec(), 0.0);

                Interval::make(
                    -hipi.clone(),
                    hipi,
                    ErrorInterval {
                        lo: self.err.lo
                            || other.err.lo
                            || other.err.lo
                            || (self.lo() == 0
                                && self.hi() == 0
                                && other.lo() == 0
                                && other.hi() == 0),
                        hi: self.err.lo || self.hi() >= zero,
                    },
                )
            }
        }
    }

    pub fn cosh(&self) -> Interval {
        self.fabs().monotonic_mut(Float::cosh_round)
    }

    pub fn sinh(&self) -> Interval {
        self.monotonic_mut(Float::sinh_round)
    }

    pub fn tanh(&self) -> Interval {
        self.monotonic_mut(Float::tanh_round)
    }

    pub fn asinh(&self) -> Interval {
        self.monotonic_mut(Float::asinh_round)
    }

    pub fn acosh(&self) -> Interval {
        self.clamp(
            &bf(self.lo().prec(), 1.0),
            &bf(self.lo().prec(), f64::INFINITY),
        )
        .monotonic_mut(Float::acosh_round)
    }

    pub fn atanh(&self) -> Interval {
        self.clamp_strict(&bf(self.lo().prec(), -1.0), &bf(self.lo().prec(), 1.0))
            .monotonic_mut(Float::atanh_round)
    }

    pub fn asin(&self) -> Interval {
        self.clamp(&bf(self.lo().prec(), -1.0), &bf(self.lo().prec(), 1.0))
            .monotonic_mut(Float::asin_round)
    }

    pub fn acos(&self) -> Interval {
        self.clamp(&bf(self.lo().prec(), -1.0), &bf(self.lo().prec(), 1.0))
            .comonotonic_mut(Float::acos_round)
    }

    pub fn atan(&self) -> Interval {
        self.monotonic_mut(Float::atan_round)
    }

    // both self and other are positive
    pub fn fmod_pos(&self, other: &Interval) -> Interval {
        let a = div_round(&self.lo(), &other.hi(), Round::Down).floor();
        let b = div_round(&self.hi(), &other.hi(), Round::Up).ceil();

        // no intersection along y.hi edge
        if a == b {
            let mut c = self.hi();
            c.div_assign_round(other.hi(), Round::Down);
            let mut d = self.hi();
            d.div_assign_round(other.lo(), Round::Up);

            // no intersection along x.hi either; use top-left/bottom-right point
            if c == d {
                let mut tmplo = c.clone();
                tmplo.mul_sub_round(&other.hi(), &self.lo(), Round::Up);
                tmplo = -tmplo;
                let mut tmphi = c;
                tmphi.mul_sub_round(&other.lo(), &self.hi(), Round::Down);
                tmphi = -tmphi;

                Interval::make(tmplo, tmphi, self.err.clone())
            } else {
                let mut cplusone = c.clone();
                cplusone.mul_add_round(
                    &bf(self.lo().prec(), 1.0),
                    &bf(self.lo().prec(), 1.0),
                    Round::Down,
                );
                let mut tmphi = self.hi();
                tmphi.div_assign_round(cplusone, Round::Up);
                Interval::make(bf(self.lo().prec(), 0.0), tmphi, self.err.clone())
            }
        } else {
            return Interval::make(bf(self.lo().prec(), 0 as f64), other.hi(), self.err.clone());
        }
    }

    fn with_error(&self, err: ErrorInterval) -> Interval {
        Interval::make(self.lo(), self.hi(), err)
    }

    pub fn fmod(&self, other: &Interval) -> Interval {
        let zero = bf(self.lo().prec(), 0.0);
        let error = ErrorInterval {
            lo: self.err.lo || other.err.lo || (other.lo() == zero && other.hi() == zero),
            hi: self.err.hi || other.err.hi || (other.lo() <= zero && other.hi() >= zero),
        };

        let abs_other = other.fabs();

        if self.hi() <= zero {
            self.neg().fmod_pos(&abs_other).neg().with_error(error)
        } else if self.lo() >= zero {
            self.fmod_pos(&abs_other).with_error(error)
        } else {
            let (neg, pos) = self.split(&zero).unwrap();
            pos.fmod_pos(&abs_other)
                .union(&neg.neg().fmod_pos(&abs_other).neg())
                .with_error(error)
        }
    }

    // mostly copied from fmod_pos
    pub fn remainder_pos(&self, other: &Interval) -> Interval {
        let a = div_round(&self.lo(), &other.hi(), Round::Down).floor();
        let b = div_round(&self.hi(), &other.hi(), Round::Up).ceil();

        // no intersection along y.hi edge
        if a == b {
            let mut c = self.hi();
            c.div_assign_round(other.hi(), Round::Down);
            let mut d = self.hi();
            d.div_assign_round(other.lo(), Round::Up);

            // no intersection along x.hi either; use top-left/bottom-right point
            if c == d {
                let halfway = div_round(&other.hi(), &bf(self.lo().prec(), 2.0), Round::Down);

                let mut tmplo = c.clone();
                tmplo.mul_sub_round(&other.hi(), &self.lo(), Round::Up);
                tmplo = -tmplo;
                // DIFFERENCE FROM fmod_pos
                tmplo = tmplo.max(&-halfway.clone());

                let mut tmphi = c;
                tmphi.mul_sub_round(&other.lo(), &self.hi(), Round::Down);
                tmphi = -tmphi;
                // DIFFERENCE FROM fmod_pos
                tmphi = tmphi.min(&halfway);

                Interval::make(tmplo, tmphi, self.err.clone())
            } else {
                // DIFFERENCE FROM fmod_pos
                // NOPE! need to subtract half.bf one way, add it another!
                let y_hi = div_round(
                    &div_round(
                        &self.hi(),
                        &add_round(&c, &bf(self.lo().prec(), 0.5), Round::Down),
                        Round::Down,
                    ),
                    &bf(self.lo().prec(), 2.0),
                    Round::Down,
                );
                let y_lo = sub_round(
                    &self.lo(),
                    &mul_round(&c, &other.hi(), Round::Down),
                    Round::Down,
                )
                .max(&-div_round(
                    &other.hi(),
                    &bf(self.lo().prec(), 2.0),
                    Round::Down,
                ));

                Interval::make(y_lo.min(&-y_hi.clone()), y_hi, self.err.clone())
            }
        } else {
            let y = div_round(&other.hi(), &bf(self.lo().prec(), 2.0), Round::Up);
            Interval::make(
                // DIFFERENCE FROM fmod_pos
                -y.clone(),
                y,
                self.err.clone(),
            )
        }
    }

    pub fn remainder(&self, other: &Interval) -> Interval {
        let zero = bf(self.lo().prec(), 0.0);
        let error = ErrorInterval {
            lo: self.err.lo || other.err.lo || (other.lo() == zero && other.hi() == zero),
            hi: self.err.hi || other.err.hi || (other.lo() <= zero && other.hi() >= zero),
        };

        let abs_other = other.fabs();

        if self.hi() <= zero {
            self.neg().remainder_pos(&abs_other).neg().with_error(error)
        } else if self.lo() >= zero {
            self.remainder_pos(&abs_other).with_error(error)
        } else {
            let (neg, pos) = self.split(&zero).unwrap();
            pos.remainder_pos(&abs_other)
                .union(&neg.neg().remainder_pos(&abs_other).neg())
                .with_error(error)
        }
    }

    pub fn erf(&self) -> Interval {
        self.monotonic_mut(Float::erf_round)
    }

    pub fn erfc(&self) -> Interval {
        self.comonotonic_mut(Float::erfc_round)
    }

    pub fn cmp(&self, other: &Interval) -> (bool, bool, bool, bool) {
        (
            self.lo() < other.hi(),
            self.hi() < other.lo(),
            self.hi() > other.lo(),
            self.lo() > other.hi(),
        )
    }

    pub fn less_than(&self, other: &Interval) -> BooleanInterval {
        let (can_less, must_less, _can_greater, _must_greater) = self.cmp(other);
        BooleanInterval {
            lo: must_less,
            hi: can_less,
            err: self.err.union(&other.err),
        }
    }

    pub fn less_than_or_equal(&self, other: &Interval) -> BooleanInterval {
        let (_can_less, _must_less, can_greater, must_greater) = self.cmp(other);
        BooleanInterval {
            lo: !can_greater,
            hi: !must_greater,
            err: self.err.union(&other.err),
        }
    }

    pub fn greater_than(&self, other: &Interval) -> BooleanInterval {
        let (_can_less, _must_less, can_greater, must_greater) = self.cmp(other);
        BooleanInterval {
            lo: must_greater,
            hi: can_greater,
            err: self.err.union(&other.err),
        }
    }

    pub fn greater_than_or_equal(&self, other: &Interval) -> BooleanInterval {
        let (can_less, must_less, _can_greater, _must_greater) = self.cmp(other);
        BooleanInterval {
            lo: !can_less,
            hi: !must_less,
            err: self.err.union(&other.err),
        }
    }

    pub fn equal_to(&self, other: &Interval) -> BooleanInterval {
        let (can_less, must_less, can_greater, must_greater) = self.cmp(other);
        BooleanInterval {
            lo: !can_less && !can_greater,
            hi: !must_less && !must_greater,
            err: self.err.union(&other.err),
        }
    }

    pub fn not_equal_to(&self, other: &Interval) -> BooleanInterval {
        let (can_less, must_less, can_greater, must_greater) = self.cmp(other);
        BooleanInterval {
            lo: must_less || must_greater,
            hi: can_less || can_greater,
            err: self.err.union(&other.err),
        }
    }

    pub fn fmin(&self, other: &Interval) -> Interval {
        Interval::make(
            self.lo().min(&other.lo()),
            self.hi().min(&other.hi()),
            self.err.union(&other.err),
        )
    }

    pub fn fmax(&self, other: &Interval) -> Interval {
        Interval::make(
            self.lo().max(&other.lo()),
            self.hi().max(&other.hi()),
            self.err.union(&other.err),
        )
    }

    pub fn copysign(&self, other: &Interval) -> Interval {
        let abs = self.fabs();
        let can_neg = other.lo() < 0.0 as f64;
        let can_pos = other.hi() >= 0.0 as f64;
        match (can_neg, can_pos) {
            (true, true) => Interval::make(-abs.hi(), abs.hi(), self.err.union(&other.err)),
            (true, false) => Interval::make(-abs.hi(), -abs.lo(), self.err.union(&other.err)),
            (false, true) => Interval::make(abs.lo(), abs.hi(), self.err.union(&other.err)),
            (false, false) => panic!("Should not be possible to have neither sign"),
        }
    }

    pub fn fdim(&self, other: &Interval) -> Interval {
        self.sub(&other)
            .fmax(&Interval::new(other.lo().prec(), 0.0, 0.0))
    }

    pub fn sort(intervals: Vec<Interval>) -> Vec<Interval> {
        let error = ErrorInterval {
            lo: intervals.iter().any(|ival| ival.err.lo),
            hi: intervals.iter().any(|ival| ival.err.hi),
        };
        let mut upper = intervals
            .iter()
            .map(|ival| ival.hi())
            .collect::<Vec<Float>>();
        upper.sort_by(|a, b| a.partial_cmp(b).unwrap());
        let mut lower = intervals
            .iter()
            .map(|ival| ival.lo())
            .collect::<Vec<Float>>();
        lower.sort_by(|a, b| a.partial_cmp(b).unwrap());
        let mut res = vec![];
        for (hi, lo) in upper.into_iter().zip(lower.into_iter()) {
            res.push(Interval::make(lo, hi, error.clone()));
        }
        res
    }
}

#[cfg(test)]
mod tests {
    use std::f64::NAN;

    use super::*;
    use egg::Symbol;
    use rand;
    use rand::Rng;
    use rug::ops::Pow;
    use rug::{float::Round, ops::*, Float, Rational};

    const NUM_TESTS: usize = 200_000;

    fn rat_to_interval(rat: &Rational, prec: u32) -> Interval {
        let mut lo = Float::with_val(prec, 0.0);
        let mut hi = Float::with_val(prec, 0.0);
        lo.add_from_round(rat, Round::Down);
        hi.add_from_round(rat, Round::Up);
        Interval::make(lo, hi, ErrorInterval {
            lo: false,
            hi: false,
        })
    }

    fn assert_contains(ival: &Interval, val: rug::Float, name: &Symbol) {
        assert!(
            val <= ival.hi(),
            "{}: {} <= {}",
            name,
            val,
            ival.hi()
        );

        assert!(
            val >= ival.lo(),
            "{}: {} >= {}",
            name,
            val,
            ival.lo()
        );
    }

    fn random_error() -> ErrorInterval {
        let mut rng = rand::thread_rng();
        if rng.gen_bool(0.05) {
            ErrorInterval {
                lo: true,
                hi: true,
            }
        } else if rng.gen_bool(0.05) {
            ErrorInterval {
                lo: false,
                hi: true,
            }
        } else {
            ErrorInterval {
                lo: false,
                hi: false,
            }   
        }
    }

    fn random_interval() -> Interval {
        let mut rng = rand::thread_rng();
        let sample_max = 100000000000000000.0;
        // half the time generate constants
        if rng.gen_bool(0.5) {
            let constants = vec![0.0, -1.0, 1.0, 0.5];
            if rng.gen_bool(0.8) {
                let val: f64 = constants[rng.gen_range(0..constants.len())];
                Interval::new(F64_PREC, val, val)
            } else {
                let val: f64 = rng.gen_range(-sample_max..=sample_max);
                Interval::new(F64_PREC, val, val)
            }
        // also generate small intervals
        } else if rng.gen_bool(0.5) {
            let val = rng.gen_range(-sample_max..=sample_max);
            Interval::new(F64_PREC, val, val.next_after(f64::INFINITY))
        } else {
            let lo: f64 = rng.gen_range(-sample_max..=sample_max);
            let hi = rng.gen_range(sample_max..=sample_max);
            Interval::new(F64_PREC, lo, hi)
        }
    }

    #[test]
    fn test_pos_fractional_power() {
        let one_third = rat_to_interval(&Rational::from((1, 3)), F64_PREC);
        let mut rng = rand::thread_rng();

        for _i in 0..NUM_TESTS {
            let ival1 = random_interval();

            let upper = if rng.gen_bool(0.8) {
                random_interval().union(&one_third)
            } else {
                one_third.clone()
            };
            let y = ival1.pow(&upper);
            let realval1 = rng.gen_range(ival1.lo().to_f64()..=ival1.hi().to_f64());
            assert_contains(&y, bf(F64_PREC, realval1.cbrt()), &"pow_cbrt".into());
            assert!(!y.err.lo);
            if ival1.lo() < 0.0 {
                assert!(y.err.hi);
            }
        }
    }

    #[test]
    fn smaller_intervals_refine() {
        type Operator = fn(&Interval, &Interval) -> Interval;
        type SingleOperator = fn(&Interval) -> Interval;
        type FOperator = fn(Float, &Float) -> Float;
        type SingleFOperator = fn(Float) -> Float;
        type FloatToBool = fn(&Interval, &Interval) -> BooleanInterval;
        //type SingleFloatToBool = fn(&Interval) -> BooleanInterval;
        type FFloatToBool = fn(Float, &Float) -> bool;
        //type SingleFFloatToBool = fn(Float) -> bool;

        let interval_functions: Vec<(Symbol, Operator, FOperator)> = vec![
            ("add".into(), Interval::add, |x, y| x + y),
            ("sub".into(), Interval::sub, |x, y| x - y),
            ("mul".into(), Interval::mul, |x, y| x * y),
            ("div".into(), Interval::div, |x, y| x / y),
            ("hypot".into(), Interval::hypot, |x, y| x.hypot(y)),
            ("pow".into(), Interval::pow, |x, y| {
                if x.is_zero() && y.is_zero() {
                    Float::with_val(F64_PREC, NAN)
                } else {
                    x.pow(y)
                }
            }),
            ("atan2".into(), Interval::atan2, |x, y| {
                if x.is_zero() && y.is_zero() {
                    Float::with_val(F64_PREC, NAN)
                } else {
                    x.atan2(y)
                }
            }),
            ("fmod".into(), Interval::fmod, |x, y| x % y),
            ("remainder".into(), Interval::remainder, |x, y| {
                x.remainder(y)
            }),
            ("fmin".into(), Interval::fmin, |x, y| x.min(y)),
            ("fmax".into(), Interval::fmax, |x, y| x.max(y)),
            ("copysign".into(), Interval::copysign, |x, y| x.copysign(y)),
        ];
        let to_boolean_functions: Vec<(Symbol, FloatToBool, FFloatToBool)> = vec![
            ("less_than".into(), Interval::less_than, |x, y| &x < y),
            (
                "less_than_or_equal".into(),
                Interval::less_than_or_equal,
                |x, y| &x <= y,
            ),
            ("greater_than".into(), Interval::greater_than, |x, y| &x > y),
            (
                "greater_than_or_equal".into(),
                Interval::greater_than_or_equal,
                |x, y| &x >= y,
            ),
            ("equal_to".into(), Interval::equal_to, |x, y| &x == y),
        ];
        let single_operand_functions: Vec<(Symbol, SingleOperator, SingleFOperator)> = vec![
            ("round".into(), Interval::round, |x| x.round()),
            ("ceil".into(), Interval::ceil, Float::ceil),
            ("floor".into(), Interval::floor, Float::floor),
            ("trunc".into(), Interval::trunc, Float::trunc),
            ("fabs".into(), Interval::fabs, Float::abs),
            ("sqrt".into(), Interval::sqrt, Float::sqrt),
            ("exp".into(), Interval::exp, Float::exp),
            ("exp_m1".into(), Interval::exp_m1, Float::exp_m1),
            ("exp2".into(), Interval::exp2, Float::exp2),
            ("ln".into(), Interval::ln, Float::ln),
            ("ln_1p".into(), Interval::ln_1p, Float::ln_1p),
            ("log2".into(), Interval::log2, Float::log2),
            ("log10".into(), Interval::log10, Float::log10),
            ("cbrt".into(), Interval::cbrt, Float::cbrt),
            ("sin".into(), Interval::sin, Float::sin),
            ("cos".into(), Interval::cos, Float::cos),
            ("tan".into(), Interval::tan, Float::tan),
            ("asin".into(), Interval::asin, Float::asin),
            ("acos".into(), Interval::acos, Float::acos),
            ("atan".into(), Interval::atan, Float::atan),
            ("sinh".into(), Interval::sinh, Float::sinh),
            ("cosh".into(), Interval::cosh, Float::cosh),
            ("tanh".into(), Interval::tanh, Float::tanh),
            ("asinh".into(), Interval::asinh, Float::asinh),
            ("acosh".into(), Interval::acosh, Float::acosh),
            ("atanh".into(), Interval::atanh, Float::atanh),
            ("erf".into(), Interval::erf, Float::erf),
            ("erfc".into(), Interval::erfc, Float::erfc),
        ];
        let mut rng = rand::thread_rng();
            
        for _i in 0..NUM_TESTS {
            for (name, ifun, realfun) in &to_boolean_functions {
                let ival1 = random_interval();
                let ival2 = random_interval();

                let realval1 = rng.gen_range(ival1.lo().to_f64()..=ival1.hi().to_f64());
                let realval2 = rng.gen_range(ival2.lo().to_f64()..=ival2.hi().to_f64());
                let finalival = ifun(&ival1, &ival2);
                let finalreal = realfun(bf(F64_PREC, realval1), &bf(F64_PREC, realval2));

                assert!(finalival.is_valid());
                if finalreal {
                    assert!(
                        finalival.hi,
                        "Should have a possibility of true value for {}({:?} {:?})",
                        name, ival1, ival2
                    );
                } else {
                    assert!(!finalival.lo);
                }
            }

            // todo collapse duplicated code into one function, using this as template
            for (name, ifun, realfun) in &interval_functions {
                let ival1 = random_interval().with_error(random_error());
                let ival2 = random_interval().with_error(random_error());

                let realval1 = rng.gen_range(ival1.lo().to_f64()..=ival1.hi().to_f64());
                let realval2 = rng.gen_range(ival2.lo().to_f64()..=ival2.hi().to_f64());
                let finalival = ifun(&ival1, &ival2);
                let finalreal = realfun(bf(F64_PREC, realval1), &bf(F64_PREC, realval2));

                assert!(finalival.is_valid(), "{:?} and {:?} resulted in invalid interval for {}: {:?}", ival1, ival2, name, finalival);

                if ival1.err.is_possible() || ival2.err.is_possible() {
                    assert!(finalival.err.is_possible(),
                            "{:?} and {:?} resulted in no possible error for {}. Got: {:?}",
                            ival1, ival2, name, finalival);
                }

                if ival1.err.is_guaranteed() || ival2.err.is_guaranteed() {
                    assert!(finalival.err.is_guaranteed());
                } else if finalreal.is_nan() {
                    assert!(
                        finalival.err.hi,
                        "{:?} and {:?} resulted in invalid value for {}. Expected possible error.",
                        ival1, ival2, name
                    );
                } else {
                    if finalreal.is_finite() {
                        assert!(
                            !finalival.err.lo,
                            "{:?} and {:?} gave us a guaranteed error for {}. Got: {}",
                            ival1, ival2, name, finalreal
                        );
                    }
                    assert_contains(&finalival, finalreal, name);
                }
            }

            for (name, ifun, realfun) in &single_operand_functions {
                let ival1 = random_interval();
                let realval1 = rng.gen_range(ival1.lo().to_f64()..=ival1.hi().to_f64());
                let finalival = ifun(&ival1);
                let finalreal = realfun(bf(F64_PREC, realval1));

                assert!(finalival.is_valid(), "{:?} resulted in invalid interval for {}: {:?}", ival1, name, finalival);
                if finalreal.is_nan() {
                    assert!(finalival.err.hi, "Should have a possibility of error for {}({:?})", name, ival1);
                } else {
                    assert_contains(&finalival, finalreal, name);
                }
            }
        }
    }
}
