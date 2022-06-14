use core::cmp::Ordering;
use rug::{float::Constant, float::Round, float::Special, ops::DivAssignRound, Float};
use egg::Symbol;

enum IntervalClassification {
    StrictlyPos,
    StrictlyNeg,
    Mixed,
}

#[derive(Debug, Clone)]
pub struct ErrorInterval {
    pub lo: bool,
    pub hi: bool,
}

impl ErrorInterval {
    pub fn union(&self, other: &ErrorInterval) -> ErrorInterval {
        ErrorInterval {
            lo: self.lo || other.lo,
            hi: self.hi || other.hi,
        }
    }
}

#[derive(Debug, Clone)]
pub struct BooleanInterval {
    pub lo: bool,
    pub hi: bool,
    pub err: ErrorInterval,
}

#[derive(Debug, Clone)]
pub struct Interval {
    pub lo: Float,
    pub hi: Float,
    pub err: ErrorInterval,
}

pub(crate) fn is_even(val: Float) -> bool {
    if let Some(int) = val.to_integer() {
        int.is_even()
    } else {
        false
    }
}

pub(crate) fn is_odd(val: Float) -> bool {
    if let Some(int) = val.to_integer() {
        int.is_odd()
    } else {
        false
    }
}

fn classify_interval(interval: &Interval) -> IntervalClassification {
    if interval.lo > 0 {
        IntervalClassification::StrictlyPos
    } else if interval.hi < 0 {
        IntervalClassification::StrictlyNeg
    } else {
        IntervalClassification::Mixed
    }
}

impl Interval {
    pub fn new(prec: u32, lo: f64, hi: f64) -> Interval {
        Interval {
            lo: Float::with_val(prec, lo),
            hi: Float::with_val(prec, hi),
            err: ErrorInterval {
                lo: false,
                hi: false,
            },
        }
    }

    pub fn union(&self, other: &Interval) -> Interval {
        Interval {
            lo: self.lo.clone().min(&other.lo),
            hi: self.hi.clone().max(&other.hi),
            err: self.err.union(&other.err),
        }
    }

    pub fn neg(&self) -> Interval {
        Interval {
            lo: -self.hi.clone(),
            hi: -self.lo.clone(),
            err: self.err.clone(),
        }
    }

    pub fn add(&self, other: &Interval) -> Interval {
        let mut lo = self.lo.clone();
        lo.mul_add_round(&Float::with_val(other.lo.prec(), 1), &other.lo, Round::Down);
        let mut hi = self.hi.clone();
        hi.mul_add_round(&Float::with_val(other.lo.prec(), 1), &other.hi, Round::Up);
        Interval {
            lo,
            hi,
            err: self.err.union(&other.err),
        }
    }

    pub fn sub(&self, other: &Interval) -> Interval {
        let mut lo = self.lo.clone();
        lo.mul_sub_round(&Float::with_val(other.lo.prec(), 1), &other.hi, Round::Down);
        let mut hi = self.hi.clone();
        hi.mul_sub_round(&Float::with_val(other.lo.prec(), 1), &other.lo, Round::Up);
        Interval {
            lo,
            hi,
            err: self.err.union(&other.err),
        }
    }

    pub fn mul(&self, other: &Interval) -> Interval {
        let perform_mult = |lo1: &Float, lo2: &Float, hi1: &Float, hi2: &Float| {
            let mut lo = lo1.clone();
            lo.mul_add_round(lo2, &Float::with_val(lo1.prec(), 0), Round::Down);
            let mut hi = hi1.clone();
            hi.mul_add_round(&hi2, &Float::with_val(hi1.prec(), 0), Round::Up);
            Interval {
                lo,
                hi,
                err: self.err.union(&other.err),
            }
        };

        use IntervalClassification::*;
        match (classify_interval(&self), classify_interval(&other)) {
            (StrictlyPos, StrictlyPos) => perform_mult(&self.lo, &other.lo, &self.hi, &other.hi),

            (StrictlyPos, StrictlyNeg) => perform_mult(&self.hi, &other.lo, &self.lo, &other.hi),

            (StrictlyPos, Mixed) => perform_mult(&self.hi, &other.lo, &self.hi, &other.hi),

            (StrictlyNeg, Mixed) => perform_mult(&self.lo, &other.hi, &self.lo, &other.lo),

            (StrictlyNeg, StrictlyPos) => perform_mult(&self.lo, &other.hi, &self.hi, &other.lo),

            (StrictlyNeg, StrictlyNeg) => perform_mult(&self.hi, &other.hi, &self.lo, &other.lo),

            (Mixed, StrictlyPos) => perform_mult(&self.lo, &other.hi, &self.hi, &other.hi),

            (Mixed, StrictlyNeg) => perform_mult(&self.hi, &other.lo, &self.lo, &other.lo),

            (Mixed, Mixed) => perform_mult(&self.hi, &other.lo, &self.lo, &other.lo)
                .union(&perform_mult(&self.lo, &other.hi, &self.hi, &other.hi)),
        }
    }

    pub fn get_const(&self) -> Option<Float> {
        if self.lo == self.hi {
            Some(self.lo.clone())
        } else {
            None
        }
    }

    pub fn div(&self, other: &Interval) -> Interval {
        let zero = Float::with_val(other.lo.prec(), 0);
        let error = ErrorInterval {
            lo: self.err.lo || other.err.lo || (other.lo <= zero && other.hi >= zero),
            hi: self.err.hi || other.err.hi || other.get_const() == Some(zero),
        };

        let perform_div = |lo1: &Float, lo2: &Float, hi1: &Float, hi2: &Float| {
            let mut lo = lo1.clone();
            lo.div_assign_round(lo2, Round::Down);
            let mut hi = hi1.clone();
            hi.div_assign_round(hi2, Round::Up);

            Interval {
                lo,
                hi,
                err: self.err.union(&other.err),
            }
        };

        use IntervalClassification::*;
        match (classify_interval(&self), classify_interval(&other)) {
            (_any, Mixed) => Interval {
                lo: Float::with_val(self.lo.prec(), std::f64::NEG_INFINITY),
                hi: Float::with_val(self.lo.prec(), std::f64::INFINITY),
                err: error,
            },
            (StrictlyPos, StrictlyPos) => perform_div(&self.lo, &other.hi, &self.hi, &other.lo),
            (StrictlyPos, StrictlyNeg) => perform_div(&self.hi, &other.hi, &self.lo, &other.lo),
            (StrictlyNeg, StrictlyPos) => perform_div(&self.lo, &other.lo, &self.hi, &other.hi),
            (StrictlyNeg, StrictlyNeg) => perform_div(&self.hi, &other.lo, &self.lo, &other.hi),
            (Mixed, StrictlyPos) => perform_div(&self.lo, &other.lo, &self.hi, &other.lo),
            (Mixed, StrictlyNeg) => perform_div(&self.hi, &other.hi, &self.lo, &other.hi),
        }
    }

    fn monotonic(&self, fun: fn(&Float, Round) -> Float) -> Interval {
        let tmplo = fun(&self.lo, Round::Down);
        let tmphi = fun(&self.hi, Round::Up);
        if tmplo.is_nan() {
            panic!("monotonic: lo is NaN");
        }
        if tmphi.is_nan() {
            panic!("monotonic: hi is NaN");
        }
        Interval {
            lo: tmplo,
            hi: tmphi,
            err: self.err.clone(),
        }
    }

    fn monotonic_mut(&self, fun: fn(&mut Float, Round) -> std::cmp::Ordering) -> Interval {
        let mut tmplo = self.lo.clone();
        let mut tmphi = self.hi.clone();
        fun(&mut tmplo, Round::Down);
        fun(&mut tmphi, Round::Up);
        if tmplo.is_nan() {
            panic!("monotonic_mut: lo is NaN");
        }
        if tmphi.is_nan() {
            panic!("monotonic_mut: hi is NaN");
        }
        Interval {
            lo: tmplo,
            hi: tmphi,
            err: self.err.clone(),
        }
    }

    fn clamp(&self, lo: &Float, hi: &Float) -> Interval {
        Interval {
            lo: self.lo.clone().max(lo).min(hi),
            hi: self.hi.clone().min(hi).max(lo),
            err: ErrorInterval {
                lo: self.err.lo || &self.lo < lo || &self.hi > hi,
                hi: self.err.hi || &self.hi < lo || &self.lo > hi,
            },   
        }
    }

    pub fn round_nearest_int(&self) -> Interval {
        Interval {
            lo: self.lo.clone().round(),
            hi: self.hi.clone().round(),
            err: self.err.clone(),
        }
    }

    pub fn ceil(&self) -> Interval {
        self.monotonic(|x, _| x.clone().ceil())
    }

    pub fn floor(&self) -> Interval {
        self.monotonic(|x, _| x.clone().floor())
    }

    pub fn trunc(&self) -> Interval {
        self.monotonic(|x, _| x.clone().trunc())
    }

    pub fn fabs(&self) -> Interval {
        let zero = Float::with_val(self.lo.prec(), 0);
        if self.lo >= zero {
            self.clone()
        } else if self.hi <= zero {
            self.neg()
        } else {
            Interval {
                lo: zero,
                hi: self.hi.clone().max(&-self.lo.clone()),
                err: self.err.clone(),
            }
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
        self.clamp(&Float::with_val(self.lo.prec(), 0), &Float::with_val(self.lo.prec(), f64::INFINITY)).monotonic_mut(Float::ln_round)
    }

    pub fn log10(&self) -> Interval {
        self.clamp(&Float::with_val(self.lo.prec(), 0), &Float::with_val(self.lo.prec(), f64::INFINITY)).monotonic_mut(Float::log10_round)
    }

    pub fn log2(&self) -> Interval {
        self.clamp(&Float::with_val(self.lo.prec(), 0), &Float::with_val(self.lo.prec(), f64::INFINITY)).monotonic_mut(Float::log2_round)
    }

    pub fn ln_1p(&self) -> Interval {
        self.clamp(&Float::with_val(self.lo.prec(), -1), &Float::with_val(self.lo.prec(), f64::INFINITY)).monotonic_mut(Float::ln_1p_round)
    }

    pub fn sqrt(&self) -> Interval {
        self.clamp(&Float::with_val(self.lo.prec(), 0), &Float::with_val(self.lo.prec(), f64::INFINITY)).monotonic_mut(Float::sqrt_round)
    }

    pub fn cbrt(&self) -> Interval {
        self.monotonic_mut(Float::cbrt_round)
    }

    pub fn hypot(&self, other: &Interval) -> Interval {
        let self_pos = self.fabs();
        let other_pos = other.fabs();
        let mut tmp_lo = self_pos.lo.clone();
        tmp_lo.hypot_round(&other_pos.lo, Round::Down);
        let mut tmp_hi = self_pos.hi.clone();
        tmp_hi.hypot_round(&other_pos.hi, Round::Up);
        Interval {
            lo: tmp_lo,
            hi: tmp_hi,
            err: self.err.union(&other.err),
        }

    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rand;
    use rand::Rng;
    use std::ops::Add;

    #[test]
    fn smaller_intervals_refine() {
        type Operator = fn(&Interval, &Interval) -> Interval;
        type SingleOperator = fn(&Interval) -> Interval;
        type FOperator = fn(f64, f64) -> f64;
        type SingleFOperator = fn(f64) -> f64;
        let interval_functions: Vec<(Symbol, Operator, FOperator)> = vec![
            ("add".into(), Interval::add, std::ops::Add::add),
            ("sub".into(), Interval::sub, std::ops::Sub::sub),
            ("mul".into(), Interval::mul, std::ops::Mul::mul),
            ("div".into(), Interval::div, std::ops::Div::div),
            ("hypot".into(), Interval::hypot, |x, y| x.hypot(y)),
        ];
        let single_operand_functions: Vec<(Symbol, SingleOperator, SingleFOperator)> = vec![
            ("round_nearest_int".into(), Interval::round_nearest_int, |x| x.round()),
            ("ceil".into(), Interval::ceil, |x| x.ceil()),
            ("floor".into(), Interval::floor, |x| x.floor()),
            ("trunc".into(), Interval::trunc, |x| x.trunc()),
            ("fabs".into(), Interval::fabs, |x| x.abs()),
            ("exp".into(), Interval::exp, |x| x.exp()),
            ("exp_m1".into(), Interval::exp_m1, |x| x.exp_m1()),
            ("exp2".into(), Interval::exp2, |x| x.exp2()),
            ("ln".into(), Interval::ln, |x| x.ln()),
            ("log10".into(), Interval::log10, |x| x.log10()),
            ("log2".into(), Interval::log2, |x| x.log2()),
            ("ln_1p".into(), Interval::ln_1p, |x| x.ln_1p()),
            ("sqrt".into(), Interval::sqrt, |x| x.sqrt()),
            ("cbrt".into(), Interval::cbrt, |x| x.cbrt()),
        ];
        let mut rng = rand::thread_rng();

        for _i in 0..10000 {
            for (name, ifun, realfun) in &interval_functions {
                let lo1 = rng.gen_range(-40.0..40.0);
                let lo2 = rng.gen_range(lo1..41.0);
                let ival1 = Interval::new(53, lo1, lo2);
                let hi1 = rng.gen_range(-40.0..40.0);
                let hi2 = rng.gen_range(hi1..41.0);
                let ival2 = Interval::new(53, hi1, hi2);

                let realval1 = rng.gen_range(lo1..lo2);
                let realval2 = rng.gen_range(hi1..hi2);
                let finalival = ifun(&ival1, &ival2);
                let finalreal = realfun(realval1, realval2);

                if finalreal.is_nan() {
                    assert!(finalival.err.lo);
                } else {
                    assert!(
                        finalreal <= finalival.hi.clone(),
                        "{}: {} <= {}",
                        name,
                        finalreal,
                        finalival.hi
                    );
                    assert!(finalreal >= finalival.lo.clone(),
                            "{} >= {}",
                            finalreal,
                            finalival.lo);
                }
            }

            for (name, ifun, realfun) in &single_operand_functions {
                let lo1 = rng.gen_range(-40.0..40.0);
                let hi1 = rng.gen_range(lo1..41.0);
                let ival1 = Interval::new(53, lo1, hi1);
                let realval1 = rng.gen_range(lo1..hi1);
                let finalival = ifun(&ival1);
                let finalreal = realfun(realval1);

                if finalreal.is_nan() {
                    println!("fun: {}", name);
                    println!("lo1: {} hi1: {} realval1: {}", lo1, hi1, realval1);
                    assert!(finalival.err.lo);
                } else {
                    assert!(
                        finalreal <= finalival.hi.clone(),
                        "{}: {} <= {}",
                        name,
                        finalreal,
                        finalival.hi
                    );
                    assert!(finalreal >= finalival.lo.clone(),
                            "{}: {} >= {}",
                            name,
                            finalreal,
                            finalival.lo);
                }
            }
        }
    }
}
