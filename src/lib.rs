use rug::{Float, float::Special, float::Constant, float::Round};

enum IntervalClassification {
    StrictlyPos,
    StrictlyNeg,
    Mixed
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

        match (classify_interval(&self), classify_interval(&other)) {
            (IntervalClassification::StrictlyPos, IntervalClassification::StrictlyPos) => {
                perform_mult(&self.lo, &other.lo, &self.hi, &other.hi)
            }

            (IntervalClassification::StrictlyPos, IntervalClassification::StrictlyNeg) => {
                perform_mult(&self.hi, &other.lo, &self.lo, &other.hi)
            }

            (IntervalClassification::StrictlyPos, IntervalClassification::Mixed) => {
                perform_mult(&self.hi, &other.lo, &self.hi, &other.hi)
            }

            (IntervalClassification::StrictlyNeg, IntervalClassification::Mixed) => {
                perform_mult(&self.lo, &other.hi, &self.lo, &other.lo)
            }

            (IntervalClassification::StrictlyNeg, IntervalClassification::StrictlyPos) => {
                perform_mult(&self.lo, &other.hi, &self.hi, &other.lo)
            }

            (IntervalClassification::StrictlyNeg, IntervalClassification::StrictlyNeg) => {
                perform_mult(&self.hi, &other.hi, &self.lo, &other.lo)
            }

            (IntervalClassification::Mixed, IntervalClassification::StrictlyPos) => {
                perform_mult(&self.lo, &other.hi, &self.hi, &other.hi)
            }

            (IntervalClassification::Mixed, IntervalClassification::StrictlyNeg) => {
                perform_mult(&self.hi, &other.lo, &self.lo, &other.lo)
            }

            (IntervalClassification::Mixed, IntervalClassification::Mixed) => {
                perform_mult(&self.hi, &other.lo, &self.lo, &other.lo).union(
                    &perform_mult(&self.lo, &other.hi, &self.hi, &other.hi))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rug::Float;
    use std::ops::Add;
    use rand;
    use rand::Rng;

    #[test]
    fn smaller_intervals_refine() {
        let interval_functions = vec![
            (Interval::add, Add::add),
        ];
        let mut rng = rand::thread_rng();

        for i in 0..10000 {
            for (ifun, realfun) in &interval_functions {
                let lo1 = rng.gen_range(-40.0..40.0);
                let lo2 = rng.gen_range(lo1..40.0);
                let ival1 = Interval::new(53, lo1, lo2);
                let hi1 = rng.gen_range(-40.0..40.0);
                let hi2 = rng.gen_range(hi1..40.0);
                let ival2 = Interval::new(53, hi1, hi2);

                let realval1 = rng.gen_range(lo1..lo2);
                let realval2 = rng.gen_range(hi1..hi2);
                let finalival = ifun(&ival1, &ival2);
                let finalreal = realfun(realval1, realval2);

                assert!(finalreal <= finalival.hi.clone(), "{} <= {}", finalreal, finalival.hi);
                assert!(finalreal >= finalival.lo.clone());
            }

        }
    }
}
