use std::fmt::Write;
use std::ops::{Range, RangeInclusive};

use crate::{traits::BoxGenIter, Float, Generator};

const SMALL_COEFF_MAX: i32 = 10_000;
const SMALL_EXP_MAX: i32 = 301;

const SMALL_COEFF_RANGE: RangeInclusive<i32> = (-SMALL_COEFF_MAX)..=SMALL_COEFF_MAX;
const SMALL_EXP_RANGE: RangeInclusive<i32> = (-SMALL_EXP_MAX)..=SMALL_EXP_MAX;

const LARGE_COEFF_MAX: u32 = 100_000;
const LARGE_EXP_RANGE: Range<u32> = 300..350;

pub struct SmallExponents<F: Float> {
    iter: BoxGenIter<Self, F>,
}

impl<F: Float> Generator<F> for SmallExponents<F> {
    const NAME: &'static str = "small exponents";
    const SHORT_NAME: &'static str = "small exp";

    /// `(coefficient, exponent)`
    type WriteCtx = (i32, i32);

    fn total_tests() -> u64 {
        ((SMALL_COEFF_RANGE.end() - SMALL_COEFF_RANGE.start())
            * (SMALL_EXP_RANGE.end() - SMALL_EXP_RANGE.start())
            * 2)
        .try_into()
        .unwrap()
    }

    fn new() -> Self {
        let iter = SMALL_EXP_RANGE
            .flat_map(|exp| SMALL_COEFF_RANGE.map(move |coeff| (coeff, exp)))
            // If it ends in zeros, the parser will strip those (and adjust the exponent),
            // which almost always (except for exponents near +/- 300) result in an input
            // equivalent to something we already generate in a different way.
            .filter(|(coeff, _exp)| coeff % 10 != 0);

        Self {
            iter: Box::new(iter),
        }
    }

    fn write_string(s: &mut String, ctx: Self::WriteCtx) {
        let (coeff, exp) = ctx;
        write!(s, "{coeff}e{exp}").unwrap();
    }
}

impl<F: Float> Iterator for SmallExponents<F> {
    type Item = (i32, i32);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}

pub struct LargeExponents<F: Float> {
    iter: BoxGenIter<Self, F>,
}

impl<F: Float> Generator<F> for LargeExponents<F> {
    const NAME: &'static str = "large positive exponents";
    const SHORT_NAME: &'static str = "large +exp";

    /// `(coefficient, exponent, is_negative)`
    type WriteCtx = (u32, u32);

    fn total_tests() -> u64 {
        ((LARGE_COEFF_MAX - 1) * (LARGE_EXP_RANGE.end + 1 - LARGE_EXP_RANGE.start) * 2).into()
    }

    fn new() -> Self {
        let iter =
            LARGE_EXP_RANGE.flat_map(|exp| (0..LARGE_COEFF_MAX).map(move |coeff| (exp, coeff)));

        Self {
            iter: Box::new(iter),
        }
    }

    fn write_string(s: &mut String, ctx: Self::WriteCtx) {
        let (coeff, exp) = ctx;
        write!(s, "{coeff}e{exp}").unwrap();
    }
}

impl<F: Float> Iterator for LargeExponents<F> {
    type Item = (u32, u32);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}

pub struct LargeNegExponents<F: Float>(LargeExponents<F>);

impl<F: Float> Generator<F> for LargeNegExponents<F> {
    const NAME: &'static str = "large negative exponents";
    const SHORT_NAME: &'static str = "large -exp";

    /// `(coefficient, exponent, is_negative)`
    type WriteCtx = (u32, u32);

    fn total_tests() -> u64 {
        ((LARGE_COEFF_MAX - 1) * (LARGE_EXP_RANGE.end + 1 - LARGE_EXP_RANGE.start) * 2).into()
    }

    fn new() -> Self {
        Self(LargeExponents::new())
    }

    fn write_string(s: &mut String, ctx: Self::WriteCtx) {
        let (coeff, exp) = ctx;
        write!(s, "{coeff}e-{exp}").unwrap();
    }
}

impl<F: Float> Iterator for LargeNegExponents<F> {
    type Item = <Self as Generator<F>>::WriteCtx;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.iter.next()
    }
}
