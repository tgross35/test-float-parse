use std::ops::{Range, RangeInclusive};

use crate::{Float, Generator};

const SMALL_COEFF_MAX: i32 = 10_000;
const SMALL_EXP_MAX: i32 = 301;

const SMALL_COEFF_RANGE: RangeInclusive<i32> = (-SMALL_COEFF_MAX)..=SMALL_COEFF_MAX;
const SMALL_EXP_RANGE: RangeInclusive<i32> = (-SMALL_EXP_MAX)..=SMALL_EXP_MAX;

const LARGE_COEFF_MAX: u32 = 100_000;
const LARGE_EXP_RANGE: Range<u32> = 300..350;

pub struct SmallExponents {
    iter: Box<dyn Iterator<Item = String>>,
    /// Even though the iterator allocates, we still need the internal buffer
    /// to meet the function signature.
    buf: String,
}

impl<F: Float> Generator<F> for SmallExponents {
    const NAME: &'static str = "small exponents";
    const SHORT_NAME: &'static str = "small exp";

    fn estimated_tests() -> u64 {
        ((SMALL_COEFF_RANGE.end() - SMALL_COEFF_RANGE.start())
            * (SMALL_EXP_RANGE.end() - SMALL_EXP_RANGE.start())
            * 2)
        .try_into()
        .unwrap()
    }

    fn new() -> Self {
        let iter = SMALL_EXP_RANGE
            .flat_map(|exp| SMALL_COEFF_RANGE.map(move |coeff| (exp, coeff)))
            // If it ends in zeros, the parser will strip those (and adjust the exponent),
            // which almost always (except for exponents near +/- 300) result in an input
            // equivalent to something we already generate in a different way.
            .filter(|(exp, coeff)| coeff % 10 != 0)
            .flat_map(|(exp, coeff)| [format!("{coeff}e{exp}"), format!("{coeff}e-{exp}")]);

        Self {
            iter: Box::new(iter),
            buf: String::new(),
        }
    }

    fn next<'a>(&'a mut self) -> Option<&'a str> {
        self.iter.next().map(|s| {
            self.buf = s;
            self.buf.as_str()
        })
    }
}

pub struct LargeExponents {
    iter: Box<dyn Iterator<Item = String>>,
    /// Even though the iterator allocates, we still need the internal buffer
    /// to meet the function signature.
    buf: String,
}

impl<F: Float> Generator<F> for LargeExponents {
    const NAME: &'static str = "large positive exponents";
    const SHORT_NAME: &'static str = "large +exp";

    fn estimated_tests() -> u64 {
        ((LARGE_COEFF_MAX - 1) * (LARGE_EXP_RANGE.end + 1 - LARGE_EXP_RANGE.start) * 2).into()
    }

    fn new() -> Self {
        let iter = LARGE_EXP_RANGE
            .flat_map(|exp| (0..LARGE_COEFF_MAX).map(move |coeff| (exp, coeff)))
            .flat_map(|(exp, coeff)| [format!("{coeff}e{exp}"), format!("{coeff}e-{exp}")]);

        Self {
            iter: Box::new(iter),
            buf: String::new(),
        }
    }

    fn next<'a>(&'a mut self) -> Option<&'a str> {
        self.iter.next().map(|s| {
            self.buf = s;
            self.buf.as_str()
        })
    }
}

pub struct LargeNegExponents {
    iter: Box<dyn Iterator<Item = String>>,
    /// Even though the iterator allocates, we still need the internal buffer
    /// to meet the function signature.
    buf: String,
}

impl<F: Float> Generator<F> for LargeNegExponents {
    const NAME: &'static str = "large negative exponents";
    const SHORT_NAME: &'static str = "large -exp";

    fn estimated_tests() -> u64 {
        ((LARGE_COEFF_MAX - 1) * (LARGE_EXP_RANGE.end + 1 - LARGE_EXP_RANGE.start) * 2).into()
    }

    fn new() -> Self {
        let iter = LARGE_EXP_RANGE
            .flat_map(|exp| (0..LARGE_COEFF_MAX).map(move |coeff| (exp, coeff)))
            .flat_map(|(exp, coeff)| [format!("{coeff}e{exp}"), format!("{coeff}e-{exp}")]);

        Self {
            iter: Box::new(iter),
            buf: String::new(),
        }
    }

    fn next<'a>(&'a mut self) -> Option<&'a str> {
        self.iter.next().map(|s| {
            self.buf = s;
            self.buf.as_str()
        })
    }
}
