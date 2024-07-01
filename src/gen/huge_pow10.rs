use std::ops::Range;

use crate::{Float, Generator};

/// Skip e = 0 because small-u32 already does those.
const COEFF_MAX: u32 = 100_000;
const EXP_RANGE: Range<u32> = 300..310;

pub struct LargeExponents {
    iter: Box<dyn Iterator<Item = String>>,
    /// Even though the iterator allocates, we still need the internal buffer
    /// to meet the function signature.
    buf: String,
}

impl<F: Float> Generator<F> for LargeExponents {
    const NAME: &'static str = "short decimals";
    const SHORT_NAME: &'static str = "short dec";

    fn estimated_tests() -> u64 {
        ((COEFF_MAX - 1) * (EXP_RANGE.end - EXP_RANGE.start) * 2).into()
    }

    fn new() -> Self {
        let iter = EXP_RANGE
            .flat_map(|exp| (0..COEFF_MAX).map(move |coeff| (exp, coeff)))
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
