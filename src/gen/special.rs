use std::fmt::Write;

use crate::traits::{Float, Generator};

const S: &[&str] = &[
    "inf", "Inf", "iNf", "INF", "-inf", "-Inf", "-iNf", "-INF", "+inf", "+Inf", "+iNf", "+INF",
    "nan", "NaN", "NAN", "nAn", "-nan", "-NaN", "-NAN", "-nAn", "+nan", "+NaN", "+NAN", "+nAn",
    "1", "-1", "+1", "1e1", "-1e1", "+1e1", "1e-1", "-1e-1", "+1e-1", "1e+1", "-1e+1", "+1e+1",
    "0", "-0", "+0",
];

pub struct Special {
    iter: std::slice::Iter<'static, &'static str>,
}

impl<F: Float> Generator<F> for Special {
    const NAME: &'static str = "special values";

    const SHORT_NAME: &'static str = "special";

    const PATTERNS_CONTAIN_NAN: bool = true;

    type WriteCtx = &'static str;

    fn total_tests() -> u64 {
        S.len().try_into().unwrap()
    }

    fn new() -> Self {
        Self { iter: S.iter() }
    }

    fn write_string(s: &mut String, ctx: Self::WriteCtx) {
        s.write_str(ctx).unwrap();
    }
}

impl Iterator for Special {
    type Item = &'static str;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|p| *p)
    }
}
