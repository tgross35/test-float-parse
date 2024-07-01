use std::{
    fmt::Write,
    ops::{Range, RangeInclusive},
    sync::Arc,
};

use crate::{Float, Generator, Int};

const MAX_POW2: u32 = 19;
const RANGE_ITER: RangeInclusive<i32> = {
    let max = 1i32 << MAX_POW2;
    (-max)..=max
};

/// Test all integers up to 2 ^ MAX_POW2
pub struct SmallInt {
    iter: RangeInclusive<i32>,
    buf: String,
}

impl<F: Float> Generator<F> for SmallInt {
    const NAME: &'static str = "small integer values";
    const SHORT_NAME: &'static str = "int small";

    fn estimated_tests() -> u64 {
        (RANGE_ITER.end() + 1 - RANGE_ITER.start())
            .try_into()
            .unwrap()
    }

    fn new() -> Self {
        Self {
            iter: RANGE_ITER,
            buf: String::new(),
        }
    }

    fn next<'a>(&'a mut self) -> Option<&'a str> {
        let num = self.iter.next()?;
        self.buf.clear();
        write!(self.buf, "{num}");
        Some(self.buf.as_str())
    }
}
