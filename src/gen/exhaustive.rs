use std::ops::RangeInclusive;

use crate::{update_buf_from_bits, Float, Generator, Int, SEED};

/// Test every possible bit pattern. Not recommended for anything larger than `f32`!
pub struct Exhaustive<F: Float> {
    iter: RangeInclusive<F::Int>,
    buf: String,
}

impl<F: Float> Generator<F> for Exhaustive<F>
where
    RangeInclusive<F::Int>: Iterator<Item = F::Int>,
{
    const NAME: &'static str = "exhaustive";
    const SHORT_NAME: &'static str = "exhaustive";

    fn estimated_tests() -> u64 {
        F::Int::MAX.try_into().unwrap()
    }

    fn new() -> Self {
        Self {
            iter: F::Int::ZERO..=F::Int::MAX,
            buf: String::new(),
        }
    }

    fn next<'a>(&'a mut self) -> Option<&'a str> {
        let i = self.iter.next()?;

        Some(update_buf_from_bits::<F>(&mut self.buf, i))
    }
}
