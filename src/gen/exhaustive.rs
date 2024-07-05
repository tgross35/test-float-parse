use std::fmt::Write;
use std::ops::RangeInclusive;

use crate::{Float, Generator, Int};

/// Test every possible bit pattern. Not recommended for anything larger than `f32`!
pub struct Exhaustive<F: Float> {
    iter: RangeInclusive<F::Int>,
}

impl<F: Float> Generator<F> for Exhaustive<F>
where
    RangeInclusive<F::Int>: Iterator<Item = F::Int>,
{
    const NAME: &'static str = "exhaustive";
    const SHORT_NAME: &'static str = "exhaustive";

    type WriteCtx = F;

    fn total_tests() -> u64 {
        F::Int::MAX.try_into().unwrap()
    }

    fn new() -> Self {
        Self {
            iter: F::Int::ZERO..=F::Int::MAX,
        }
    }

    fn write_string(s: &mut String, ctx: Self::WriteCtx) {
        write!(s, "{ctx:e}").unwrap();
    }
}

impl<F: Float> Iterator for Exhaustive<F>
where
    RangeInclusive<F::Int>: Iterator<Item = F::Int>,
{
    type Item = F;

    fn next(&mut self) -> Option<Self::Item> {
        Some(F::from_bits(self.iter.next()?))
    }
}
