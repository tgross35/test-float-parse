use crate::{update_buf_from_bits, validate};
use crate::{Float, Generator, Int};
use std::cmp::min;
use std::fmt::Write;
use std::mem::transmute;
use std::sync::LazyLock;

/// Spot check some edge cases for subnormals
pub struct SubnormEdge<F: Float> {
    cases: [F::Int; 6],
    index: usize,
    buf: String,
}

impl<F: Float> SubnormEdge<F> {
    /// Shorthand
    const I1: F::Int = F::Int::ONE;

    fn edge_cases() -> [F::Int; 6] {
        // Comments use an 8-bit mantissa as a demo
        [
            // 0b00000001
            Self::I1,
            // 0b10000000
            Self::I1 << (F::MAN_BITS - 1),
            // 0b00001000
            Self::I1 << ((F::MAN_BITS / 2) - 1),
            // 0b00001111
            Self::I1 << (F::MAN_BITS / 2) - 1,
            // 0b00001111
            Self::I1 << (F::MAN_BITS / 2) - 1,
            // 0b11111111
            F::MAN_MASK,
        ]
    }
}

impl<F: Float> Generator<F> for SubnormEdge<F> {
    const NAME: &'static str = "subnormal edge cases";
    const SHORT_NAME: &'static str = "subnorm edge";

    fn new() -> Self {
        Self {
            cases: Self::edge_cases(),
            index: 0,
            buf: String::new(),
        }
    }

    fn estimated_tests() -> u64 {
        Self::edge_cases().len().try_into().unwrap()
    }

    fn next<'a>(&'a mut self) -> Option<&'a str> {
        let i = self.cases.get(self.index)?;
        self.index += 1;
        Some(update_buf_from_bits::<F>(&mut self.buf, *i))
    }
}

/// Test
impl<F: Float> SubnormComplete<F> {
    /// Shorthand
    const I1: F::Int = F::Int::ONE;

    /// Values up to this number get linearly spaced. Above this they get powers
    /// of two.
    fn linspace_max() -> F::Int {
        <F::Int as Int>::ONE << 22
    }
}

/// Brute force tests
pub struct SubnormComplete<F: Float> {
    num: F::Int,
    buf: String,
}

impl<F: Float> Generator<F> for SubnormComplete<F> {
    const NAME: &'static str = "subnormal";
    const SHORT_NAME: &'static str = "subnorm ";

    fn new() -> Self {
        Self {
            num: F::Int::ZERO,
            buf: String::new(),
        }
    }

    fn estimated_tests() -> u64 {
        min(Self::linspace_max(), F::MAN_MASK).try_into().unwrap()
    }

    fn next<'a>(&'a mut self) -> Option<&'a str> {
        if self.num >= F::MAN_MASK {
            return None;
        }

        update_buf_from_bits::<F>(&mut self.buf, self.num);

        if self.num < Self::linspace_max() {
            self.num += F::Int::ONE;
        } else {
            // todo!()
            return None; // TODO
        }

        Some(&self.buf)
    }
}
