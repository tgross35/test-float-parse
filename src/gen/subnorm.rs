use crate::{Float, Generator, Int};
use std::cmp::min;

/// Spot check some edge cases for subnormals
pub struct SubnormEdge<F: Float> {
    cases: [F::Int; 6],
    index: usize,
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
        }
    }

    fn total_tests() -> u64 {
        Self::edge_cases().len().try_into().unwrap()
    }
}

impl<F: Float> Iterator for SubnormEdge<F> {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        let i = self.cases.get(self.index)?;
        self.index += 1;

        Some(format!("{:e}", F::from_bits(*i)))
    }
}

impl<F: Float> SubnormComplete<F> {
    /// Values up to this number get linearly spaced. Above this they get powers
    /// of two.
    fn linspace_max() -> F::Int {
        <F::Int as Int>::ONE << 22
    }
}

/// Brute force tests
pub struct SubnormComplete<F: Float> {
    num: F::Int,
}

impl<F: Float> Generator<F> for SubnormComplete<F> {
    const NAME: &'static str = "subnormal";
    const SHORT_NAME: &'static str = "subnorm ";

    fn new() -> Self {
        Self { num: F::Int::ZERO }
    }

    fn total_tests() -> u64 {
        min(Self::linspace_max(), F::MAN_MASK).try_into().unwrap()
    }
}

impl<F: Float> Iterator for SubnormComplete<F> {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        if self.num >= F::MAN_MASK {
            return None;
        }

        let ret = format!("{:e}", F::from_bits(self.num));

        if self.num < Self::linspace_max() {
            self.num += F::Int::ONE;
        } else {
            // todo!()
            return None; // TODO
        }

        Some(ret)
    }
}
