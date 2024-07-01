use crate::validate;
use crate::{Float, Generator, Int};
use std::cmp::min;
use std::fmt::Write;
use std::mem::transmute;
use std::sync::LazyLock;

fn update_buf_exp<F: Float>(s: &mut String, i: F::Int) -> &str {
    s.clear();
    write!(s, "{:e}", F::from_bits(i));
    s
}

/// Edge cases
pub struct SubnormEdge<F: Float> {
    iter: std::slice::Iter<'static, F::Int>,
    buf: String,
}

impl<F: Float> SubnormEdge<F> {
    // fn cases() -> &'static str {

    // }
}

impl<F: Float> Generator<F> for SubnormEdge<F> {
    const NAME: &'static str = "subnorm edge cases";

    fn new() -> Self {
        Self {
            iter: [].iter(),
            buf: String::new(),
        }
    }

    fn estimated_tests() -> u64 {
        10

        // TODO
        // Self::edge_cases().len() as u64 + 0
        // todo!()
    }

    fn next<'a>(&'a mut self) -> Option<&'a str> {
        // todo!()
        None
        // if self.num >= F::MAN_MASK {
        //     return None;
        // }

        // self.update_buf(self.num);

        // if self.num < Self::linspace_max() {
        //     self.num += F::Int::ONE;
        // } else {
        //     todo!()
        // }

        // Some(&self.buf)
    }
}

impl<F: Float> SubnormComplete<F> {
    /// Shorthand
    const I1: F::Int = F::Int::ONE;

    /// Values up to this number get linearly spaced. Above this they get powers
    /// of two.
    // const LINSPACE_MAX: F::Int = <F::Int as Int>::ONE << 20;

    fn linspace_max() -> F::Int {
        <F::Int as Int>::ONE << 20
    }

    fn edge_cases() -> &'static [F::Int] {
        //         static X = LazyLock::new(||

        // vec![
        //         // // Comments use an 8-bit mantissa as a demo
        //         // // 0b00000001
        //         // Self::I1,
        //         // // 0b10000000
        //         // Self::I1 << (F::MAN_BITS - 1),
        //         // // 0b00001000
        //         // Self::I1 << ((F::MAN_BITS / 2) - 1),
        //         // // 0b00001111
        //         // Self::I1 << (F::MAN_BITS / 2) - 1,
        //         // // 0b00001111
        //         // Self::I1 << (F::MAN_BITS / 2) - 1,
        //         // // 0b11111111
        //         // F::MAN_MASK,
        //     ]        );

        // X.as_ref()
        &[]
    }
    // /// Quick checks
    // const EDGE_CASES: &'static [F::Int] = &[
    //     // Comments use an 8-bit mantissa as a demo
    //     // 0b00000001
    //     Self::I1,
    //     // 0b10000000
    //     Self::I1 << (F::MAN_BITS - 1),
    //     // 0b00001000
    //     Self::I1 << ((F::MAN_BITS / 2) - 1),
    //     // 0b00001111
    //     Self::I1 << (F::MAN_BITS / 2) - 1,
    //     // 0b00001111
    //     Self::I1 << (F::MAN_BITS / 2) - 1,
    //     // 0b11111111
    //     F::MAN_MASK,
    // ];
}

/// Brute force tests
pub struct SubnormComplete<F: Float> {
    num: F::Int,
    buf: String,
}

impl<F: Float> Generator<F> for SubnormComplete<F> {
    const NAME: &'static str = "subnorm";

    fn new() -> Self {
        Self {
            num: F::Int::ZERO,
            buf: String::new(),
        }
    }

    fn estimated_tests() -> u64 {
        // TODO
        // Self::edge_cases().len() as u64 + 0
        min(Self::linspace_max(), F::MAN_MASK).try_into().unwrap()
    }

    // fn next_edge_caxse<'a>(&'a mut self) -> Option<&'a str> {
    //     self.edge_case_iter.next().map(|bits| self.update_buf(*bits))
    // }

    fn next<'a>(&'a mut self) -> Option<&'a str> {
        if self.num >= F::MAN_MASK {
            return None;
        }

        update_buf_exp::<F>(&mut self.buf, self.num);

        if self.num < Self::linspace_max() {
            self.num += F::Int::ONE;
        } else {
            // todo!()
            return None; // TODO
        }

        Some(&self.buf)
    }
}
