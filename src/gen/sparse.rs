use std::{fmt::Write, sync::Arc};

use crate::{Float, Generator, Int};

const POWERS_OF_TWO: [u128; 128] = make_powers_of_two();

const fn make_powers_of_two() -> [u128; 128] {
    let mut ret = [0; 128];
    let mut i = 0;
    while i < 128 {
        ret[i] = 1 << i;
        i += 1;
    }

    ret
}

/// Can't clone this result because of lifetime errors, just use a macro
macro_rules! pow_iter {
    () => {
        (0..F::BITS).map(|i| F::Int::try_from(POWERS_OF_TWO[i as usize]).unwrap())
    };
}

/// Test all numbers that include three 1s in the binary representation
pub struct FewOnes<F: Float> {
    iter: Box<dyn Iterator<Item = F::Int>>,
    buf: String,
}

impl<F: Float> Generator<F> for FewOnes<F>
where
    <F::Int as TryFrom<u128>>::Error: std::fmt::Debug,
{
    const NAME: &'static str = "few ones";
    const SHORT_NAME: &'static str = "few ones";

    fn estimated_tests() -> u64 {
        u64::from(F::BITS).pow(3)
    }

    fn new() -> Self {
        let iter = pow_iter!()
            .flat_map(move |a| pow_iter!().map(move |b| (a, b)))
            .flat_map(move |(a, b)| pow_iter!().map(move |c| (a, b, c)))
            .map(|(a, b, c)| a | b | c);

        Self {
            iter: Box::new(iter),
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
