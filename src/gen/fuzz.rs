// extern crate rand;

// use rand::{IsaacRng, Rng, SeedableRng};
// use std::mem::transmute;
// use test_float_parse::{validate, SEED};

// fn main() {
//     let mut rnd = IsaacRng::from_seed(&SEED);
//     let mut i = 0;
//     while i < 10_000_000 {
//         let bits = rnd.next_u64();
//         let x: f64 = unsafe { transmute(bits) };
//         if x.is_finite() {
//             validate(&format!("{:e}", x));
//             i += 1;
//         }
//     }
// }

use std::{marker::PhantomData, ops::Range};

use crate::{Float, Generator, Int, SEED};
use rand_chacha::{
    rand_core::{RngCore, SeedableRng},
    ChaCha8Rng,
};

const FUZZ_COUNT: u64 = 20_000_000;

pub struct Fuzz<F> {
    iter: Range<u64>,
    rng: ChaCha8Rng,
    /// Allow us to use generics in `Iterator`
    marker: PhantomData<F>,
}

impl<F: Float> Generator<F> for Fuzz<F> {
    const NAME: &'static str = "fuzz";
    const SHORT_NAME: &'static str = "fuzz";
    const PATTERNS_CONTAIN_NAN: bool = true;

    fn total_tests() -> u64 {
        FUZZ_COUNT
    }

    fn new() -> Self {
        let rng = ChaCha8Rng::from_seed(SEED);

        Self {
            iter: 0..FUZZ_COUNT,
            rng,
            marker: PhantomData,
        }
    }
}

impl<F: Float> Iterator for Fuzz<F> {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        let _ = self.iter.next()?;

        let mut buf = <F::Int as Int>::Bytes::default();
        self.rng.fill_bytes(buf.as_mut());
        let i = F::Int::from_le_bytes(buf);

        Some(format!("{:e}", F::from_bits(i)))
    }
}
