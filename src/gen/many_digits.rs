// TODO

use std::char;
use std::marker::PhantomData;
use std::ops::RangeInclusive;

use rand::distributions::{Distribution, Uniform};
use rand::Rng;
// use rand::distributions::{Range, Sample};
// use rand::{IsaacRng, Rng, SeedableRng};
use rand_chacha::rand_core::{RngCore, SeedableRng};
use rand_chacha::ChaCha8Rng;

use crate::{Float, Generator, Int, SEED};

// fn main() {
//     let mut rnd = IsaacRng::from_seed(&SEED);
//     let mut range = Range::new(0, 10);
//     for _ in 0..5_000_000u64 {
//         let num_digits = rnd.gen_range(100, 400);
//         let digits = gen_digits(num_digits, &mut range, &mut rnd);
//         validate(&digits);
//     }
// }

// fn gen_digits<R: Rng>(n: u32, range: &mut Range<u32>, rnd: &mut R) -> String {
//     let mut s = String::new();
//     for _ in 0..n {
//         let digit = char::from_digit(range.sample(rnd), 10).unwrap();
//         s.push(digit);
//     }
//     s
// }

const ITERATIONS: u64 = 5_000_000;
const DIGITS_RANGE: RangeInclusive<usize> = 100..=400;

pub struct RandDigits<F> {
    rng: ChaCha8Rng,
    iter: RangeInclusive<u64>,
    range: Uniform<u32>,
    /// Allow us to use generics in `Iterator`.
    marker: PhantomData<F>,
}

impl<F: Float> Generator<F> for RandDigits<F> {
    const NAME: &'static str = "random digits";

    const SHORT_NAME: &'static str = "rand digits";

    type WriteCtx = String;

    fn total_tests() -> u64 {
        todo!()
    }

    fn new() -> Self {
        let rng = ChaCha8Rng::from_seed(SEED);
        let range = Uniform::from(0..=10);

        Self {
            rng,
            iter: 0..=ITERATIONS,
            range,
            marker: PhantomData,
        }
    }

    fn write_string(s: &mut String, ctx: Self::WriteCtx) {
        *s = ctx;
    }
}

impl<F: Float> Iterator for RandDigits<F> {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        let _ = self.iter.next()?;
        let num_digits = self.rng.gen_range(DIGITS_RANGE);

        let mut s = String::with_capacity(num_digits);

        for _ in 0..num_digits {
            let digit = char::from_digit(self.range.sample(&mut self.rng), 10).unwrap();
            s.push(digit);
        }

        Some(s)
    }
}
