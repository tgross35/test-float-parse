use std::fmt::Write;
use std::marker::PhantomData;
use std::ops::Range;
use std::sync::atomic::{AtomicU64, Ordering};

use rand::distributions::{Distribution, Standard};
use rand::Rng;
use rand_chacha::rand_core::SeedableRng;
use rand_chacha::ChaCha8Rng;

use crate::{Float, Generator, SEED};

/// How many iterations to fuzz for; can be updated before launching. We use a static since
/// `Generator::new` doesn't take any input.
pub static FUZZ_COUNT: AtomicU64 = AtomicU64::new(crate::DEFAULT_FUZZ_COUNT);

/// Generic fuzzer; just tests deterministic random bit patterns N times.
pub struct Fuzz<F> {
    iter: Range<u64>,
    rng: ChaCha8Rng,
    /// Allow us to use generics in `Iterator`.
    marker: PhantomData<F>,
}

impl<F: Float> Generator<F> for Fuzz<F>
where
    Standard: Distribution<<F as Float>::Int>,
{
    const NAME: &'static str = "fuzz";
    const SHORT_NAME: &'static str = "fuzz";
    const PATTERNS_CONTAIN_NAN: bool = true;

    type WriteCtx = F;

    fn total_tests() -> u64 {
        FUZZ_COUNT.load(Ordering::Relaxed)
    }

    fn new() -> Self {
        let rng = ChaCha8Rng::from_seed(SEED);

        Self {
            iter: 0..FUZZ_COUNT.load(Ordering::Relaxed),
            rng,
            marker: PhantomData,
        }
    }

    fn write_string(s: &mut String, ctx: Self::WriteCtx) {
        write!(s, "{ctx:e}").unwrap();
    }
}

impl<F: Float> Iterator for Fuzz<F>
where
    Standard: Distribution<<F as Float>::Int>,
{
    type Item = <Self as Generator<F>>::WriteCtx;

    fn next(&mut self) -> Option<Self::Item> {
        let _ = self.iter.next()?;
        let i: F::Int = self.rng.gen();

        Some(F::from_bits(i))
    }
}
