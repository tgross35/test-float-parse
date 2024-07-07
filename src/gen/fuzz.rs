use std::fmt::Write;
use std::marker::PhantomData;
use std::ops::Range;
use std::sync::atomic::{AtomicU64, Ordering};

use rand_chacha::rand_core::{RngCore, SeedableRng};
use rand_chacha::ChaCha8Rng;

use crate::{Float, Generator, Int, SEED};

/// how many iterations to fuzz for; can be updated before launching.
pub static FUZZ_COUNT: AtomicU64 = AtomicU64::new(crate::DEFAULT_FUZZ_COUNT);

/// Generic fuzzer; just tests random bit patterns N times.
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

impl<F: Float> Iterator for Fuzz<F> {
    type Item = <Self as Generator<F>>::WriteCtx;

    fn next(&mut self) -> Option<Self::Item> {
        let _ = self.iter.next()?;

        let mut buf = <F::Int as Int>::Bytes::default();
        self.rng.fill_bytes(buf.as_mut());
        let i = F::Int::from_le_bytes(buf);

        Some(F::from_bits(i))
    }
}
