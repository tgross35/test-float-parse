use crate::{Float, Generator};

/// Skip e = 0 because small-u32 already does those.
const COEFF_MAX: u32 = 10_000;
const EXP_MAX: u32 = 301;

pub struct ShortDecimals {
    iter: Box<dyn Iterator<Item = String>>,
    /// Even though the iterator allocates, we still need the internal buffer
    /// to meet the function signature.
    buf: String,
}

impl<F: Float> Generator<F> for ShortDecimals {
    const NAME: &'static str = "short decimals";

    fn estimated_tests() -> u64 {
        (COEFF_MAX * EXP_MAX * 2).into()
    }

    fn new() -> Self {
        let iter = (0..EXP_MAX)
            .flat_map(move |exp| (0..COEFF_MAX).map(move |coeff| (exp, coeff)))
            // If it ends in zeros, the parser will strip those (and adjust the exponent),
            // which almost always (except for exponents near +/- 300) result in an input
            // equivalent to something we already generate in a different way.
            .filter(|(exp, coeff)| coeff % 10 != 0)
            .flat_map(|(exp, coeff)| [format!("{coeff}e{exp}"), format!("{coeff}e-{exp}")]);

        Self {
            iter: Box::new(iter),
            buf: String::new(),
        }
    }

    fn next<'a>(&'a mut self) -> Option<&'a str> {
        self.iter.next().map(|s| {
            self.buf = s;
            self.buf.as_str()
        })
    }
}
