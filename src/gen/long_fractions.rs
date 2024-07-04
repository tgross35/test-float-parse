use std::{char, fmt::Write};

use crate::{Float, Generator};

const MAX_DIGIT: u32 = 9;
const MAX_LEN: usize = 410;
const PREFIX: &str = "0.";

/// Test e.g. `0.1`, `0.11`, `0.111`, `0.1111`, ..., `0.2`, `0.22`, ...
pub struct RepeatingDecimal {
    digit: u32,
    buf: String,
}

impl<F: Float> Generator<F> for RepeatingDecimal {
    const NAME: &'static str = "repeating decimal";
    const SHORT_NAME: &'static str = "dec rep";

    fn estimated_tests() -> u64 {
        u64::try_from((MAX_DIGIT + 1) * u32::try_from(MAX_LEN + 1 - PREFIX.len()).unwrap()).unwrap()
            + 1
    }

    fn new() -> Self {
        Self {
            digit: 0,
            buf: PREFIX.to_owned(),
        }
    }
}

impl Iterator for RepeatingDecimal {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        if self.digit > MAX_DIGIT {
            return None;
        }

        let inc_digit = self.buf.len() > MAX_LEN;

        if inc_digit {
            // Reset the string
            self.buf.clear();
            self.buf.write_str(PREFIX);
        }

        self.buf.push(char::from_digit(self.digit, 10).unwrap());

        if inc_digit {
            self.digit += 1;
        }

        Some(self.buf.clone())
    }
}
