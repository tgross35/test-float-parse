use num::{bigint::ToBigInt, BigInt, BigRational, FromPrimitive};

use crate::{Failure, Float, Int, Update};
// use bigdecimal::BigDecimal;
use std::{any::type_name, cmp::min, collections::BTreeMap, str::FromStr, sync::LazyLock};

#[derive(Debug)]
pub struct Constants {
    min_subnormal: BigRational,
    max: BigRational,
    zero_cutoff: BigRational,
    inf_cutoff: BigRational,
    neg_inf_cutoff: BigRational,
    powers_of_two: BTreeMap<i32, BigRational>,
    half_ulp: BTreeMap<i32, BigRational>,
}

impl Constants {
    fn new<F: Float>() -> Self {
        let two_int = &BigInt::from_u32(2).unwrap();
        let two = &BigRational::from_integer(2.into());

        let min_subnormal = two.pow(-(F::EXP_BIAS + F::MAN_BITS - 1).to_signed());
        let max = (two - two.pow(-F::MAN_BITS.to_signed())) * (two.pow(F::EXP_BIAS.to_signed()));
        let zero_cutoff = &min_subnormal / two_int;
        let inf_cutoff = &max * two_int.pow(F::EXP_BIAS - F::MAN_BITS - 1);
        let neg_inf_cutoff = -&inf_cutoff;

        let powers_of_two: BTreeMap<i32, _> = (-1100_i32..1100).map(|n| (n, two.pow(n))).collect();
        let mut half_ulp = powers_of_two.clone();
        half_ulp.iter_mut().for_each(|(_k, v)| *v = &*v / two);

        Self {
            min_subnormal,
            max,
            zero_cutoff,
            inf_cutoff,
            neg_inf_cutoff,
            powers_of_two,
            half_ulp,
        }
    }
}

static F32_CONSTANTS: LazyLock<Constants> = LazyLock::new(|| Constants::new::<f32>());
static F64_CONSTANTS: LazyLock<Constants> = LazyLock::new(|| Constants::new::<f64>());

pub trait FloatConstants {
    fn constants() -> &'static Constants;
}

impl FloatConstants for f32 {
    fn constants() -> &'static Constants {
        &F32_CONSTANTS
    }
}

impl FloatConstants for f64 {
    fn constants() -> &'static Constants {
        &F64_CONSTANTS
    }
}

/// The result of parsing to a float type
#[derive(Clone, Copy, Debug, PartialEq)]
enum FloatRes<F: Float> {
    Inf,
    NegInf,
    Zero,
    Real {
        /// The significand as a signed integer
        sig: F::SInt,
        /// The exponent
        exp: i32,
    },
}

impl<F: Float> FloatRes<F> {
    fn normalize(self) -> Self {
        match self {
            Self::Inf => Self::Inf,
            Self::NegInf => Self::NegInf,
            Self::Zero => Self::Zero,
            Self::Real { sig, exp } => {
                if exp < 0 {
                    let norm = min(sig.trailing_zeros(), exp.wrapping_neg().try_into().unwrap());
                    Self::Real {
                        sig: sig >> norm,
                        exp: exp + i32::try_from(norm).unwrap(),
                    }
                } else {
                    self
                }
            }
        }
    }
}

pub fn validate<F: Float>(input: &str) -> Result<(), Update> {
    let parsed: F = input.parse().unwrap_or_else(|e| {
        panic!(
            "parsing failed for {}: {e}. Input: {input}",
            type_name::<F>()
        )
    });

    // Parsed float, decoded into significand and exponent
    let decoded = decode(parsed);
    // Float parsed separately into a rational
    let rational = parse_ratioinal(input);

    let consts = F::constants();

    match decoded {
        FloatRes::Zero => check(
            rational <= consts.zero_cutoff,
            input,
            Failure::RoundedToZero,
        ),
        FloatRes::Inf => check(rational >= consts.inf_cutoff, input, Failure::RoundedToInf),
        FloatRes::NegInf => check(
            rational <= consts.neg_inf_cutoff,
            input,
            Failure::RoundedToNegInf,
        ),
        FloatRes::Real { sig, exp } => {
            let approx = consts.powers_of_two.get(&exp).unwrap() * sig.to_bigint().unwrap();
            Ok(())
        }
    }
}

fn check(correct: bool, input: &str, failure: Failure) -> Result<(), Update> {
    if correct {
        Ok(())
    } else {
        Err(Update::Failure {
            fail: failure,
            input: input.into(),
        })
    }
}

fn decode<F: Float>(f: F) -> FloatRes<F> {
    let ione = F::SInt::ONE;
    let izero = F::SInt::ZERO;

    // let bits = output.to_bits();
    let mut exponent_biased = f.exponent();
    let mut mantissa = f.mantissa().to_signed();

    if exponent_biased == 0 {
        if mantissa == izero {
            return FloatRes::Zero;
        }

        exponent_biased += 1;
    } else if exponent_biased == F::EXP_SAT {
        assert_eq!(mantissa, izero, "Unexpected NaN for {}", f.to_bits().hex());
        if f.is_sign_negative() {
            return FloatRes::NegInf;
        } else {
            return FloatRes::Inf;
        };
    } else {
        // Set implicit bit
        mantissa |= ione << F::MAN_BITS;
    }

    let mut exponent = exponent_biased as i32;

    // Adjust for bias and the rnage of the mantissa
    exponent -= (F::EXP_BIAS + F::MAN_BITS) as i32;

    if f.is_sign_negative() {
        mantissa = mantissa.wrapping_neg();
    }

    FloatRes::Real {
        sig: mantissa,
        exp: exponent,
    }
}

fn parse_ratioinal(s: &str) -> BigRational {
    let mut s = s; // lifetime rules

    // Fast path; no decimals or exponents ot parse
    if s.bytes().all(|b| b.is_ascii_digit() || b == b'-') {
        return BigRational::from_str(s).unwrap();
    }

    let mut ten_exp: i32 = 0;

    // Remove and handle e.g. `e-4`, `e+10`, `e5` suffixes
    if let Some(pos) = s.bytes().position(|b| b == b'e') {
        let (dec, exp) = s.split_at(pos);
        s = dec;
        ten_exp = exp[1..].parse().unwrap();
    }

    // Remove the decimal and instead change our exponent
    // E.g. "12.3456" becomes "123456 * 10^-4"
    let mut s_owned;
    if let Some(pos) = s.bytes().position(|b| b == b'.') {
        ten_exp = ten_exp
            .checked_sub((s.len() - pos - 1).try_into().unwrap())
            .unwrap();
        s_owned = s.to_owned();
        s_owned.remove(pos);
        s = &s_owned;
    }

    let mut r = BigRational::from_str(s).unwrap();
    r *= BigRational::from_u32(10).unwrap().pow(ten_exp);
    r
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_rational() {
        assert_eq!(
            parse_ratioinal("1234"),
            BigRational::new(1234.into(), 1.into())
        );
        assert_eq!(
            parse_ratioinal("-1234"),
            BigRational::new((-1234).into(), 1.into())
        );
        assert_eq!(
            parse_ratioinal("1e+6"),
            BigRational::new(1000000.into(), 1.into())
        );
        assert_eq!(
            parse_ratioinal("1e-6"),
            BigRational::new(1.into(), 1000000.into())
        );
        assert_eq!(
            parse_ratioinal("10.4e6"),
            BigRational::new(10400000.into(), 1.into())
        );
        assert_eq!(
            parse_ratioinal("10.4e+6"),
            BigRational::new(10400000.into(), 1.into())
        );
        assert_eq!(
            parse_ratioinal("10.4e-6"),
            BigRational::new(13.into(), 1250000.into())
        );
        assert_eq!(
            parse_ratioinal("10.4243566462342456234124"),
            BigRational::new(
                104243566462342456234124_i128.into(),
                10000000000000000000000_i128.into()
            )
        );
    }

    #[test]
    fn test_decode() {
        assert_eq!(decode(0f32), FloatRes::Zero);
        assert_eq!(decode(f32::INFINITY), FloatRes::Inf);
        assert_eq!(decode(f32::NEG_INFINITY), FloatRes::NegInf);
        assert_eq!(
            decode(1.0f32).normalize(),
            FloatRes::Real { sig: 1, exp: 0 }
        );
        assert_eq!(
            decode(-1.0f32).normalize(),
            FloatRes::Real { sig: -1, exp: 0 }
        );
        assert_eq!(
            decode(100.0f32).normalize(),
            FloatRes::Real { sig: 100, exp: 0 }
        );
        assert_eq!(
            decode(100.5f32).normalize(),
            FloatRes::Real { sig: 201, exp: -1 }
        );
        assert_eq!(
            decode(-4.004f32).normalize(),
            FloatRes::Real {
                sig: -8396997,
                exp: -21
            }
        );
        assert_eq!(
            decode(0.0004f32).normalize(),
            FloatRes::Real {
                sig: 13743895,
                exp: -35
            }
        );
        assert_eq!(
            decode(f32::from_bits(0x1)).normalize(),
            FloatRes::Real { sig: 1, exp: -149 }
        );
    }

    #[test]
    fn test_validate() {
        validate::<f32>("0").unwrap();
        validate::<f32>("-0").unwrap();
        validate::<f32>("1").unwrap();
        validate::<f32>("-1").unwrap();
        validate::<f32>("1.1").unwrap();
        validate::<f32>("-1.1").unwrap();
        validate::<f32>("1e10").unwrap();
        validate::<f32>("1e1000").unwrap();
        validate::<f32>("-1e1000").unwrap();
        validate::<f32>("1e-1000").unwrap();
        validate::<f32>("-1e-1000").unwrap();
    }
}
