//! Everything related to verifying that parsed outputs are correct.

use std::any::type_name;
use std::collections::BTreeMap;
use std::ops::RangeInclusive;
use std::str::FromStr;
use std::sync::LazyLock;

use num::bigint::ToBigInt;
use num::{BigInt, BigRational, FromPrimitive, Signed, ToPrimitive};

use crate::{CheckFailure, Float, Int, Update};

/// Powers of two that we store for constants. Account for binary128 which has a 15-bit exponent.
const POWERS_OF_TWO_RANGE: RangeInclusive<i32> = (-(2 << 15))..=(2 << 15);

/// Powers of ten that we cache. Account for binary128, which can fit +4932/-4931
const POWERS_OF_TEN_RANGE: RangeInclusive<i32> = -5_000..=5_000;

/// Cached powers of 10 so we can look them up rather than recreating.
static POWERS_OF_TEN: LazyLock<BTreeMap<i32, BigRational>> = LazyLock::new(|| {
    POWERS_OF_TEN_RANGE
        .map(|exp| (exp, BigRational::from_u32(10).unwrap().pow(exp)))
        .collect()
});

/// Rational property-related constants for a specific float type.
#[allow(dead_code)]
#[derive(Debug)]
pub struct Constants {
    /// The minimum positive value (a subnormal).
    min_subnormal: BigRational,
    /// The maximum possible finite value.
    max: BigRational,
    /// Cutoff between rounding to zero and rounding to the minimum value (min subnormal).
    zero_cutoff: BigRational,
    /// Cutoff between rounding to the max value and rounding to infinity.
    inf_cutoff: BigRational,
    /// Opposite of `inf_cutoff`
    neg_inf_cutoff: BigRational,
    /// The powers of two for all relevant integers.
    powers_of_two: BTreeMap<i32, BigRational>,
    /// Half of each power of two. ULP = "unit in last position", so these are one bit
    /// more precise than what is representable by the float (i.e. in between representable
    /// values).
    half_ulp: BTreeMap<i32, BigRational>,
    /// Handy to have around so we don't need to reallocate for it
    two: BigInt,
}

impl Constants {
    pub fn new<F: Float>() -> Self {
        let two_int = &BigInt::from_u32(2).unwrap();
        let two = &BigRational::from_integer(2.into());

        // The minimum subnormal (aka minimum positive) value. Most negative power of two is the
        // minimum exponent (bias - 1) plus the extra from shifting within the mantissa bits.
        let min_subnormal = two.pow(-(F::EXP_BIAS + F::MAN_BITS - 1).to_signed());

        // The maximum value is the maximum exponent with a fully saturated mantissa. This
        // is easiest to calculate by evaluating what the next value up would be if representable
        // (zeroed mantissa, exponent increments by one, i.e. `2^(bias + 1)`), and subtracting
        // a single LSB (`2 ^ (-mantissa_bits)`).
        let max = (two - two.pow(-F::MAN_BITS.to_signed())) * (two.pow(F::EXP_BIAS.to_signed()));
        let zero_cutoff = &min_subnormal / two_int;

        let inf_cutoff = &max + two_int.pow(F::EXP_BIAS - F::MAN_BITS - 1);
        let neg_inf_cutoff = -&inf_cutoff;

        let powers_of_two: BTreeMap<i32, _> =
            (POWERS_OF_TWO_RANGE).map(|n| (n, two.pow(n))).collect();
        let mut half_ulp = powers_of_two.clone();
        half_ulp.iter_mut().for_each(|(_k, v)| *v = &*v / two_int);

        Self {
            min_subnormal,
            max,
            zero_cutoff,
            inf_cutoff,
            neg_inf_cutoff,
            powers_of_two,
            half_ulp,
            two: two_int.clone(),
        }
    }
}

/// Validate that a string parses correctly
pub fn validate<F: Float>(input: &str, allow_nan: bool) -> Result<(), Update> {
    let parsed: F = input.parse().unwrap_or_else(|e| {
        panic!(
            "parsing failed for {}: {e}. Input: {input}",
            type_name::<F>()
        )
    });

    // Parsed float, decoded into significand and exponent
    let decoded = decode(parsed, allow_nan);

    // Float parsed separately into a rational
    let rational = Rational::parse(input);

    // Verify that the values match
    decoded.check(rational, input)
}

/// The result of parsing a string to a float type.
#[derive(Clone, Copy, Debug, PartialEq)]
enum FloatRes<F: Float> {
    Inf,
    NegInf,
    Zero,
    Nan,
    /// A real number with significand and exponent. Value is `sig * 2 ^ exp`.
    Real {
        sig: F::SInt,
        exp: i32,
    },
}

impl<F: Float> FloatRes<F> {
    /// Given a known exact rational, check that this representation is accurate within the
    /// limits of the float representation. If not, construct a failure `Update` to send.
    fn check(self, expected: Rational, input: &str) -> Result<(), Update> {
        let consts = F::constants();
        // let bool_helper = |cond: bool, err| cond.then_some(()).ok_or(err);

        let res = match (expected, self) {
            // Easy correct cases
            (Rational::Inf, FloatRes::Inf)
            | (Rational::NegInf, FloatRes::NegInf)
            | (Rational::Nan, FloatRes::Nan) => Ok(()),

            // Easy incorrect cases
            (
                Rational::Inf,
                FloatRes::NegInf | FloatRes::Zero | FloatRes::Nan | FloatRes::Real { .. },
            ) => Err(CheckFailure::ExpectedInf),
            (
                Rational::NegInf,
                FloatRes::Inf | FloatRes::Zero | FloatRes::Nan | FloatRes::Real { .. },
            ) => Err(CheckFailure::ExpectedNegInf),
            (
                Rational::Nan,
                FloatRes::Inf | FloatRes::NegInf | FloatRes::Zero | FloatRes::Real { .. },
            ) => Err(CheckFailure::ExpectedNan),
            (Rational::Finite(_), FloatRes::Nan) => Err(CheckFailure::UnexpectedNan),

            // Cases near limits
            (Rational::Finite(r), FloatRes::Zero) => {
                if r <= consts.zero_cutoff {
                    Ok(())
                } else {
                    Err(CheckFailure::UnexpectedZero)
                }
            }
            (Rational::Finite(r), FloatRes::Inf) => {
                if r >= consts.inf_cutoff {
                    Ok(())
                } else {
                    Err(CheckFailure::UnexpectedInf)
                }
            }
            (Rational::Finite(r), FloatRes::NegInf) => {
                if r <= consts.neg_inf_cutoff {
                    Ok(())
                } else {
                    Err(CheckFailure::UnexpectedNegInf)
                }
            }

            // Actual numbers
            (Rational::Finite(r), FloatRes::Real { sig, exp }) => Self::validate_real(r, sig, exp),
        };

        res.map_err(|fail| Update::Failure {
            fail,
            input: input.into(),
        })
    }

    /// Check that `sig * 2^exp` is the same as `rational`, within the float's error margin.
    fn validate_real(rational: BigRational, sig: F::SInt, exp: i32) -> Result<(), CheckFailure> {
        let consts = F::constants();
        // Rational from the parsed value (`sig * 2^exp`). Use cached powers of two to be a bit
        // faster.
        let parsed_rational = consts.powers_of_two.get(&exp).unwrap() * sig.to_bigint().unwrap();
        let error = (parsed_rational - rational).abs();

        // Determine acceptable error at this exponent
        let half_ulp = consts.half_ulp.get(&exp).unwrap();

        if &error <= half_ulp {
            return Ok(());
        }

        let one_ulp = consts.half_ulp.get(&(exp + 1)).unwrap();
        assert_eq!(one_ulp, &(half_ulp * &consts.two));

        let relative_error = error / one_ulp;

        Err(CheckFailure::InvalidReal {
            error_float: relative_error.to_f64(),
            error_str: relative_error.to_string().into(),
        })
    }

    /// Remove trailing zeros in the significand and adjust the exponent
    #[cfg(test)]
    fn normalize(self) -> Self {
        use std::cmp::min;

        match self {
            Self::Real { sig, exp } => {
                // If there are trailing zeroes, remove them and increment the exponent instead
                let shift = min(sig.trailing_zeros(), exp.wrapping_neg().try_into().unwrap());
                Self::Real {
                    sig: sig >> shift,
                    exp: exp + i32::try_from(shift).unwrap(),
                }
            }
            _ => self,
        }
    }
}

/// Decompose a float into its integral components.
///
/// If `allow_nan` is `false`, panic if `NaN` values are reached.
fn decode<F: Float>(f: F, allow_nan: bool) -> FloatRes<F> {
    let ione = F::SInt::ONE;
    let izero = F::SInt::ZERO;

    let mut exponent_biased = f.exponent();
    let mut mantissa = f.mantissa().to_signed();

    if exponent_biased == 0 {
        if mantissa == izero {
            return FloatRes::Zero;
        }

        exponent_biased += 1;
    } else if exponent_biased == F::EXP_SAT {
        if !allow_nan {
            assert_eq!(mantissa, izero, "Unexpected NaN for {}", f.to_bits().hex());
        } else if mantissa != izero {
            return FloatRes::Nan;
        }

        if f.is_sign_negative() {
            return FloatRes::NegInf;
        }

        return FloatRes::Inf;
    } else {
        // Set implicit bit
        mantissa |= ione << F::MAN_BITS;
    }

    let mut exponent = i32::try_from(exponent_biased).unwrap();

    // Adjust for bias and the rnage of the mantissa
    exponent -= i32::try_from(F::EXP_BIAS + F::MAN_BITS).unwrap();

    if f.is_sign_negative() {
        mantissa = mantissa.wrapping_neg();
    }

    FloatRes::Real {
        sig: mantissa,
        exp: exponent,
    }
}

/// A rational or its unrepresentable values.
#[derive(Clone, Debug, PartialEq)]
enum Rational {
    Inf,
    NegInf,
    Nan,
    Finite(BigRational),
}

impl Rational {
    /// Turn a string into a rational. `None` if `NaN`.
    fn parse(s: &str) -> Rational {
        let mut s = s; // lifetime rules

        if s.strip_prefix('+').unwrap_or(s).eq_ignore_ascii_case("nan")
            || s.eq_ignore_ascii_case("-nan")
        {
            return Rational::Nan;
        }

        if s.strip_prefix('+').unwrap_or(s).eq_ignore_ascii_case("inf") {
            return Rational::Inf;
        }

        if s.eq_ignore_ascii_case("-inf") {
            return Rational::NegInf;
        }

        // Fast path; no decimals or exponents ot parse
        if s.bytes().all(|b| b.is_ascii_digit() || b == b'-') {
            return Rational::Finite(BigRational::from_str(s).unwrap());
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

        // let pow = BigRational::from_u32(10).unwrap().pow(ten_exp);
        let pow = POWERS_OF_TEN
            .get(&ten_exp)
            .unwrap_or_else(|| panic!("missing power of ten {ten_exp}"));
        let r = pow
            * BigInt::from_str(s)
                .unwrap_or_else(|e| panic!("`BigInt::from_str(\"{s}\")` failed with {e}"));
        Rational::Finite(r)
    }

    #[cfg(test)]
    fn expect_finite(self) -> BigRational {
        let Self::Finite(r) = self else {
            panic!("got non rational: {self:?}");
        };

        r
    }
}

#[cfg(test)]
mod tests {
    use num::ToPrimitive;

    use super::*;

    #[test]
    fn test_parse_rational() {
        assert_eq!(
            Rational::parse("1234").expect_finite(),
            BigRational::new(1234.into(), 1.into())
        );
        assert_eq!(
            Rational::parse("-1234").expect_finite(),
            BigRational::new((-1234).into(), 1.into())
        );
        assert_eq!(
            Rational::parse("1e+6").expect_finite(),
            BigRational::new(1000000.into(), 1.into())
        );
        assert_eq!(
            Rational::parse("1e-6").expect_finite(),
            BigRational::new(1.into(), 1000000.into())
        );
        assert_eq!(
            Rational::parse("10.4e6").expect_finite(),
            BigRational::new(10400000.into(), 1.into())
        );
        assert_eq!(
            Rational::parse("10.4e+6").expect_finite(),
            BigRational::new(10400000.into(), 1.into())
        );
        assert_eq!(
            Rational::parse("10.4e-6").expect_finite(),
            BigRational::new(13.into(), 1250000.into())
        );
        assert_eq!(
            Rational::parse("10.4243566462342456234124").expect_finite(),
            BigRational::new(
                104243566462342456234124_i128.into(),
                10000000000000000000000_i128.into()
            )
        );
        assert_eq!(Rational::parse("inf"), Rational::Inf);
        assert_eq!(Rational::parse("+inf"), Rational::Inf);
        assert_eq!(Rational::parse("-inf"), Rational::NegInf);
        assert_eq!(Rational::parse("NaN"), Rational::Nan);
    }

    #[test]
    fn test_decode() {
        assert_eq!(decode(0f32, false), FloatRes::Zero);
        assert_eq!(decode(f32::INFINITY, false), FloatRes::Inf);
        assert_eq!(decode(f32::NEG_INFINITY, false), FloatRes::NegInf);
        assert_eq!(
            decode(1.0f32, false).normalize(),
            FloatRes::Real { sig: 1, exp: 0 }
        );
        assert_eq!(
            decode(-1.0f32, false).normalize(),
            FloatRes::Real { sig: -1, exp: 0 }
        );
        assert_eq!(
            decode(100.0f32, false).normalize(),
            FloatRes::Real { sig: 100, exp: 0 }
        );
        assert_eq!(
            decode(100.5f32, false).normalize(),
            FloatRes::Real { sig: 201, exp: -1 }
        );
        assert_eq!(
            decode(-4.004f32, false).normalize(),
            FloatRes::Real {
                sig: -8396997,
                exp: -21
            }
        );
        assert_eq!(
            decode(0.0004f32, false).normalize(),
            FloatRes::Real {
                sig: 13743895,
                exp: -35
            }
        );
        assert_eq!(
            decode(f32::from_bits(0x1), false).normalize(),
            FloatRes::Real { sig: 1, exp: -149 }
        );
    }

    #[test]
    fn test_validate() {
        validate::<f32>("0", false).unwrap();
        validate::<f32>("-0", false).unwrap();
        validate::<f32>("1", false).unwrap();
        validate::<f32>("-1", false).unwrap();
        validate::<f32>("1.1", false).unwrap();
        validate::<f32>("-1.1", false).unwrap();
        validate::<f32>("1e10", false).unwrap();
        validate::<f32>("1e1000", false).unwrap();
        validate::<f32>("-1e1000", false).unwrap();
        validate::<f32>("1e-1000", false).unwrap();
        validate::<f32>("-1e-1000", false).unwrap();
    }

    #[test]
    fn test_check() {
        // Most of the arbitrary values come from checking against <http://weitz.de/ieee/>.
        let r = &BigRational::from_float(10.0).unwrap();
        FloatRes::<f32>::validate_real(r.clone(), 10, 0).unwrap();
        FloatRes::<f32>::validate_real(r.clone(), 10, -1).unwrap_err();
        FloatRes::<f32>::validate_real(r.clone(), 10, 1).unwrap_err();

        let r = &BigRational::from_float(0.25).unwrap();
        FloatRes::<f32>::validate_real(r.clone(), 1, -2).unwrap();
        FloatRes::<f32>::validate_real(r.clone(), 2, -2).unwrap_err();

        let r = &BigRational::from_float(1234.5678).unwrap();
        FloatRes::<f32>::validate_real(r.clone(), 0b100110100101001000101011, -13).unwrap();
        FloatRes::<f32>::validate_real(r.clone(), 0b100110100101001000101010, -13).unwrap_err();
        FloatRes::<f32>::validate_real(r.clone(), 0b100110100101001000101100, -13).unwrap_err();

        let r = &BigRational::from_float(-1234.5678).unwrap();
        FloatRes::<f32>::validate_real(r.clone(), -0b100110100101001000101011, -13).unwrap();
        FloatRes::<f32>::validate_real(r.clone(), -0b100110100101001000101010, -13).unwrap_err();
        FloatRes::<f32>::validate_real(r.clone(), -0b100110100101001000101100, -13).unwrap_err();
    }

    #[test]
    fn check_constants() {
        assert_eq!(f32::constants().max.to_f32().unwrap(), f32::MAX);
        assert_eq!(
            f32::constants().min_subnormal.to_f32().unwrap(),
            f32::from_bits(0x1)
        );
        assert_eq!(f64::constants().max.to_f64().unwrap(), f64::MAX);
        assert_eq!(
            f64::constants().min_subnormal.to_f64().unwrap(),
            f64::from_bits(0x1)
        );
    }
}
