use num::{BigInt, BigRational, FromPrimitive};

use crate::{Float, Int};
use bigdecimal::BigDecimal;
use std::{any::type_name, str::FromStr};

#[derive(Debug, PartialEq)]
enum FRes<F: Float> {
    Inf,
    NegInf,
    Zero,
    Other(F::SInt, i32),
}

pub fn validate<F: Float>(input: &str) {
    let output: F = input.parse().unwrap_or_else(|e| {
        panic!(
            "parsing failed for {}: {e}. Input: {input}",
            type_name::<F>()
        )
    });

    let decoded = decode(output);

    let two = &BigInt::from_u32(2).unwrap();
    let frac_2_0 = &BigRational::from_integer(2.into());

    let min_subnormal = frac_2_0.pow(-(F::EXP_BIAS + F::MAN_BITS - 1).to_signed());
    let max = (frac_2_0 - frac_2_0.pow(-F::MAN_BITS.to_signed()))
        * (frac_2_0.pow(F::EXP_BIAS.to_signed()));
    let zero_cutoff = min_subnormal / two;
    let inf_cutoff = max * two.pow(F::EXP_BIAS - F::MAN_BITS - 1);

    // let two = &BigInt::from_u32(2).unwrap();
    // let min_subnormal_bd = BigDecimal::new(two.pow(F::EXP_BIAS + F::MAN_BITS - 1), 0).inverse();
    // let max_bd =
    //     (two - BigDecimal::new(two.pow(F::MAN_BITS), 0).inverse()) * (two.pow(F::EXP_BIAS));
    // let zero_cutoff_bd = min_subnormal_bd / 2;
    // let int_cutoff_bd = max_bd * two.pow(F::EXP_BIAS - F::MAN_BITS - 1);

    // let min_subnormal = frac_2_0.pow(-(F::EXP_BIAS + F::MAN_BITS - 1).to_signed());
    // let max = (frac_2_0 - frac_2_0.pow(-F::MAN_BITS.to_signed()))
    //     * (frac_2_0.pow(F::EXP_BIAS.to_signed()));
    // let zero_cutoff = min_subnormal / two;
    // let inf_cutoff = max * two.pow(F::EXP_BIAS - F::MAN_BITS - 1);

    // let max_ulp = F::EXP_BIAS - F::MAN_BITS;
    // let max = (two - frac_2_0.pow(-(F::MAN_BITS as i32))) * ;

    // let min = frac_2_0 - frac_2_0.pow(-(F::MAN_BITS as i32));
    // let min = frac_2_0 - frac_2_0.pow(-(F::MAN_BITS as i32));
    // let zero_cutoff = min_subnormal / two;
    // let inf_cutoff = min_subnormal / two;

    match decoded {
        FRes::Zero => {}
        FRes::Inf => todo!(),
        FRes::NegInf => todo!(),
        FRes::Other(_, _) => todo!(),
    }
}

fn decode<F: Float>(f: F) -> FRes<F> {
    let ione = F::SInt::ONE;
    let izero = F::SInt::ZERO;

    // let bits = output.to_bits();
    let mut exponent = f.exponent();
    let mut mantissa = f.mantissa().to_signed();

    if exponent == 0 {
        exponent += 1;

        if mantissa == izero {
            return FRes::Zero;
        }
    } else if exponent == F::EXP_MAX {
        assert_eq!(mantissa, izero, "Unexpected NaN for {}", f.to_bits().hex());
        if f.is_sign_negative() {
            return FRes::NegInf;
        } else {
            return FRes::Inf;
        };
    } else {
        // Set implicit bit
        mantissa |= ione << F::MAN_BITS;
    }

    let mut exponent_s = exponent as i32;

    // Adjust for bias and the rnage of the mantissa
    exponent_s -= (F::EXP_BITS + F::MAN_BITS) as i32;

    if f.is_sign_negative() {
        mantissa = mantissa.wrapping_neg();
    }

    FRes::Other(mantissa, exponent_s)
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
    fn test_parse() {
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
}
