use crate::validate::FloatConstants;
use num::bigint::ToBigInt;
use num::Integer;
use std::str::FromStr;
use std::{fmt, ops};

pub trait Int:
    Copy
    + fmt::Debug
    + fmt::Display
    + fmt::LowerHex
    + ops::Add<Output = Self>
    + ops::Sub<Output = Self>
    + ops::Shl<u32, Output = Self>
    + ops::Shr<u32, Output = Self>
    + ops::BitAnd<Output = Self>
    + ops::BitOr<Output = Self>
    + ops::Not<Output = Self>
    + ops::AddAssign
    + ops::BitAndAssign
    + ops::BitOrAssign
    + From<u8>
    + TryFrom<i8>
    + TryFrom<u32, Error: fmt::Debug>
    + TryFrom<u64, Error: fmt::Debug>
    + TryFrom<u128, Error: fmt::Debug>
    + TryInto<u64, Error: fmt::Debug>
    + TryInto<u32, Error: fmt::Debug>
    + ToBigInt
    + PartialOrd
    + Integer
    + Send
    + 'static
{
    type Signed: Int;
    type Bytes: Default + AsMut<[u8]>;

    const BITS: u32;
    const ZERO: Self;
    const ONE: Self;
    const MAX: Self;

    fn to_signed(self) -> Self::Signed;
    fn wrapping_neg(self) -> Self;
    fn trailing_zeros(self) -> u32;
    fn from_le_bytes(bytes: Self::Bytes) -> Self;

    fn hex(self) -> String {
        format!("{:x}", self)
    }
}

macro_rules! impl_int {
    ($($uty:ty, $sty:ty);+) => {
        $(
            impl Int for $uty {
                type Signed = $sty;
                type Bytes = [u8; Self::BITS as usize / 8];
                const BITS: u32 = Self::BITS;
                const ZERO: Self = 0;
                const ONE: Self = 1;
                const MAX: Self = Self::MAX;
                fn to_signed(self) -> Self::Signed {
                    self.try_into().unwrap()
                }
                fn wrapping_neg(self) -> Self {
                    self.wrapping_neg()
                }
                fn trailing_zeros(self) -> u32 {
                    self.trailing_zeros()
                }
                fn from_le_bytes(bytes: Self::Bytes) -> Self {
                    Self::from_be_bytes(bytes)
                }
            }

            impl Int for $sty {
                type Signed = Self;
                type Bytes = [u8; Self::BITS as usize / 8];
                const BITS: u32 = Self::BITS;
                const ZERO: Self = 0;
                const ONE: Self = 1;
                const MAX: Self = Self::MAX;
                fn to_signed(self) -> Self::Signed {
                    self
                }
                fn wrapping_neg(self) -> Self {
                    self.wrapping_neg()
                }
                fn trailing_zeros(self) -> u32 {
                    self.trailing_zeros()
                }
                fn from_le_bytes(bytes: Self::Bytes) -> Self {
                    Self::from_be_bytes(bytes)
                }
            }
        )+
    }
}

impl_int!(u32, i32; u64, i64);

pub trait Float:
    Copy
    + fmt::Debug
    + fmt::LowerExp
    + FromStr<Err: fmt::Display>
    + FloatConstants
    + Sized
    + Send
    + 'static
{
    /// Unsigned integer of same width
    type Int: Int<Signed = Self::SInt>;
    type SInt: Int;

    /// Total bits
    const BITS: u32;

    /// (Stored) bits in the mantissa)
    const MAN_BITS: u32;

    /// Bits in the exponent
    const EXP_BITS: u32 = Self::BITS - Self::MAN_BITS - 1;

    /// A saturated exponent (all ones)
    const EXP_SAT: u32 = (1 << Self::EXP_BITS) - 1;

    /// The exponent bias, also its maximum value
    const EXP_BIAS: u32 = Self::EXP_SAT >> 1;

    const MAN_MASK: Self::Int;
    const SIGN_MASK: Self::Int;

    fn from_bits(i: Self::Int) -> Self;
    fn to_bits(self) -> Self::Int;

    fn is_sign_negative(self) -> bool {
        (self.to_bits() & Self::SIGN_MASK) > Self::Int::ZERO
    }

    /// Exponent without adjustment
    fn exponent(self) -> u32 {
        ((self.to_bits() >> Self::MAN_BITS) & Self::EXP_SAT.try_into().unwrap())
            .try_into()
            .unwrap()
    }

    /// Adjusted for the bias
    fn exponent_adj(self) -> i32 {
        self.exponent() as i32 - Self::EXP_BIAS as i32
    }

    fn mantissa(self) -> Self::Int {
        self.to_bits() & Self::MAN_MASK
    }
}

macro_rules! impl_float {
    ($($ty:ty, $ity:ty, $bits:literal);+) => {
        $(
            impl Float for $ty {
                type Int = $ity;
                type SInt = <Self::Int as Int>::Signed;
                const BITS: u32 = $bits;
                const MAN_BITS: u32 = Self::MANTISSA_DIGITS - 1;
                const MAN_MASK: Self::Int = (Self::Int::ONE << Self::MAN_BITS) - Self::Int::ONE;
                const SIGN_MASK: Self::Int = Self::Int::ONE << (Self::BITS-1);
                fn from_bits(i: Self::Int) -> Self { Self::from_bits(i) }
                fn to_bits(self) -> Self::Int { self.to_bits() }
            }
        )+
    }
}

impl_float!(f32, u32, 32; f64, u64, 64);

/// A test generator. Should provide an iterator that produces strings with unique patterns
/// to parse.
pub trait Generator<F: Float>: Iterator<Item = String> + Send + 'static {
    const NAME: &'static str;
    const SHORT_NAME: &'static str;
    /// If false (default), validation will assert on NaN.
    const PATTERNS_CONTAIN_NAN: bool = false;

    /// Approximate number of tests that will be run
    fn total_tests() -> u64;

    /// Create this generator
    fn new() -> Self;
}
