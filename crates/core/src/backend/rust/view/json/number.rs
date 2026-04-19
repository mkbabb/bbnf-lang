//! `Number` — JSON numeric value with preserved integer precision.
//!
//! AX.W1.A.1 — isomorphic to `sonic_rs::Number`. Three-variant split
//! (`PosInt(u64)`, `NegInt(i64)`, `Float(f64)`) mirrors sonic-rs's
//! private `N` enum exactly so `PartialEq` against a `sonic_rs::Value`
//! comparator decides by bit-identity over the same cases.
//!
//! Integer values from sonic-rs arrive via `as_u64` / `as_i64` and
//! land in the matching variant. Floating-point values from the BBNF
//! tape — the JSON grammar projects numbers through `-> f64` — land
//! in [`N::Float`]. Comparing an integer-only sonic-rs `Number`
//! against a BBNF `Number::Float` goes through [`Number::eq_sonic`]
//! which widens both sides to `f64` for a single-pass equality check
//! without the integer-typed variants.
//!
//! No `NaN` values reach this type: the tape-walker entry point
//! [`Number::from_f64_finite`] rejects non-finite doubles (matching
//! sonic-rs's `from_f64` rejection), and `sonic_rs::Number::from_f64`
//! has the same contract. `f64::NAN != f64::NAN` therefore never
//! decides a `PartialEq` branch; `Eq` is a sound derivation.

use core::fmt;
use core::hash::{Hash, Hasher};

/// Internal representation of a JSON number — three disjoint kinds.
///
/// Mirrors `sonic_rs::Number`'s private `N` enum one-to-one:
/// non-negative integers up to `u64::MAX` → [`N::PosInt`]; strictly
/// negative integers down to `i64::MIN` → [`N::NegInt`]; finite
/// floating-point doubles (including `0.0` / `-0.0` and every value
/// with a fractional part or exponent) → [`N::Float`]. Non-finite
/// doubles never inhabit this enum.
#[derive(Clone, Copy, Debug)]
pub enum N {
    /// Non-negative integer. Values that fit `i64` are representable
    /// via both `PosInt` and `NegInt`; construction always picks
    /// `PosInt` so the representation is canonical.
    PosInt(u64),
    /// Strictly negative integer — values are always < 0 here.
    NegInt(i64),
    /// Finite IEEE-754 double. `f64::NAN` / `f64::INFINITY` cannot
    /// reach this variant (constructors reject them).
    Float(f64),
}

impl PartialEq for N {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        // Intra-variant equality: bitwise where the variant gives
        // canonical form (PosInt, NegInt); `f64 == f64` for floats.
        //
        // Cross-variant equality is intentionally false — the three
        // variants partition the value space (positive int, negative
        // int, finite float), so two `Number` values with different
        // kind tags describe different numbers by construction.
        // Every pair is enumerated explicitly so no arm is a
        // placeholder (invariant 18 discipline).
        match (self, other) {
            (N::PosInt(a), N::PosInt(b)) => a == b,
            (N::NegInt(a), N::NegInt(b)) => a == b,
            (N::Float(a), N::Float(b)) => a == b,
            (N::PosInt(_), N::NegInt(_))
            | (N::PosInt(_), N::Float(_))
            | (N::NegInt(_), N::PosInt(_))
            | (N::NegInt(_), N::Float(_))
            | (N::Float(_), N::PosInt(_))
            | (N::Float(_), N::NegInt(_)) => false,
        }
    }
}

// All inhabitants of `N` are total (no NaN, no infinity), so `Eq` is
// a sound derivation: `N::eq` satisfies reflexivity + symmetry +
// transitivity.
impl Eq for N {}

impl Hash for N {
    fn hash<H: Hasher>(&self, h: &mut H) {
        // Matches `sonic_rs::Number`'s `Hash` impl: collapse
        // `+0.0` / `-0.0` to the same bit pattern so `hash(+0) ==
        // hash(-0)`, matching the PartialEq contract (`+0.0 == -0.0`).
        match *self {
            N::PosInt(i) => {
                0u8.hash(h);
                i.hash(h);
            }
            N::NegInt(i) => {
                1u8.hash(h);
                i.hash(h);
            }
            N::Float(f) => {
                2u8.hash(h);
                if f == 0.0_f64 {
                    0.0_f64.to_bits().hash(h);
                } else {
                    f.to_bits().hash(h);
                }
            }
        }
    }
}

/// A JSON number — integer or floating-point.
///
/// Construction routes through [`Number::from_u64`], [`Number::from_i64`],
/// or [`Number::from_f64_finite`]; the three constructors pick the
/// canonical variant for any given numeric input. Non-finite floats
/// (`NaN`, `+∞`, `-∞`) are rejected at construction — they are not
/// JSON numbers and do not inhabit this type.
#[derive(Clone, Copy, Debug)]
pub struct Number {
    pub(crate) n: N,
}

impl Number {
    /// Construct a `Number` from a non-negative integer. Always
    /// materialises as [`N::PosInt`].
    #[inline]
    pub const fn from_u64(v: u64) -> Self {
        Self { n: N::PosInt(v) }
    }

    /// Construct a `Number` from a signed integer. Non-negative
    /// inputs canonicalise through [`N::PosInt`]; strictly negative
    /// inputs materialise as [`N::NegInt`].
    #[inline]
    pub const fn from_i64(v: i64) -> Self {
        if v >= 0 {
            Self {
                n: N::PosInt(v as u64),
            }
        } else {
            Self { n: N::NegInt(v) }
        }
    }

    /// Construct a `Number` from a finite double. Returns `None` for
    /// `NaN` / `±∞` — non-finite floats are not JSON numbers.
    ///
    /// Matches sonic-rs's `Number::from_f64` contract one-to-one.
    #[inline]
    pub fn from_f64_finite(v: f64) -> Option<Self> {
        if v.is_finite() {
            Some(Self { n: N::Float(v) })
        } else {
            None
        }
    }

    /// True iff the number is representable as an `i64`.
    #[inline]
    pub fn is_i64(&self) -> bool {
        match self.n {
            N::PosInt(v) => v <= i64::MAX as u64,
            N::NegInt(_) => true,
            N::Float(_) => false,
        }
    }

    /// True iff the number is a non-negative integer (fits `u64`).
    #[inline]
    pub fn is_u64(&self) -> bool {
        matches!(self.n, N::PosInt(_))
    }

    /// True iff the number is a floating-point value (not integer).
    #[inline]
    pub fn is_f64(&self) -> bool {
        matches!(self.n, N::Float(_))
    }

    /// Represent the number as `i64` when possible.
    #[inline]
    pub fn as_i64(&self) -> Option<i64> {
        match self.n {
            N::PosInt(n) if n <= i64::MAX as u64 => Some(n as i64),
            N::PosInt(_) => None,
            N::NegInt(n) => Some(n),
            N::Float(_) => None,
        }
    }

    /// Represent the number as `u64` when possible.
    #[inline]
    pub fn as_u64(&self) -> Option<u64> {
        match self.n {
            N::PosInt(n) => Some(n),
            N::NegInt(_) | N::Float(_) => None,
        }
    }

    /// Represent the number as `f64`. Always succeeds: integer
    /// variants widen (`as f64` for both `u64` and `i64`); float
    /// variants pass through.
    #[inline]
    pub fn as_f64(&self) -> f64 {
        match self.n {
            N::PosInt(n) => n as f64,
            N::NegInt(n) => n as f64,
            N::Float(n) => n,
        }
    }
}

impl PartialEq for Number {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.n == other.n
    }
}

impl Eq for Number {}

impl Hash for Number {
    fn hash<H: Hasher>(&self, h: &mut H) {
        self.n.hash(h);
    }
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.n {
            N::PosInt(u) => write!(f, "{u}"),
            N::NegInt(i) => write!(f, "{i}"),
            N::Float(x) => write!(f, "{x}"),
        }
    }
}

/// Cross-type equality with `sonic_rs::Number` — both sides widen to
/// `f64` when the variant tags differ, so an integer-preserving
/// sonic-rs value compares equal to BBNF's float-only tape payload
/// when the two represent the same mathematical number.
///
/// Pure integer-vs-integer pairs compare by their canonical variants;
/// mixed integer-vs-float pairs compare by `as_f64()` — matching
/// sonic-rs's own cross-variant behaviour where a `PosInt(5)` and a
/// `Float(5.0)` are considered equal by the public `PartialEq`
/// trampoline on `Value::as_ref()`.
impl PartialEq<sonic_rs::Number> for Number {
    #[inline]
    fn eq(&self, other: &sonic_rs::Number) -> bool {
        use sonic_rs::JsonNumberTrait;
        // If both sides canonicalise to the same integer tag, compare
        // at integer precision (full `u64` / `i64` range).
        if self.is_u64() && other.is_u64() {
            return self.as_u64() == other.as_u64();
        }
        if self.is_i64() && other.is_i64() {
            return self.as_i64() == other.as_i64();
        }
        // Otherwise widen to f64. Both sides' `as_f64` are total on
        // their respective domains and agree on finite doubles.
        let lhs = self.as_f64();
        let Some(rhs) = other.as_f64() else {
            return false;
        };
        lhs == rhs
    }
}

impl From<sonic_rs::Number> for Number {
    fn from(sn: sonic_rs::Number) -> Self {
        use sonic_rs::JsonNumberTrait;
        if sn.is_u64() {
            Self::from_u64(sn.as_u64().expect("is_u64"))
        } else if sn.is_i64() {
            Self::from_i64(sn.as_i64().expect("is_i64"))
        } else {
            // `JsonNumberTrait::as_f64` returns `Some` for every
            // inhabitable sonic-rs `Number`; the sonic constructor
            // rejects non-finite values at parse time.
            let f = sn.as_f64().expect("is_f64");
            Self::from_f64_finite(f).expect("sonic-rs never stores non-finite")
        }
    }
}
