//! Body source for the Eisel-Lemire fast-path number-decode kernel.
//!
//! # Paired runtime
//!
//! `parse_that::parsers::eisel_lemire::compute_f64` +
//! `parse_that::parsers::eisel_lemire::algorithm::{compute_float,
//! compute_product_approx, power, full_multiplication}`. The prototype
//! consumes these via `parse-that`'s crate-level
//! `#[inline(always)]` discipline; W2.1's canada.json samply
//! (`.profiles/samply/aw5-w2/json_canada/profile_v5.json.{gz,syms.json}`)
//! confirms 98.6% self-time on a single monomorphised `parse_value`
//! symbol with no `compute_f64` / `compute_product_approx` /
//! `compute_float` in the bench binary's `nm` output.
//!
//! Lifting the body into an emit fragment allows the per-shape
//! Number-shape emitter (W3.2) to splice the decode inline at the
//! per-grammar call site — the shape emitter owns the outer digit
//! scan (scalar + `simd_str2int` for aarch64 fraction paths) and
//! pastes this block at the point where it has the accumulated
//! (mantissa, exponent, negative) triple ready.
//!
//! # Splice-target type
//!
//! [`syn::Block`]. The block is an expression block whose trailing
//! expression is `Option<f64>` — `Some(value)` on successful decode,
//! `None` on the ~0.01% ambiguous-rounding case where the caller falls
//! back to `<f64 as FromStr>::from_str(bytes)` on the original literal.
//!
//! # Prebound names
//!
//! - `exponent: i64` — decimal exponent (negative for fractional inputs).
//! - `mantissa: u64` — pre-accumulated mantissa (≤ 2^63 for the fast
//!   path; caller has already routed `many_digits` through fallback).
//! - `negative: bool` — sign bit.
//! - `POWER_OF_FIVE_128: &[(u64, u64)]` — the 128-bit powers-of-5 table
//!   from `parse_that::parsers::eisel_lemire::table::POWER_OF_FIVE_128`
//!   (const-addressable at splice time; the emitter re-emits or
//!   re-exports it as a `pub const` in the generated module).
//! - Constants in scope (caller-emitted `const` bindings):
//!   `MANTISSA_EXPLICIT_BITS: usize = 52;`
//!   `MIN_EXPONENT_ROUND_TO_EVEN: i64 = -4;`
//!   `MAX_EXPONENT_ROUND_TO_EVEN: i64 = 23;`
//!   `MINIMUM_EXPONENT: i32 = -1023;`
//!   `INFINITE_POWER: i32 = 0x7FF;`
//!   `SMALLEST_POWER_OF_FIVE: i32 = -342;`
//!   `LARGEST_POWER_OF_FIVE: i32 = 308;`
//!   `MAX_MANTISSA_FAST_PATH: u64 = 2 << MANTISSA_EXPLICIT_BITS;`
//!   `MIN_EXPONENT_FAST_PATH: i64 = -22;`
//!   `MAX_EXPONENT_FAST_PATH: i64 = 22;`
//!   `MAX_EXPONENT_DISGUISED_FAST_PATH: i64 = 37;`
//!   `FAST_PATH_POW10: [f64; 23] = [ ... ];`
//!   `INT_POW10: [u64; 16] = [ ... ];`
//!
//! The per-shape emitter emits these constants (and `POWER_OF_FIVE_128`)
//! once per generated module — the Number-shape emitter's codegen pass
//! writes the table exactly once and points every Number-shape splice
//! at it.
//!
//! # Shape
//!
//! The body mirrors `parse_that::parsers::eisel_lemire::compute_f64` +
//! `algorithm::compute_float` + `algorithm::compute_product_approx`
//! inlined into a single expression block.
//!
//! ```text
//! {
//!     if mantissa == 0 {
//!         Some(if negative { -0.0 } else { 0.0 })
//!     } else if exponent < SMALLEST_POWER_OF_FIVE as i64 {
//!         Some(if negative { -0.0 } else { 0.0 })
//!     } else if exponent > LARGEST_POWER_OF_FIVE as i64 {
//!         Some(if negative { f64::NEG_INFINITY } else { f64::INFINITY })
//!     } else if let Some(v) = try_fast_path(exponent, mantissa, negative) {
//!         Some(v)
//!     } else {
//!         compute_slow(exponent, mantissa, negative)
//!     }
//! }
//! ```
//!
//! The body below is the verbatim inlined shape used by
//! `parse_that::parsers::eisel_lemire::compute_f64`; see
//! `parse_that/rust/parse_that/src/parsers/eisel_lemire/{mod,algorithm}.rs`
//! for provenance.

use proc_macro2::TokenStream;
use quote::ToTokens;

/// Verbatim body of the Eisel-Lemire decode pipeline.
///
/// The fragment assumes the enclosing scope exposes the Clinger
/// fast-path helper as `try_fast_path_f64` and the slow-path helper as
/// `compute_float_slow`. The per-shape emitter emits both as sibling
/// fns in the generated module (their bodies are also available as
/// [`SOURCE_TRY_FAST_PATH`] and [`SOURCE_COMPUTE_FLOAT_SLOW`]); keeping
/// them as separate fns rather than inlining into this block preserves
/// the per-fn `#[inline(always)]` contract the W2.1 prototype verified.
pub const SOURCE: &str = r#"{
    if mantissa == 0 {
        Some(if negative { -0.0f64 } else { 0.0f64 })
    } else if exponent < SMALLEST_POWER_OF_FIVE as i64 {
        Some(if negative { -0.0f64 } else { 0.0f64 })
    } else if exponent > LARGEST_POWER_OF_FIVE as i64 {
        Some(if negative { f64::NEG_INFINITY } else { f64::INFINITY })
    } else if let Some(v) = try_fast_path_f64(exponent, mantissa, negative) {
        Some(v)
    } else {
        let am = compute_float_slow(exponent, mantissa);
        if am.1 < 0 {
            None
        } else {
            let mut word = am.0;
            word |= (am.1 as u64) << MANTISSA_EXPLICIT_BITS;
            if negative {
                word |= 1u64 << 63;
            }
            Some(f64::from_bits(word))
        }
    }
}"#;

/// Sibling helper body — Clinger fast-path decision. Parses as
/// `syn::Block` with a trailing `Option<f64>` expression. The per-shape
/// emitter wraps this as
/// `#[inline(always)] fn try_fast_path_f64(exponent: i64, mantissa: u64,
/// negative: bool) -> Option<f64> { <SOURCE_TRY_FAST_PATH> }` in the
/// generated module.
pub const SOURCE_TRY_FAST_PATH: &str = r#"{
    if mantissa > MAX_MANTISSA_FAST_PATH {
        None
    } else if exponent < MIN_EXPONENT_FAST_PATH || exponent > MAX_EXPONENT_DISGUISED_FAST_PATH {
        None
    } else {
        let value = if exponent <= MAX_EXPONENT_FAST_PATH {
            let m = mantissa as f64;
            if exponent < 0 {
                m / FAST_PATH_POW10[(-exponent) as usize]
            } else {
                m * FAST_PATH_POW10[exponent as usize]
            }
        } else {
            let shift = (exponent - MAX_EXPONENT_FAST_PATH) as usize;
            match mantissa.checked_mul(INT_POW10[shift]) {
                Some(scaled) if scaled <= MAX_MANTISSA_FAST_PATH => {
                    (scaled as f64) * FAST_PATH_POW10[MAX_EXPONENT_FAST_PATH as usize]
                }
                _ => return None,
            }
        };
        Some(if negative { -value } else { value })
    }
}"#;

/// Sibling helper body — full Eisel-Lemire slow-path decode. Parses as
/// `syn::Block` with a trailing `(u64, i32)` expression. Wrap as
/// `#[inline(always)] fn compute_float_slow(q: i64, mut w: u64) ->
/// (u64, i32) { <SOURCE_COMPUTE_FLOAT_SLOW> }`.
pub const SOURCE_COMPUTE_FLOAT_SLOW: &str = r#"{
    if w == 0 || q < SMALLEST_POWER_OF_FIVE as i64 {
        return (0, 0);
    }
    if q > LARGEST_POWER_OF_FIVE as i64 {
        return (0, INFINITE_POWER);
    }
    let lz = w.leading_zeros();
    w <<= lz;
    let (lo, hi) = compute_product_approx(q, w, MANTISSA_EXPLICIT_BITS + 3);
    if lo == 0xFFFF_FFFF_FFFF_FFFF {
        let inside_safe_exponent = (-27..=55).contains(&q);
        if !inside_safe_exponent {
            return (0, -1);
        }
    }
    let upperbit = (hi >> 63) as i32;
    let mut mantissa = hi >> (upperbit + 64 - MANTISSA_EXPLICIT_BITS as i32 - 3);
    let mut power2 = power_of_q(q as i32) + upperbit - lz as i32 - MINIMUM_EXPONENT;
    if power2 <= 0 {
        if -power2 + 1 >= 64 {
            return (0, 0);
        }
        mantissa >>= -power2 + 1;
        mantissa += mantissa & 1;
        mantissa >>= 1;
        power2 = (mantissa >= (1_u64 << MANTISSA_EXPLICIT_BITS)) as i32;
        return (mantissa, power2);
    }
    if lo <= 1
        && q >= MIN_EXPONENT_ROUND_TO_EVEN
        && q <= MAX_EXPONENT_ROUND_TO_EVEN
        && mantissa & 3 == 1
        && (mantissa << (upperbit + 64 - MANTISSA_EXPLICIT_BITS as i32 - 3)) == hi
    {
        mantissa &= !1_u64;
    }
    mantissa += mantissa & 1;
    mantissa >>= 1;
    if mantissa >= (2_u64 << MANTISSA_EXPLICIT_BITS) {
        mantissa = 1_u64 << MANTISSA_EXPLICIT_BITS;
        power2 += 1;
    }
    mantissa &= !(1_u64 << MANTISSA_EXPLICIT_BITS);
    if power2 >= INFINITE_POWER {
        return (0, INFINITE_POWER);
    }
    (mantissa, power2)
}"#;

/// Sibling helper — `power(q)` + `full_multiplication(a, b)` +
/// `compute_product_approx(q, w, precision)`. Parses as `syn::Block`
/// whose trailing expression is `(u64, u64)`. Wrap as
/// `#[inline(always)] fn compute_product_approx(q: i64, w: u64, precision:
/// usize) -> (u64, u64) { <SOURCE_COMPUTE_PRODUCT_APPROX> }`.
pub const SOURCE_COMPUTE_PRODUCT_APPROX: &str = r#"{
    let mask = if precision < 64 {
        0xFFFF_FFFF_FFFF_FFFF_u64 >> precision
    } else {
        0xFFFF_FFFF_FFFF_FFFF_u64
    };
    let index = (q - SMALLEST_POWER_OF_FIVE as i64) as usize;
    let (lo5, hi5) = POWER_OF_FIVE_128[index];
    let r_first = (w as u128) * (lo5 as u128);
    let mut first_lo = r_first as u64;
    let mut first_hi = (r_first >> 64) as u64;
    if first_hi & mask == mask {
        let r_second = (w as u128) * (hi5 as u128);
        let second_hi = (r_second >> 64) as u64;
        first_lo = first_lo.wrapping_add(second_hi);
        if second_hi > first_lo {
            first_hi += 1;
        }
    }
    (first_lo, first_hi)
}"#;

/// Sibling helper — Schubfach-style `power(q)` approximation. Parses as
/// `syn::Block` whose trailing expression is `i32`. Wrap as
/// `#[inline(always)] fn power_of_q(q: i32) -> i32 {
/// <SOURCE_POWER_OF_Q> }`.
pub const SOURCE_POWER_OF_Q: &str = r#"{
    (q.wrapping_mul(152_170 + 65536) >> 16) + 63
}"#;

/// Parse [`SOURCE`] into a [`TokenStream`].
pub fn fragment() -> TokenStream {
    syn::parse_str::<syn::Block>(SOURCE)
        .expect("bbnf-simd-scan::emit::eisel_lemire_body: SOURCE must parse as syn::Block")
        .to_token_stream()
}

/// Parse [`SOURCE_TRY_FAST_PATH`] into a [`TokenStream`].
pub fn fragment_try_fast_path() -> TokenStream {
    syn::parse_str::<syn::Block>(SOURCE_TRY_FAST_PATH)
        .expect(
            "bbnf-simd-scan::emit::eisel_lemire_body: SOURCE_TRY_FAST_PATH must parse as syn::Block",
        )
        .to_token_stream()
}

/// Parse [`SOURCE_COMPUTE_FLOAT_SLOW`] into a [`TokenStream`].
pub fn fragment_compute_float_slow() -> TokenStream {
    syn::parse_str::<syn::Block>(SOURCE_COMPUTE_FLOAT_SLOW)
        .expect(
            "bbnf-simd-scan::emit::eisel_lemire_body: SOURCE_COMPUTE_FLOAT_SLOW must parse as syn::Block",
        )
        .to_token_stream()
}

/// Parse [`SOURCE_COMPUTE_PRODUCT_APPROX`] into a [`TokenStream`].
pub fn fragment_compute_product_approx() -> TokenStream {
    syn::parse_str::<syn::Block>(SOURCE_COMPUTE_PRODUCT_APPROX)
        .expect(
            "bbnf-simd-scan::emit::eisel_lemire_body: SOURCE_COMPUTE_PRODUCT_APPROX must parse as syn::Block",
        )
        .to_token_stream()
}

/// Parse [`SOURCE_POWER_OF_Q`] into a [`TokenStream`].
pub fn fragment_power_of_q() -> TokenStream {
    syn::parse_str::<syn::Block>(SOURCE_POWER_OF_Q)
        .expect(
            "bbnf-simd-scan::emit::eisel_lemire_body: SOURCE_POWER_OF_Q must parse as syn::Block",
        )
        .to_token_stream()
}
