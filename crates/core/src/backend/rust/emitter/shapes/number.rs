//! Number-shape emitter — `parse_number_<grammar>_<rule>`.
//!
//! # Role — AW-V.W3.2
//!
//! Emits the per-grammar Number-shape parse function mirroring
//! `json_prototype::number::parse_number_body` (crates/bbnf-json-
//! prototype/src/number.rs:75). The emitted function accumulates the
//! mantissa + decimal exponent during a single scalar digit scan,
//! feeds them to `parse_that::parsers::eisel_lemire::compute_f64`,
//! falls back to `f64::from_str` on overflow / ambiguous rounding.
//!
//! The emitted function pushes a `TapeKind::Span` leaf carrying the
//! `f64` payload via [`Tape<R>::push_leaf_with_f64_direct`]
//! (AY.W4.2). The Eisel-Lemire-decoded bits land in the dedicated
//! [`Columns::pay_f64`] direct-write column, bypassing the
//! generic `PayloadData::WideScalar` → `pay_wide` round-trip; the
//! leaf carries [`TapeRec::PAYLOAD_F64_DIRECT_BIT`] so
//! `tape.payload_f64(rec)` projects through the dense column directly.
//! The `variant_idx` on the leaf is the rule's low-8-bit id.

use bbnf_ir::{GrammarIR, IrRule};
use proc_macro2::TokenStream;
use quote::quote;

use super::dispatcher::{shape_fn_ident, visitor_shape_fn_ident};
use bbnf_ir::registry::EmitStrategy;

/// Emit `pub fn parse_number_<grammar>_<rule>(input, p, first_byte,
/// builder) -> Result<(), DtaError>`.
///
/// AZ-I.W2.RC / AZ-II.cutover.O4 — emits the struct-builder body.
/// The Eisel-Lemire body is shared in spirit with the old tape path,
/// but production code now routes the parsed `f64` directly into the
/// resolved grammar builder.
pub fn emit_parse_number(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
    strategy: &EmitStrategy,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("number", grammar_suffix, rule_name);
    let variant_idx = (rule.id & 0xFF) as u8;
    let _ = grammar_suffix;

    let _ = variant_idx;
    let EmitStrategy::StructDirect { rust, .. } = strategy;
    emit_parse_number_struct_direct(&fn_ident, rust.builder_path)
}

/// AZ-I.W2.RC — StructDirect Number body. Mirrors the scalar scan
/// (Eisel-Lemire mantissa + decimal exponent accumulator); the leaf
/// emission routes through `builder.push_leaf_with_f64(value)` against
/// the resolved struct-builder type. The `'p` lifetime threads through
/// the parse-fn signature so the builder's typed in-flight stack
/// retains arena-borrowed slices.
fn emit_parse_number_struct_direct(
    fn_ident: &proc_macro2::Ident,
    builder_path: &str,
) -> TokenStream {
    let builder_ty: syn::Path = syn::parse_str(builder_path)
        .expect("EmitStrategy::StructDirect.builder_path must parse as a Rust path");
    quote! {
        /// AZ-I.W2.RC — per-grammar Number-shape parse function
        /// (struct-direct substrate).
        ///
        /// Mirrors `json_prototype::number::parse_number_body` for the
        /// scan body; the trailing leaf push routes through
        /// `builder.push_leaf_with_f64(value)` against the
        /// grammar-specific concrete `StructBuilder` impl. `first_byte`
        /// is the byte the dispatcher already matched; passing it
        /// avoids a redundant re-read for the sign check.
        #[inline(always)]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
        pub fn #fn_ident<'p>(
            input: &'p [u8],
            p: &mut usize,
            first_byte: u8,
            builder: &mut #builder_ty<'p>,
        ) -> ::core::result::Result<(), crate::runtime::DtaError> {
            use crate::runtime::builder::StructBuilder as _;
            const POW10_U64: [u64; 17] = [
                1, 10, 100, 1_000, 10_000, 100_000, 1_000_000,
                10_000_000, 100_000_000, 1_000_000_000,
                10_000_000_000, 100_000_000_000, 1_000_000_000_000,
                10_000_000_000_000, 100_000_000_000_000,
                1_000_000_000_000_000, 10_000_000_000_000_000,
            ];
            let _ = POW10_U64;
            let start = *p;
            let len = input.len();
            let negative = first_byte == b'-';
            if negative { *p += 1; }

            let int_start = *p;
            let mut mantissa: u64 = 0;
            let mut many_digits = false;
            while *p < len {
                let b = input[*p];
                if b.is_ascii_digit() {
                    mantissa = mantissa.wrapping_mul(10)
                        .wrapping_add((b - b'0') as u64);
                    *p += 1;
                } else { break; }
            }
            if *p == int_start {
                return Err(crate::runtime::DtaError::Syntax {
                    offset: start as u32,
                });
            }
            let int_digit_count = *p - int_start;
            if int_digit_count > 19 { many_digits = true; }

            let mut fractional_digit_count: i64 = 0;
            if input.get(*p) == Some(&b'.') {
                *p += 1;
                let frac_start = *p;
                while *p < len {
                    let b = input[*p];
                    if b.is_ascii_digit() {
                        mantissa = mantissa.wrapping_mul(10)
                            .wrapping_add((b - b'0') as u64);
                        *p += 1;
                    } else { break; }
                }
                fractional_digit_count = (*p - frac_start) as i64;
                if fractional_digit_count == 0 {
                    return Err(crate::runtime::DtaError::Syntax {
                        offset: start as u32,
                    });
                }
                if int_digit_count as i64 + fractional_digit_count > 19 {
                    many_digits = true;
                }
            }

            let mut exponent: i64 = -fractional_digit_count;
            let exp_byte = input.get(*p).copied();
            if exp_byte == Some(b'e') || exp_byte == Some(b'E') {
                *p += 1;
                let exp_negative = match input.get(*p) {
                    Some(b'+') => { *p += 1; false }
                    Some(b'-') => { *p += 1; true }
                    _ => false,
                };
                let exp_start = *p;
                let mut exp_val: i64 = 0;
                while *p < len {
                    let b = input[*p];
                    if b.is_ascii_digit() {
                        exp_val = exp_val.saturating_mul(10)
                            .saturating_add((b - b'0') as i64);
                        *p += 1;
                    } else { break; }
                }
                if *p == exp_start {
                    return Err(crate::runtime::DtaError::Syntax {
                        offset: start as u32,
                    });
                }
                exponent += if exp_negative { -exp_val } else { exp_val };
            }

            let end = *p;
            let bytes = &input[start..end];
            let value = if many_digits {
                parse_number_fallback(bytes)
            } else {
                match ::parse_that::parsers::eisel_lemire::compute_f64(
                    exponent, mantissa, negative,
                ) {
                    Some(v) => v,
                    None => parse_number_fallback(bytes),
                }
            };

            // AZ-I.W2.RC — struct-direct leaf emit. The Eisel-Lemire
            // value lands directly on the in-flight builder frame; no
            // tape column write, no `TapeKind::Span` tag (the typed
            // frame on the builder's stack already carries the
            // discriminating shape).
            builder.push_leaf_with_f64(value);
            Ok(())
        }
    }
}

/// Fallback helper emitted alongside the number shape fns. One
/// instance per grammar; `#[cold]` + `#[inline(never)]`.
pub fn emit_number_fallback_helper() -> TokenStream {
    quote! {
        #[inline(never)]
        #[cold]
        #[allow(non_snake_case)]
        fn parse_number_fallback(bytes: &[u8]) -> f64 {
            let s = unsafe { ::core::str::from_utf8_unchecked(bytes) };
            s.parse::<f64>().unwrap_or(f64::NAN)
        }
    }
}

// ─────────────────────────────────────────────────────────────────────
// AW-V.W3-bench-fix — visitor-path Number emitter.
//
// Mirrors the prototype's
// `json_prototype::number::parse_number_body::<V>`. Bypasses the
// tape; `visitor.number_f64(value)` replaces the Span + WideScalar
// payload push the tape-path emits.
// ─────────────────────────────────────────────────────────────────────

/// Emit `pub fn parse_number_visitor_<grammar>_<rule><V: JsonVisitor>(
/// input, p, first_byte, visitor) -> Result<(), ParseErr>`.
pub fn emit_parse_number_visitor(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = visitor_shape_fn_ident("number", grammar_suffix, rule_name);
    let _ = grammar_suffix;

    quote! {
        /// AW-V.W3-bench-fix — visitor-path Number-shape parse function.
        ///
        /// Mirrors `json_prototype::number::parse_number_body::<V>`.
        #[inline(always)]
        #[allow(non_snake_case, clippy::too_many_arguments)]
        pub fn #fn_ident<V>(
            input: &[u8],
            p: &mut usize,
            first_byte: u8,
            visitor: &mut V,
        ) -> ::core::result::Result<(), crate::runtime::ParseErr>
        where
            V: crate::runtime::tape::NumberVisitor,
        {
            const POW10_U64: [u64; 17] = [
                1, 10, 100, 1_000, 10_000, 100_000, 1_000_000,
                10_000_000, 100_000_000, 1_000_000_000,
                10_000_000_000, 100_000_000_000, 1_000_000_000_000,
                10_000_000_000_000, 100_000_000_000_000,
                1_000_000_000_000_000, 10_000_000_000_000_000,
            ];
            let start = *p;
            let len = input.len();
            let negative = first_byte == b'-';
            if negative { *p += 1; }

            let int_start = *p;
            let mut mantissa: u64 = 0;
            let mut many_digits = false;
            while *p < len {
                let b = input[*p];
                if b.is_ascii_digit() {
                    mantissa = mantissa.wrapping_mul(10)
                        .wrapping_add((b - b'0') as u64);
                    *p += 1;
                } else { break; }
            }
            if *p == int_start {
                return Err(crate::runtime::ParseErr::Syntax {
                    offset: start as u32, rule: None,
                });
            }
            let int_digit_count = *p - int_start;
            if int_digit_count > 19 { many_digits = true; }

            let mut fractional_digit_count: i64 = 0;
            if input.get(*p) == Some(&b'.') {
                *p += 1;
                let frac_start = *p;
                // aarch64 NEON 16-digit parallel accumulator — matches
                // the prototype's canonical path. Canada's 15-digit
                // fractions amortise across the stripe; other corpora
                // fall through to the scalar tail.
                #[cfg(target_arch = "aarch64")]
                {
                    while *p + 16 <= len {
                        let (sum, count) = unsafe {
                            __number_simd_str2int(&input[*p..*p + 16])
                        };
                        if count == 0 { break; }
                        mantissa = mantissa
                            .wrapping_mul(POW10_U64[count])
                            .wrapping_add(sum);
                        *p += count;
                        if count < 16 { break; }
                    }
                }
                while *p < len {
                    let b = input[*p];
                    if b.is_ascii_digit() {
                        mantissa = mantissa.wrapping_mul(10)
                            .wrapping_add((b - b'0') as u64);
                        *p += 1;
                    } else { break; }
                }
                fractional_digit_count = (*p - frac_start) as i64;
                if fractional_digit_count == 0 {
                    return Err(crate::runtime::ParseErr::Syntax {
                        offset: start as u32, rule: None,
                    });
                }
                if int_digit_count as i64 + fractional_digit_count > 19 {
                    many_digits = true;
                }
            }

            let mut exponent: i64 = -fractional_digit_count;
            // Direct equality rather than `matches!`: nightly's
            // `matches!` expansion decorates the inner `match` with
            // `#[allow(non_exhaustive_omitted_patterns)]` — an
            // attribute on an expression (unstable, E0658) — which
            // `cargo expand` surfaces in the bootstrap-emitted
            // `generated.rs`.
            let exp_byte = input.get(*p).copied();
            if exp_byte == Some(b'e') || exp_byte == Some(b'E') {
                *p += 1;
                let exp_negative = match input.get(*p) {
                    Some(b'+') => { *p += 1; false }
                    Some(b'-') => { *p += 1; true }
                    _ => false,
                };
                let exp_start = *p;
                let mut exp_val: i64 = 0;
                while *p < len {
                    let b = input[*p];
                    if b.is_ascii_digit() {
                        exp_val = exp_val.saturating_mul(10)
                            .saturating_add((b - b'0') as i64);
                        *p += 1;
                    } else { break; }
                }
                if *p == exp_start {
                    return Err(crate::runtime::ParseErr::Syntax {
                        offset: start as u32, rule: None,
                    });
                }
                exponent += if exp_negative { -exp_val } else { exp_val };
            }

            let end = *p;
            let bytes = &input[start..end];
            let value = if many_digits {
                parse_number_fallback(bytes)
            } else {
                match ::parse_that::parsers::eisel_lemire::compute_f64(
                    exponent, mantissa, negative,
                ) {
                    Some(v) => v,
                    None => parse_number_fallback(bytes),
                }
            };
            visitor.number_f64(value)
                .map_err(|_| crate::runtime::ParseErr::Syntax {
                    offset: start as u32, rule: None,
                })
        }
    }
}

/// Emit the aarch64 NEON `simd_str2int` fraction helper + supporting
/// helpers — one instance per grammar. Gated on
/// `target_arch = "aarch64"`; non-aarch64 builds see a stub that's
/// never referenced from the visitor path.
///
/// Mirrors `json_prototype::number::simd_str2int`
/// (crates/json-prototype/src/number.rs:306) — the packadd
/// cascade + 17-arm dispatch on `count`. Feeds the visitor-path
/// Number emitter's aarch64 fraction path.
pub fn emit_number_simd_fraction_helper() -> TokenStream {
    quote! {
        /// AW-V.W3-bench-fix — NEON 16-digit parallel accumulator.
        ///
        /// Ported from `sonic-number-0.1.2/src/arch/aarch64.rs:83–137`
        /// (MIT) — mirrors the prototype's `simd_str2int`. Returns
        /// `(sum, count)` where `count` is the number of leading
        /// decimal digits consumed in the 16-byte window.
        ///
        /// SAFETY: caller must guarantee `c.len() >= 16`.
        #[cfg(target_arch = "aarch64")]
        #[inline(always)]
        #[allow(non_snake_case, clippy::too_many_arguments)]
        unsafe fn __number_simd_str2int(c: &[u8]) -> (u64, usize) {
            use core::arch::aarch64::*;
            debug_assert!(c.len() >= 16);
            unsafe {
                let data = vld1q_u8(c.as_ptr());
                let zero_char = vdupq_n_u8(b'0');
                let digits = vsubq_u8(data, zero_char);
                let gt_nine = vcgtq_u8(digits, vdupq_n_u8(9));
                let mask16 = vreinterpretq_u16_u8(gt_nine);
                let mask8 = vshrn_n_u16::<4>(mask16);
                let mask64 = vget_lane_u64::<0>(vreinterpret_u64_u8(mask8));
                let need = 16usize;
                let mut count = need;
                if mask64 != 0 {
                    let parsed = (mask64.trailing_zeros() >> 2) as usize;
                    if parsed < need { count = parsed; }
                }

                #[inline(always)]
                unsafe fn packadd_1_local(v: uint8x16_t) -> uint16x8_t {
                    use core::arch::aarch64::*;
                    unsafe {
                        let even = vuzp1q_u8(v, v);
                        let odd = vuzp2q_u8(v, v);
                        vaddw_u8(
                            vmull_u8(vget_low_u8(even), vdup_n_u8(10)),
                            vget_low_u8(odd),
                        )
                    }
                }
                #[inline(always)]
                unsafe fn packadd_2_local(v: uint16x8_t) -> uint32x4_t {
                    use core::arch::aarch64::*;
                    unsafe {
                        let even = vuzp1q_u16(v, v);
                        let odd = vuzp2q_u16(v, v);
                        vaddw_u16(
                            vmull_n_u16(vget_low_u16(even), 100),
                            vget_low_u16(odd),
                        )
                    }
                }
                #[inline(always)]
                unsafe fn packadd_4_local(v: uint32x4_t) -> uint64x2_t {
                    use core::arch::aarch64::*;
                    unsafe {
                        let even = vuzp1q_u32(v, v);
                        let odd = vuzp2q_u32(v, v);
                        vaddw_u32(
                            vmull_n_u32(vget_low_u32(even), 10000),
                            vget_low_u32(odd),
                        )
                    }
                }

                let sum = match count {
                    0 => 0,
                    1 => vgetq_lane_u8::<0>(digits) as u64,
                    2 => (vgetq_lane_u8::<0>(digits) as u64) * 10
                        + (vgetq_lane_u8::<1>(digits) as u64),
                    3 => {
                        let shifted = vextq_u8::<13>(vdupq_n_u8(0), digits);
                        let p1 = packadd_1_local(shifted);
                        (vgetq_lane_u16::<6>(p1) as u64) * 100
                            + (vgetq_lane_u16::<7>(p1) as u64)
                    }
                    4 => {
                        let shifted = vextq_u8::<12>(vdupq_n_u8(0), digits);
                        let p1 = packadd_1_local(shifted);
                        (vgetq_lane_u16::<6>(p1) as u64) * 100
                            + (vgetq_lane_u16::<7>(p1) as u64)
                    }
                    5 => {
                        let shifted = vextq_u8::<11>(vdupq_n_u8(0), digits);
                        let p1 = packadd_1_local(shifted);
                        let p2 = packadd_2_local(p1);
                        (vgetq_lane_u32::<2>(p2) as u64) * 10000
                            + (vgetq_lane_u32::<3>(p2) as u64)
                    }
                    6 => {
                        let shifted = vextq_u8::<10>(vdupq_n_u8(0), digits);
                        let p1 = packadd_1_local(shifted);
                        let p2 = packadd_2_local(p1);
                        (vgetq_lane_u32::<2>(p2) as u64) * 10000
                            + (vgetq_lane_u32::<3>(p2) as u64)
                    }
                    7 => {
                        let shifted = vextq_u8::<9>(vdupq_n_u8(0), digits);
                        let p1 = packadd_1_local(shifted);
                        let p2 = packadd_2_local(p1);
                        (vgetq_lane_u32::<2>(p2) as u64) * 10000
                            + (vgetq_lane_u32::<3>(p2) as u64)
                    }
                    8 => {
                        let shifted = vextq_u8::<8>(vdupq_n_u8(0), digits);
                        let p1 = packadd_1_local(shifted);
                        let p2 = packadd_2_local(p1);
                        (vgetq_lane_u32::<2>(p2) as u64) * 10000
                            + (vgetq_lane_u32::<3>(p2) as u64)
                    }
                    9 => {
                        let shifted = vextq_u8::<7>(vdupq_n_u8(0), digits);
                        let p1 = packadd_1_local(shifted);
                        let p2 = packadd_2_local(p1);
                        let p4 = packadd_4_local(p2);
                        vgetq_lane_u64::<0>(p4) * 100000000
                            + vgetq_lane_u64::<1>(p4)
                    }
                    10 => {
                        let shifted = vextq_u8::<6>(vdupq_n_u8(0), digits);
                        let p1 = packadd_1_local(shifted);
                        let p2 = packadd_2_local(p1);
                        let p4 = packadd_4_local(p2);
                        vgetq_lane_u64::<0>(p4) * 100000000
                            + vgetq_lane_u64::<1>(p4)
                    }
                    11 => {
                        let shifted = vextq_u8::<5>(vdupq_n_u8(0), digits);
                        let p1 = packadd_1_local(shifted);
                        let p2 = packadd_2_local(p1);
                        let p4 = packadd_4_local(p2);
                        vgetq_lane_u64::<0>(p4) * 100000000
                            + vgetq_lane_u64::<1>(p4)
                    }
                    12 => {
                        let shifted = vextq_u8::<4>(vdupq_n_u8(0), digits);
                        let p1 = packadd_1_local(shifted);
                        let p2 = packadd_2_local(p1);
                        let p4 = packadd_4_local(p2);
                        vgetq_lane_u64::<0>(p4) * 100000000
                            + vgetq_lane_u64::<1>(p4)
                    }
                    13 => {
                        let shifted = vextq_u8::<3>(vdupq_n_u8(0), digits);
                        let p1 = packadd_1_local(shifted);
                        let p2 = packadd_2_local(p1);
                        let p4 = packadd_4_local(p2);
                        vgetq_lane_u64::<0>(p4) * 100000000
                            + vgetq_lane_u64::<1>(p4)
                    }
                    14 => {
                        let shifted = vextq_u8::<2>(vdupq_n_u8(0), digits);
                        let p1 = packadd_1_local(shifted);
                        let p2 = packadd_2_local(p1);
                        let p4 = packadd_4_local(p2);
                        vgetq_lane_u64::<0>(p4) * 100000000
                            + vgetq_lane_u64::<1>(p4)
                    }
                    15 => {
                        let shifted = vextq_u8::<1>(vdupq_n_u8(0), digits);
                        let p1 = packadd_1_local(shifted);
                        let p2 = packadd_2_local(p1);
                        let p4 = packadd_4_local(p2);
                        vgetq_lane_u64::<0>(p4) * 100000000
                            + vgetq_lane_u64::<1>(p4)
                    }
                    16 => {
                        let p1 = packadd_1_local(digits);
                        let p2 = packadd_2_local(p1);
                        let p = packadd_4_local(p2);
                        vgetq_lane_u64::<0>(p) * 100000000
                            + vgetq_lane_u64::<1>(p)
                    }
                    _ => core::hint::unreachable_unchecked(),
                };
                (sum, count)
            }
        }
    }
}
