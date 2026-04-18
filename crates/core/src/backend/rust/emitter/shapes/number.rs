//! Number-shape emitter — `parse_number_<grammar>_<rule>`.
//!
//! # Role — AW-V.W3.2
//!
//! Emits the per-grammar Number-shape parse function mirroring
//! `bbnf_json_prototype::number::parse_number_body` (crates/bbnf-json-
//! prototype/src/number.rs:75). The emitted function accumulates the
//! mantissa + decimal exponent during a single scalar digit scan,
//! feeds them to `parse_that::parsers::eisel_lemire::compute_f64`,
//! falls back to `f64::from_str` on overflow / ambiguous rounding.
//!
//! The emitted function pushes a `TapeKind::Regex` leaf carrying the
//! `f64` payload via [`TapeBuilder::push_leaf_with`] with
//! [`PayloadData::WideScalar`]. The `variant_idx` on the leaf is the
//! rule's low-8-bit id; this matches the AW-IV walker's emit shape so
//! the bench's `tape.payload_f64(rec)` accessor resolves.

use bbnf_ir::{GrammarIR, IrRule};
use proc_macro2::TokenStream;
use quote::quote;

use super::dispatcher::shape_fn_ident;

/// Emit `pub fn parse_number_<grammar>_<rule>(input, p, first_byte,
/// builder) -> Result<TapeOffset, DtaError>`.
pub fn emit_parse_number(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("number", grammar_suffix, rule_name);
    let variant_idx = (rule.id & 0xFF) as u8;
    let _ = grammar_suffix;

    quote! {
        /// AW-V.W3.2 — per-grammar Number-shape parse function.
        ///
        /// Mirrors `bbnf_json_prototype::number::parse_number_body`.
        /// `first_byte` is the byte the dispatcher already matched;
        /// passing it avoids a redundant re-read for the sign check.
        #[inline(always)]
        #[allow(non_snake_case, clippy::too_many_arguments)]
        pub fn #fn_ident(
            input: &[u8],
            p: &mut usize,
            first_byte: u8,
            builder: &mut ::bbnf::runtime::tape::TapeBuilder,
        ) -> ::core::result::Result<
            ::bbnf::runtime::tape::TapeOffset,
            ::bbnf::runtime::tape::DtaError,
        > {
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
                return Err(::bbnf::runtime::tape::DtaError::Syntax {
                    offset: start as u32,
                    failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
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
                    return Err(::bbnf::runtime::tape::DtaError::Syntax {
                        offset: start as u32,
                        failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                        failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                    });
                }
                if int_digit_count as i64 + fractional_digit_count > 19 {
                    many_digits = true;
                }
            }

            let mut exponent: i64 = -fractional_digit_count;
            if matches!(input.get(*p), Some(b'e') | Some(b'E')) {
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
                    return Err(::bbnf::runtime::tape::DtaError::Syntax {
                        offset: start as u32,
                        failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                        failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
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

            let off = builder.push_leaf_with(
                ::bbnf::runtime::tape::TapeKind::Regex,
                start as u32,
                end as u32,
                #variant_idx,
                0,
                ::bbnf::runtime::tape::PayloadData::WideScalar(value.to_bits()),
            );
            Ok(off)
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
