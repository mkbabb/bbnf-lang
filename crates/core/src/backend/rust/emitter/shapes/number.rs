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

use bbnf_ir::{GrammarIR, IrNode, IrRule};
use proc_macro2::TokenStream;
use quote::quote;

use super::dispatcher::shape_fn_ident;
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
    emit_parse_number_struct_direct(&fn_ident, rust.builder_path, NumberForm::of(rule, ir))
}

/// The parts of the rule's number regex the scanner reads, from its
/// `RegexClass::Numeric` classification (sk-v27 LW3.ORC1-NUM). The scanner
/// reads what the regex reads, with the regex's own semantics: an optional
/// group that does not match whole is not taken, so `1.` reads `1` and
/// leaves the `.`, and `0|[1-9]\d*` reads a lone `0` from `01`.
#[derive(Clone, Copy, Debug)]
struct NumberForm {
    /// `0|[1-9]\d*`: a leading `0` is the whole integer part.
    reject_leading_zero: bool,
    /// The regex has a `(\.\d+)?` part.
    allows_fraction: bool,
    /// The regex has a `([eE][+-]?\d+)?` part.
    allows_exponent: bool,
}

impl NumberForm {
    fn of(rule: &IrRule, ir: &GrammarIR) -> Self {
        use parse_that::regex::classify::RegexClass;
        fn regex(node: &IrNode) -> Option<u32> {
            match node {
                IrNode::Regex(sid) => Some(*sid),
                IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => regex(inner),
                _ => None,
            }
        }
        let class = regex(&rule.body)
            .and_then(|sid| ir.regex_info.get(&sid))
            .map(|info| &info.classification);
        match class {
            Some(RegexClass::Numeric {
                reject_leading_zero,
                allows_fraction,
                allows_exponent,
                ..
            }) => Self {
                reject_leading_zero: *reject_leading_zero,
                allows_fraction: *allows_fraction,
                allows_exponent: *allows_exponent,
            },
            _ => panic!(
                "Number shape on rule `{}` without a Numeric regex classification",
                ir.get_string(rule.name)
            ),
        }
    }
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
    form: NumberForm,
) -> TokenStream {
    let builder_ty: syn::Path = syn::parse_str(builder_path)
        .expect("EmitStrategy::StructDirect.builder_path must parse as a Rust path");
    // The integer part: `0|[1-9]\d*` reads a lone `0` when the run starts
    // with `0`; `\d+` reads the whole run.
    let int_scan = if form.reject_leading_zero {
        quote! {
            if input.get(*p) == Some(&b'0') {
                *p += 1;
            } else {
                while *p < len {
                    let b = input[*p];
                    if b.is_ascii_digit() {
                        mantissa = mantissa.wrapping_mul(10)
                            .wrapping_add((b - b'0') as u64);
                        *p += 1;
                    } else { break; }
                }
            }
        }
    } else {
        quote! {
            while *p < len {
                let b = input[*p];
                if b.is_ascii_digit() {
                    mantissa = mantissa.wrapping_mul(10)
                        .wrapping_add((b - b'0') as u64);
                    *p += 1;
                } else { break; }
            }
        }
    };
    // `(\.\d+)?`: taken only when a digit follows the `.`.
    let frac_scan = if form.allows_fraction {
        quote! {
            if input.get(*p) == Some(&b'.')
                && input.get(*p + 1).is_some_and(u8::is_ascii_digit)
            {
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
                if int_digit_count as i64 + fractional_digit_count > 19 {
                    many_digits = true;
                }
            }
        }
    } else {
        quote! {}
    };
    // `([eE][+-]?\d+)?`: taken only when a digit follows the marker and
    // its optional sign.
    let exp_scan = if form.allows_exponent {
        quote! {
            let exp_byte = input.get(*p).copied();
            if exp_byte == Some(b'e') || exp_byte == Some(b'E') {
                let (exp_negative, digits_at) = match input.get(*p + 1) {
                    Some(b'+') => (false, *p + 2),
                    Some(b'-') => (true, *p + 2),
                    _ => (false, *p + 1),
                };
                if input.get(digits_at).is_some_and(u8::is_ascii_digit) {
                    *p = digits_at;
                    let mut exp_val: i64 = 0;
                    while *p < len {
                        let b = input[*p];
                        if b.is_ascii_digit() {
                            exp_val = exp_val.saturating_mul(10)
                                .saturating_add((b - b'0') as i64);
                            *p += 1;
                        } else { break; }
                    }
                    exponent += if exp_negative { -exp_val } else { exp_val };
                }
            }
        }
    } else {
        quote! {}
    };
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
        #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
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
            #int_scan
            if *p == int_start {
                return Err(crate::runtime::DtaError::Syntax {
                    offset: start as u32,
                });
            }
            let int_digit_count = *p - int_start;
            if int_digit_count > 19 { many_digits = true; }

            let mut fractional_digit_count: i64 = 0;
            #frac_scan

            let mut exponent: i64 = -fractional_digit_count;
            #exp_scan

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
