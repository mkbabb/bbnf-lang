//! Generalized regex direct emission — strength-reduces regex patterns into
//! inline byte-scanning loops (char ranges, char sets, shorthand classes,
//! literal-prefix + class tail, whitespace-padded literals).
//!
//! Pattern detection uses `RegexClass::CharClassQuantified` and
//! `RegexClass::PrefixThenClass` from the structural classifier rather
//! than hand-rolled regex string parsers.

mod class_segments;

use class_segments::{emit_class_quantified, emit_prefix_then_class};

use proc_macro2::TokenStream;
use quote::quote;

use parse_that::regex::classify::RegexClass;

use super::negated_class::try_strip_ws_padded_literal;
use crate::backend::kernels;
use crate::generate::regex::cost_model::EmitOpts;

/// Emit a direct call for generalized regex patterns beyond JSON/CSS.
///
/// Covers:
/// - `[a-z]` → single byte range check
/// - `[abc]` → small character set match (2-8 chars)
/// - `[a-z]+` → byte range scan loop
/// - `[a-z]*` → byte range scan loop (zero-or-more)
/// - `\s+`, `\d+`, `\w+` → shorthand class scan loops
/// - `prefix[class]+` → literal prefix + char class tail (e.g., `--[\w-]+`, `@[a-z][\w-]*`)
///
/// Pattern detection uses `opts.classify_regex(pattern)` to resolve via
/// cached `RegexInfo` when available, eliminating hand-rolled string parsing.
pub fn emit_generalized_regex_direct(pattern: &str, opts: &EmitOpts) -> Option<TokenStream> {
    // Whitespace-padded literal: \s*LITERAL\s* — matches a fixed literal
    // with optional surrounding whitespace. Common in CSS for comma separators,
    // combinator operators, etc. Detected via HIR concat inspection.
    if let Some(inner_bytes) = try_strip_ws_padded_literal(pattern) {
        let inner_len = inner_bytes.len();
        let byte_lits: Vec<proc_macro2::Literal> = inner_bytes
            .iter()
            .map(|b| proc_macro2::Literal::byte_character(*b))
            .collect();
        let check = if inner_len == 1 {
            quote! {
                __pos < state.src_bytes.len()
                    && unsafe { *state.src_bytes.get_unchecked(__pos) } == #(#byte_lits)*
            }
        } else {
            quote! {
                state.src_bytes.get(__pos..__pos + #inner_len) == Some(&[#(#byte_lits),*] as &[u8])
            }
        };
        return Some(quote! {
            {
                let __start = state.offset;
                let mut __pos = __start;
                while __pos < state.src_bytes.len()
                    && unsafe { *state.src_bytes.get_unchecked(__pos) }.is_ascii_whitespace()
                { __pos += 1; }
                if #check {
                    __pos += #inner_len;
                    while __pos < state.src_bytes.len()
                        && unsafe { *state.src_bytes.get_unchecked(__pos) }.is_ascii_whitespace()
                    { __pos += 1; }
                    state.offset = __pos;
                    Some(::parse_that::Span::new(__start, __pos, state.src))
                } else {
                    None
                }
            }
        });
    }

    // Classify the pattern structurally via the cached RegexInfo.
    let class = opts.classify_regex(pattern);

    match class {
        // Quantified char class: [a-z], [abc], [a-z]+, [0-9a-fA-F]*, etc.
        RegexClass::CharClassQuantified(ref info) => {
            // Negated classes are handled by the negated_class path upstream.
            if info.negated {
                return None;
            }
            emit_class_quantified(info)
        }

        // Literal prefix + class tail: --[\w-]+, @[a-zA-Z][\w-]*, etc.
        RegexClass::PrefixThenClass {
            ref prefix,
            ref tail,
        } => emit_prefix_then_class(prefix, tail),

        // Shorthand class with quantifier: \s+, \d+, \w+, \s*, \d*, \w*
        // These don't classify as CharClassQuantified because the classifier
        // resolves shorthands into the broader class variants. Route through
        // the shorthand emitter which handles kernel routing.
        _ => {
            if let Some(ts) = emit_shorthand_class_loop(pattern) {
                return Some(ts);
            }
            None
        }
    }
}

/// Emit inline byte scan for shorthand class + quantifier: `\s+`, `\d+`, `\w+`, `\s*`, etc.
///
/// These are bare escape sequences (not wrapped in `[...]`) followed by `+` or `*`.
/// Compiles to a tight byte-predicate loop with no regex engine overhead.
///
/// Tranche X phase 1: routes the recognized shapes (`\d+`, `\d*`, `\w+`)
/// through `kernels::charclass::emit_call_opt` first; falls through to
/// the inline predicate loop for `\s+`/`\s*` and the unrecognized
/// shapes per §3 rule 16.
pub(crate) fn emit_shorthand_class_loop(pattern: &str) -> Option<TokenStream> {
    let (shorthand, is_plus) = if let Some(s) = pattern.strip_suffix('+') {
        (s, true)
    } else if let Some(s) = pattern.strip_suffix('*') {
        (s, false)
    } else {
        return None;
    };

    // Tranche X phase 1: kernel routing short-circuit for the
    // recognized shapes (`\d+`, `\d*`, `\w+`). The kernel emits a
    // direct call to `parse_that::scan_*_mut` instead of the
    // sixteen-line inline `is_ascii_*` while-loop below.
    if let Some(chars) = kernels::charclass::charset_from_shorthand(shorthand) {
        let lo = if is_plus { 1u32 } else { 0u32 };
        if let Some(call) = kernels::charclass::emit_call_opt(&chars, false, lo, None) {
            return Some(quote! { { #call } });
        }
    }

    // Must be exactly a shorthand: \s, \d, \w
    let predicate = match shorthand {
        r"\s" => quote! { __b.is_ascii_whitespace() },
        r"\d" => quote! { __b.is_ascii_digit() },
        r"\w" => quote! { (__b.is_ascii_alphanumeric() || __b == b'_') },
        _ => return None,
    };

    if is_plus {
        Some(quote! {
            {
                let __start = state.offset;
                let __end = state.src_bytes.len();
                let mut __pos = __start;
                while __pos < __end {
                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                    if #predicate { __pos += 1; } else { break; }
                }
                if __pos > __start {
                    state.offset = __pos;
                    Some(::parse_that::Span::new(__start, __pos, state.src))
                } else {
                    None
                }
            }
        })
    } else {
        Some(quote! {
            {
                let __start = state.offset;
                let __end = state.src_bytes.len();
                let mut __pos = __start;
                while __pos < __end {
                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                    if #predicate { __pos += 1; } else { break; }
                }
                state.offset = __pos;
                Some(::parse_that::Span::new(__start, __pos, state.src))
            }
        })
    }
}
