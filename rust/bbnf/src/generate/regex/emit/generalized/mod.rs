//! Generalized regex direct emission — strength-reduces regex patterns into
//! inline byte-scanning loops (char ranges, char sets, shorthand classes,
//! literal-prefix + class tail, whitespace-padded literals).

mod class_segments;

use class_segments::{emit_char_class_loop, emit_literal_prefix_class};

use proc_macro2::TokenStream;
use quote::quote;

use super::negated_class::try_strip_ws_padded_literal;

/// Whether a pattern is a simple character range like `[a-z]` or `[0-9]`.
/// Returns `(lo, hi)` byte range if detected.
fn is_single_char_range_regex(pattern: &str) -> Option<(u8, u8)> {
    let inner = pattern.strip_prefix('[')?.strip_suffix(']')?;
    // Must be exactly "X-Y" where X and Y are single ASCII characters.
    if inner.len() == 3 && inner.as_bytes()[1] == b'-' {
        let lo = inner.as_bytes()[0];
        let hi = inner.as_bytes()[2];
        if lo.is_ascii() && hi.is_ascii() && lo < hi {
            return Some((lo, hi));
        }
    }
    None
}

/// Whether a pattern is a simple character set like `[abc]` (no ranges, no escapes).
/// Returns the set of bytes if detected (max 8 bytes for practical emission).
fn is_small_char_set_regex(pattern: &str) -> Option<Vec<u8>> {
    let inner = pattern.strip_prefix('[')?.strip_suffix(']')?;
    // Must not contain ranges, escapes, or negation.
    if inner.starts_with('^') || inner.contains('-') || inner.contains('\\') {
        return None;
    }
    let bytes: Vec<u8> = inner.bytes().collect();
    if bytes.len() >= 2 && bytes.len() <= 8 && bytes.iter().all(|b| b.is_ascii()) {
        Some(bytes)
    } else {
        None
    }
}

/// Whether a pattern is a single-char range with `+` quantifier like `[a-z]+`.
/// Returns `(lo, hi)` byte range.
fn is_char_range_plus_regex(pattern: &str) -> Option<(u8, u8)> {
    let inner = pattern.strip_suffix('+')?;
    is_single_char_range_regex(inner)
}

/// Whether a pattern is a single-char range with `*` quantifier like `[a-z]*`.
/// Returns `(lo, hi)` byte range.
fn is_char_range_star_regex(pattern: &str) -> Option<(u8, u8)> {
    let inner = pattern.strip_suffix('*')?;
    is_single_char_range_regex(inner)
}

/// Emit a direct call for generalized regex patterns beyond JSON/CSS.
///
/// Covers:
/// - `[a-z]` → single byte range check
/// - `[abc]` → small character set match (2-8 chars)
/// - `[a-z]+` → byte range scan loop
/// - `[a-z]*` → byte range scan loop (zero-or-more)
/// - `\s+`, `\d+`, `\w+` → shorthand class scan loops
/// - `prefix[class]+` → literal prefix + char class tail (e.g., `--[\w-]+`, `@[a-z][\w-]*`)
pub fn emit_generalized_regex_direct(pattern: &str) -> Option<TokenStream> {
    // Whitespace-padded literal: \s*LITERAL\s* — matches a fixed literal
    // with optional surrounding whitespace. Common in CSS for comma separators,
    // combinator operators, etc. Generalizes the \s*,\s* pattern.
    if let Some(inner) = try_strip_ws_padded_literal(pattern) {
        let inner_bytes = inner.as_bytes();
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

    // Single character range: [a-z]
    if let Some((lo, hi)) = is_single_char_range_regex(pattern) {
        let lo_lit = proc_macro2::Literal::byte_character(lo);
        let hi_lit = proc_macro2::Literal::byte_character(hi);
        return Some(quote! {
            {
                let __start = state.offset;
                if let Some(&__b) = state.src_bytes.get(__start) {
                    if __b >= #lo_lit && __b <= #hi_lit {
                        state.offset = __start + 1;
                        Some(::parse_that::Span::new(__start, __start + 1, state.src))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
        });
    }

    // Small character set: [abc]
    if let Some(bytes) = is_small_char_set_regex(pattern) {
        let byte_lits: Vec<_> = bytes
            .iter()
            .map(|b| proc_macro2::Literal::byte_character(*b))
            .collect();
        return Some(quote! {
            {
                let __start = state.offset;
                if let Some(&__b) = state.src_bytes.get(__start) {
                    if matches!(__b, #(#byte_lits)|*) {
                        state.offset = __start + 1;
                        Some(::parse_that::Span::new(__start, __start + 1, state.src))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
        });
    }

    // Character range with + quantifier: [a-z]+
    if let Some((lo, hi)) = is_char_range_plus_regex(pattern) {
        let lo_lit = proc_macro2::Literal::byte_character(lo);
        let hi_lit = proc_macro2::Literal::byte_character(hi);
        return Some(quote! {
            {
                let __start = state.offset;
                let __end = state.src_bytes.len();
                let mut __pos = __start;
                while __pos < __end {
                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                    if __b >= #lo_lit && __b <= #hi_lit {
                        __pos += 1;
                    } else {
                        break;
                    }
                }
                if __pos > __start {
                    state.offset = __pos;
                    Some(::parse_that::Span::new(__start, __pos, state.src))
                } else {
                    None
                }
            }
        });
    }

    // Character range with * quantifier: [a-z]*
    if let Some((lo, hi)) = is_char_range_star_regex(pattern) {
        let lo_lit = proc_macro2::Literal::byte_character(lo);
        let hi_lit = proc_macro2::Literal::byte_character(hi);
        return Some(quote! {
            {
                let __start = state.offset;
                let __end = state.src_bytes.len();
                let mut __pos = __start;
                while __pos < __end {
                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                    if __b >= #lo_lit && __b <= #hi_lit {
                        __pos += 1;
                    } else {
                        break;
                    }
                }
                state.offset = __pos;
                Some(::parse_that::Span::new(__start, __pos, state.src))
            }
        });
    }

    // General char class loop: [0-9a-fA-F]+, [\w-]+, etc.
    if let Some(ts) = emit_char_class_loop(pattern) {
        return Some(ts);
    }

    // Shorthand class with quantifier: \s+, \d+, \w+, \s*, \d*, \w*
    if let Some(ts) = emit_shorthand_class_loop(pattern) {
        return Some(ts);
    }

    // Literal prefix followed by char class: --[\w-]+, @[a-zA-Z][\w-]*, etc.
    if let Some(ts) = emit_literal_prefix_class(pattern) {
        return Some(ts);
    }

    None
}

/// Emit inline byte scan for shorthand class + quantifier: `\s+`, `\d+`, `\w+`, `\s*`, etc.
///
/// These are bare escape sequences (not wrapped in `[...]`) followed by `+` or `*`.
/// Compiles to a tight byte-predicate loop with no regex engine overhead.
pub(crate) fn emit_shorthand_class_loop(pattern: &str) -> Option<TokenStream> {
    let (shorthand, is_plus) = if let Some(s) = pattern.strip_suffix('+') {
        (s, true)
    } else if let Some(s) = pattern.strip_suffix('*') {
        (s, false)
    } else {
        return None;
    };

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
