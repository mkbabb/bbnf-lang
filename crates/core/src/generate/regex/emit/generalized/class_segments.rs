//! Emission for classified char class patterns — `ClassRangeInfo` and
//! `PrefixThenClass` from the structural classifier.
//!
//! Replaces the hand-rolled `parse_class_segments` / `char_class_to_predicate`
//! string parsers with direct emission from `CharSet128` bitsets.

use proc_macro2::TokenStream;
use quote::quote;

use parse_that::regex::classify::ClassRangeInfo;
use parse_that::regex::sets::charset::CharSet128;

use crate::backend::kernels;

/// Emit scanning code for a `CharClassQuantified` pattern.
///
/// Handles single-byte match (`{1,1}`), optional (`{0,1}`), quantified
/// loops (`+`, `*`), and bounded repeats (`{n,m}`).
///
/// Routes through `kernels::charclass::emit_call_opt` first for
/// shapes the kernel recognizes; falls through to inline predicate
/// emission for the rest.
pub(super) fn emit_class_quantified(info: &ClassRangeInfo) -> Option<TokenStream> {
    let ClassRangeInfo {
        chars,
        negated,
        min,
        max,
    } = info;

    // Negated classes are handled by the negated_class path upstream.
    if *negated {
        return None;
    }

    let min_usize = *min as usize;
    let max_usize = max.map(|m| m as usize).unwrap_or(usize::MAX);

    // Tranche X phase 1: kernel routing short-circuit.
    // The kernel collapses `[0-9]+`, `[0-9]*`, `[a-zA-Z0-9]+`,
    // `[0-9a-fA-F]+` onto the hoisted `parse_that::scan_*_mut` helpers.
    if max.is_none() && (*min == 0 || *min == 1) {
        if let Some(call) = kernels::charclass::emit_call_opt(chars, *negated, *min, *max) {
            return Some(quote! { { #call } });
        }
    }

    // Build a byte predicate from the CharSet128 bitset.
    let predicate = charset_to_predicate(chars)?;

    // Single character match: {1,1}
    if min_usize == 1 && max_usize == 1 {
        return Some(quote! {
            {
                let __start = state.offset;
                if let Some(&__b) = state.src_bytes.get(__start) {
                    if #predicate {
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

    // Optional: {0,1}
    if min_usize == 0 && max_usize == 1 {
        return Some(quote! {
            {
                let __start = state.offset;
                if let Some(&__b) = state.src_bytes.get(__start) {
                    if #predicate {
                        state.offset = __start + 1;
                    }
                }
                Some(::parse_that::Span::new(__start, state.offset, state.src))
            }
        });
    }

    // Unbounded repeat: + or *
    if max_usize == usize::MAX {
        let min_lit = proc_macro2::Literal::usize_unsuffixed(min_usize);
        if min_usize >= 1 {
            return Some(quote! {
                {
                    let __start = state.offset;
                    let __end = state.src_bytes.len();
                    let mut __pos = __start;
                    while __pos < __end {
                        let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                        if #predicate { __pos += 1; } else { break; }
                    }
                    if __pos >= __start + #min_lit {
                        state.offset = __pos;
                        Some(::parse_that::Span::new(__start, __pos, state.src))
                    } else {
                        None
                    }
                }
            });
        } else {
            return Some(quote! {
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
            });
        }
    }

    // Bounded repeat: {n,m}
    let min_lit = proc_macro2::Literal::usize_unsuffixed(min_usize);
    let max_lit = proc_macro2::Literal::usize_unsuffixed(max_usize);
    Some(quote! {
        {
            let __start = state.offset;
            let __end = state.src_bytes.len();
            let mut __pos = __start;
            let mut __count: usize = 0;
            while __pos < __end && __count < #max_lit {
                let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                if #predicate { __pos += 1; __count += 1; } else { break; }
            }
            if __count >= #min_lit {
                state.offset = __pos;
                Some(::parse_that::Span::new(__start, __pos, state.src))
            } else {
                None
            }
        }
    })
}

/// Emit scanning code for a `PrefixThenClass` pattern.
///
/// Matches a literal prefix exactly, then scans a quantified char class
/// tail. Routes through `kernels::prefix_class::emit_call_opt` first;
/// falls through to inline emission for unrecognized shapes.
pub(super) fn emit_prefix_then_class(
    prefix: &[u8],
    tail: &ClassRangeInfo,
) -> Option<TokenStream> {
    let ClassRangeInfo {
        chars,
        negated,
        min,
        max,
    } = tail;

    // Negated tails not supported here.
    if *negated {
        return None;
    }

    // Tranche X.9a: kernel routing for common `prefix[class]+` shapes.
    if *min == 1 && max.is_none() {
        if let Some(call) = kernels::prefix_class::emit_call_opt(prefix, chars) {
            return Some(call);
        }
    }

    // Build inline emission from the classifier data.
    let prefix_bytes = prefix;
    let prefix_len = prefix_bytes.len();
    let prefix_lit = proc_macro2::Literal::byte_string(prefix_bytes);
    let prefix_len_lit = proc_macro2::Literal::usize_unsuffixed(prefix_len);

    let predicate = charset_to_predicate(chars)?;

    let min_usize = *min as usize;
    let max_usize = max.map(|m| m as usize).unwrap_or(usize::MAX);

    let (scan_body, check) = if max_usize == usize::MAX {
        // Unbounded: scan loop
        let scan = quote! {
            while __pos < __end {
                let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                if #predicate { __pos += 1; } else { break; }
            }
        };
        if min_usize > 0 {
            let total_min_lit =
                proc_macro2::Literal::usize_unsuffixed(prefix_len + min_usize);
            (
                scan,
                quote! {
                    if __pos >= __start + #total_min_lit {
                        state.offset = __pos;
                        Some(::parse_that::Span::new(__start, __pos, state.src))
                    } else {
                        None
                    }
                },
            )
        } else {
            (
                scan,
                quote! {
                    state.offset = __pos;
                    Some(::parse_that::Span::new(__start, __pos, state.src))
                },
            )
        }
    } else {
        // Bounded: {min,max}
        let min_lit = proc_macro2::Literal::usize_unsuffixed(min_usize);
        let max_lit = proc_macro2::Literal::usize_unsuffixed(max_usize);
        let scan = quote! {
            let mut __count: usize = 0;
            while __pos < __end && __count < #max_lit {
                let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                if #predicate { __pos += 1; __count += 1; } else { break; }
            }
        };
        let check = if min_usize > 0 {
            quote! {
                if __count >= #min_lit {
                    state.offset = __pos;
                    Some(::parse_that::Span::new(__start, __pos, state.src))
                } else {
                    None
                }
            }
        } else {
            quote! {
                state.offset = __pos;
                Some(::parse_that::Span::new(__start, __pos, state.src))
            }
        };
        (scan, check)
    };

    Some(quote! {
        {
            let __start = state.offset;
            let __end = state.src_bytes.len();
            if state.src_bytes.get(__start..__start + #prefix_len_lit) == Some(#prefix_lit as &[u8]) {
                let mut __pos = __start + #prefix_len_lit;
                #scan_body
                #check
            } else {
                None
            }
        }
    })
}

// ── CharSet128 → TokenStream predicate ─────────────────────────────────────

/// Convert a `CharSet128` bitset to a Rust byte predicate expression.
///
/// Detects contiguous ranges and emits range checks (`__b >= lo && __b <= hi`)
/// for efficiency; falls back to individual byte matches for sparse sets.
/// Also detects common shorthands (ascii_digit, ascii_alphanumeric, etc.).
fn charset_to_predicate(chars: &CharSet128) -> Option<TokenStream> {
    if chars.is_empty() {
        return None;
    }

    // Extract contiguous ranges from the bitset.
    let ranges = extract_ranges(chars);
    if ranges.is_empty() {
        return None;
    }

    // Check for shorthand predicates first.
    if let Some(shorthand) = try_shorthand_predicate(chars, &ranges) {
        return Some(shorthand);
    }

    let mut conditions: Vec<TokenStream> = Vec::new();

    for (lo, hi) in &ranges {
        if lo == hi {
            let lit = proc_macro2::Literal::byte_character(*lo);
            conditions.push(quote! { __b == #lit });
        } else {
            let lo_lit = proc_macro2::Literal::byte_character(*lo);
            let hi_lit = proc_macro2::Literal::byte_character(*hi);
            conditions.push(quote! { (__b >= #lo_lit && __b <= #hi_lit) });
        }
    }

    if conditions.len() == 1 {
        Some(conditions.into_iter().next().unwrap())
    } else {
        Some(quote! { #(#conditions)||* })
    }
}

/// Extract contiguous `(lo, hi)` byte ranges from a `CharSet128`.
fn extract_ranges(chars: &CharSet128) -> Vec<(u8, u8)> {
    let mut ranges = Vec::new();
    let mut start: Option<u8> = None;
    let mut prev: u8 = 0;

    for b in 0u8..128 {
        if chars.has(b) {
            match start {
                None => {
                    start = Some(b);
                    prev = b;
                }
                Some(_) => {
                    if b == prev + 1 {
                        prev = b;
                    } else {
                        ranges.push((start.unwrap(), prev));
                        start = Some(b);
                        prev = b;
                    }
                }
            }
        }
    }
    if let Some(s) = start {
        ranges.push((s, prev));
    }
    ranges
}

/// Try to recognize common shorthand predicates for compact emission.
fn try_shorthand_predicate(
    chars: &CharSet128,
    ranges: &[(u8, u8)],
) -> Option<TokenStream> {
    // \w → [0-9A-Za-z_]
    let mut word_chars = CharSet128::new();
    word_chars.add_range(b'0', b'9');
    word_chars.add_range(b'A', b'Z');
    word_chars.add(b'_');
    word_chars.add_range(b'a', b'z');
    if *chars == word_chars {
        return Some(quote! { (__b.is_ascii_alphanumeric() || __b == b'_') });
    }

    // \w + '-' → [\w-]
    let mut word_dash = word_chars;
    word_dash.add(b'-');
    if *chars == word_dash {
        return Some(quote! { (__b.is_ascii_alphanumeric() || __b == b'_' || __b == b'-') });
    }

    // \d → [0-9]
    if ranges.len() == 1 && ranges[0] == (b'0', b'9') {
        return Some(quote! { __b.is_ascii_digit() });
    }

    // \s → whitespace
    let mut ws_chars = CharSet128::new();
    ws_chars.add_range(0x09, 0x0D);
    ws_chars.add(b' ');
    if *chars == ws_chars {
        return Some(quote! { __b.is_ascii_whitespace() });
    }

    None
}
