//! Unified shorthand character-class detection.
//!
//! Detects well-known character classes (`\d`, `\w`, `\s`, `[a-zA-Z]`,
//! `[a-zA-Z0-9]`, `[0-9a-fA-F]`) from their canonical byte representations
//! and emits `is_ascii_*` predicate expressions.
//!
//! Single implementation shared by the HIR walker, DFA emitter, and
//! generalized fast-path emitter.

use parse_that::regex::hir::ByteRange;
use proc_macro2::TokenStream;
use quote::quote;

/// Recognized shorthand character classes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ShorthandClass {
    /// `\d` / `[0-9]`
    Digit,
    /// `\w` / `[0-9A-Za-z_]`
    Word,
    /// `\s` / ASCII whitespace
    Whitespace,
    /// `[a-zA-Z]`
    Alpha,
    /// `[a-zA-Z0-9]`
    Alphanumeric,
    /// `[0-9a-fA-F]`
    Hex,
}

/// Detect a shorthand class from canonical `ByteRange` slices
/// (as produced by the bespoke regex parser normalization).
pub fn detect_from_ranges(ranges: &[ByteRange]) -> Option<ShorthandClass> {
    match ranges.len() {
        1 if ranges[0].start == b'0' && ranges[0].end == b'9' => Some(ShorthandClass::Digit),
        2 if ranges[0] == ByteRange::new(0x09, 0x0D) && ranges[1] == ByteRange::new(0x20, 0x20) => {
            Some(ShorthandClass::Whitespace)
        }
        2 if ranges[0] == ByteRange::new(b'A', b'Z') && ranges[1] == ByteRange::new(b'a', b'z') => {
            Some(ShorthandClass::Alpha)
        }
        3 if ranges[0] == ByteRange::new(b'0', b'9')
            && ranges[1] == ByteRange::new(b'A', b'Z')
            && ranges[2] == ByteRange::new(b'a', b'z') =>
        {
            Some(ShorthandClass::Alphanumeric)
        }
        3 if ranges[0] == ByteRange::new(b'0', b'9')
            && ranges[1] == ByteRange::new(b'A', b'F')
            && ranges[2] == ByteRange::new(b'a', b'f') =>
        {
            Some(ShorthandClass::Hex)
        }
        4 if ranges[0] == ByteRange::new(b'0', b'9')
            && ranges[1] == ByteRange::new(b'A', b'Z')
            && ranges[2] == ByteRange::new(b'_', b'_')
            && ranges[3] == ByteRange::new(b'a', b'z') =>
        {
            Some(ShorthandClass::Word)
        }
        _ => None,
    }
}

/// Detect a shorthand class from an unsorted byte slice (DFA path).
pub fn detect_from_bytes(bytes: &[u8]) -> Option<ShorthandClass> {
    // Build sorted, deduplicated ranges from raw bytes, then delegate.
    let mut sorted = bytes.to_vec();
    sorted.sort_unstable();
    sorted.dedup();
    let ranges = bytes_to_ranges(&sorted);
    detect_from_ranges(&ranges)
}

/// Emit a `TokenStream` predicate expression for a detected shorthand class.
pub fn emit_predicate(class: ShorthandClass) -> TokenStream {
    match class {
        ShorthandClass::Digit => quote! { __b.is_ascii_digit() },
        ShorthandClass::Word => quote! { (__b.is_ascii_alphanumeric() || __b == b'_') },
        ShorthandClass::Whitespace => quote! { __b.is_ascii_whitespace() },
        ShorthandClass::Alpha => quote! { __b.is_ascii_alphabetic() },
        ShorthandClass::Alphanumeric => quote! { __b.is_ascii_alphanumeric() },
        ShorthandClass::Hex => quote! { __b.is_ascii_hexdigit() },
    }
}

/// Convert sorted byte slice to `ByteRange` for range-based detection.
fn bytes_to_ranges(sorted: &[u8]) -> Vec<ByteRange> {
    if sorted.is_empty() {
        return Vec::new();
    }
    let mut ranges = Vec::new();
    let mut start = sorted[0];
    let mut end = sorted[0];
    for &b in &sorted[1..] {
        if b == end + 1 {
            end = b;
        } else {
            ranges.push(ByteRange::new(start, end));
            start = b;
            end = b;
        }
    }
    ranges.push(ByteRange::new(start, end));
    ranges
}
