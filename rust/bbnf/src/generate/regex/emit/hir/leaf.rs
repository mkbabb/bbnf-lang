//! Leaf HIR emitters: literals, character classes, look-around assertions.
//!
//! Self-contained emitters for terminal HIR nodes that match a single
//! character, fixed byte sequence, or position assertion.

use proc_macro2::TokenStream;
use quote::quote;
use regex_syntax::hir::{Class, ClassBytes, ClassBytesRange, ClassUnicode, Look};

// ── Literal ──────────────────────────────────────────────────────────────────

/// Emit inline code for a fixed byte sequence literal.
pub(super) fn emit_literal(bytes: &[u8]) -> Option<TokenStream> {
    if bytes.is_empty() {
        return Some(quote! {});
    }
    let len = bytes.len();
    if len == 1 {
        let b = proc_macro2::Literal::byte_character(bytes[0]);
        Some(quote! {
            if state.src_bytes.get(state.offset).copied() != Some(#b) {
                return None;
            }
            state.offset += 1;
        })
    } else {
        let byte_lits: Vec<proc_macro2::Literal> = bytes
            .iter()
            .map(|b| proc_macro2::Literal::byte_character(*b))
            .collect();
        Some(quote! {
            if state.src_bytes.get(state.offset..state.offset + #len)
                != Some(&[#(#byte_lits),*] as &[u8])
            {
                return None;
            }
            state.offset += #len;
        })
    }
}

// ── Class (single character) ─────────────────────────────────────────────────

/// Emit code to match a single character from a character class.
///
/// This handles matching ONE character. For repeated classes (e.g., `[a-z]+`),
/// the `Repetition` handler calls `emit_class_predicate` for the loop body.
pub(super) fn emit_class_single(class: &Class) -> Option<TokenStream> {
    let predicate = emit_class_predicate(class)?;
    Some(quote! {
        {
            let __b = *state.src_bytes.get(state.offset)?;
            if !(#predicate) {
                return None;
            }
            state.offset += 1;
        }
    })
}

/// Emit a boolean predicate expression that checks if `__b: u8` matches the class.
///
/// Returns `None` for Unicode classes with non-ASCII ranges.
pub(super) fn emit_class_predicate(class: &Class) -> Option<TokenStream> {
    match class {
        Class::Bytes(cb) => emit_bytes_class_predicate(cb),
        Class::Unicode(cu) => emit_unicode_class_predicate(cu),
    }
}

/// Emit a predicate for a byte-mode character class.
fn emit_bytes_class_predicate(cb: &ClassBytes) -> Option<TokenStream> {
    let ranges = cb.ranges();
    if ranges.is_empty() {
        return Some(quote! { false });
    }

    // Detect well-known shorthand patterns by their canonical ranges.
    if let Some(shorthand) = detect_shorthand_bytes(ranges) {
        return Some(shorthand);
    }

    // General case: emit range checks.
    emit_ranges_predicate(ranges)
}

/// Emit a predicate for a Unicode character class.
///
/// We only handle classes that are entirely within ASCII (0..=127).
/// Anything with non-ASCII codepoints bails out -- caller uses DFA tier.
fn emit_unicode_class_predicate(cu: &ClassUnicode) -> Option<TokenStream> {
    let ranges = cu.ranges();
    // Ensure all ranges are ASCII.
    for r in ranges {
        if r.start() > '\x7F' || r.end() > '\x7F' {
            return None;
        }
    }
    // Convert Unicode ranges to byte ranges and reuse byte logic.
    let byte_ranges: Vec<ClassBytesRange> = ranges
        .iter()
        .map(|r| ClassBytesRange::new(r.start() as u8, r.end() as u8))
        .collect();

    if let Some(shorthand) = detect_shorthand_bytes(&byte_ranges) {
        return Some(shorthand);
    }
    emit_ranges_predicate(&byte_ranges)
}

/// Try to detect well-known shorthand classes from their canonical byte ranges.
///
/// regex-syntax normalizes `\d` to `[0-9]`, `\s` to the canonical whitespace
/// ranges, `\w` to `[0-9A-Za-z_]`, etc. We detect these and emit the
/// corresponding Rust `is_ascii_*` calls.
fn detect_shorthand_bytes(ranges: &[ClassBytesRange]) -> Option<TokenStream> {
    // \d = [0-9]
    if ranges.len() == 1 && ranges[0].start() == b'0' && ranges[0].end() == b'9' {
        return Some(quote! { __b.is_ascii_digit() });
    }

    // \w = [0-9A-Za-z_] -- regex-syntax normalizes to 4 ranges:
    // 0-9, A-Z, _, a-z  (sorted by start byte)
    if ranges.len() == 4
        && ranges[0] == ClassBytesRange::new(b'0', b'9')
        && ranges[1] == ClassBytesRange::new(b'A', b'Z')
        && ranges[2] == ClassBytesRange::new(b'_', b'_')
        && ranges[3] == ClassBytesRange::new(b'a', b'z')
    {
        return Some(quote! { (__b.is_ascii_alphanumeric() || __b == b'_') });
    }

    // \s (ASCII mode) -- regex-syntax normalizes to:
    // [\t\n\x0B\x0C\r ] = ranges: [0x09-0x0D, 0x20-0x20]
    if ranges.len() == 2
        && ranges[0] == ClassBytesRange::new(0x09, 0x0D)
        && ranges[1] == ClassBytesRange::new(0x20, 0x20)
    {
        return Some(quote! { __b.is_ascii_whitespace() });
    }

    // [a-zA-Z] -- 2 ranges for alpha
    if ranges.len() == 2
        && ranges[0] == ClassBytesRange::new(b'A', b'Z')
        && ranges[1] == ClassBytesRange::new(b'a', b'z')
    {
        return Some(quote! { __b.is_ascii_alphabetic() });
    }

    // [a-zA-Z0-9] -- 3 ranges for alphanumeric
    if ranges.len() == 3
        && ranges[0] == ClassBytesRange::new(b'0', b'9')
        && ranges[1] == ClassBytesRange::new(b'A', b'Z')
        && ranges[2] == ClassBytesRange::new(b'a', b'z')
    {
        return Some(quote! { __b.is_ascii_alphanumeric() });
    }

    // [0-9a-fA-F] -- hex digits
    if ranges.len() == 3
        && ranges[0] == ClassBytesRange::new(b'0', b'9')
        && ranges[1] == ClassBytesRange::new(b'A', b'F')
        && ranges[2] == ClassBytesRange::new(b'a', b'f')
    {
        return Some(quote! { __b.is_ascii_hexdigit() });
    }

    None
}

/// Emit a general-purpose byte-range predicate from a slice of `ClassBytesRange`.
fn emit_ranges_predicate(ranges: &[ClassBytesRange]) -> Option<TokenStream> {
    let mut conditions: Vec<TokenStream> = Vec::new();

    for r in ranges {
        let start = r.start();
        let end = r.end();
        if start == end {
            // Single byte.
            let lit = proc_macro2::Literal::byte_character(start);
            conditions.push(quote! { __b == #lit });
        } else {
            // Byte range.
            let lo = proc_macro2::Literal::byte_character(start);
            let hi = proc_macro2::Literal::byte_character(end);
            conditions.push(quote! { (__b >= #lo && __b <= #hi) });
        }
    }

    if conditions.is_empty() {
        return None;
    }
    if conditions.len() == 1 {
        Some(conditions.into_iter().next().unwrap())
    } else {
        Some(quote! { (#(#conditions)||*) })
    }
}

// ── Look (assertions) ───────────────────────────────────────────────────────

/// Emit inline code for a look-around assertion.
///
/// We support anchors (`^`, `$`) by checking `state.offset`. Word boundaries
/// and other complex assertions are not supported -- returns `None`.
pub(super) fn emit_look(look: Look) -> Option<TokenStream> {
    match look {
        Look::Start => Some(quote! {
            if state.offset != 0 {
                return None;
            }
        }),
        Look::End => Some(quote! {
            if state.offset != state.src_bytes.len() {
                return None;
            }
        }),
        Look::StartLF => Some(quote! {
            if state.offset != 0
                && state.src_bytes.get(state.offset.wrapping_sub(1)).copied() != Some(b'\n')
            {
                return None;
            }
        }),
        Look::EndLF => Some(quote! {
            if state.offset != state.src_bytes.len()
                && state.src_bytes.get(state.offset).copied() != Some(b'\n')
            {
                return None;
            }
        }),
        // Word boundaries and other complex assertions: bail to DFA tier.
        _ => None,
    }
}
