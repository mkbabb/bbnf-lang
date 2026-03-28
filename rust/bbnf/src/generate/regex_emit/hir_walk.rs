//! HIR walker that emits inline Rust byte operations for regex patterns.
//!
//! Parses a regex pattern via `regex_syntax::ParserBuilder` in byte mode,
//! walks the resulting HIR tree, and emits `proc_macro2::TokenStream` with
//! direct byte operations on `state.src_bytes` / `state.offset`.
//!
//! Returns `None` for patterns with features that cannot be inlined
//! (lookahead/lookbehind, Unicode properties beyond ASCII, backreferences).
//! The caller falls back to `emit_regex_lazy_static` in that case.

use proc_macro2::TokenStream;
use quote::quote;
use regex_syntax::hir::{
    Class, ClassBytes, ClassBytesRange, ClassUnicode, Hir, HirKind, Look, Repetition,
};

/// Try to compile a regex pattern into inline byte-operation code.
///
/// On success, returns a `TokenStream` that evaluates to `Option<Span<'a>>`,
/// reading from `state.src_bytes` and advancing `state.offset`.
///
/// Returns `None` if the pattern contains features that cannot be inlined:
/// - Lookahead / lookbehind assertions (`Look` variants other than `Start`/`End`)
/// - Unicode properties beyond ASCII
/// - Patterns that regex-syntax cannot parse
pub fn try_emit_regex_inline(pattern: &str) -> Option<TokenStream> {
    let hir = regex_syntax::ParserBuilder::new()
        .utf8(false)
        .unicode(false)
        .build()
        .parse(pattern)
        .ok()?;

    let body = emit_hir(&hir)?;

    Some(quote! {
        {
            let __start = state.offset;
            let __result: Option<()> = (|| {
                #body
                Some(())
            })();
            if __result.is_some() && state.offset > __start {
                Some(::parse_that::Span::new(__start, state.offset, state.src))
            } else {
                state.offset = __start;
                None
            }
        }
    })
}

/// Recursively emit code for an HIR node.
///
/// The emitted code operates on `state.src_bytes` / `state.offset` and uses
/// `?` (the try operator) for early exit on mismatch. The caller wraps
/// everything in an IIFE closure that returns `Option<()>`.
fn emit_hir(hir: &Hir) -> Option<TokenStream> {
    match hir.kind() {
        HirKind::Empty => Some(quote! {}),

        HirKind::Literal(lit) => emit_literal(&lit.0),

        HirKind::Class(class) => emit_class_single(class),

        HirKind::Look(look) => emit_look(*look),

        HirKind::Repetition(rep) => emit_repetition(rep),

        HirKind::Capture(cap) => emit_hir(&cap.sub),

        HirKind::Concat(subs) => emit_concat(subs),

        HirKind::Alternation(alts) => emit_alternation(alts),
    }
}

// ── Literal ──────────────────────────────────────────────────────────────────

/// Emit inline code for a fixed byte sequence literal.
fn emit_literal(bytes: &[u8]) -> Option<TokenStream> {
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
        let byte_lits: Vec<proc_macro2::Literal> =
            bytes.iter().map(|b| proc_macro2::Literal::byte_character(*b)).collect();
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
fn emit_class_single(class: &Class) -> Option<TokenStream> {
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
fn emit_class_predicate(class: &Class) -> Option<TokenStream> {
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
/// Anything with non-ASCII codepoints bails out — caller uses fallback.
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

    // \w = [0-9A-Za-z_] — regex-syntax normalizes to 4 ranges:
    // 0-9, A-Z, _, a-z  (sorted by start byte)
    if ranges.len() == 4
        && ranges[0] == ClassBytesRange::new(b'0', b'9')
        && ranges[1] == ClassBytesRange::new(b'A', b'Z')
        && ranges[2] == ClassBytesRange::new(b'_', b'_')
        && ranges[3] == ClassBytesRange::new(b'a', b'z')
    {
        return Some(quote! { (__b.is_ascii_alphanumeric() || __b == b'_') });
    }

    // \s (ASCII mode) — regex-syntax normalizes to:
    // [\t\n\x0B\x0C\r ] = ranges: [0x09-0x0D, 0x20-0x20]
    if ranges.len() == 2
        && ranges[0] == ClassBytesRange::new(0x09, 0x0D)
        && ranges[1] == ClassBytesRange::new(0x20, 0x20)
    {
        return Some(quote! { __b.is_ascii_whitespace() });
    }

    // [a-zA-Z] — 2 ranges for alpha
    if ranges.len() == 2
        && ranges[0] == ClassBytesRange::new(b'A', b'Z')
        && ranges[1] == ClassBytesRange::new(b'a', b'z')
    {
        return Some(quote! { __b.is_ascii_alphabetic() });
    }

    // [a-zA-Z0-9] — 3 ranges for alphanumeric
    if ranges.len() == 3
        && ranges[0] == ClassBytesRange::new(b'0', b'9')
        && ranges[1] == ClassBytesRange::new(b'A', b'Z')
        && ranges[2] == ClassBytesRange::new(b'a', b'z')
    {
        return Some(quote! { __b.is_ascii_alphanumeric() });
    }

    // [0-9a-fA-F] — hex digits
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
/// and other complex assertions are not supported — returns `None`.
fn emit_look(look: Look) -> Option<TokenStream> {
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
        // Word boundaries and other complex assertions: bail to fallback.
        _ => None,
    }
}

// ── Repetition ──────────────────────────────────────────────────────────────

/// Emit inline code for a repetition (quantifier).
fn emit_repetition(rep: &Repetition) -> Option<TokenStream> {
    let min = rep.min;
    let max = rep.max;

    // Special case: optional `?` (min=0, max=1) of a class or literal.
    if min == 0 && max == Some(1) {
        return emit_optional(&rep.sub);
    }

    // Special case: class-based loops (`[a-z]+`, `\d*`, etc.)
    // For a tight byte-predicate loop without per-iteration IIFE overhead.
    if let HirKind::Class(class) = rep.sub.kind() {
        if let Some(predicate) = emit_class_predicate(class) {
            return emit_class_loop(&predicate, min, max);
        }
    }

    // General repetition: emit a counted loop with checkpoint per iteration.
    emit_general_loop(&rep.sub, min, max)
}

/// Emit an optional match (`?` quantifier): try, succeed either way.
fn emit_optional(sub: &Hir) -> Option<TokenStream> {
    let body = emit_hir(sub)?;
    Some(quote! {
        {
            let __save = state.offset;
            let __ok = (|| -> Option<()> {
                #body
                Some(())
            })();
            if __ok.is_none() {
                state.offset = __save;
            }
        }
    })
}

/// Emit a tight byte-predicate loop for a character class quantifier.
///
/// No checkpoint save/restore per iteration — just a while loop with
/// direct byte checks.
fn emit_class_loop(
    predicate: &TokenStream,
    min: u32,
    max: Option<u32>,
) -> Option<TokenStream> {
    match max {
        None => {
            // Unbounded: `+` or `*`
            if min >= 1 {
                let min_lit = proc_macro2::Literal::u32_unsuffixed(min);
                Some(quote! {
                    {
                        let __loop_start = state.offset;
                        let __end = state.src_bytes.len();
                        let mut __pos = state.offset;
                        while __pos < __end {
                            let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                            if #predicate { __pos += 1; } else { break; }
                        }
                        if __pos < __loop_start + #min_lit as usize {
                            return None;
                        }
                        state.offset = __pos;
                    }
                })
            } else {
                // Zero-or-more: always succeeds.
                Some(quote! {
                    {
                        let __end = state.src_bytes.len();
                        let mut __pos = state.offset;
                        while __pos < __end {
                            let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                            if #predicate { __pos += 1; } else { break; }
                        }
                        state.offset = __pos;
                    }
                })
            }
        }
        Some(max_val) => {
            // Bounded: `{n,m}`
            let min_lit = proc_macro2::Literal::u32_unsuffixed(min);
            let max_lit = proc_macro2::Literal::u32_unsuffixed(max_val);
            Some(quote! {
                {
                    let __end = state.src_bytes.len();
                    let mut __pos = state.offset;
                    let mut __count: u32 = 0;
                    while __pos < __end && __count < #max_lit {
                        let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                        if #predicate { __pos += 1; __count += 1; } else { break; }
                    }
                    if __count < #min_lit {
                        return None;
                    }
                    state.offset = __pos;
                }
            })
        }
    }
}

/// Emit a general-purpose loop for repetition of arbitrary sub-expressions.
///
/// Uses checkpoint save/restore per iteration. More overhead than the
/// class-specific loop, but handles any HIR sub-expression.
fn emit_general_loop(sub: &Hir, min: u32, max: Option<u32>) -> Option<TokenStream> {
    let body = emit_hir(sub)?;

    match max {
        None => {
            // Unbounded: `+` or `*`
            let min_lit = proc_macro2::Literal::u32_unsuffixed(min);
            Some(quote! {
                {
                    let mut __rep_count: u32 = 0;
                    loop {
                        let __save = state.offset;
                        let __ok = (|| -> Option<()> {
                            #body
                            Some(())
                        })();
                        if __ok.is_none() {
                            state.offset = __save;
                            break;
                        }
                        // Guard against zero-width matches causing infinite loops.
                        if state.offset == __save {
                            break;
                        }
                        __rep_count += 1;
                    }
                    if __rep_count < #min_lit {
                        return None;
                    }
                }
            })
        }
        Some(max_val) => {
            // Bounded: `{n,m}`
            let min_lit = proc_macro2::Literal::u32_unsuffixed(min);
            let max_lit = proc_macro2::Literal::u32_unsuffixed(max_val);
            Some(quote! {
                {
                    let mut __rep_count: u32 = 0;
                    while __rep_count < #max_lit {
                        let __save = state.offset;
                        let __ok = (|| -> Option<()> {
                            #body
                            Some(())
                        })();
                        if __ok.is_none() {
                            state.offset = __save;
                            break;
                        }
                        if state.offset == __save {
                            break;
                        }
                        __rep_count += 1;
                    }
                    if __rep_count < #min_lit {
                        return None;
                    }
                }
            })
        }
    }
}

// ── Concat ──────────────────────────────────────────────────────────────────

/// Emit inline code for a concatenation of sub-expressions.
///
/// Each sub-expression must succeed in sequence. Uses the `?` operator
/// for propagation via the enclosing IIFE closure.
fn emit_concat(subs: &[Hir]) -> Option<TokenStream> {
    let mut stmts: Vec<TokenStream> = Vec::new();
    for sub in subs {
        let code = emit_hir(sub)?;
        stmts.push(code);
    }
    Some(quote! { #(#stmts)* })
}

// ── Alternation ─────────────────────────────────────────────────────────────

/// Emit inline code for an alternation (`a|b|c`).
///
/// Uses a result variable and cascading if/else blocks with checkpoint
/// save/restore per branch.
fn emit_alternation(alts: &[Hir]) -> Option<TokenStream> {
    if alts.is_empty() {
        return Some(quote! { return None; });
    }
    if alts.len() == 1 {
        return emit_hir(&alts[0]);
    }

    // Check if all alternatives are single-byte classes or literals.
    // If so, we can emit a single `match` statement instead of cascading ifs.
    if let Some(ts) = try_emit_byte_match_alt(alts) {
        return Some(ts);
    }

    let mut branches: Vec<TokenStream> = Vec::new();
    for (i, alt) in alts.iter().enumerate() {
        let body = emit_hir(alt)?;
        if i == 0 {
            branches.push(quote! {
                let __save_alt = state.offset;
                let __alt_ok = (|| -> Option<()> {
                    #body
                    Some(())
                })();
            });
        } else {
            branches.push(quote! {
                let __alt_ok = if __alt_ok.is_none() {
                    state.offset = __save_alt;
                    (|| -> Option<()> {
                        #body
                        Some(())
                    })()
                } else {
                    __alt_ok
                };
            });
        }
    }

    Some(quote! {
        {
            #(#branches)*
            if __alt_ok.is_none() {
                return None;
            }
        }
    })
}

/// Try to emit a compact single-byte match for alternations where every
/// branch matches exactly one byte (single-byte literal or single-char class).
///
/// Returns `None` if any branch is more complex.
fn try_emit_byte_match_alt(alts: &[Hir]) -> Option<TokenStream> {
    // Collect all possible byte values from each branch.
    let mut all_bytes: Vec<u8> = Vec::new();
    for alt in alts {
        match alt.kind() {
            HirKind::Literal(lit) if lit.0.len() == 1 => {
                all_bytes.push(lit.0[0]);
            }
            HirKind::Class(Class::Bytes(cb)) => {
                for r in cb.ranges() {
                    for b in r.start()..=r.end() {
                        all_bytes.push(b);
                    }
                    // If the range is too large, bail.
                    if r.end() as u32 - r.start() as u32 > 64 {
                        return None;
                    }
                }
            }
            HirKind::Class(Class::Unicode(cu)) => {
                for r in cu.ranges() {
                    if r.end() > '\x7F' {
                        return None;
                    }
                    for c in r.start()..=r.end() {
                        all_bytes.push(c as u8);
                    }
                    if r.end() as u32 - r.start() as u32 > 64 {
                        return None;
                    }
                }
            }
            _ => return None,
        }
    }

    if all_bytes.is_empty() {
        return None;
    }

    let byte_lits: Vec<proc_macro2::Literal> = all_bytes
        .iter()
        .map(|b| proc_macro2::Literal::byte_character(*b))
        .collect();

    Some(quote! {
        {
            let __b = *state.src_bytes.get(state.offset)?;
            if !matches!(__b, #(#byte_lits)|*) {
                return None;
            }
            state.offset += 1;
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Helper: check that a pattern produces Some (i.e., can be inlined).
    fn assert_inlinable(pattern: &str) {
        let result = try_emit_regex_inline(pattern);
        assert!(
            result.is_some(),
            "Expected pattern to be inlinable: {pattern}"
        );
    }

    /// Helper: check that a pattern produces None (i.e., needs fallback).
    fn assert_not_inlinable(pattern: &str) {
        let result = try_emit_regex_inline(pattern);
        assert!(
            result.is_none(),
            "Expected pattern to need fallback: {pattern}"
        );
    }

    // ── Literals ────────────────────────────────────────────────────────

    #[test]
    fn literal_simple() {
        assert_inlinable("from");
    }

    #[test]
    fn literal_single_char() {
        assert_inlinable(":");
    }

    // ── Character classes ───────────────────────────────────────────────

    #[test]
    fn char_class_simple_range() {
        assert_inlinable("[a-z]");
    }

    #[test]
    fn char_class_multi_range() {
        assert_inlinable("[a-zA-Z0-9_]");
    }

    #[test]
    fn char_class_digit() {
        assert_inlinable(r"\d");
    }

    #[test]
    fn char_class_word() {
        assert_inlinable(r"\w");
    }

    #[test]
    fn char_class_whitespace() {
        assert_inlinable(r"\s");
    }

    #[test]
    fn char_class_small_set() {
        assert_inlinable("[iIsS]");
    }

    // ── Quantifiers ─────────────────────────────────────────────────────

    #[test]
    fn quantifier_plus() {
        assert_inlinable(r"\d+");
    }

    #[test]
    fn quantifier_star() {
        assert_inlinable(r"\s*");
    }

    #[test]
    fn quantifier_optional() {
        assert_inlinable(r"\d?");
    }

    #[test]
    fn quantifier_bounded() {
        assert_inlinable(r"[0-9a-fA-F]{4}");
    }

    // ── Alternation ─────────────────────────────────────────────────────

    #[test]
    fn alternation_literals() {
        assert_inlinable("from|to");
    }

    #[test]
    fn alternation_mixed() {
        assert_inlinable(r"from|to|\d+%");
    }

    // ── Concat ──────────────────────────────────────────────────────────

    #[test]
    fn concat_literal_class() {
        assert_inlinable(r"0x[0-9a-fA-F]+");
    }

    // ── CSS patterns ────────────────────────────────────────────────────

    #[test]
    fn css_combinator_separators() {
        assert_inlinable(r"\s*>\s*|\s*\+\s*|\s*~\s*|\s+");
    }

    #[test]
    fn css_anb_full() {
        assert_inlinable(r"[-+]?\d*n\s*[+-]\s*\d+");
    }

    #[test]
    fn css_anb_short() {
        assert_inlinable(r"[-+]?\d*n");
    }

    #[test]
    fn css_signed_integer() {
        assert_inlinable(r"[-+]?\d+");
    }

    #[test]
    fn css_ident_with_escapes() {
        assert_inlinable(r"(?:-?[a-zA-Z_]|\\[^\n])(?:[\w-]|\\[^\n])*");
    }

    #[test]
    fn css_hash_selector() {
        assert_inlinable(r"#(?:[\w-]|\\[^\n])+");
    }

    // ── Negated classes ─────────────────────────────────────────────────

    #[test]
    fn negated_class_plus() {
        assert_inlinable(r"[^\n]+");
    }

    #[test]
    fn negated_class_star() {
        assert_inlinable(r"[^\n]*");
    }

    // ── Fallback cases ──────────────────────────────────────────────────

    #[test]
    fn unicode_property_not_inlinable() {
        assert_not_inlinable(r"\p{L}+");
    }
}
