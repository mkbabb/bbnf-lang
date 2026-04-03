//! HIR walker that emits inline Rust byte operations for regex patterns.
//!
//! Parses a regex pattern via `regex_syntax::ParserBuilder` in byte mode,
//! walks the resulting HIR tree, and emits `proc_macro2::TokenStream` with
//! direct byte operations on `state.src_bytes` / `state.offset`.
//!
//! Returns `None` for patterns with features that cannot be inlined
//! (lookahead/lookbehind, Unicode properties beyond ASCII, backreferences,
//! lazy quantifiers outside of `.*?literal` sequences).
//! The caller falls back to `emit_regex_unsupported` (compile-time error) in that case.

mod alternation;
mod leaf;
mod repetition;

use proc_macro2::TokenStream;
use quote::quote;
use regex_syntax::hir::{Class, Hir, HirKind};

use alternation::emit_alternation;
use leaf::{emit_class_single, emit_literal, emit_look};
use repetition::emit_repetition;

// ── Lazy quantifier detection ──────────────────────────────────────────────

/// Check if an HIR tree contains any lazy (non-greedy) quantifiers.
///
/// Used by the DFA emitter to bail on patterns with lazy quantifiers,
/// since DFA-based matching always produces longest-match semantics.
pub(crate) fn contains_lazy_quantifier(hir: &Hir) -> bool {
    match hir.kind() {
        HirKind::Repetition(rep) => !rep.greedy || contains_lazy_quantifier(&rep.sub),
        HirKind::Concat(subs) | HirKind::Alternation(subs) => {
            subs.iter().any(contains_lazy_quantifier)
        }
        HirKind::Capture(cap) => contains_lazy_quantifier(&cap.sub),
        _ => false,
    }
}

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
pub(super) fn emit_hir(hir: &Hir) -> Option<TokenStream> {
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

// ── Concat ──────────────────────────────────────────────────────────────────

/// Emit inline code for a concatenation of sub-expressions.
///
/// Each sub-expression must succeed in sequence. Uses the `?` operator
/// for propagation via the enclosing IIFE closure.
///
/// Detects `[lazy_rep, Literal]` sequences and emits `memmem`-based scan
/// for the literal, avoiding char-by-char iteration (D1 optimization).
fn emit_concat(subs: &[Hir]) -> Option<TokenStream> {
    let mut stmts: Vec<TokenStream> = Vec::new();
    let mut i = 0;
    while i < subs.len() {
        // Optimization: lazy repetition of broad class + literal -> memmem scan.
        if i + 1 < subs.len() {
            if let Some((code, consumed)) = try_emit_lazy_literal_scan(&subs[i..]) {
                stmts.push(code);
                i += consumed;
                continue;
            }
        }
        stmts.push(emit_hir(&subs[i])?);
        i += 1;
    }
    Some(quote! { #(#stmts)* })
}

/// Try to fuse a lazy repetition with a following literal into a single
/// `memchr`/`memmem` scan. Returns `(code, elements_consumed)`.
///
/// Only applies when the repetition sub is a broad byte class (`.` or similar)
/// so that we can safely skip to the literal without validating intervening bytes.
fn try_emit_lazy_literal_scan(subs: &[Hir]) -> Option<(TokenStream, usize)> {
    if subs.len() < 2 {
        return None;
    }

    let rep = match subs[0].kind() {
        HirKind::Repetition(rep) if !rep.greedy => rep,
        _ => return None,
    };

    // Only apply when the repetition sub is a broad byte class (covers >= 240 bytes).
    // This ensures skipping to the literal is valid (sub matches nearly everything).
    if !is_broad_byte_class(&rep.sub) {
        return None;
    }

    let lit_bytes = match subs[1].kind() {
        HirKind::Literal(lit) if !lit.0.is_empty() => &lit.0,
        _ => return None,
    };

    let min = rep.min as usize;

    let code = if lit_bytes.len() == 1 {
        let b = proc_macro2::Literal::byte_character(lit_bytes[0]);
        quote! {
            {
                let __search_start = state.offset + #min;
                let __rem = state.src_bytes.get(__search_start..)?;
                let __pos = memchr::memchr(#b, __rem)?;
                state.offset = __search_start + __pos + 1;
            }
        }
    } else {
        let needle = proc_macro2::Literal::byte_string(lit_bytes);
        let needle_len = lit_bytes.len();
        quote! {
            {
                let __search_start = state.offset + #min;
                let __rem = state.src_bytes.get(__search_start..)?;
                let __pos = memchr::memmem::find(__rem, #needle)?;
                state.offset = __search_start + __pos + #needle_len;
            }
        }
    };

    // Consumed: the lazy repetition + the literal = 2 concat elements.
    Some((code, 2))
}

/// Check if an HIR node represents a broad byte class (covers >= 240 out of 256 bytes).
/// This matches `.` (non-dotall: 255 bytes) and `(?s).` (dotall: 256 bytes).
fn is_broad_byte_class(hir: &Hir) -> bool {
    match hir.kind() {
        HirKind::Class(Class::Bytes(cb)) => {
            let total: usize = cb
                .ranges()
                .iter()
                .map(|r| (r.end() as usize - r.start() as usize) + 1)
                .sum();
            total >= 240
        }
        HirKind::Class(Class::Unicode(cu)) => {
            // Check if it's an ASCII-only broad class.
            let ranges = cu.ranges();
            if ranges.iter().any(|r| r.end() > '\u{FF}') {
                return true; // Unicode broad class -- covers everything
            }
            let total: usize = ranges
                .iter()
                .map(|r| (r.end() as usize - r.start() as usize) + 1)
                .sum();
            total >= 240
        }
        _ => false,
    }
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

    /// Helper: check that a pattern produces None (i.e., needs DFA tier).
    fn assert_not_inlinable(pattern: &str) {
        let result = try_emit_regex_inline(pattern);
        assert!(
            result.is_none(),
            "Expected pattern to need DFA tier: {pattern}"
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
