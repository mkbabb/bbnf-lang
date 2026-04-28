//! Recursively collect nonterminal references from a struct-direct
//! [`BbnfView`] rule RHS.
//!
//! AZ-II.cutover.D4 — walks the [`BbnfView`] shape-agnostically:
//! `Term` identifier-call branches, nested `Factor` / `MappedFactor`
//! wrappers, grouped `Term` expressions, alternation / concatenation,
//! and binary-factor chains all cascade into a recursive descent
//! that bottoms out at the identifier Span leaf.
//!
//! The struct-direct alphabet has no anonymous-wrapper compounds
//! (the tape-first generator's `int_lit`-collision wrappers around
//! quantified groups). Compound entries always resolve to an
//! explicit [`BbnfCompoundKind`] arm (including `Other` for rules
//! outside the alphabet); descent walks all children uniformly.

use bbnf::runtime::RuntimeView;
use bbnf::runtime::bbnf::{BbnfCompoundKind, BbnfView};

use super::super::types::ReferenceInfo;

/// Recursively collect nonterminal references from a [`BbnfView`].
pub fn collect_references(node: BbnfView<'_, '_>, refs: &mut Vec<ReferenceInfo>) {
    // Span-leaf focus: emit a reference if the slice is identifier-shaped.
    if !node.is_compound() {
        if let Some(text) = node.span_text_opt() {
            let trimmed = text.trim();
            if !trimmed.is_empty() && is_ident(trimmed) && !is_reserved_word(trimmed) {
                if let Some((raw_lo, raw_hi)) = node.span_range() {
                    let raw = text;
                    let lead_ws = raw.len() - raw.trim_start().len();
                    let trail_ws = raw.len() - raw.trim_end().len();
                    let lo = raw_lo + lead_ws;
                    let hi = raw_hi - trail_ws;
                    refs.push(ReferenceInfo {
                        name: trimmed.to_string(),
                        span: (lo, hi),
                    });
                }
            }
        }
        return;
    }

    match node.compound_kind() {
        // Term: the identifier-call branch carries the ident as
        // child(0); other branches descend uniformly.
        Some(BbnfCompoundKind::Term) => {
            match node.branch_tag() {
                Some(1) => {
                    // Identifier with optional call-args.
                    if let Some(ident) = node.child(0) {
                        if let (Some(text), Some((lo, hi))) = (ident.span_text_opt(), ident.span_range()) {
                            let trimmed = text.trim();
                            if !trimmed.is_empty() && is_ident(trimmed) {
                                let lead_ws = text.len() - text.trim_start().len();
                                let trail_ws = text.len() - text.trim_end().len();
                                refs.push(ReferenceInfo {
                                    name: trimmed.to_string(),
                                    span: (lo + lead_ws, hi - trail_ws),
                                });
                            }
                        }
                    }
                    // Recurse into call_arg children — they may carry
                    // additional nonterminal references.
                    for c in node.children().skip(1) {
                        collect_references(c, refs);
                    }
                }
                Some(b @ 4..=7) => {
                    // Grouped: descend into the inner Rhs subtree.
                    let _ = b;
                    if let Some(inner) = node
                        .children()
                        .find(|c| c.compound_kind() == Some(BbnfCompoundKind::Rhs))
                    {
                        collect_references(inner, refs);
                    }
                }
                _ => {
                    // term_0 / term_2..3 (epsilon / literal / regex) —
                    // no nonterminal references. Descend into any
                    // substantive child (defensive: handles future
                    // alphabet extensions).
                    for c in node.children() {
                        collect_references(c, refs);
                    }
                }
            }
        }

        // Structural compounds — walk every iteration body.
        Some(BbnfCompoundKind::Alternation) | Some(BbnfCompoundKind::Concatenation) => {
            for child in super::iter_iteration_views(node) {
                collect_references(child, refs);
            }
        }

        // Binary factor: walk every operand.
        Some(BbnfCompoundKind::BinaryFactor) => {
            for operand in super::collect_binary_operand_views(node) {
                collect_references(operand, refs);
            }
        }

        // Mapped factor / factor: walk every child (the inner Term
        // and any modifier Span / mapping siblings).
        Some(BbnfCompoundKind::MappedFactor) | Some(BbnfCompoundKind::Factor) => {
            for c in node.children() {
                collect_references(c, refs);
            }
        }

        // Single-child transparent wrappers — descend into child(0).
        Some(BbnfCompoundKind::Rhs)
        | Some(BbnfCompoundKind::GrammarItem)
        | Some(BbnfCompoundKind::Directive)
        | Some(BbnfCompoundKind::Lhs) => {
            if let Some(inner) = node.child(0) {
                collect_references(inner, refs);
            }
        }

        // Closure: |params| body — recurse into the body Rhs child.
        Some(BbnfCompoundKind::Closure) => {
            if let Some(body) = node
                .children()
                .find(|c| c.compound_kind() == Some(BbnfCompoundKind::Rhs))
            {
                collect_references(body, refs);
            }
        }

        // Call arg: walk every child operand.
        Some(BbnfCompoundKind::CallArg) => {
            for c in node.children() {
                collect_references(c, refs);
            }
        }

        // Everything else (directives, value-expression compounds,
        // pretty hints, …) — recurse into children but do not emit
        // references for the compound's own children gratuitously.
        _ => {
            for c in node.children() {
                collect_references(c, refs);
            }
        }
    }
}

/// Check if a string is a valid BBNF identifier: `[_a-zA-Z][_a-zA-Z0-9-]*`.
fn is_ident(s: &str) -> bool {
    let mut chars = s.bytes();
    match chars.next() {
        Some(b) if b == b'_' || b.is_ascii_alphabetic() => {}
        _ => return false,
    }
    chars.all(|b| b == b'_' || b == b'-' || b.is_ascii_alphanumeric())
}

/// Reject reserved-word identifiers that are not nonterminal references.
fn is_reserved_word(s: &str) -> bool {
    matches!(s, "epsilon")
}
