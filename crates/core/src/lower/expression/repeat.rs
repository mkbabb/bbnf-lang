//! Factor + quantifier-modifier lowering.
//!
//! `factor = big_comment? term ?w modifier? big_comment?`
//!
//! Children occupy fixed positional slots; under struct-direct
//! emission, comment / modifier slots project to typed leaves
//! (Span for comments, Span for modifiers). The wrapper's SPAN
//! disambiguates: the modifier slot's trimmed span is exactly one
//! of `?w` / `?` / `*` / `+`; the term slot's span is the term
//! expression text.
//!
//! **AZ-IV.W1.6 (Fermat F8)**: the source-byte modifier recovery
//! (`recover_modifier` and the grouped/non-grouped helpers) is
//! deleted. Per the audit + REGEN-redress evidence, the codegen
//! `parse_keyword_*_modifier` fn pushes the matched punctuator as a
//! typed Span via `push_leaf_with_str` — the modifier child arrives
//! with `span_text() ∈ {"?w", "?", "*", "+"}`. The structural
//! classification loop below resolves the punctuator without ever
//! needing to scan source bytes past the term's `byte_span()`.

use bbnf_ir::IrNode;

use crate::runtime::RuntimeView;
use crate::runtime::bbnf::{BbnfCompoundKind, BbnfKind, BbnfView};

use super::super::LowerCtx;
use super::super::view_walk::find_sibling_by_kind;
use super::lower_term;

/// Lower a `factor = big_comment? term ?w modifier? big_comment?` view.
///
/// Children occupy fixed positional slots `[big_comment?, term-wrapper,
/// modifier-wrapper, big_comment?]`. Dispatch by role from the
/// direct-child sequence:
///
/// 1. Walk direct children once, classifying each by its trimmed
///    SPAN text:
///    - empty trimmed span → placeholder, skip
///    - `compound_kind ∈ {BigComment, Comment-like}` → metadata, skip
///    - trimmed span ∈ `{?w, ?, *, +}` → modifier
///    - otherwise → first such child is the term wrapper
/// 2. Fall back to `find_sibling_by_kind(Term)` if the classifier
///    produced no term candidate.
/// 3. Apply the modifier's quantifier to the base term.
///
/// **AZ-IV.W0.3 typed-materialization invariant**: if structural
/// detection observed a `Unit` modifier marker (the codegen
/// emitter's signal that the `modifier?` slot fired) but
/// span-text classification did not resolve a punctuator, panic
/// loudly rather than silently returning the bare term. Per
/// `feedback_typed-materialization-invariant`, every modifier
/// annotation in the grammar source must reach the IR — silent
/// drops corrupt every downstream rule body invisibly.
pub(crate) fn lower_factor<'a>(node: BbnfView<'a, 'a>, ctx: &mut LowerCtx<'a>) -> IrNode {
    let mut term_node: Option<BbnfView<'a, 'a>> = None;
    let mut modifier_text: Option<String> = None;
    let mut has_unit_marker = false;
    for child in node.children() {
        // AZ-IV.W1.6: the canonical generated parser pushes the
        // matched modifier as a typed Span via `push_leaf_with_str`,
        // so the structural-classification loop below resolves the
        // punctuator from `span_text()` directly. A `Unit` child
        // would mean a degenerate emitter push (no typed Span) —
        // we record it for the typed-materialization invariant
        // panic but no longer attempt source-byte recovery.
        if matches!(child.kind(), BbnfKind::Unit) {
            has_unit_marker = true;
            continue;
        }
        let span = child.span_text();
        let trimmed = span.trim();
        if trimmed.is_empty() {
            continue;
        }
        // Comment leaves carry their own span text (the comment
        // slug). Filter them out by leading marker — `//` for line
        // comments and `/*` for block comments.
        if is_comment_span(trimmed) {
            continue;
        }
        if matches!(trimmed, "?w" | "?" | "*" | "+") {
            modifier_text = Some(trimmed.to_string());
            continue;
        }
        if term_node.is_none() {
            term_node = Some(child);
        }
    }
    let term = term_node
        .or_else(|| find_sibling_by_kind(node, BbnfCompoundKind::Term))
        .or_else(|| find_term_child_by_elimination(node))
        .unwrap_or_else(|| panic!("factor: missing term child in span {:?}", node.span_text(),));
    let base = lower_term(term, ctx);

    // Typed-materialization invariant: a `Unit` marker without a
    // resolved modifier means the codegen emitter recorded a
    // matched modifier but no typed Span carried the punctuator
    // text — that's a structural-detection failure (post-W1.6 the
    // modifier rule pushes typed Span, never Unit). Surface it
    // loudly so the offending shape is fixed at the source rather
    // than silently dropped from the IR.
    if has_unit_marker && modifier_text.is_none() {
        panic!(
            "factor: Unit modifier marker present but no typed Span carried the \
             punctuator text in factor span {:?} (term branch_tag = {:?}). \
             Post-AZ-IV.W1.6 the codegen modifier emitter pushes the matched \
             token as a typed Span via push_leaf_with_str — a Unit-only modifier \
             child violates the typed-materialization invariant.",
            node.span_text(),
            term.branch_tag(),
        );
    }

    if let Some(text) = modifier_text {
        return apply_modifier(base, &text);
    }
    base
}

/// Whether a trimmed span is recognisably a comment leaf. Covers
/// the bbnf `comment` (`//...`) and `big_comment` (`/* ... */`)
/// rules that project to Span leaves under struct-direct.
fn is_comment_span(trimmed: &str) -> bool {
    trimmed.starts_with("//") || trimmed.starts_with("/*")
}

/// Locate the term child of a `factor` compound by eliminating the
/// known metadata / placeholder children.
///
/// The factor body is `big_comment? term ?w modifier? big_comment?`,
/// so any child whose compound kind is not a comment/modifier slot
/// and whose span is non-empty carries the term.
fn find_term_child_by_elimination<'a>(node: BbnfView<'a, 'a>) -> Option<BbnfView<'a, 'a>> {
    for child in node.children() {
        let span = child.span_text();
        let trimmed = span.trim();
        if trimmed.is_empty() {
            continue;
        }
        if is_comment_span(trimmed) {
            continue;
        }
        if matches!(trimmed, "?w" | "?" | "*" | "+") {
            continue;
        }
        return Some(child);
    }
    None
}

/// Apply a quantifier modifier (`?` / `*` / `+` / `?w`) to a base
/// expression, producing the wrapping `Repeat` / `OptionalWhitespace`
/// IR node.
pub(super) fn apply_modifier(base: IrNode, text: &str) -> IrNode {
    match text {
        "?" => IrNode::Repeat {
            inner: Box::new(base),
            lo: 0,
            hi: 1,
        },
        "*" => IrNode::Repeat {
            inner: Box::new(base),
            lo: 0,
            hi: u32::MAX,
        },
        "+" => IrNode::Repeat {
            inner: Box::new(base),
            lo: 1,
            hi: u32::MAX,
        },
        "?w" => IrNode::OptionalWhitespace(Box::new(base)),
        _ => base,
    }
}
