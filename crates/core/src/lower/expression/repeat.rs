//! Factor + quantifier-modifier lowering.
//!
//! `factor = big_comment? term ?w modifier? big_comment?`
//!
//! Children occupy fixed positional slots whose `rule_kind` often
//! maps to the `int_lit` sentinel under DTA (the walker stamps
//! `variant_idx = 0` on anonymous wrappers emitted for absent-
//! optional slots). The wrapper's SPAN disambiguates: the modifier
//! wrapper's trimmed span is exactly one of `?w` / `?` / `*` / `+`;
//! the term wrapper's span is the term expression text.

use bbnf_ir::IrNode;

use crate::grammar::generated::{BbnfBootstrapNodeView, BbnfBootstrapRuleKind};

use super::super::LowerCtx;
use super::super::tape_walk::find_sibling_by_kind;
use super::lower_term;

/// Lower a `factor = big_comment? term ?w modifier? big_comment?` view.
///
/// Children occupy fixed positional slots `[big_comment?, term-wrapper,
/// modifier-wrapper, big_comment?]` — each a Rule / Seq compound
/// whose `rule_kind` often maps to the `int_lit` sentinel under DTA
/// (the walker stamps `variant_idx = 0` on anonymous wrappers emitted
/// for absent-optional slots and for the term / modifier body Seqs).
/// Dispatch by role from the direct-child sequence:
///
/// 1. Walk direct children once, classifying each by its trimmed
///    SPAN text:
///    - empty trimmed span → placeholder, skip
///    - `rule_kind ∈ {big_comment, comment}` → metadata, skip
///    - trimmed span ∈ `{?w, ?, *, +}` → modifier
///    - otherwise → first such child is the term wrapper
/// 2. Fall back to `find_sibling_by_kind(term)` +
///    `find_term_child_by_elimination` if the classifier produced
///    no term candidate (defensive under tape-shape variants the
///    direct-child loop may not cover).
/// 3. Apply the modifier's quantifier to the base term.
///
/// The classifier deliberately avoids descending into the term
/// wrapper's subtree: a bare `?` / `*` / `+` is never a well-formed
/// term, so the modifier token can't come from inside the term, and
/// nested factors inside grouped terms carry their own modifiers that
/// must NOT bubble up.
pub(crate) fn lower_factor<'a>(node: BbnfBootstrapNodeView<'a>, ctx: &mut LowerCtx<'a>) -> IrNode {
    // Under DTA a factor compound's direct children occupy fixed
    // positional slots `[big_comment?, term-wrapper, modifier-wrapper,
    // big_comment?]` — each a Rule / Seq compound whose `rule_kind`
    // often maps to the `int_lit` sentinel (walker stamps `variant_idx
    // = 0` on anonymous wrappers emitted for absent-optional slots and
    // for the term / modifier body Seqs). The wrapper's SPAN
    // disambiguates: the modifier wrapper's trimmed span is exactly
    // one of `?w` / `?` / `*` / `+`; the term wrapper's span is the
    // term expression text (never one of those four tokens because a
    // bare `?` / `*` / `+` isn't a well-formed term). Classify by
    // role across the direct children — not by a descent that would
    // walk into the term's own nested expressions and return a
    // modifier belonging to some deeper factor inside the term.
    let mut term_node: Option<BbnfBootstrapNodeView<'a>> = None;
    let mut modifier_text: Option<&'a str> = None;
    for child in node.children() {
        let span = child.span_text();
        let trimmed = span.trim();
        if trimmed.is_empty() {
            continue;
        }
        if matches!(
            child.rule_kind(),
            BbnfBootstrapRuleKind::big_comment | BbnfBootstrapRuleKind::comment,
        ) {
            continue;
        }
        if matches!(trimmed, "?w" | "?" | "*" | "+") {
            modifier_text = Some(trimmed);
            continue;
        }
        if term_node.is_none() {
            term_node = Some(child);
        }
    }
    let term = term_node
        .or_else(|| find_sibling_by_kind(node, BbnfBootstrapRuleKind::term))
        .or_else(|| find_term_child_by_elimination(node))
        .unwrap_or_else(|| {
            panic!(
                "factor: missing term child in span {:?}",
                node.span_text(),
            )
        });
    let base = lower_term(term, ctx);

    if let Some(text) = modifier_text {
        return apply_modifier(base, text);
    }
    base
}

/// Locate the term child of a `factor` compound by eliminating the
/// known metadata / placeholder children.
///
/// The factor body is `big_comment? term ?w modifier? big_comment?`,
/// so any child whose rule_kind is not `big_comment` / `comment` /
/// `modifier` and whose span is non-empty carries the term. This
/// path is the substrate-break fallback under HEAD's hand-patched
/// schema where `term` may surface under a dedupe-dropped rule_kind
/// or inline directly as a `literal` / `regex` / `identifier` leaf.
fn find_term_child_by_elimination<'a>(
    node: BbnfBootstrapNodeView<'a>,
) -> Option<BbnfBootstrapNodeView<'a>> {
    for child in node.children() {
        match child.rule_kind() {
            BbnfBootstrapRuleKind::big_comment
            | BbnfBootstrapRuleKind::comment
            | BbnfBootstrapRuleKind::modifier => continue,
            _ => {
                if is_empty_placeholder(child) {
                    continue;
                }
                return Some(child);
            }
        }
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

/// True if `view` is an empty placeholder compound — a Repeat
/// (or Rule) compound whose span has zero width (`lo == hi`) AND
/// whose child run is empty. These arise from the post-AC.2
/// emission of missing optional groups in `factor` / `mapped_factor`
/// (e.g. an absent leading `big_comment?` or trailing modifier
/// `?w`): the emitter pushes a Repeat compound with `(state.offset,
/// state.offset)` and zero children to keep positional layout
/// stable, and downstream consumers detect the placeholder by its
/// empty span.
fn is_empty_placeholder(view: BbnfBootstrapNodeView<'_>) -> bool {
    let (lo, hi) = view.span();
    lo == hi && view.children().next().is_none()
}
