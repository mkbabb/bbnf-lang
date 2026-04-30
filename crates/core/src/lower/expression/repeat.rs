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

use bbnf_ir::IrNode;

use crate::runtime::RuntimeView;
use crate::runtime::bbnf::{BbnfCompoundKind, BbnfView};

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
pub(crate) fn lower_factor<'a>(node: BbnfView<'a, 'a>, ctx: &mut LowerCtx<'a>) -> IrNode {
    let mut term_node: Option<BbnfView<'a, 'a>> = None;
    let mut modifier_text: Option<String> = None;
    for child in node.children() {
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
        .unwrap_or_else(|| {
            panic!(
                "factor: missing term child in span {:?}",
                node.span_text(),
            )
        });
    let base = lower_term(term, ctx);

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
fn find_term_child_by_elimination<'a>(
    node: BbnfView<'a, 'a>,
) -> Option<BbnfView<'a, 'a>> {
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
