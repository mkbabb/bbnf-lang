//! Shape-agnostic tape CST traversal helpers for lowering.
//!
//! Tranche AE substrate. The lowering pipeline walks tape-first CST
//! views where the optimizer may or may not have inlined
//! transparent wrapper rules, Repeat wrappers for `+` / `*` / `?`
//! quantifiers, and single-child rule wrappers. To lower correctly
//! under both structural-mode (preserve_identity=true, nothing
//! inlined) and non-structural-mode (optimizer flattens wrappers),
//! every layer function in `expression.rs` / `value_expr.rs` calls
//! through these helpers rather than making positional or wrapper
//! assumptions about its input view.
//!
//! The three primitives:
//!
//! - [`iter_rep_children`] — iterate iteration children, unwrapping
//!   a single top-level `TapeKind::Repeat` compound if present.
//!   Mirrors the pattern already live in
//!   [`crate::grammar::host::extract_grammar`] (the top-level root
//!   walker). Used whenever a `+` / `*` / `sep_by` body's children
//!   need to be iterated as a flat sequence.
//!
//! - [`find_child_by_kind`] — scan direct children and return the
//!   first one whose `rule_kind()` matches a target. Replaces every
//!   positional `child(0) / child(1) / child(2)` read in the
//!   lowering; positions shift when preserved Optional wrappers
//!   collapse their spans but keep their compound records, so rule-
//!   kind dispatch is the only stable reference.
//!
//! - [`peel_transparent`] — recursively descend through a closed
//!   whitelist of single-child wrapper rules (`rhs`, `grammar_item`,
//!   `directive`) until a semantic-content rule_kind is reached.
//!   Used by `lower_rhs` to reach the actual expression head under
//!   structural mode where these wrappers aren't inlined.
//!
//! The view layer is schema-generic and stays that way — these
//! helpers live in `lower/` because they encode bbnf.bbnf-specific
//! semantic knowledge (which rule_kinds are transparent wrappers)
//! and shouldn't leak into `bbnf-tape::TapeCursor` or the generated
//! view types.

use ::bbnf::runtime::tape::TapeKind;

use crate::grammar::generated::{BbnfBootstrapNodeView, BbnfBootstrapRuleKind};

/// Iterate a view's "iteration children" — the logical elements of
/// a `+` / `*` / `sep_by` body, regardless of whether the tape
/// wraps them in an explicit `TapeKind::Repeat` compound.
///
/// Shape handling:
/// - If `view.children()` yields exactly one child and that child
///   is a `TapeKind::Repeat` compound, iterate that Repeat's own
///   children (the flattened iteration stream).
/// - Otherwise iterate `view.children()` directly (the optimizer
///   has inlined the Repeat wrapper, or the view is a flat
///   grouping that produced iteration children directly).
///
/// Returns a boxed iterator so the caller's match arms don't have
/// to name the iterator type.
pub(crate) fn iter_rep_children<'tape>(
    view: BbnfBootstrapNodeView<'tape>,
) -> Box<dyn Iterator<Item = BbnfBootstrapNodeView<'tape>> + 'tape> {
    let mut children = view.children();
    let first = match children.next() {
        Some(c) => c,
        None => return Box::new(std::iter::empty()),
    };
    if children.next().is_none() && first.kind() == TapeKind::Repeat {
        return Box::new(first.children());
    }
    // Not a single-Repeat wrap — re-iterate from the start.
    Box::new(view.children())
}

/// Scan a view's direct children and return the first one whose
/// `rule_kind()` matches `target`. Returns `None` if no such child
/// exists.
///
/// Use this to pluck a positional child by its semantic role
/// (e.g. "find the `term` child of this `factor` compound")
/// without hardcoding child indices that shift under preserved
/// Optional wrappers.
pub(crate) fn find_child_by_kind<'tape>(
    view: BbnfBootstrapNodeView<'tape>,
    target: BbnfBootstrapRuleKind,
) -> Option<BbnfBootstrapNodeView<'tape>> {
    view.children().find(|c| c.rule_kind() == target)
}

/// Peel a closed whitelist of single-child transparent wrapper
/// rules. Returns the innermost non-wrapper view.
///
/// The whitelist enumerates the bbnf.bbnf transparent wrapper
/// rules that survive preserve_identity mode and need to be
/// unwrapped during lowering descent: `grammar_item`,
/// `directive`, `lhs`.
///
/// `rhs` and other wrapper rules whose enum entries were dropped
/// by an earlier optimizer pass are NOT peeled here — they are
/// detected by `lower_leaf_by_span_text` in `expression.rs` once
/// the lowering reaches a span whose source slice matches a
/// closed bbnf leaf vocabulary. After AE.4's clean regen the
/// enum entries return and the named whitelist becomes
/// authoritative again.
///
/// If the view's rule_kind is not in the whitelist, returns the
/// view unchanged. If the view has no `child(0)`, returns it
/// unchanged (defensive against empty compounds).
pub(crate) fn peel_transparent<'tape>(
    mut view: BbnfBootstrapNodeView<'tape>,
) -> BbnfBootstrapNodeView<'tape> {
    loop {
        match view.rule_kind() {
            BbnfBootstrapRuleKind::grammar_item
            | BbnfBootstrapRuleKind::directive
            | BbnfBootstrapRuleKind::lhs
            | BbnfBootstrapRuleKind::rhs => {
                let Some(child) = view.child(0) else {
                    return view;
                };
                view = child;
            }
            _ => return view,
        }
    }
}
