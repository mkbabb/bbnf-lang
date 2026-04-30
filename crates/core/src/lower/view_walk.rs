//! Shape-agnostic [`BbnfView`] traversal helpers for lowering.
//!
//! AZ-II.cutover.D1 substrate. The struct-direct BBNF runtime exposes
//! a typed-value tree rooted at a [`BbnfDocument`]; consumers walk the
//! tree via [`BbnfView`] (focused value + parent doc handle). This
//! module is the shared CST-view traversal layer: every layer function
//! in `expression/` and the sister `value_expr/` lowering call through
//! these helpers rather than making positional or wrapper assumptions
//! about its input view.
//!
//! The four primitives:
//!
//! - [`iter_rep_children`] — iterate iteration children, transparently
//!   peeling a single `Repeat`-shaped wrapper compound when the
//!   structural emitter wraps the iteration body.
//!
//! - [`find_child_by_kind`] — scan direct children and return the
//!   first one whose [`BbnfCompoundKind`] matches a target.
//!
//! - [`find_descendant_by_kind`] — depth-first descent with a target
//!   compound kind. Used whenever the target compound may surface one
//!   or more wrappers deeper than the caller's immediate view.
//!
//! - [`peel_transparent`] — recursively descend through a closed
//!   whitelist of single-child wrapper rules (`rhs`, `grammar_item`,
//!   `directive`, `lhs`) until a semantic-content compound kind is
//!   reached.
//!
//! The view layer is schema-generic and stays that way — these
//! helpers live in `lower/` because they encode bbnf.bbnf-specific
//! semantic knowledge (which compound kinds are transparent wrappers).
//!
//! # Anonymous-wrapper handling
//!
//! Under struct-direct, every compound has a real
//! [`BbnfCompoundKind`] discriminator (set by
//! [`crate::runtime::bbnf::BbnfStructBuilder::begin_compound`] from
//! the rule-name resolver). Wrapper rules whose names are NOT in
//! [`BbnfCompoundKind::from_rule_name`]'s alphabet — including the
//! imported `expressions.bbnf` value-expression sub-grammar
//! (`value_expr`, `value_atom`, etc.) — collapse into the catch-all
//! [`BbnfCompoundKind::Other`] arm. Sibling-scoped traversal
//! ([`find_sibling_by_kind`], [`collect_siblings_by_kind`]) treats
//! `Other` as the only "anonymous wrapper" descent target — the
//! struct-direct emitter no longer needs erased discriminator
//! sentinels, and every BBNF-main-grammar compound carries its
//! authoritative kind.

use crate::runtime::RuntimeView;
use crate::runtime::bbnf::{BbnfCompoundKind, BbnfView};

/// Iterate a view's "iteration children" — the logical elements of
/// a `+` / `*` / `sep_by` body, regardless of whether the emitter
/// wraps them in an explicit Repeat-shaped compound.
///
/// Shape handling:
/// - If `view.children()` yields exactly one child whose
///   [`BbnfCompoundKind`] is [`BbnfCompoundKind::Other`] (the
///   anonymous-wrapper alphabet), iterate that child's own children
///   (the flattened iteration stream).
/// - Otherwise iterate `view.children()` directly (the optimizer
///   has inlined the wrapper, or the view is a flat grouping that
///   produced iteration children directly).
///
/// Returns a boxed iterator so the caller's match arms don't have
/// to name the iterator type.
pub(crate) fn iter_rep_children<'a, 'p>(
    view: BbnfView<'a, 'p>,
) -> Box<dyn Iterator<Item = BbnfView<'a, 'p>> + 'a> {
    let mut children = view.children();
    let first = match children.next() {
        Some(c) => c,
        None => return Box::new(std::iter::empty()),
    };
    if children.next().is_none() && first.compound_kind() == Some(BbnfCompoundKind::Other) {
        // Single anonymous-wrapper child — flatten its children as
        // the iteration stream.
        return Box::new(first.children().collect::<Vec<_>>().into_iter());
    }
    // Not a single-wrapper wrap — re-iterate from the start.
    Box::new(view.children().collect::<Vec<_>>().into_iter())
}

/// Scan a view's direct children and return the first one whose
/// [`BbnfCompoundKind`] matches `target`. Returns `None` if no such
/// child exists.
///
/// Use this to pluck a positional child by its semantic role
/// (e.g. "find the `term` child of this `factor` compound")
/// without hardcoding child indices that shift under preserved
/// Optional wrappers.
pub(crate) fn find_child_by_kind<'a, 'p>(
    view: BbnfView<'a, 'p>,
    target: BbnfCompoundKind,
) -> Option<BbnfView<'a, 'p>> {
    view.children().find(|c| c.compound_kind() == Some(target))
}

/// Depth-first search for the first descendant whose
/// [`BbnfCompoundKind`] matches `target`. Checks the root first,
/// then the direct children, then recurses into each child in
/// document order.
///
/// Use this whenever the target compound may surface one or two
/// compounds deeper than the caller's immediate view.
pub(crate) fn find_descendant_by_kind<'a, 'p>(
    view: BbnfView<'a, 'p>,
    target: BbnfCompoundKind,
) -> Option<BbnfView<'a, 'p>> {
    if view.compound_kind() == Some(target) {
        return Some(view);
    }
    if let Some(direct) = find_child_by_kind(view, target) {
        return Some(direct);
    }
    for child in view.children() {
        if let Some(found) = find_descendant_by_kind(child, target) {
            return Some(found);
        }
    }
    None
}

/// Find the rhs expression-head descendant under either the
/// preserved `rhs` wrapper or one of its inlined branches (`closure`
/// / `alternation`).
///
/// `rhs = closure | alternation`. The struct-direct emitter may
/// preserve the named `rhs` compound or inline its chosen branch,
/// so a search descends through both shapes uniformly.
pub(crate) fn find_rhs_expression_descendant<'a, 'p>(
    view: BbnfView<'a, 'p>,
) -> Option<BbnfView<'a, 'p>> {
    if let Some(rhs) = find_descendant_by_kind(view, BbnfCompoundKind::Rhs) {
        return Some(rhs);
    }
    for alt in [BbnfCompoundKind::Closure, BbnfCompoundKind::Alternation] {
        if let Some(head) = find_descendant_by_kind(view, alt) {
            return Some(head);
        }
    }
    None
}

/// True iff `view` is an anonymous structural wrapper — a compound
/// the rule-name resolver mapped to [`BbnfCompoundKind::Other`].
/// These are the only nodes a sibling-scoped search (see
/// [`find_sibling_by_kind`]) steps through when looking for a
/// sibling-level body component.
fn is_anonymous_wrapper(view: BbnfView<'_, '_>) -> bool {
    view.compound_kind() == Some(BbnfCompoundKind::Other)
}

/// Find the first descendant of `view` whose [`BbnfCompoundKind`]
/// matches `target`, descending only through structurally-anonymous
/// wrappers ([`BbnfCompoundKind::Other`] compounds emitted for
/// non-main-grammar rules and structural Seq groups).
///
/// This is the correct companion to [`find_descendant_by_kind`] for
/// "find my own body's direct child under structural wrappers": the
/// caller's grammar body may be wrapped in one or more anonymous
/// compounds emitted by structural grammar grouping, and the
/// target is a genuine sibling of the other body components (not a
/// descendant inside another semantic rule's subtree). Blind descent
/// via [`find_descendant_by_kind`] would step past a sibling
/// boundary — e.g. searching a `factor` for a `modifier` would
/// descend into the sibling `term`'s subtree and return a modifier
/// belonging to a nested expression.
pub(crate) fn find_sibling_by_kind<'a, 'p>(
    view: BbnfView<'a, 'p>,
    target: BbnfCompoundKind,
) -> Option<BbnfView<'a, 'p>> {
    if view.compound_kind() == Some(target) {
        return Some(view);
    }
    for child in view.children() {
        if child.compound_kind() == Some(target) {
            return Some(child);
        }
        if is_anonymous_wrapper(child) {
            if let Some(found) = find_sibling_by_kind(child, target) {
                return Some(found);
            }
        }
        // Otherwise `child` is a semantic-rule compound that is NOT
        // the target — its subtree carries the nested grammar, not a
        // sibling body component. Skip descending into it.
    }
    None
}

/// Collect every sibling-scoped occurrence of `target` reachable
/// from `view` by descending only through anonymous structural
/// wrappers, appending each to `out` with stop-at-hit semantics.
///
/// The multi-hit analogue of [`find_sibling_by_kind`]: positional
/// occurrences of `target` that surface as disjoint subtrees under
/// the caller's body wrapper (e.g. multiple `call_arg` rules under
/// a term compound) are gathered without descending into any
/// individual target's own body.
pub(crate) fn collect_siblings_by_kind<'a, 'p>(
    view: BbnfView<'a, 'p>,
    target: BbnfCompoundKind,
    out: &mut Vec<BbnfView<'a, 'p>>,
) {
    if view.compound_kind() == Some(target) {
        out.push(view);
        return;
    }
    for child in view.children() {
        if child.compound_kind() == Some(target) {
            out.push(child);
            continue;
        }
        if is_anonymous_wrapper(child) {
            collect_siblings_by_kind(child, target, out);
        }
    }
}

/// Peel a closed whitelist of single-child transparent wrapper
/// rules. Returns the innermost non-wrapper view.
///
/// The whitelist enumerates the bbnf.bbnf transparent wrapper rules
/// that survive structural emission and need to be unwrapped during
/// lowering descent: `rhs`, `grammar_item`, `directive`, `lhs`.
///
/// `rhs` (`= closure | alternation`) is the top-level rule-body
/// wrapper in the bbnf grammar. Under structural emission it
/// appears as a single-child compound wrapping the actual
/// expression head. Peeling it lets `dispatch_expression` see the
/// semantic content directly.
///
/// If the view's compound kind is not in the whitelist (or the view
/// is a leaf), returns the view unchanged. If the view has no
/// `child(0)`, returns it unchanged (defensive against empty
/// compounds).
pub(crate) fn peel_transparent<'a, 'p>(mut view: BbnfView<'a, 'p>) -> BbnfView<'a, 'p> {
    loop {
        let kind = match view.compound_kind() {
            Some(k) => k,
            None => return view,
        };
        match kind {
            BbnfCompoundKind::Rhs
            | BbnfCompoundKind::GrammarItem
            | BbnfCompoundKind::Directive
            | BbnfCompoundKind::Lhs => {
                let Some(child) = view.child(0) else {
                    return view;
                };
                view = child;
            }
            _ => return view,
        }
    }
}
