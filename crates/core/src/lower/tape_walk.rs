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

/// Depth-first search for the first descendant whose `rule_kind()`
/// matches `target`. Checks the root first, then the direct children
/// (via [`find_child_by_kind`]), then recurses into each child in
/// document order.
///
/// Under DTA, semantic children of a rule compound often sit inside
/// an intermediate Seq wrapper emitted by the lifter's tape-shape
/// requirement. Direct-child scans (`find_child_by_kind`) miss the
/// target when the rule body is `Seq([..., target_rule, ...])`;
/// this descent sees through those wrappers without false positives
/// (the search is targeted — it returns the first occurrence only).
///
/// Use this whenever the target rule_kind is a nested-rule output
/// that may surface one or two compounds deeper than the caller's
/// immediate view. Leaf-immediate targets (identifier, literal,
/// regex that are guaranteed direct children under the grammar)
/// can keep `find_child_by_kind` — it's cheaper.
pub(crate) fn find_descendant_by_kind<'tape>(
    view: BbnfBootstrapNodeView<'tape>,
    target: BbnfBootstrapRuleKind,
) -> Option<BbnfBootstrapNodeView<'tape>> {
    if view.rule_kind() == target {
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

/// Find the rhs expression-head descendant under shape-authoritative
/// OR walker-routed tape.
///
/// `rhs` is a transparent Wrap alias of `closure | alternation`
/// (see `grammar/bbnf/bbnf.bbnf`). The Wrap shape emitter elides the
/// `rhs` compound entirely at parse time — its chosen branch's shape
/// fn owns the tape record directly (see
/// `crates/core/src/backend/rust/emitter/shapes/wrap.rs::emit_alt_tape_
/// dispatch`). The DTA walker, by contrast, pushes an explicit `rhs`
/// Rule compound (variant_idx = 38) that wraps the chosen branch.
///
/// Callers ultimately hand the returned view to `lower_rhs`, which
/// calls [`peel_transparent`] → `dispatch_expression`. That pipeline
/// handles either the named wrapper OR the branch head directly —
/// `rhs` is in the peel whitelist, and `closure` / `alternation` are
/// explicit arms in `dispatch_expression`.
///
/// Search order:
/// 1. `rhs` compound (walker-routed tape).
/// 2. `closure` compound (shape-authoritative tape, grammar-closure
///    branch — leading `|`).
/// 3. `alternation` compound (shape-authoritative tape, standard
///    expression branch).
///
/// Returns `None` only when the caller handed in a view that contains
/// no expression at all (malformed rule with empty body).
pub(crate) fn find_rhs_expression_descendant<'tape>(
    view: BbnfBootstrapNodeView<'tape>,
) -> Option<BbnfBootstrapNodeView<'tape>> {
    if let Some(rhs) = find_descendant_by_kind(view, BbnfBootstrapRuleKind::rhs) {
        return Some(rhs);
    }
    for alt in [
        BbnfBootstrapRuleKind::closure,
        BbnfBootstrapRuleKind::alternation,
    ] {
        if let Some(head) = find_descendant_by_kind(view, alt) {
            return Some(head);
        }
    }
    None
}

/// Find the first descendant of `view` whose `rule_kind()` matches
/// `target`, descending only through structurally-anonymous wrappers
/// (Seq / Alt / Repeat compounds whose own `rule_kind` is `Unknown`
/// or the catch-all `int_lit` sentinel the walker stamps when a
/// compound isn't preceded by a `DtaState::Ref`).
///
/// This is the correct companion to [`find_descendant_by_kind`] for
/// "find my own body's direct child under DTA's Seq wrappers": the
/// caller's grammar body is wrapped in one or more anonymous Seq /
/// Alt compounds emitted by the lifter's tape-shape requirement, and
/// the target is a genuine sibling of the other body components (not
/// a descendant inside another semantic rule's subtree). Blind
/// descent via `find_descendant_by_kind` would step past a sibling
/// boundary — e.g. searching a `factor` for a `modifier` would
/// descend into the sibling `term`'s subtree and return a modifier
/// belonging to a nested expression.
///
/// Stop conditions when descending:
/// - reached the target rule_kind → return
/// - hit a known semantic rule_kind (i.e. any variant of
///   `BbnfBootstrapRuleKind` that is NOT `Unknown` and NOT `int_lit`)
///   that differs from `target` → don't descend past it (skip its
///   subtree)
/// - anonymous wrapper (`Unknown` or `int_lit` sentinel) → descend
///   one level and repeat
pub(crate) fn find_sibling_by_kind<'tape>(
    view: BbnfBootstrapNodeView<'tape>,
    target: BbnfBootstrapRuleKind,
) -> Option<BbnfBootstrapNodeView<'tape>> {
    if view.rule_kind() == target {
        return Some(view);
    }
    for child in view.children() {
        if child.rule_kind() == target {
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

/// Whether `view` is an anonymous structural wrapper — a compound
/// the DTA lifter emitted without a semantic-rule identity. These
/// are the only nodes a sibling-scoped search (see
/// [`find_sibling_by_kind`]) steps through when looking for a
/// sibling-level body component.
///
/// The DTA driver stamps `variant_idx = 0` on compounds whose
/// `variant_idx` was never captured from a `DtaState::Ref` dispatch;
/// the generated `rule_kind()` projection maps `variant_idx = 0` to
/// `int_lit` (the zeroth enum variant) and unmapped variant_idx
/// values to `Unknown`. Either mean "no semantic-rule identity" —
/// but `int_lit` is ambiguous: a real `int_lit` leaf from
/// `value_expr` lowering carries `TapeKind::Span` / `Literal` /
/// `Regex`, not a compound shape. Gate on both rule_kind AND tape
/// kind so a real leaf doesn't get descended through.
fn is_anonymous_wrapper(view: BbnfBootstrapNodeView<'_>) -> bool {
    use ::bbnf::runtime::tape::TapeKind;
    if !matches!(
        view.kind(),
        TapeKind::Rule | TapeKind::Seq | TapeKind::Alt | TapeKind::Repeat,
    ) {
        return false;
    }
    matches!(
        view.rule_kind(),
        BbnfBootstrapRuleKind::Unknown | BbnfBootstrapRuleKind::int_lit,
    )
}

/// Collect every sibling-scoped occurrence of `target` reachable from
/// `view` by descending only through anonymous structural wrappers,
/// appending each to `out` with stop-at-hit semantics.
///
/// The multi-hit analogue of [`find_sibling_by_kind`]: positional
/// occurrences of `target` that surface as disjoint subtrees under
/// the caller's body wrapper (e.g. multiple `call_arg` rules under a
/// term compound) are gathered without descending into any
/// individual target's own body. Semantic-rule compounds that aren't
/// the target (e.g. an `identifier` sibling of the `call_arg` list)
/// are skipped entirely so their subtrees don't leak nested targets
/// into the sibling set.
pub(crate) fn collect_siblings_by_kind<'tape>(
    view: BbnfBootstrapNodeView<'tape>,
    target: BbnfBootstrapRuleKind,
    out: &mut Vec<BbnfBootstrapNodeView<'tape>>,
) {
    if view.rule_kind() == target {
        out.push(view);
        return;
    }
    for child in view.children() {
        if child.rule_kind() == target {
            out.push(child);
            continue;
        }
        if is_anonymous_wrapper(child) {
            collect_siblings_by_kind(child, target, out);
        }
        // Semantic-rule sibling that isn't the target — skip its
        // subtree (don't flatten nested occurrences).
    }
}

/// Peel a closed whitelist of single-child transparent wrapper
/// rules. Returns the innermost non-wrapper view.
///
/// The whitelist enumerates the bbnf.bbnf transparent wrapper
/// rules that survive preserve_identity mode and need to be
/// unwrapped during lowering descent: `rhs`, `grammar_item`,
/// `directive`, `lhs`.
///
/// `rhs` (`= closure | alternation`) is the top-level rule-body
/// wrapper in the bbnf grammar. Under structural mode (and in
/// the generated bootstrap parser) it appears as a single-child
/// compound wrapping the actual expression head. Peeling it here
/// lets `dispatch_expression` see the semantic content directly.
///
/// If the view's rule_kind is not in the whitelist, returns the
/// view unchanged. If the view has no `child(0)`, returns it
/// unchanged (defensive against empty compounds).
pub(crate) fn peel_transparent<'tape>(
    mut view: BbnfBootstrapNodeView<'tape>,
) -> BbnfBootstrapNodeView<'tape> {
    loop {
        match view.rule_kind() {
            BbnfBootstrapRuleKind::rhs
            | BbnfBootstrapRuleKind::grammar_item
            | BbnfBootstrapRuleKind::directive
            | BbnfBootstrapRuleKind::lhs => {
                let Some(child) = view.child(0) else {
                    return view;
                };
                view = child;
            }
            _ => return view,
        }
    }
}
