//! `BbnfView`-shaped descent helpers local to `value_expr` lowering.
//!
//! Replaces the pre-cutover descendant walk previously provided over
//! the bootstrap node-view surface. Under struct-direct projection
//! the parser emits typed compounds with semantic identity preserved on
//! compounds with semantic identity preserved on
//! [`crate::runtime::bbnf::BbnfCompoundKind`]; descent functions
//! match on that discriminator rather than `BbnfBootstrapRuleKind`.
//!
//! These helpers live local to `value_expr/` because they encode
//! value-expression-specific descent semantics (which compound kinds
//! count as transparent wrappers, when to short-circuit at semantic
//! boundaries, etc.). The corresponding helpers for the outer
//! grammar-level lowering live alongside that pass in
//! `crate::lower::view_walk` (D1's territory).

use crate::runtime::RuntimeView;
use crate::runtime::bbnf::{BbnfCompoundKind, BbnfKind, BbnfView};

/// Depth-first search for the first descendant of `view` whose
/// `compound_kind()` matches `target`. Checks the root first, then
/// recurses through each direct child in document order.
///
/// Returns `None` when no compound-kind match exists in the subtree.
/// Leaf focuses (Int / Float / Bool / Span / Tag / Unit) are
/// non-matches by definition (`compound_kind()` is `None`).
pub(super) fn find_descendant_by_compound_kind<'a, 'p: 'a>(
    view: BbnfView<'a, 'p>,
    target: BbnfCompoundKind,
) -> Option<BbnfView<'a, 'p>> {
    if view.compound_kind() == Some(target) {
        return Some(view);
    }
    if view.kind() != BbnfKind::Compound {
        return None;
    }
    for child in RuntimeView::children(&view) {
        if let Some(found) = find_descendant_by_compound_kind(child, target) {
            return Some(found);
        }
    }
    None
}
