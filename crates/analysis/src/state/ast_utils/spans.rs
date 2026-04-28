//! Compute byte-offset spans on struct-direct [`BbnfView`] nodes.
//!
//! AZ-II.cutover.D4 — every BBNF compound carries a recoverable
//! byte-span via [`BbnfView::byte_span`] (a pointer-arithmetic walk
//! over descendant `Span` leaves through the document's input
//! slice). `compute_expression_end` is therefore a thin wrapper that
//! returns the upper bound of the focus's byte span; consumers
//! historically asked "where does this expression's last byte
//! land?", and the struct-direct runtime answers that authentically.
//!
//! Returns `None` only when the focused value carries no descendant
//! `Span` leaf at all (an empty compound, or a numeric / boolean /
//! unit leaf). The same signal the pre-AE code used to skip absent
//! optional wrappers.

use bbnf::runtime::bbnf::BbnfView;

/// Public wrapper for `compute_expression_end` (used by selection_range).
pub fn compute_expression_end_pub(node: BbnfView<'_, '_>) -> Option<usize> {
    compute_expression_end(node)
}

/// Compute the end byte offset of a [`BbnfView`] focus.
pub fn compute_expression_end(node: BbnfView<'_, '_>) -> Option<usize> {
    let (_lo, hi) = node.byte_span()?;
    Some(hi)
}
