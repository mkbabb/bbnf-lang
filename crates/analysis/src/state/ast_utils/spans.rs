//! Compute byte-offset spans on tape-first `BbnfBootstrapNodeView` nodes.
//!
//! Under the tape-first view API, every rule compound carries its
//! own `(lo, hi)` span on the cursor. `compute_expression_end` still
//! exists as a thin wrapper for consumers that historically asked
//! "where does this expression's last byte land?", but it simply
//! forwards to `view.span().1` — the tape already records the
//! answer. The signature is kept to preserve the current call sites
//! (which thread `Option<usize>` through their callers).

use bbnf::grammar::generated::BbnfBootstrapNodeView;

/// Public wrapper for `compute_expression_end` (used by selection_range).
pub fn compute_expression_end_pub(node: BbnfBootstrapNodeView<'_>) -> Option<usize> {
    compute_expression_end(node)
}

/// Compute the end byte offset of a bootstrap tape view.
///
/// Returns `None` only for empty placeholder compounds (span
/// `(lo, lo)` with no children) — the same signal the pre-AE
/// enum-based code used to skip absent optional wrappers.
pub fn compute_expression_end(node: BbnfBootstrapNodeView<'_>) -> Option<usize> {
    let (lo, hi) = node.span();
    if hi == lo && node.children().next().is_none() {
        return None;
    }
    Some(hi as usize)
}
