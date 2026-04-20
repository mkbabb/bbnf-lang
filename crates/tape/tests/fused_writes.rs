//! AY.W1.1 — Flat-AoS write API regression suite.
//!
//! Per AY.md prop 1: the seven-way SoA push pivot reverts to flat-AoS
//! `Vec<TapeRec>` + parallel `sib_skip: Vec<u32>`. These tests assert:
//!
//! - `push_compound_fused` returns the row index it wrote at.
//! - The structural row + parallel `sib_skip` stay in lockstep across
//!   the AoS substrate (one record-store + one sib_skip-store per
//!   call).
//! - `push_leaf_fused` wires custom `flags` + `extra` + `child_off`
//!   through to the flat record.
//! - The grow path keeps the `records` and `sib_skip` columns
//!   synchronised across many pushes.

use tape::{kind::TapeKind, tape::TapeOffset, Columns};

#[test]
fn push_compound_fused_returns_row_index() {
    let mut cols = Columns::new();
    let idx0 = cols.push_compound_fused(TapeKind::Seq, 0);
    let idx1 = cols.push_compound_fused(TapeKind::Alt, 5);
    assert_eq!(idx0, 0);
    assert_eq!(idx1, 1);
    assert_eq!(cols.len(), 2);
}

#[test]
fn push_compound_fused_columns_stay_in_lockstep() {
    let mut cols = Columns::new();
    cols.push_compound_fused(TapeKind::Seq, 0);
    cols.push_compound_fused(TapeKind::Alt, 5);
    cols.push_compound_fused(TapeKind::Rule, 10);

    assert_eq!(cols.len(), 3);

    assert_eq!(cols.kind_at(0), TapeKind::Seq);
    assert_eq!(cols.kind_at(1), TapeKind::Alt);
    assert_eq!(cols.kind_at(2), TapeKind::Rule);

    assert_eq!(cols.span_lo_at(0), 0);
    assert_eq!(cols.span_lo_at(1), 5);
    assert_eq!(cols.span_lo_at(2), 10);

    // Provisional `span_hi == span_lo` per the W1.1 contract — the
    // compound's frame-pop site overwrites this with the real end.
    assert_eq!(cols.span_hi_at(0), 0);
    assert_eq!(cols.span_hi_at(1), 5);
    assert_eq!(cols.span_hi_at(2), 10);

    // Provisional `child_off == NONE`.
    assert_eq!(cols.child_off_at(0), TapeOffset::NONE);

    // Defaults for the rest.
    assert_eq!(cols.flags_at(0), 0);
    assert_eq!(cols.extra_at(0), 0);
    assert_eq!(cols.sib_skip_at(0), 0);
}

#[test]
fn push_leaf_fused_writes_custom_flags() {
    let mut cols = Columns::new();
    let idx = cols.push_leaf_fused(
        TapeKind::Span,
        7,         // flags = variant_idx
        0xCAFE,    // extra = bitfield
        100,       // span_lo
        200,       // span_hi
        TapeOffset(42),
    );
    assert_eq!(idx, 0);

    assert_eq!(cols.kind_at(0), TapeKind::Span);
    assert_eq!(cols.flags_at(0), 7);
    assert_eq!(cols.extra_at(0), 0xCAFE);
    assert_eq!(cols.span_lo_at(0), 100);
    assert_eq!(cols.span_hi_at(0), 200);
    assert_eq!(cols.sib_skip_at(0), 0);
    assert_eq!(cols.child_off_at(0), TapeOffset(42));
}

/// AY.W1.1 — the AoS substrate's `Vec<TapeRec>` + parallel `Vec<u32>`
/// stay in lockstep across the grow boundary. Pre-AY this guarded a
/// bug where `Vec<u8>::with_capacity(6)` and `Vec<u16>::with_capacity(6)`
/// rounded up to different actual capacities; post-AY both `records`
/// (one `Vec<TapeRec>`) and `sib_skip` (one `Vec<u32>`) grow under
/// the same `Vec::push` policy and the per-element-type capacity skew
/// is gone.
#[test]
fn push_compound_fused_grows_with_min_cap_invariant() {
    let mut cols = Columns::with_capacity(3);
    for i in 0..1000u32 {
        let idx = cols.push_compound_fused(TapeKind::Seq, i);
        assert_eq!(idx, i, "push_compound_fused index sequence");
    }
    assert_eq!(cols.len(), 1000);
    // Spot-check first/middle/last entries.
    assert_eq!(cols.span_lo_at(0), 0);
    assert_eq!(cols.span_lo_at(500), 500);
    assert_eq!(cols.span_lo_at(999), 999);
}

#[test]
fn push_leaf_fused_grows_with_min_cap_invariant() {
    let mut cols = Columns::with_capacity(3);
    for i in 0..1000u32 {
        let idx = cols.push_leaf_fused(
            TapeKind::Literal,
            (i & 0xFF) as u8,
            (i & 0xFFFF) as u16,
            i,
            i + 1,
            TapeOffset::NONE,
        );
        assert_eq!(idx, i);
    }
    assert_eq!(cols.len(), 1000);
    assert_eq!(cols.flags_at(100), 100);
    assert_eq!(cols.extra_at(300), 300);
    assert_eq!(cols.span_lo_at(999), 999);
    assert_eq!(cols.span_hi_at(999), 1000);
}

/// Interleaving fused compound + fused leaf pushes must keep the
/// columns aligned. This is the parser's actual usage pattern.
#[test]
fn fused_compound_and_leaf_interleave() {
    let mut cols = Columns::new();
    let c0 = cols.push_compound_fused(TapeKind::Seq, 0);
    let l0 = cols.push_leaf_fused(
        TapeKind::Literal, 1, 0, 0, 1, TapeOffset::NONE,
    );
    let l1 = cols.push_leaf_fused(
        TapeKind::Literal, 1, 0, 1, 2, TapeOffset::NONE,
    );
    let c1 = cols.push_compound_fused(TapeKind::Alt, 2);
    let l2 = cols.push_leaf_fused(
        TapeKind::Span, 7, 0xCAFE, 2, 5, TapeOffset(42),
    );

    assert_eq!(c0, 0);
    assert_eq!(l0, 1);
    assert_eq!(l1, 2);
    assert_eq!(c1, 3);
    assert_eq!(l2, 4);

    assert_eq!(cols.len(), 5);

    // Spot-check the leaf with custom flags survived.
    assert_eq!(cols.flags_at(4), 7);
    assert_eq!(cols.extra_at(4), 0xCAFE);
    assert_eq!(cols.child_off_at(4), TapeOffset(42));
}

/// `push_compound_fused`'s `idx` return matches `cols.len()` at call
/// time. Caller can rely on this for `parent_rec`.
#[test]
fn push_compound_fused_index_matches_len_at_call() {
    let mut cols = Columns::new();
    for expected in 0..50u32 {
        let pre_len = cols.len();
        let idx = cols.push_compound_fused(TapeKind::Seq, expected);
        assert_eq!(idx as usize, pre_len);
        assert_eq!(idx, expected);
    }
}
