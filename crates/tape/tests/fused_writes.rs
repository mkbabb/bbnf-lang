//! AY.W1.1 + AY-II.W0.a — Flat-AoS write + rollback regression suite.
//!
//! AY.W1.1 landed `Columns::push_compound_fused` / `push_leaf_fused`
//! over the flat-AoS substrate (one `TapeRec` store + one `sib_skip`
//! store per call). AY-II.W0.a added `Columns::rollback_to` — the
//! canonical rollback primitive emitter retry-IIFE sites call in
//! place of ad-hoc `columns_mut().truncate(save)` invocations — and
//! restored `finaliser::finalise` as the sole writer of `sib_skip`.
//! These tests assert:
//!
//! - `push_compound_fused` returns the row index it wrote at.
//! - The structural row + parallel `sib_skip` stay in lockstep across
//!   the AoS substrate (one record-store + one sib_skip-store per
//!   call).
//! - `push_leaf_fused` wires custom `flags` + `extra` + `child_off`
//!   through to the flat record.
//! - The grow path keeps the `records` and `sib_skip` columns
//!   synchronised across many pushes.
//! - `Columns::rollback_to` truncates both per-record columns in
//!   lockstep and is idempotent on a no-op rewind.
//! - The finaliser derives `sib_skip` unconditionally (no stamp-bit
//!   skipping post-AY-II.W0.a) on tapes built purely through the
//!   fused Columns primitives.

use tape::{finaliser, kind::TapeKind, tape::TapeOffset, Columns};

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
    // compound's `end_compound` site overwrites this with the real
    // end.
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
/// stay in lockstep across the grow boundary.
#[test]
fn push_compound_fused_grows_with_min_cap_invariant() {
    let mut cols = Columns::with_capacity(3);
    for i in 0..1000u32 {
        let idx = cols.push_compound_fused(TapeKind::Seq, i);
        assert_eq!(idx, i, "push_compound_fused index sequence");
    }
    assert_eq!(cols.len(), 1000);
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

    assert_eq!(cols.flags_at(4), 7);
    assert_eq!(cols.extra_at(4), 0xCAFE);
    assert_eq!(cols.child_off_at(4), TapeOffset(42));
}

/// AY-II.W0.a — `Columns::rollback_to(open_offset)` truncates both
/// per-record columns (`records` + `sib_skip`) in lockstep.
#[test]
fn rollback_to_truncates_per_record_columns_in_lockstep() {
    let mut cols = Columns::new();
    cols.push_compound_fused(TapeKind::Seq, 0);
    cols.push_leaf_fused(TapeKind::Literal, 1, 0, 0, 1, TapeOffset::NONE);
    cols.push_leaf_fused(TapeKind::Literal, 1, 0, 1, 2, TapeOffset::NONE);
    cols.push_compound_fused(TapeKind::Alt, 2);
    assert_eq!(cols.len(), 4);

    cols.rollback_to(2);
    assert_eq!(
        cols.len(),
        2,
        "rollback_to discards every row at-or-after open_offset"
    );
    // Remaining rows untouched.
    assert_eq!(cols.kind_at(0), TapeKind::Seq);
    assert_eq!(cols.kind_at(1), TapeKind::Literal);
}

/// AY-II.W0.a — rollback is idempotent: rolling past the end, or
/// rolling twice to the same offset, is a no-op on the second call.
#[test]
fn rollback_to_idempotent_at_columns_level() {
    let mut cols = Columns::new();
    cols.push_compound_fused(TapeKind::Seq, 0);
    cols.push_leaf_fused(TapeKind::Literal, 0, 0, 0, 1, TapeOffset::NONE);

    // Rollback beyond the end is a no-op.
    cols.rollback_to(u32::MAX);
    assert_eq!(cols.len(), 2);

    // Rollback to the current length is a no-op.
    cols.rollback_to(2);
    assert_eq!(cols.len(), 2);

    // Rollback to 1 discards the leaf.
    cols.rollback_to(1);
    assert_eq!(cols.len(), 1);

    // Second rollback to 1 is a no-op.
    cols.rollback_to(1);
    assert_eq!(cols.len(), 1);
}

/// AY-II.W0.a — the finaliser is the sole writer of `sib_skip`.
/// Build a compound manually via the fused Columns primitives, hand
/// a matching `frame_depth` stream to the finaliser, and verify the
/// sibling-skip column is derived unconditionally (no stamp-bit
/// short-circuit, no pre-stamped values).
#[test]
fn finaliser_derives_sib_skip_unconditionally() {
    let mut cols = Columns::new();
    // Layout: (root (a b c))
    //   row 0 = root compound (depth 0)
    //   row 1 = a leaf         (depth 1)
    //   row 2 = b leaf         (depth 1)
    //   row 3 = c leaf         (depth 1)
    cols.push_compound_fused(TapeKind::Seq, 0);
    cols.push_leaf_fused(TapeKind::Literal, 0, 0, 0, 1, TapeOffset::NONE);
    cols.push_leaf_fused(TapeKind::Literal, 0, 0, 1, 2, TapeOffset::NONE);
    cols.push_leaf_fused(TapeKind::Literal, 0, 0, 2, 3, TapeOffset::NONE);
    // Simulate the end_compound back-patch on the root row.
    cols.set_span_hi_at(0, 3);
    cols.set_child_off_at(0, TapeOffset(1));
    cols.or_extra_at(0, tape::TapeRec::HAS_CHILDREN_BIT);

    // Every sib_skip slot is the default 0 pre-finalise.
    for i in 0..cols.len() as u32 {
        assert_eq!(cols.sib_skip_at(i), 0);
    }

    let frame_depth = vec![0u8, 1, 1, 1];
    finaliser::finalise(&mut cols, &frame_depth);

    // Finaliser-derived distances: row 1 → row 2 = 1; row 2 → row 3 =
    // 1; row 3 is last sibling → 0. Root (row 0) has no outer frame
    // at depth 0 so its sib_skip stays 0.
    assert_eq!(cols.sib_skip_at(0), 0, "root has no outer frame");
    assert_eq!(cols.sib_skip_at(1), 1);
    assert_eq!(cols.sib_skip_at(2), 1);
    assert_eq!(cols.sib_skip_at(3), 0, "c is last sibling");
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
