//! AW-III.W5.c — Fused SoA write API regression suite.
//!
//! Per AW-III.md §W5: the fused write API replaces the seven-way
//! `Vec::push` tax that `reserve_compound` paid per compound row with
//! one bounds-check + 7 unchecked stores. These tests assert:
//!
//! - `push_compound_fused` returns the row index it wrote at.
//! - The seven structural columns (`kinds`, `flags`, `extra`, `span_lo`,
//!   `span_hi`, `sib_skip`, `child_off`) all stay in lockstep.
//! - `push_leaf_fused` wires custom `flags` + `extra` + `child_off`.
//! - The grow-all path (the cold growth point past current capacity)
//!   keeps the columns synchronised even when individual `Vec`s
//!   over-allocate by different amounts (the bug that surfaced during
//!   `serialize_roundtrip::json_empty_obj` post-W5.c — `ensure_one`
//!   must consult the MIN capacity across all columns, not just
//!   `kinds.capacity()`).

use bbnf_tape::{kind::TapeKind, tape::TapeOffset, Columns};

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

    assert_eq!(cols.kinds.len(), 3);
    assert_eq!(cols.flags.len(), 3);
    assert_eq!(cols.extra.len(), 3);
    assert_eq!(cols.span_lo.len(), 3);
    assert_eq!(cols.span_hi.len(), 3);
    assert_eq!(cols.sib_skip.len(), 3);
    assert_eq!(cols.child_off.len(), 3);

    assert_eq!(cols.kinds[0] & 0x0F, TapeKind::Seq as u8);
    assert_eq!(cols.kinds[1] & 0x0F, TapeKind::Alt as u8);
    assert_eq!(cols.kinds[2] & 0x0F, TapeKind::Rule as u8);

    assert_eq!(cols.span_lo[0], 0);
    assert_eq!(cols.span_lo[1], 5);
    assert_eq!(cols.span_lo[2], 10);

    // Provisional `span_hi == span_lo` per the W5.c contract — the
    // compound's frame-pop site overwrites this with the real end.
    assert_eq!(cols.span_hi[0], 0);
    assert_eq!(cols.span_hi[1], 5);
    assert_eq!(cols.span_hi[2], 10);

    // Provisional `child_off == NONE`.
    assert_eq!(cols.child_off[0], TapeOffset::NONE);

    // Defaults for the rest.
    assert_eq!(cols.flags[0], 0);
    assert_eq!(cols.extra[0], 0);
    assert_eq!(cols.sib_skip[0], 0);
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

    assert_eq!(cols.kinds[0] & 0x0F, TapeKind::Span as u8);
    assert_eq!(cols.flags[0], 7);
    assert_eq!(cols.extra[0], 0xCAFE);
    assert_eq!(cols.span_lo[0], 100);
    assert_eq!(cols.span_hi[0], 200);
    assert_eq!(cols.sib_skip[0], 0);
    assert_eq!(cols.child_off[0], TapeOffset(42));
}

/// AW-III.W5.c regression: the bug that surfaced post-W5.c integration.
///
/// `Vec::with_capacity(n)` may round up past `n` differently for
/// different element types. A `Vec<u8>` for `kinds` and a `Vec<u16>`
/// for `extra` requested with capacity 6 may end up with capacity 16
/// vs 8 respectively. `ensure_one` must consult the MIN capacity
/// across all seven structural columns, not just `kinds.capacity()`,
/// or `set_len(idx + 1)` on `extra` violates `Vec::set_len`'s
/// `new_len <= capacity()` precondition (panics under
/// `unsafe_precondition` checks; UB on release without checks).
///
/// Test: build a `Columns` with `with_capacity(3)`, push enough
/// compound rows to exceed every column's actual capacity, verify no
/// panics and lengths stay aligned.
#[test]
fn push_compound_fused_grows_with_min_cap_invariant() {
    let mut cols = Columns::with_capacity(3);
    for i in 0..1000u32 {
        let idx = cols.push_compound_fused(TapeKind::Seq, i);
        assert_eq!(idx, i, "push_compound_fused index sequence");
    }
    assert_eq!(cols.kinds.len(), 1000);
    assert_eq!(cols.flags.len(), 1000);
    assert_eq!(cols.extra.len(), 1000);
    assert_eq!(cols.span_lo.len(), 1000);
    assert_eq!(cols.span_hi.len(), 1000);
    assert_eq!(cols.sib_skip.len(), 1000);
    assert_eq!(cols.child_off.len(), 1000);
    // Spot-check first/middle/last entries.
    assert_eq!(cols.span_lo[0], 0);
    assert_eq!(cols.span_lo[500], 500);
    assert_eq!(cols.span_lo[999], 999);
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
    assert_eq!(cols.kinds.len(), 1000);
    assert_eq!(cols.flags[100], 100);
    assert_eq!(cols.extra[300], 300);
    assert_eq!(cols.span_lo[999], 999);
    assert_eq!(cols.span_hi[999], 1000);
}

/// AW-III.W5.c — interleaving fused compound + fused leaf pushes
/// must keep the columns aligned. This is the dispatch_one's actual
/// usage pattern.
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
    assert_eq!(cols.kinds.len(), 5);
    assert_eq!(cols.flags.len(), 5);
    assert_eq!(cols.extra.len(), 5);
    assert_eq!(cols.span_lo.len(), 5);
    assert_eq!(cols.span_hi.len(), 5);
    assert_eq!(cols.sib_skip.len(), 5);
    assert_eq!(cols.child_off.len(), 5);

    // Spot-check the leaf with custom flags survived.
    assert_eq!(cols.flags[4], 7);
    assert_eq!(cols.extra[4], 0xCAFE);
    assert_eq!(cols.child_off[4], TapeOffset(42));
}

/// AW-III.W5.c — `push_compound_fused`'s `idx` return matches
/// `cols.len()` at call time. Caller can rely on this for `parent_rec`.
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
