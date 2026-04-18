//! AW-IV.W4.3 — Runtime bloom + GADT dedup substrate coverage.
//!
//! Tests the core admission contract: first probe inserts, second
//! probe with identical bytes hits, bloom false-positive handling,
//! columns_range_eq correctness, and push_compound_referring layout.

use bbnf_tape::columns::Columns;
use bbnf_tape::kind::TapeKind;
use bbnf_tape::tape::TapeOffset;
use bbnf_tape::{columns_range_eq, push_compound_referring, BloomDedup};

fn push_compound_row(columns: &mut Columns, kind: TapeKind, span_lo: u32, span_hi: u32) -> u32 {
    let idx = columns.push_compound_fused(kind, span_lo);
    columns.span_hi[idx as usize] = span_hi;
    idx
}

#[test]
fn first_probe_inserts_second_hits() {
    let mut columns = Columns::with_capacity(32);
    let mut dedup = BloomDedup::new();

    // Push compound A at rows [0..1].
    let a_start = push_compound_row(&mut columns, TapeKind::Rule, 0, 10);
    assert_eq!(a_start, 0);
    let first = dedup.try_dedup(&columns, 0, 1);
    assert!(first.is_none(), "first probe must record, not hit");

    // Push compound B at rows [1..2] — identical structural bytes
    // (same kind, same row count).
    let b_start = push_compound_row(&mut columns, TapeKind::Rule, 100, 110);
    assert_eq!(b_start, 1);
    // Note: span_lo differs (100 vs 0) so hash differs — probe should
    // insert, not hit.
    let second = dedup.try_dedup(&columns, 1, 1);
    assert!(second.is_none(), "different spans produce different hashes");
}

#[test]
fn identical_skeletons_at_same_span_dedup() {
    let mut columns = Columns::with_capacity(32);
    let mut dedup = BloomDedup::new();

    // Push the same structural skeleton twice: same kind, same spans.
    // Note this isn't a realistic parser scenario (parser never emits
    // at the same span twice) but it tests the dedup path cleanly.
    let _a = push_compound_row(&mut columns, TapeKind::Rule, 0, 10);
    let first = dedup.try_dedup(&columns, 0, 1);
    assert!(first.is_none(), "first insert");

    let _b = push_compound_row(&mut columns, TapeKind::Rule, 0, 10);
    let second = dedup.try_dedup(&columns, 1, 1);
    assert_eq!(second, Some(0), "identical skeleton hits at index 0");
}

#[test]
fn reset_clears_bloom_and_gadt() {
    let mut columns = Columns::with_capacity(32);
    let mut dedup = BloomDedup::new();

    let _a = push_compound_row(&mut columns, TapeKind::Rule, 0, 10);
    dedup.try_dedup(&columns, 0, 1);

    dedup.reset();

    // After reset, the same probe returns None again (not a hit).
    let after = dedup.try_dedup(&columns, 0, 1);
    assert!(after.is_none(), "reset wipes bloom + GADT");
}

#[test]
fn columns_range_eq_returns_true_on_match() {
    let mut columns = Columns::with_capacity(32);
    let _a = push_compound_row(&mut columns, TapeKind::Rule, 0, 10);
    let _b = push_compound_row(&mut columns, TapeKind::Rule, 0, 10);
    // sib_skip is 0 for both (no finalize), flags 0, child_off NONE —
    // rows are structurally identical.
    assert!(columns_range_eq(&columns, 0, 1, 1));
}

#[test]
fn columns_range_eq_returns_false_on_mismatch() {
    let mut columns = Columns::with_capacity(32);
    let _a = push_compound_row(&mut columns, TapeKind::Rule, 0, 10);
    let _b = push_compound_row(&mut columns, TapeKind::Alt, 0, 10);
    // Different kinds — not equal.
    assert!(!columns_range_eq(&columns, 0, 1, 1));
}

#[test]
fn push_compound_referring_emits_single_row() {
    let mut columns = Columns::with_capacity(32);
    // Fake existing skeleton at row 0.
    let _existing = push_compound_row(&mut columns, TapeKind::Rule, 0, 10);
    let len_before = columns.len();

    let ref_idx = push_compound_referring(
        &mut columns,
        TapeKind::Rule,
        42, // rule_id
        0,  // existing row
        (100, 110),
    );

    assert_eq!(ref_idx as usize, len_before);
    assert_eq!(columns.len(), len_before + 1, "exactly one row pushed");
    assert_eq!(columns.kind_at(ref_idx), TapeKind::Rule);
    assert_eq!(columns.span_at(ref_idx), (100, 110));
    assert_eq!(columns.child_off_at(ref_idx), TapeOffset(0));
    assert!(columns.has_children_at(ref_idx), "HAS_CHILDREN_BIT set");
    // flags holds the rule_id's low byte.
    assert_eq!(columns.flags[ref_idx as usize], 42);
}
