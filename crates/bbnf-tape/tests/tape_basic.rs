//! Tranche AA.13 — basic round-trip tests for the tape crate.
//!
//! Verifies:
//! 1. `TapeRec` size is 16 bytes (compile-time assert already
//!    enforces this; this test is a runtime-visible proof).
//! 2. `TapeBuilder::push_leaf` / `push_compound` append records in
//!    insertion order with stable offsets.
//! 3. `TapeCursor::record` / `kind` / `span` / `child` round-trip.
//! 4. `ChunkedArena` spans chunk boundaries without data corruption.

use bbnf_tape::{Tape, TapeBuilder, TapeCursor, TapeKind, TapeOffset, TapeRec};

#[test]
fn tape_rec_size() {
    assert_eq!(std::mem::size_of::<TapeRec>(), 16);
    assert_eq!(std::mem::align_of::<TapeRec>(), 4);
}

#[test]
fn push_leaf_round_trip() {
    let mut b = TapeBuilder::new();
    let off = b.push_leaf(TapeKind::Span, 0, 5, 0);
    assert_eq!(off, TapeOffset(0));

    let tape = b.finish().unwrap();
    assert_eq!(tape.len(), 1);

    let rec = tape.get(off);
    assert_eq!(rec.kind, TapeKind::Span);
    assert_eq!(rec.span_lo, 0);
    assert_eq!(rec.span_hi, 5);
    assert_eq!(rec.span_len(), 5);
    assert_eq!(rec.child_off, TapeOffset::NONE);
    assert!(!rec.has_children());
}

#[test]
fn push_compound_with_children() {
    let mut b = TapeBuilder::new();

    // Mark children start for a rule with two leaf children.
    let children_start = b.mark_children();
    let _c1 = b.push_leaf(TapeKind::Span, 0, 3, 0);
    let _c2 = b.push_leaf(TapeKind::Literal, 3, 6, 1);

    // Now push the compound header that points at the run.
    let compound = b.push_compound(TapeKind::Seq, children_start, 0, 6, 0);
    assert_eq!(compound, TapeOffset(2));

    let tape = b.finish().unwrap();
    assert_eq!(tape.len(), 3);

    let rec = tape.get(compound);
    assert_eq!(rec.kind, TapeKind::Seq);
    assert!(rec.has_children());
    assert_eq!(rec.child_off, TapeOffset(0));
    assert_eq!(rec.span_lo, 0);
    assert_eq!(rec.span_hi, 6);
}

#[test]
fn cursor_accesses_record_fields() {
    let mut b = TapeBuilder::new();
    let off = b.push_leaf(TapeKind::Literal, 10, 20, 3);
    let tape = b.finish().unwrap();

    let cursor = TapeCursor::new(&tape, off);
    assert_eq!(cursor.kind(), TapeKind::Literal);
    assert_eq!(cursor.span(), (10, 20));
    assert_eq!(cursor.variant_idx(), 3);
}

#[test]
fn cursor_walks_children() {
    let mut b = TapeBuilder::new();
    let children_start = b.mark_children();
    b.push_leaf(TapeKind::Span, 0, 1, 0);
    b.push_leaf(TapeKind::Span, 1, 2, 0);
    b.push_leaf(TapeKind::Span, 2, 3, 0);
    let compound = b.push_compound(TapeKind::Seq, children_start, 0, 3, 0);

    let tape = b.finish().unwrap();
    let cursor = TapeCursor::new(&tape, compound);

    let children: Vec<TapeCursor<'_>> = cursor.children().collect();
    assert_eq!(children.len(), 3);
    assert_eq!(children[0].span(), (0, 1));
    assert_eq!(children[1].span(), (1, 2));
    assert_eq!(children[2].span(), (2, 3));
}

#[test]
fn chunked_arena_crosses_chunk_boundary() {
    // CHUNK_CAPACITY is 4096; push 5000 leaves to force a spill into
    // the second chunk and verify every record is readable by offset.
    let mut b = TapeBuilder::with_capacity(5000);
    let mut offsets = Vec::with_capacity(5000);
    for i in 0..5000u32 {
        offsets.push(b.push_leaf(TapeKind::Span, i, i + 1, 0));
    }
    let tape = b.finish().unwrap();
    assert_eq!(tape.len(), 5000);

    // Readback across the chunk boundary.
    for (i, &off) in offsets.iter().enumerate() {
        let rec = tape.get(off);
        assert_eq!(rec.span_lo, i as u32);
        assert_eq!(rec.span_hi, i as u32 + 1);
    }
}

#[test]
fn empty_tape() {
    let tape = Tape::new();
    assert!(tape.is_empty());
    assert_eq!(tape.len(), 0);
}

#[test]
fn try_get_none_sentinel() {
    let tape = Tape::new();
    assert!(tape.try_get(TapeOffset::NONE).is_none());
    assert!(tape.try_get(TapeOffset(9999)).is_none());
}

#[test]
fn flags_encode_variant_and_has_children() {
    let mut b = TapeBuilder::new();
    let leaf_off = b.push_leaf(TapeKind::Literal, 0, 4, 7);
    let compound_children = b.mark_children();
    // Empty children run is fine — compound just points past the leaf.
    let compound_off = b.push_compound(TapeKind::Rule, compound_children, 0, 4, 2);

    let tape = b.finish().unwrap();
    let leaf = tape.get(leaf_off);
    assert_eq!(leaf.variant_idx(), 7);
    assert!(!leaf.has_children());

    let compound = tape.get(compound_off);
    assert_eq!(compound.variant_idx(), 2);
    assert!(compound.has_children());
}
