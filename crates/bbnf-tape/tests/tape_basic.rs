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

    // Compound with at least one child — mark the child pointer
    // BEFORE pushing the leaf so the compound's child run is
    // non-empty. `push_compound` clears `has_children` for empty
    // runs (fixes the parent-as-own-child cycle in `TapeCursor`).
    let mut b2 = TapeBuilder::new();
    let compound_children = b2.mark_children();
    let _inner_leaf_off = b2.push_leaf(TapeKind::Literal, 0, 4, 7);
    let compound_off = b2.push_compound(TapeKind::Rule, compound_children, 0, 4, 2);

    let tape = b.finish().unwrap();
    let leaf = tape.get(leaf_off);
    assert_eq!(leaf.variant_idx(), 7);
    assert!(!leaf.has_children());

    let tape2 = b2.finish().unwrap();
    let compound = tape2.get(compound_off);
    assert_eq!(compound.variant_idx(), 2);
    assert!(compound.has_children());
}

#[test]
fn empty_compound_clears_has_children() {
    // Regression guard for the `push_compound` empty-run fix:
    // a compound whose child mark/finalize points at the same
    // record slot must not advertise `has_children` — otherwise
    // `TapeCursor::children` follows `child_off` back to the
    // parent and recurses forever.
    let mut b = TapeBuilder::new();
    let leaf_off = b.push_leaf(TapeKind::Literal, 0, 4, 7);
    let compound_children = b.mark_children();
    let compound_off = b.push_compound(TapeKind::Rule, compound_children, 0, 4, 2);
    let tape = b.finish().unwrap();
    let _ = tape.get(leaf_off);
    let compound = tape.get(compound_off);
    assert_eq!(compound.variant_idx(), 2);
    assert!(!compound.has_children(), "empty compound must clear has_children");
}

// ── Payload round-trip tests (AM.2) ──────────────────────────────

#[test]
fn payload_f64_round_trip() {
    let mut b = TapeBuilder::new();
    let off = b.push_leaf_with_f64(TapeKind::Regex, 0, 5, 0, std::f64::consts::PI);
    let tape = b.finish().unwrap();

    let rec = tape.get(off);
    assert_eq!(rec.kind, TapeKind::Regex);
    assert_ne!(rec.payload_idx, 0, "payload_idx must be non-zero");
    let val = tape.payload_f64(rec).expect("should read f64 payload");
    assert!((val - std::f64::consts::PI).abs() < f64::EPSILON);
}

#[test]
fn payload_bool_round_trip() {
    let mut b = TapeBuilder::new();
    let off_t = b.push_leaf_with_bool(TapeKind::Literal, 0, 4, 0, true);
    let off_f = b.push_leaf_with_bool(TapeKind::Literal, 4, 9, 1, false);
    let tape = b.finish().unwrap();

    assert_eq!(tape.payload_bool(tape.get(off_t)), Some(true));
    assert_eq!(tape.payload_bool(tape.get(off_f)), Some(false));
}

#[test]
fn payload_u8_round_trip() {
    let mut b = TapeBuilder::new();
    let off = b.push_leaf_with_u8(TapeKind::Literal, 0, 2, 3, 42);
    let tape = b.finish().unwrap();

    assert_eq!(tape.payload_u8(tape.get(off)), Some(42));
}

#[test]
fn payload_idx_zero_returns_none() {
    let mut b = TapeBuilder::new();
    let off = b.push_leaf(TapeKind::Span, 0, 5, 0);
    let tape = b.finish().unwrap();
    let rec = tape.get(off);

    assert_eq!(rec.payload_idx, 0);
    assert!(tape.payload_f64(rec).is_none());
    assert!(tape.payload_bool(rec).is_none());
    assert!(tape.payload_u8(rec).is_none());
}

#[test]
fn multiple_payloads_independent() {
    let mut b = TapeBuilder::new();
    let off1 = b.push_leaf_with_f64(TapeKind::Regex, 0, 3, 0, 1.5);
    let off2 = b.push_leaf_with_f64(TapeKind::Regex, 3, 6, 1, -99.0);
    let off3 = b.push_leaf_with_u8(TapeKind::Literal, 6, 8, 2, 255);
    let off_plain = b.push_leaf(TapeKind::Span, 8, 10, 0);
    let tape = b.finish().unwrap();

    let v1 = tape.payload_f64(tape.get(off1)).unwrap();
    let v2 = tape.payload_f64(tape.get(off2)).unwrap();
    let v3 = tape.payload_u8(tape.get(off3)).unwrap();

    assert!((v1 - 1.5).abs() < f64::EPSILON);
    assert!((v2 - (-99.0)).abs() < f64::EPSILON);
    assert_eq!(v3, 255);
    assert!(tape.payload_f64(tape.get(off_plain)).is_none());
}
