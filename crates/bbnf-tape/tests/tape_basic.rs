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
    let off = b.push_leaf(TapeKind::Span, 0, 5, 0, 0);
    assert_eq!(off, TapeOffset(0));

    let tape = b.finish().unwrap();
    assert_eq!(tape.len(), 1);

    let rec = tape.get(off);
    assert_eq!(rec.kind(), TapeKind::Span);
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
    let _c1 = b.push_leaf(TapeKind::Span, 0, 3, 0, 0);
    let _c2 = b.push_leaf(TapeKind::Literal, 3, 6, 1, 0);

    // Now push the compound header that points at the run.
    let compound = b.push_compound(TapeKind::Seq, children_start, 0, 6, 0, 0);
    assert_eq!(compound, TapeOffset(2));

    let tape = b.finish().unwrap();
    assert_eq!(tape.len(), 3);

    let rec = tape.get(compound);
    assert_eq!(rec.kind(), TapeKind::Seq);
    assert!(rec.has_children());
    assert_eq!(rec.child_off, TapeOffset(0));
    assert_eq!(rec.span_lo, 0);
    assert_eq!(rec.span_hi, 6);
}

#[test]
fn cursor_accesses_record_fields() {
    let mut b = TapeBuilder::new();
    let off = b.push_leaf(TapeKind::Literal, 10, 20, 3, 0);
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
    b.push_leaf(TapeKind::Span, 0, 1, 0, 0);
    b.push_leaf(TapeKind::Span, 1, 2, 0, 0);
    b.push_leaf(TapeKind::Span, 2, 3, 0, 0);
    let compound = b.push_compound(TapeKind::Seq, children_start, 0, 3, 0, 0);

    let tape = b.finish().unwrap();
    let cursor = TapeCursor::new(&tape, compound);

    // Forward iteration via `children` (Vec-backed, source order).
    let children: Vec<TapeCursor<'_>> = cursor.children().collect();
    assert_eq!(children.len(), 3);
    assert_eq!(children[0].span(), (0, 1));
    assert_eq!(children[1].span(), (1, 2));
    assert_eq!(children[2].span(), (2, 3));

    // AU.3.2: zero-alloc `children_zero_alloc()` yields in reverse
    // source order. `size_of::<ChildIter>` ≤ 24 bytes; no heap
    // allocation per call.
    let rev: Vec<TapeCursor<'_>> = cursor.children_zero_alloc().collect();
    assert_eq!(rev.len(), 3);
    assert_eq!(rev[0].span(), (2, 3));
    assert_eq!(rev[1].span(), (1, 2));
    assert_eq!(rev[2].span(), (0, 1));
}

#[test]
fn chunked_arena_crosses_chunk_boundary() {
    // CHUNK_CAPACITY is 4096; push 5000 leaves to force a spill into
    // the second chunk and verify every record is readable by offset.
    let mut b = TapeBuilder::with_capacity(5000);
    let mut offsets = Vec::with_capacity(5000);
    for i in 0..5000u32 {
        offsets.push(b.push_leaf(TapeKind::Span, i, i + 1, 0, 0));
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
    let leaf_off = b.push_leaf(TapeKind::Literal, 0, 4, 7, 0);

    // Compound with at least one child — mark the child pointer
    // BEFORE pushing the leaf so the compound's child run is
    // non-empty. `push_compound` clears `has_children` for empty
    // runs (fixes the parent-as-own-child cycle in `TapeCursor`).
    let mut b2 = TapeBuilder::new();
    let compound_children = b2.mark_children();
    let _inner_leaf_off = b2.push_leaf(TapeKind::Literal, 0, 4, 7, 0);
    let compound_off = b2.push_compound(TapeKind::Rule, compound_children, 0, 4, 2, 0);

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
    let leaf_off = b.push_leaf(TapeKind::Literal, 0, 4, 7, 0);
    let compound_children = b.mark_children();
    let compound_off = b.push_compound(TapeKind::Rule, compound_children, 0, 4, 2, 0);
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
    let off = b.push_leaf_with_f64(TapeKind::Regex, 0, 5, 0, 0, std::f64::consts::PI);
    let tape = b.finish().unwrap();

    let rec = tape.get(off);
    assert_eq!(rec.kind(), TapeKind::Regex);
    assert_ne!(rec.payload_idx, 0, "payload_idx must be non-zero");
    let val = tape.payload_f64(rec).expect("should read f64 payload");
    assert!((val - std::f64::consts::PI).abs() < f64::EPSILON);
}

#[test]
fn payload_bool_round_trip() {
    let mut b = TapeBuilder::new();
    let off_t = b.push_leaf_with_bool(TapeKind::Literal, 0, 4, 0, 0, true);
    let off_f = b.push_leaf_with_bool(TapeKind::Literal, 4, 9, 1, 0, false);
    let tape = b.finish().unwrap();

    assert_eq!(tape.payload_bool(tape.get(off_t)), Some(true));
    assert_eq!(tape.payload_bool(tape.get(off_f)), Some(false));
}

#[test]
fn payload_u8_round_trip() {
    let mut b = TapeBuilder::new();
    let off = b.push_leaf_with_u8(TapeKind::Literal, 0, 2, 3, 0, 42);
    let tape = b.finish().unwrap();

    assert_eq!(tape.payload_u8(tape.get(off)), Some(42));
}

#[test]
fn payload_idx_zero_returns_none() {
    let mut b = TapeBuilder::new();
    let off = b.push_leaf(TapeKind::Span, 0, 5, 0, 0);
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
    let off1 = b.push_leaf_with_f64(TapeKind::Regex, 0, 3, 0, 0, 1.5);
    let off2 = b.push_leaf_with_f64(TapeKind::Regex, 3, 6, 1, 0, -99.0);
    let off3 = b.push_leaf_with_u8(TapeKind::Literal, 6, 8, 2, 0, 255);
    let off_plain = b.push_leaf(TapeKind::Span, 8, 10, 0, 0);
    let tape = b.finish().unwrap();

    let v1 = tape.payload_f64(tape.get(off1)).unwrap();
    let v2 = tape.payload_f64(tape.get(off2)).unwrap();
    let v3 = tape.payload_u8(tape.get(off3)).unwrap();

    assert!((v1 - 1.5).abs() < f64::EPSILON);
    assert!((v2 - (-99.0)).abs() < f64::EPSILON);
    assert_eq!(v3, 255);
    assert!(tape.payload_f64(tape.get(off_plain)).is_none());
}

// ── AQ.6.A: extended scalar suite round-trips ────────────────────

#[test]
fn payload_i8_round_trip() {
    let mut b = TapeBuilder::new();
    let off_min = b.push_leaf_with_i8(TapeKind::Literal, 0, 4, 0, 0, i8::MIN);
    let off_max = b.push_leaf_with_i8(TapeKind::Literal, 4, 8, 1, 0, i8::MAX);
    let off_neg = b.push_leaf_with_i8(TapeKind::Literal, 8, 12, 2, 0, -7);
    let tape = b.finish().unwrap();
    assert_eq!(tape.payload_i8(tape.get(off_min)), Some(i8::MIN));
    assert_eq!(tape.payload_i8(tape.get(off_max)), Some(i8::MAX));
    assert_eq!(tape.payload_i8(tape.get(off_neg)), Some(-7));
}

#[test]
fn payload_i16_u16_round_trip() {
    let mut b = TapeBuilder::new();
    let off_i = b.push_leaf_with_i16(TapeKind::Literal, 0, 4, 0, 0, -32_000);
    let off_u = b.push_leaf_with_u16(TapeKind::Literal, 4, 8, 1, 0, 60_000);
    let tape = b.finish().unwrap();
    assert_eq!(tape.payload_i16(tape.get(off_i)), Some(-32_000));
    assert_eq!(tape.payload_u16(tape.get(off_u)), Some(60_000));
}

#[test]
fn payload_i32_u32_round_trip() {
    let mut b = TapeBuilder::new();
    let off_i = b.push_leaf_with_i32(TapeKind::Literal, 0, 4, 0, 0, i32::MIN + 1);
    let off_u = b.push_leaf_with_u32(TapeKind::Literal, 4, 8, 1, 0, u32::MAX);
    let tape = b.finish().unwrap();
    assert_eq!(tape.payload_i32(tape.get(off_i)), Some(i32::MIN + 1));
    assert_eq!(tape.payload_u32(tape.get(off_u)), Some(u32::MAX));
}

#[test]
fn payload_i64_u64_round_trip() {
    let mut b = TapeBuilder::new();
    let off_i = b.push_leaf_with_i64(TapeKind::Literal, 0, 4, 0, 0, i64::MIN);
    let off_u = b.push_leaf_with_u64(TapeKind::Literal, 4, 8, 1, 0, u64::MAX);
    let tape = b.finish().unwrap();
    assert_eq!(tape.payload_i64(tape.get(off_i)), Some(i64::MIN));
    assert_eq!(tape.payload_u64(tape.get(off_u)), Some(u64::MAX));
}

#[test]
fn payload_scalar_generic_round_trip() {
    // Direct exercise of the generic write/read pair that the
    // specialized wrappers delegate to.
    let mut b = TapeBuilder::new();
    let off = b.push_leaf_with_scalar::<u32>(TapeKind::Literal, 0, 4, 0, 0, 0xDEAD_BEEF);
    let tape = b.finish().unwrap();
    assert_eq!(tape.payload_scalar::<u32>(tape.get(off)), Some(0xDEAD_BEEF));
}

// ── AR.1.1: meta_idx side-channel round-trip ─────────────────────

#[test]
fn meta_idx_round_trip_leaf() {
    let mut b = TapeBuilder::new();
    let off0 = b.push_leaf(TapeKind::Span, 0, 3, 1, 0);
    let off1 = b.push_leaf(TapeKind::Literal, 3, 6, 2, 5);
    let off2 = b.push_leaf(TapeKind::Span, 6, 9, 3, 31); // 31 = max 5-bit meta_idx
    let tape = b.finish().unwrap();

    let c0 = TapeCursor::new(&tape, off0);
    let c1 = TapeCursor::new(&tape, off1);
    let c2 = TapeCursor::new(&tape, off2);

    assert_eq!(c0.meta_idx(), 0);
    assert_eq!(c1.meta_idx(), 5);
    assert_eq!(c2.meta_idx(), 31);
    // variant_idx is independent of meta_idx.
    assert_eq!(c0.variant_idx(), 1);
    assert_eq!(c1.variant_idx(), 2);
    assert_eq!(c2.variant_idx(), 3);
}

#[test]
fn meta_idx_round_trip_compound() {
    let mut b = TapeBuilder::new();
    let children_start = b.mark_children();
    b.push_leaf(TapeKind::Span, 0, 3, 0, 7);
    let compound = b.push_compound(TapeKind::Rule, children_start, 0, 3, 4, 27); // CSS L4 max
    let tape = b.finish().unwrap();

    let cursor = TapeCursor::new(&tape, compound);
    assert_eq!(cursor.meta_idx(), 27);
    assert_eq!(cursor.variant_idx(), 4);

    // Child's meta_idx is also correct.
    let child = cursor.child(0).unwrap();
    assert_eq!(child.meta_idx(), 7);
}

#[test]
fn meta_idx_round_trip_payload_leaf() {
    let mut b = TapeBuilder::new();
    let off = b.push_leaf_with_f64(TapeKind::Regex, 0, 5, 0, 3, 2.718);
    let tape = b.finish().unwrap();

    let cursor = TapeCursor::new(&tape, off);
    assert_eq!(cursor.meta_idx(), 3);
    let val = tape.payload_f64(cursor.record()).unwrap();
    assert!((val - 2.718).abs() < f64::EPSILON);
}

#[test]
fn meta_idx_default_zero_for_plain_pushes() {
    // When callers pass meta_idx=0, the read-back is 0.
    let mut b = TapeBuilder::new();
    let off = b.push_leaf(TapeKind::Span, 0, 1, 0, 0);
    let tape = b.finish().unwrap();
    assert_eq!(TapeCursor::new(&tape, off).meta_idx(), 0);
}

// ── AT.2.2: packed meta_idx boundary tests ──────────────────────

#[test]
fn meta_idx_all_5bit_values_round_trip() {
    // Exhaustively verify every encodable meta_idx (0-31).
    let mut b = TapeBuilder::new();
    let mut offsets = Vec::new();
    for m in 0..=TapeRec::MAX_META_IDX {
        offsets.push(b.push_leaf(TapeKind::Span, 0, 1, 0, m));
    }
    let tape = b.finish().unwrap();
    for (m, &off) in offsets.iter().enumerate() {
        let rec = tape.get(off);
        assert_eq!(rec.meta_idx(), m as u8, "meta_idx mismatch at {}", m);
        assert_eq!(rec.kind(), TapeKind::Span, "kind mismatch at meta_idx={}", m);
    }
}

#[test]
fn meta_idx_15_16_boundary() {
    // The 4th bit of meta_idx spills into flags[7]. Verify the
    // boundary between 15 (fits in kind_meta alone) and 16 (needs
    // the flags overflow bit).
    let mut b = TapeBuilder::new();
    let off_15 = b.push_leaf(TapeKind::Literal, 0, 1, 10, 15);
    let off_16 = b.push_leaf(TapeKind::Literal, 1, 2, 10, 16);
    let tape = b.finish().unwrap();

    let r15 = tape.get(off_15);
    assert_eq!(r15.meta_idx(), 15);
    assert_eq!(r15.variant_idx(), 10);
    assert_eq!(r15.kind(), TapeKind::Literal);
    assert!(!r15.has_children());

    let r16 = tape.get(off_16);
    assert_eq!(r16.meta_idx(), 16);
    assert_eq!(r16.variant_idx(), 10);
    assert_eq!(r16.kind(), TapeKind::Literal);
    assert!(!r16.has_children());
}

#[test]
fn meta_idx_and_has_children_coexist() {
    // Verify that has_children (flags bit 6) and meta_idx bit 4
    // (flags bit 7) do not interfere with each other.
    let mut b = TapeBuilder::new();
    let children_start = b.mark_children();
    b.push_leaf(TapeKind::Span, 0, 1, 0, 0);
    let compound = b.push_compound(TapeKind::Rule, children_start, 0, 1, 5, 20);
    let tape = b.finish().unwrap();

    let rec = tape.get(compound);
    assert_eq!(rec.meta_idx(), 20);
    assert_eq!(rec.variant_idx(), 5);
    assert!(rec.has_children());
    assert_eq!(rec.kind(), TapeKind::Rule);
}

#[test]
fn meta_idx_max_value_with_all_kinds() {
    // Max meta_idx (31) with every TapeKind to ensure packing
    // does not corrupt the kind discriminant.
    let kinds = [
        TapeKind::Span, TapeKind::Epsilon, TapeKind::Literal,
        TapeKind::Regex, TapeKind::KvPair,
    ];
    for &kind in &kinds {
        let mut b = TapeBuilder::new();
        let off = b.push_leaf(kind, 0, 1, 0, TapeRec::MAX_META_IDX);
        let tape = b.finish().unwrap();
        let rec = tape.get(off);
        assert_eq!(rec.kind(), kind, "kind mismatch for {:?} with max meta_idx", kind);
        assert_eq!(rec.meta_idx(), TapeRec::MAX_META_IDX);
    }
}
