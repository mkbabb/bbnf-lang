//! Tranche AA.13 — basic round-trip tests for the tape crate.
//!
//! Verifies:
//! 1. `TapeRec` size is 16 bytes (compile-time assert already
//!    enforces this; this test is a runtime-visible proof).
//! 2. `TapeBuilder::push_leaf` / `push_compound` append records in
//!    insertion order with stable offsets.
//! 3. `TapeCursor::record` / `kind` / `span` / `child` round-trip.
//! 4. Flat Vec storage holds arbitrary record counts without data
//!    corruption.
//! 5. AU.6.7 — unified `PayloadData` dispatches the four payload
//!    shapes through `push_leaf_with`: inline scalars pack into
//!    `child_off`; wide scalars, aggregates, and byte strings use
//!    the shared arena.

use bbnf_tape::{PayloadData, Tape, TapeBuilder, TapeCursor, TapeKind, TapeOffset, TapeRec};

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
    assert!(!rec.has_payload());
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
fn large_tape_round_trip() {
    // Push 5000 leaves and verify every record is readable by offset.
    let mut b = TapeBuilder::with_capacity(5000);
    let mut offsets = Vec::with_capacity(5000);
    for i in 0..5000u32 {
        offsets.push(b.push_leaf(TapeKind::Span, i, i + 1, 0, 0));
    }
    let tape = b.finish().unwrap();
    assert_eq!(tape.len(), 5000);

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

// ── AU.6.7: unified PayloadData round-trips ─────────────────────

#[test]
fn payload_f64_round_trip() {
    let mut b = TapeBuilder::new();
    let off = b.push_leaf_with(
        TapeKind::Regex,
        0,
        5,
        0,
        0,
        PayloadData::WideScalar(std::f64::consts::PI.to_bits()),
    );
    let tape = b.finish().unwrap();

    let rec = tape.get(off);
    assert_eq!(rec.kind(), TapeKind::Regex);
    assert!(rec.has_payload(), "record must advertise a payload");
    let val = tape.payload_f64(rec).expect("should read f64 payload");
    assert!((val - std::f64::consts::PI).abs() < f64::EPSILON);
}

#[test]
fn payload_bool_round_trip() {
    let mut b = TapeBuilder::new();
    let off_t = b.push_leaf_with(
        TapeKind::Literal,
        0,
        4,
        0,
        0,
        PayloadData::InlineScalar(1),
    );
    let off_f = b.push_leaf_with(
        TapeKind::Literal,
        4,
        9,
        1,
        0,
        PayloadData::InlineScalar(0),
    );
    let tape = b.finish().unwrap();

    assert_eq!(tape.payload_bool(tape.get(off_t)), Some(true));
    assert_eq!(tape.payload_bool(tape.get(off_f)), Some(false));
}

#[test]
fn payload_u8_round_trip() {
    let mut b = TapeBuilder::new();
    let off = b.push_leaf_with(
        TapeKind::Literal,
        0,
        2,
        3,
        0,
        PayloadData::InlineScalar(42),
    );
    let tape = b.finish().unwrap();

    assert_eq!(tape.payload_u8(tape.get(off)), Some(42));
}

#[test]
fn payload_absent_returns_none() {
    let mut b = TapeBuilder::new();
    let off = b.push_leaf(TapeKind::Span, 0, 5, 0, 0);
    let tape = b.finish().unwrap();
    let rec = tape.get(off);

    assert!(rec.child_off.is_none());
    assert!(!rec.has_payload());
    assert!(tape.payload_f64(rec).is_none());
    assert!(tape.payload_bool(rec).is_none());
    assert!(tape.payload_u8(rec).is_none());
    assert!(tape.payload_string(rec).is_none());
}

#[test]
fn multiple_payloads_independent() {
    let mut b = TapeBuilder::new();
    let off1 = b.push_leaf_with(
        TapeKind::Regex,
        0,
        3,
        0,
        0,
        PayloadData::WideScalar(1.5_f64.to_bits()),
    );
    let off2 = b.push_leaf_with(
        TapeKind::Regex,
        3,
        6,
        1,
        0,
        PayloadData::WideScalar((-99.0_f64).to_bits()),
    );
    let off3 = b.push_leaf_with(
        TapeKind::Literal,
        6,
        8,
        2,
        0,
        PayloadData::InlineScalar(255),
    );
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

#[test]
fn payload_i8_round_trip() {
    let mut b = TapeBuilder::new();
    let off_min = b.push_leaf_with(
        TapeKind::Literal,
        0,
        4,
        0,
        0,
        PayloadData::InlineScalar(u32::from_le_bytes([i8::MIN as u8, 0, 0, 0])),
    );
    let off_max = b.push_leaf_with(
        TapeKind::Literal,
        4,
        8,
        1,
        0,
        PayloadData::InlineScalar(u32::from_le_bytes([i8::MAX as u8, 0, 0, 0])),
    );
    let off_neg = b.push_leaf_with(
        TapeKind::Literal,
        8,
        12,
        2,
        0,
        PayloadData::InlineScalar(u32::from_le_bytes([(-7i8) as u8, 0, 0, 0])),
    );
    let tape = b.finish().unwrap();
    assert_eq!(tape.payload_i8(tape.get(off_min)), Some(i8::MIN));
    assert_eq!(tape.payload_i8(tape.get(off_max)), Some(i8::MAX));
    assert_eq!(tape.payload_i8(tape.get(off_neg)), Some(-7));
}

#[test]
fn payload_i16_u16_round_trip() {
    let mut b = TapeBuilder::new();
    let off_i = b.push_leaf_with(
        TapeKind::Literal,
        0,
        4,
        0,
        0,
        PayloadData::InlineScalar(u32::from_le_bytes({
            let bytes = (-32_000_i16).to_le_bytes();
            [bytes[0], bytes[1], 0, 0]
        })),
    );
    let off_u = b.push_leaf_with(
        TapeKind::Literal,
        4,
        8,
        1,
        0,
        PayloadData::InlineScalar(u32::from_le_bytes({
            let bytes = 60_000_u16.to_le_bytes();
            [bytes[0], bytes[1], 0, 0]
        })),
    );
    let tape = b.finish().unwrap();
    assert_eq!(tape.payload_i16(tape.get(off_i)), Some(-32_000));
    assert_eq!(tape.payload_u16(tape.get(off_u)), Some(60_000));
}

#[test]
fn payload_i32_u32_round_trip() {
    let mut b = TapeBuilder::new();
    let off_i = b.push_leaf_with(
        TapeKind::Literal,
        0,
        4,
        0,
        0,
        PayloadData::InlineScalar((i32::MIN + 1) as u32),
    );
    // u32::MAX collides with the TapeOffset::NONE sentinel; route
    // u32 values that could be u32::MAX through WideScalar. Here
    // we use a smaller u32 value to exercise the inline path; the
    // sentinel-collision debug_assert is tested via payload_wide.
    let off_u = b.push_leaf_with(
        TapeKind::Literal,
        4,
        8,
        1,
        0,
        PayloadData::InlineScalar(u32::MAX - 1),
    );
    let tape = b.finish().unwrap();
    assert_eq!(tape.payload_i32(tape.get(off_i)), Some(i32::MIN + 1));
    assert_eq!(tape.payload_u32(tape.get(off_u)), Some(u32::MAX - 1));
}

#[test]
fn payload_i64_u64_round_trip() {
    let mut b = TapeBuilder::new();
    let off_i = b.push_leaf_with(
        TapeKind::Literal,
        0,
        4,
        0,
        0,
        PayloadData::WideScalar(i64::MIN as u64),
    );
    let off_u = b.push_leaf_with(
        TapeKind::Literal,
        4,
        8,
        1,
        0,
        PayloadData::WideScalar(u64::MAX),
    );
    let tape = b.finish().unwrap();
    assert_eq!(tape.payload_i64(tape.get(off_i)), Some(i64::MIN));
    assert_eq!(tape.payload_u64(tape.get(off_u)), Some(u64::MAX));
}

#[test]
fn payload_scalar_generic_round_trip() {
    let mut b = TapeBuilder::new();
    let off = b.push_leaf_with(
        TapeKind::Literal,
        0,
        4,
        0,
        0,
        PayloadData::InlineScalar(0xDEAD_BEEF_u32),
    );
    let tape = b.finish().unwrap();
    assert_eq!(tape.payload_scalar::<u32>(tape.get(off)), Some(0xDEAD_BEEF));
}

#[test]
fn payload_aggregate_round_trip() {
    // Pack a 9-byte (f64, u8) aggregate like CSS dimensions.
    let mut bytes = [0u8; 9];
    bytes[..8].copy_from_slice(&1.5_f64.to_le_bytes());
    bytes[8] = 7;

    let mut b = TapeBuilder::new();
    let off = b.push_leaf_with(
        TapeKind::Span,
        0,
        4,
        0,
        0,
        PayloadData::Aggregate(&bytes),
    );
    let tape = b.finish().unwrap();
    let rec = tape.get(off);
    let slice = tape.payload_bytes(rec, 9).expect("aggregate payload");
    let f = f64::from_le_bytes(<[u8; 8]>::try_from(&slice[0..8]).unwrap());
    let u = slice[8];
    assert!((f - 1.5).abs() < f64::EPSILON);
    assert_eq!(u, 7);
}

// ── AV.0.5: LargeAggregate (> 16 B arena-backed) round-trips ────

#[test]
fn payload_large_aggregate_round_trip() {
    // Shape: CSS `colorFunction` — u8 space + f64×3 channels + f64
    // alpha. Packed with natural scalar alignment (u8 at 0, 7 bytes
    // pad, three f64 at 8/16/24, alpha f64 at 32 → 40-byte slot).
    const CHANNELS: [f64; 3] = [255.0, 128.0, 0.0];
    const ALPHA: f64 = 0.5;
    let space: u8 = 1; // rgba discriminant

    let mut bytes = [0u8; 40];
    bytes[0] = space;
    bytes[8..16].copy_from_slice(&CHANNELS[0].to_le_bytes());
    bytes[16..24].copy_from_slice(&CHANNELS[1].to_le_bytes());
    bytes[24..32].copy_from_slice(&CHANNELS[2].to_le_bytes());
    bytes[32..40].copy_from_slice(&ALPHA.to_le_bytes());

    let mut b = TapeBuilder::new();
    let off = b.push_leaf_with(
        TapeKind::KvPair,
        0,
        24,
        1,
        0,
        PayloadData::LargeAggregate(&bytes),
    );
    let tape = b.finish().unwrap();
    let rec = tape.get(off);
    assert!(
        rec.has_payload(),
        "LargeAggregate record must advertise a payload"
    );

    // Read through the tape-level accessor (width known to caller).
    let slice = tape
        .payload_bytes(rec, 40)
        .expect("LargeAggregate payload bytes");
    assert_eq!(slice.len(), 40);
    assert_eq!(slice[0], space);
    let c0 = f64::from_le_bytes(<[u8; 8]>::try_from(&slice[8..16]).unwrap());
    let c1 = f64::from_le_bytes(<[u8; 8]>::try_from(&slice[16..24]).unwrap());
    let c2 = f64::from_le_bytes(<[u8; 8]>::try_from(&slice[24..32]).unwrap());
    let a = f64::from_le_bytes(<[u8; 8]>::try_from(&slice[32..40]).unwrap());
    assert_eq!(c0, CHANNELS[0]);
    assert_eq!(c1, CHANNELS[1]);
    assert_eq!(c2, CHANNELS[2]);
    assert!((a - ALPHA).abs() < f64::EPSILON);

    // Same read via the cursor alias, which forwards to `payload_bytes`.
    let cursor = TapeCursor::new(&tape, off);
    let alias = cursor
        .payload_aggregate_bytes(40)
        .expect("cursor forwarder");
    assert_eq!(alias, slice);
}

#[test]
fn payload_large_aggregate_empty_is_none() {
    // Empty `LargeAggregate` must not allocate an arena slot — the
    // record stores `TapeOffset::NONE` in `child_off` and reports
    // no payload, symmetric with the empty-`Aggregate` path.
    let mut b = TapeBuilder::new();
    let empty: [u8; 0] = [];
    let off = b.push_leaf_with(
        TapeKind::KvPair,
        0,
        0,
        0,
        0,
        PayloadData::LargeAggregate(&empty),
    );
    let tape = b.finish().unwrap();
    let rec = tape.get(off);
    assert_eq!(rec.child_off, bbnf_tape::TapeOffset::NONE);
    assert!(!rec.has_payload());
}

#[test]
fn payload_large_aggregate_slot_padding_is_zero() {
    // 33 bytes round up to 40 bytes (five 8-byte slots); the 7
    // trailing pad bytes must be zero-initialised so the payload is
    // deterministic across builds.
    let bytes: [u8; 33] = core::array::from_fn(|i| i as u8);
    let mut b = TapeBuilder::new();
    let off = b.push_leaf_with(
        TapeKind::KvPair,
        0,
        10,
        0,
        0,
        PayloadData::LargeAggregate(&bytes),
    );
    let tape = b.finish().unwrap();
    let rec = tape.get(off);
    let slot = tape.payload_bytes(rec, 40).expect("padded slot");
    assert_eq!(&slot[..33], &bytes[..]);
    assert!(
        slot[33..].iter().all(|b| *b == 0),
        "tail pad must be zero: {:?}",
        &slot[33..]
    );
}

#[test]
fn payload_large_aggregate_multiple_records_independent() {
    // Two records with different widths; each arena slot must be
    // independently addressed by its record's `child_off`.
    let a: [u8; 33] = [0x11; 33];
    let b_bytes: [u8; 48] = [0x22; 48];

    let mut builder = TapeBuilder::new();
    let off_a = builder.push_leaf_with(
        TapeKind::KvPair,
        0,
        5,
        0,
        0,
        PayloadData::LargeAggregate(&a),
    );
    let off_b = builder.push_leaf_with(
        TapeKind::KvPair,
        5,
        10,
        1,
        0,
        PayloadData::LargeAggregate(&b_bytes),
    );
    let tape = builder.finish().unwrap();

    let slice_a = tape.payload_bytes(tape.get(off_a), 33).unwrap();
    let slice_b = tape.payload_bytes(tape.get(off_b), 48).unwrap();
    assert_eq!(slice_a, &a[..]);
    assert_eq!(slice_b, &b_bytes[..]);
}

#[test]
fn payload_bytes_round_trip() {
    let mut b = TapeBuilder::new();
    let off = b.push_leaf_with(
        TapeKind::Span,
        0,
        5,
        0,
        0,
        PayloadData::Bytes(b"hello"),
    );
    let tape = b.finish().unwrap();
    let rec = tape.get(off);
    assert_eq!(tape.payload_string(rec), Some("hello"));
    assert_eq!(tape.payload_string_bytes(rec), Some(&b"hello"[..]));
}

#[test]
fn payload_span_round_trip() {
    let mut b = TapeBuilder::new();
    let packed = (10u64) | ((42u64) << 32);
    let off = b.push_leaf_with(
        TapeKind::Span,
        0,
        50,
        0,
        0,
        PayloadData::WideScalar(packed),
    );
    let tape = b.finish().unwrap();
    let rec = tape.get(off);
    assert_eq!(tape.payload_Span(rec), Some((10, 42)));
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
    let off = b.push_leaf_with(
        TapeKind::Regex,
        0,
        5,
        0,
        3,
        PayloadData::WideScalar(2.718_f64.to_bits()),
    );
    let tape = b.finish().unwrap();

    let cursor = TapeCursor::new(&tape, off);
    assert_eq!(cursor.meta_idx(), 3);
    let val = tape.payload_f64(cursor.record()).unwrap();
    assert!((val - 2.718).abs() < f64::EPSILON);
}

#[test]
fn meta_idx_default_zero_for_plain_pushes() {
    let mut b = TapeBuilder::new();
    let off = b.push_leaf(TapeKind::Span, 0, 1, 0, 0);
    let tape = b.finish().unwrap();
    assert_eq!(TapeCursor::new(&tape, off).meta_idx(), 0);
}

// ── AT.2.2: packed meta_idx boundary tests ──────────────────────

#[test]
fn meta_idx_all_5bit_values_round_trip() {
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
