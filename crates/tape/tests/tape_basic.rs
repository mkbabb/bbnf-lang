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

use tape::{
    GrammarProfile, PayloadData, Tape, TapeBuilder, TapeCursor, TapeKind, TapeOffset, TapeRec,
};

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
    let children_start = TapeOffset(b.columns().len() as u32);
    let _c1 = b.push_leaf(TapeKind::Span, 0, 3, 0, 0);
    let _c2 = b.push_leaf(TapeKind::Literal, 3, 6, 1, 0);

    // Now push the compound header that points at the run.
    let compound_off = b.begin_compound(TapeKind::Seq, 0, 0, 0, 0, 0);
    b.end_compound_post_order(compound_off, 6, children_start);
    let compound = TapeOffset(compound_off);
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
    let children_start = TapeOffset(b.columns().len() as u32);
    b.push_leaf(TapeKind::Span, 0, 1, 0, 0);
    b.push_leaf(TapeKind::Span, 1, 2, 0, 0);
    b.push_leaf(TapeKind::Span, 2, 3, 0, 0);
    let compound_off = b.begin_compound(TapeKind::Seq, 0, 0, 0, 0, 0);
    b.end_compound_post_order(compound_off, 3, children_start);
    let compound = TapeOffset(compound_off);

    // Post-order legacy-API tape — `finish()` derives frame_depth
    // from the `child_off` column and sweeps it through `finalise`
    // to close `sib_skip`. No inline frame-depth opt-in required.
    let tape = b.finish().unwrap();
    let cursor = TapeCursor::new(&tape, compound);

    // Forward iteration via `children` (Vec-backed, source order).
    let children: Vec<TapeCursor<'_>> = cursor.children().collect();
    assert_eq!(children.len(), 3);
    assert_eq!(children[0].span(), (0, 1));
    assert_eq!(children[1].span(), (1, 2));
    assert_eq!(children[2].span(), (2, 3));

    // AV.2.2: `children_zero_alloc()` post-substrate yields in
    // forward source order — the columnar substrate's sibling-skip
    // column makes forward iteration zero-allocation, so the pre-AV
    // split between `children()` (vec-backed, source order) and
    // `children_zero_alloc()` (linked-list, reverse order) collapses
    // into one forward-order zero-alloc iterator.
    let forward: Vec<TapeCursor<'_>> = cursor.children_zero_alloc().collect();
    assert_eq!(forward.len(), 3);
    assert_eq!(forward[0].span(), (0, 1));
    assert_eq!(forward[1].span(), (1, 2));
    assert_eq!(forward[2].span(), (2, 3));
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
    let compound_children = TapeOffset(b2.columns().len() as u32);
    let _inner_leaf_off = b2.push_leaf(TapeKind::Literal, 0, 4, 7, 0);
    let compound_open = b2.begin_compound(TapeKind::Rule, 0, 2, 0, 0, 0);
    b2.end_compound_post_order(compound_open, 4, compound_children);
    let compound_off = TapeOffset(compound_open);

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
    let compound_children = TapeOffset(b.columns().len() as u32);
    let compound_open = b.begin_compound(TapeKind::Rule, 0, 2, 0, 0, 0);
    b.end_compound_post_order(compound_open, 4, compound_children);
    let compound_off = TapeOffset(compound_open);
    let tape = b.finish().unwrap();
    let _ = tape.get(leaf_off);
    let compound = tape.get(compound_off);
    assert_eq!(compound.variant_idx(), 2);
    assert!(!compound.has_children(), "empty compound must clear has_children");
}

#[test]
fn empty_compound_stamps_child_off_none() {
    // AV.0.6: `push_compound` must write `TapeOffset::NONE` to
    // `child_off` when the children run is empty. A non-NONE
    // `child_off` on a compound without children would collide with
    // the payload encoding `has_children=false && child_off != NONE`
    // that identifies payload-bearing leaves, so the reader's
    // `has_payload()` would light up spuriously for empty compounds.
    let mut b = TapeBuilder::new();
    // Push a real child first so parent_idx > 0; the empty compound
    // lands after the leaf and its mark_children points at the next
    // (yet-unwritten) slot — exactly the same slot the compound will
    // occupy.
    let _ = b.push_leaf(TapeKind::Literal, 0, 4, 0, 0);
    let marked = TapeOffset(b.columns().len() as u32);
    let compound_open = b.begin_compound(TapeKind::Rule, 4, 0, 0, 0, 0);
    b.end_compound_post_order(compound_open, 4, marked);
    let compound_off = TapeOffset(compound_open);
    let tape = b.finish().unwrap();

    let rec = tape.get(compound_off);
    assert!(!rec.has_children(), "empty compound must not report children");
    assert_eq!(
        rec.child_off,
        TapeOffset::NONE,
        "empty compound must carry `child_off = NONE`"
    );
    assert!(
        !rec.has_payload(),
        "empty compound must report `has_payload() == false`"
    );
}

#[test]
fn nonempty_compound_preserves_child_off() {
    // Complement to the NONE-stamping test: a compound with at least
    // one child keeps the caller-supplied `child_off` verbatim.
    let mut b = TapeBuilder::new();
    let marked = TapeOffset(b.columns().len() as u32);
    b.push_leaf(TapeKind::Literal, 0, 4, 0, 0);
    b.push_leaf(TapeKind::Literal, 4, 8, 0, 0);
    let compound_open = b.begin_compound(TapeKind::Seq, 0, 0, 0, 0, 0);
    b.end_compound_post_order(compound_open, 8, marked);
    let compound_off = TapeOffset(compound_open);
    let tape = b.finish().unwrap();

    let rec = tape.get(compound_off);
    assert!(rec.has_children());
    assert_eq!(rec.child_off, marked);
    assert_ne!(rec.child_off, TapeOffset::NONE);
    // `has_payload()` is false for every compound — the payload
    // bit is gated on `!has_children`.
    assert!(!rec.has_payload());
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
    assert_eq!(rec.child_off, tape::TapeOffset::NONE);
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
    let children_start = TapeOffset(b.columns().len() as u32);
    b.push_leaf(TapeKind::Span, 0, 3, 0, 7);
    let compound_open = b.begin_compound(TapeKind::Rule, 0, 4, 27, 0, 0); // CSS L4 max meta_idx
    b.end_compound_post_order(compound_open, 3, children_start);
    let compound = TapeOffset(compound_open);
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
    let children_start = TapeOffset(b.columns().len() as u32);
    b.push_leaf(TapeKind::Span, 0, 1, 0, 0);
    let compound_open = b.begin_compound(TapeKind::Rule, 0, 5, 20, 0, 0);
    b.end_compound_post_order(compound_open, 1, children_start);
    let compound = TapeOffset(compound_open);
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

// ── Tranche AV Phase 1 — GrammarProfile const-evaluability ─────────

// Compile-time witness: `GrammarProfile::EMPTY` is const-evaluable.
// If any field's initialiser becomes non-const, this fails to compile
// at `const` site rather than at test run.
const _EMPTY_WITNESS: GrammarProfile = GrammarProfile::EMPTY;

// Compile-time witness: an emitter-shaped literal is const-evaluable.
// Mirrors the shape the emitter writes into `generated.rs` — every
// slice is a `&'static` reference to a `static` array, every scalar
// is a numeric literal.
static _SAMPLE_ALPHABET: [u8; 3] = [b'{', b'}', b','];
static _SAMPLE_DIGRAPHS: [(u8, u8); 1] = [(b'/', b'*')];
const _SAMPLE_PROFILE: GrammarProfile = GrammarProfile {
    compounds_per_input_byte: 0.5,
    leaves_per_input_byte: 0.25,
    parallel_break_even_bytes: 65_536,
    structural_alphabet: &_SAMPLE_ALPHABET,
    structural_digraphs: &_SAMPLE_DIGRAPHS,
    structural_digraph_mask: [0u64; 4],
    structural_quote_classes: &[],
};

#[test]
fn grammar_profile_empty_is_zero_everywhere() {
    assert!(GrammarProfile::EMPTY.structural_alphabet.is_empty());
    assert!(GrammarProfile::EMPTY.structural_digraphs.is_empty());
    assert!(GrammarProfile::EMPTY.structural_quote_classes.is_empty());
    assert_eq!(GrammarProfile::EMPTY.parallel_break_even_bytes, 0);
}

#[test]
fn grammar_profile_capacity_for_scales_with_input_len() {
    // AW-IV.W2.3.b — the formula combines the per-grammar density
    // estimate with the AR-audit floor (sonic-rs parity):
    //   `max(ceil(density * len), len / 2) + 2`
    //
    // _SAMPLE_PROFILE has compounds=0.5 + leaves=0.25 = 0.75
    // records/byte; 1024 bytes ⇒ density=768, floor=512, max=768,
    // +2 = 770. The density dominates for this profile.
    assert_eq!(_SAMPLE_PROFILE.capacity_for(1024), 770);
    assert_eq!(_SAMPLE_PROFILE.capacity_for(0), 2);
    // AR-audit floor activates when the per-grammar density is below
    // the sonic-rs baseline — the EMPTY profile has zero density so
    // the AR floor governs: `1024 / 2 + 2 = 514`.
    assert_eq!(GrammarProfile::EMPTY.capacity_for(1024), 514);
    // Dense grammars (density > 0.5) are unaffected by the floor; the
    // density-based estimate dominates.
    assert!(_SAMPLE_PROFILE.capacity_for(1024) > GrammarProfile::EMPTY.capacity_for(1024));
}

#[test]
fn grammar_profile_slices_reference_rodata() {
    // Round-trip through the const profile to confirm the compiler
    // keeps the slice references and they read back equal to the
    // underlying static arrays.
    assert_eq!(_SAMPLE_PROFILE.structural_alphabet, &[b'{', b'}', b',']);
    assert_eq!(
        _SAMPLE_PROFILE.structural_digraphs,
        &[(b'/', b'*')] as &[(u8, u8)],
    );
}

// ── Tranche AV Phase 2 — Columns SoA substrate ────────────────────

use tape::Columns;

#[test]
fn columns_struct_holds_soa_layout() {
    // Build a minimal tape and verify the six structural columns
    // grow in lockstep.
    let mut b = TapeBuilder::new();
    b.push_leaf(TapeKind::Span, 0, 1, 0, 0);
    b.push_leaf(TapeKind::Literal, 1, 2, 1, 0);
    let tape = b.finish().unwrap();

    // AY.W1.1 — flat-AoS substrate: structural rows live in
    // `records` (16 B AoS) + parallel `sib_skip`. The legacy SoA
    // accessors `cols.kinds.len()` etc. collapse into the single
    // `cols.len()` query.
    let cols = tape.columns();
    assert_eq!(cols.len(), 2);

    // Typed payload columns stay empty for pure-span leaves.
    assert_eq!(cols.pay_narrow.len(), 0);
    assert_eq!(cols.pay_wide.len(), 0);
    assert_eq!(cols.pay_agg.len(), 0);
}

#[test]
fn columns_pay_narrow_holds_inline_scalars() {
    // AV.2.3: inline scalars land in `pay_narrow`; the record's
    // `child_off` stores the column rank.
    let mut b = TapeBuilder::new();
    let off0 = b.push_leaf_with(
        TapeKind::Literal,
        0,
        1,
        0,
        0,
        PayloadData::InlineScalar(7),
    );
    let off1 = b.push_leaf_with(
        TapeKind::Literal,
        1,
        2,
        1,
        0,
        PayloadData::InlineScalar(42),
    );
    let tape = b.finish().unwrap();

    let cols = tape.columns();
    assert_eq!(cols.pay_narrow, vec![7u32, 42u32]);

    // `child_off` points to the column rank, not into the arena.
    let rec0 = tape.get(off0);
    let rec1 = tape.get(off1);
    assert_eq!(rec0.child_off, TapeOffset(0));
    assert_eq!(rec1.child_off, TapeOffset(1));

    // Readers project via the column.
    assert_eq!(tape.payload_u8(rec0), Some(7));
    assert_eq!(tape.payload_u8(rec1), Some(42));
}

#[test]
fn columns_pay_wide_holds_wide_scalars() {
    // AV.2.3: 8-byte scalars land in `pay_wide`; `child_off` stores
    // the column rank.
    let mut b = TapeBuilder::new();
    let off0 = b.push_leaf_with(
        TapeKind::Regex,
        0,
        3,
        0,
        0,
        PayloadData::WideScalar(1.5_f64.to_bits()),
    );
    let off1 = b.push_leaf_with(
        TapeKind::Regex,
        3,
        6,
        1,
        0,
        PayloadData::WideScalar((-99.0_f64).to_bits()),
    );
    let tape = b.finish().unwrap();

    let cols = tape.columns();
    assert_eq!(cols.pay_wide.len(), 2);
    assert_eq!(cols.pay_wide[0], 1.5_f64.to_bits());
    assert_eq!(cols.pay_wide[1], (-99.0_f64).to_bits());

    let rec0 = tape.get(off0);
    let rec1 = tape.get(off1);
    assert_eq!(rec0.child_off, TapeOffset(0));
    assert_eq!(rec1.child_off, TapeOffset(1));
}

#[test]
fn columns_pay_agg_holds_aggregate_and_bytes() {
    // Aggregate + Bytes payloads continue to land in the unified
    // arena (`pay_agg`). `child_off` holds the arena byte offset.
    let mut b = TapeBuilder::new();
    let agg_bytes: [u8; 9] = [1, 2, 3, 4, 5, 6, 7, 8, 9];
    let off_agg = b.push_leaf_with(
        TapeKind::KvPair,
        0,
        4,
        0,
        0,
        PayloadData::Aggregate(&agg_bytes),
    );
    let off_bytes = b.push_leaf_with(
        TapeKind::Span,
        4,
        9,
        1,
        0,
        PayloadData::Bytes(b"hello"),
    );
    let tape = b.finish().unwrap();

    let cols = tape.columns();
    assert!(cols.pay_agg.len() >= 9 + 4 + 5, "arena stores aggregate + bytes frame");

    let rec_agg = tape.get(off_agg);
    let rec_bytes = tape.get(off_bytes);
    // The aggregate lands at offset 0; bytes frame at 16 (aggregate
    // pads to 16 bytes = 2 slots, then the bytes frame starts).
    assert_eq!(rec_agg.child_off.0, 0);
    assert_eq!(rec_bytes.child_off.0, 16);
    assert_eq!(tape.payload_string(rec_bytes), Some("hello"));
}

#[test]
fn sibling_skip_walks_direct_children_forward() {
    // Build `(a (b c) d)` and verify sib_skip drives forward sibling
    // iteration through the outer compound's three children.
    let mut b = TapeBuilder::new();
    // a
    b.push_leaf(TapeKind::Span, 0, 1, 0, 0);
    // (b c)
    let bc_children = TapeOffset(b.columns().len() as u32);
    b.push_leaf(TapeKind::Span, 1, 2, 1, 0);
    b.push_leaf(TapeKind::Span, 2, 3, 2, 0);
    let bc_open = b.begin_compound(TapeKind::Seq, 1, 0, 0, 0, 0);
    b.end_compound_post_order(bc_open, 3, bc_children);
    let _bc = TapeOffset(bc_open);
    // d
    b.push_leaf(TapeKind::Span, 3, 4, 3, 0);
    // outer compound (children at 0, 3, 4)
    let outer_children = TapeOffset(0);
    let outer_open = b.begin_compound(TapeKind::Rule, 0, 0, 0, 0, 0);
    b.end_compound_post_order(outer_open, 4, outer_children);
    let outer = TapeOffset(outer_open);
    // Post-order legacy tape — `finish()` derives frame_depth from
    // `child_off` and closes `sib_skip` via `finalise`.
    let tape = b.finish().unwrap();

    let cursor = TapeCursor::new(&tape, outer);

    // Forward iteration yields children in source order.
    let children: Vec<_> = cursor.children().collect();
    assert_eq!(children.len(), 3);
    assert_eq!(children[0].span(), (0, 1)); // a
    assert_eq!(children[1].span(), (1, 3)); // (b c)
    assert_eq!(children[2].span(), (3, 4)); // d

    // child(i) uses sibling-skip stepping.
    assert_eq!(cursor.child(0).unwrap().span(), (0, 1));
    assert_eq!(cursor.child(1).unwrap().span(), (1, 3));
    assert_eq!(cursor.child(2).unwrap().span(), (3, 4));
    assert!(cursor.child(3).is_none());

    // sib_skip values at each child root:
    //   child@0 (a, leaf): next sibling (b c) at offset 3, skip = 3
    //   child@3 ((b c)):   next sibling d at offset 4, skip = 1
    //   child@4 (d):       last sibling, skip = 0
    let cols = tape.columns();
    assert_eq!(cols.sib_skip_at(0), 3);
    assert_eq!(cols.sib_skip_at(3), 1);
    assert_eq!(cols.sib_skip_at(4), 0);
}

#[test]
fn sibling_skip_nested_compound() {
    // Build `(x y)` nested inside `(z (x y) w)`; verify sib_skip
    // inside the nested compound.
    let mut b = TapeBuilder::new();
    // z
    b.push_leaf(TapeKind::Span, 0, 1, 0, 0);
    // (x y)
    let xy_children = TapeOffset(b.columns().len() as u32);
    b.push_leaf(TapeKind::Span, 1, 2, 1, 0);
    b.push_leaf(TapeKind::Span, 2, 3, 2, 0);
    let xy_open = b.begin_compound(TapeKind::Seq, 1, 0, 0, 0, 0);
    b.end_compound_post_order(xy_open, 3, xy_children);
    let xy = TapeOffset(xy_open);
    // w
    b.push_leaf(TapeKind::Span, 3, 4, 3, 0);
    let outer_children = TapeOffset(0);
    let outer_open = b.begin_compound(TapeKind::Rule, 0, 0, 0, 0, 0);
    b.end_compound_post_order(outer_open, 4, outer_children);
    let _outer = TapeOffset(outer_open);
    // Post-order legacy tape — `finish()` closes `sib_skip` via the
    // derived-depth Stage-C pass.
    let tape = b.finish().unwrap();

    // Inner (x y) children: x@1, y@2. sib_skip[1]=1, sib_skip[2]=0.
    let cols = tape.columns();
    assert_eq!(cols.sib_skip_at(1), 1);
    assert_eq!(cols.sib_skip_at(2), 0);

    // Walker yields x then y in forward order.
    let xy_cursor = TapeCursor::new(&tape, xy);
    let inner: Vec<_> = xy_cursor.children().collect();
    assert_eq!(inner.len(), 2);
    assert_eq!(inner[0].span(), (1, 2));
    assert_eq!(inner[1].span(), (2, 3));
}

#[test]
fn empty_compound_sibling_skip_is_zero() {
    // An empty compound's `sib_skip` stays at the default `0`
    // because there are no direct children to enumerate.
    let mut b = TapeBuilder::new();
    let marked = TapeOffset(b.columns().len() as u32);
    let empty_open = b.begin_compound(TapeKind::Rule, 0, 0, 0, 0, 0);
    b.end_compound_post_order(empty_open, 0, marked);
    let empty = TapeOffset(empty_open);
    let tape = b.finish().unwrap();
    assert_eq!(tape.columns().sib_skip_at(empty.0), 0);
    assert_eq!(TapeCursor::new(&tape, empty).child_count(), 0);
}

#[test]
fn tape_iter_materialises_all_records() {
    // The tape iterator yields each record exactly once in push
    // order.
    let mut b = TapeBuilder::new();
    b.push_leaf(TapeKind::Span, 0, 1, 0, 0);
    b.push_leaf(TapeKind::Literal, 1, 2, 1, 0);
    b.push_leaf(TapeKind::Regex, 2, 3, 2, 0);
    let tape = b.finish().unwrap();
    let recs: Vec<_> = tape.iter().collect();
    assert_eq!(recs.len(), 3);
    assert_eq!(recs[0].kind(), TapeKind::Span);
    assert_eq!(recs[1].kind(), TapeKind::Literal);
    assert_eq!(recs[2].kind(), TapeKind::Regex);
}

#[test]
fn payload_data_variant_coverage() {
    // Round-trip every PayloadData variant through the columnar
    // substrate.
    let mut b = TapeBuilder::new();
    let off_none = b.push_leaf_with(TapeKind::Span, 0, 1, 0, 0, PayloadData::None);
    let off_inline = b.push_leaf_with(
        TapeKind::Literal,
        1,
        2,
        0,
        0,
        PayloadData::InlineScalar(0xDEAD_BEEF),
    );
    let off_wide = b.push_leaf_with(
        TapeKind::Regex,
        2,
        3,
        0,
        0,
        PayloadData::WideScalar(0x0123_4567_89AB_CDEF),
    );
    let agg = [1u8, 2, 3, 4, 5];
    let off_agg = b.push_leaf_with(
        TapeKind::KvPair,
        3,
        4,
        0,
        0,
        PayloadData::Aggregate(&agg),
    );
    let large: [u8; 33] = core::array::from_fn(|i| (i as u8).wrapping_add(1));
    let off_large = b.push_leaf_with(
        TapeKind::KvPair,
        4,
        5,
        0,
        0,
        PayloadData::LargeAggregate(&large),
    );
    let off_bytes = b.push_leaf_with(
        TapeKind::Span,
        5,
        12,
        0,
        0,
        PayloadData::Bytes(b"bonjour"),
    );
    let tape = b.finish().unwrap();

    // None
    assert!(!tape.get(off_none).has_payload());

    // InlineScalar → pay_narrow
    let rec_inline = tape.get(off_inline);
    assert!(rec_inline.has_payload());
    assert_eq!(tape.payload_u32(rec_inline), Some(0xDEAD_BEEF));

    // WideScalar → pay_wide
    let rec_wide = tape.get(off_wide);
    assert!(rec_wide.has_payload());
    assert_eq!(tape.payload_u64(rec_wide), Some(0x0123_4567_89AB_CDEF));

    // Aggregate → pay_agg
    let rec_agg = tape.get(off_agg);
    assert_eq!(tape.payload_bytes(rec_agg, 5), Some(&agg[..]));

    // LargeAggregate → pay_agg
    let rec_large = tape.get(off_large);
    assert_eq!(tape.payload_bytes(rec_large, 33), Some(&large[..]));

    // Bytes → pay_agg framed
    let rec_bytes = tape.get(off_bytes);
    assert_eq!(tape.payload_string(rec_bytes), Some("bonjour"));
}

#[test]
fn inline_scalar_u32_max_does_not_collide_with_none() {
    // AV.2.3: InlineScalar routes to pay_narrow, so `u32::MAX`
    // inline values no longer collide with the `TapeOffset::NONE`
    // sentinel. Pre-AV this debug-asserted; post-AV it must round-
    // trip cleanly.
    let mut b = TapeBuilder::new();
    let off = b.push_leaf_with(
        TapeKind::Literal,
        0,
        4,
        0,
        0,
        PayloadData::InlineScalar(u32::MAX),
    );
    let tape = b.finish().unwrap();
    let rec = tape.get(off);
    assert!(rec.has_payload(), "u32::MAX inline does not collide with NONE sentinel");
    assert_eq!(tape.payload_u32(rec), Some(u32::MAX));
}

#[test]
fn columns_direct_access_for_bulk_visitors() {
    // V2.5's reordered-unrolling codegen will consume
    // `tape.columns().pay_wide` as a dense `&[u64]` (reinterpretable
    // to `&[f64]` via bit pattern). Verify the typed column is in
    // push order.
    let mut b = TapeBuilder::new();
    for i in 0..8 {
        b.push_leaf_with(
            TapeKind::Span,
            i as u32,
            (i + 1) as u32,
            0,
            0,
            PayloadData::WideScalar((i as f64 * 0.5).to_bits()),
        );
    }
    let tape = b.finish().unwrap();
    let cols = tape.columns();
    assert_eq!(cols.pay_wide.len(), 8);
    // Dense f64 view recovered via bit cast per element.
    let sum: f64 = cols
        .pay_wide
        .iter()
        .map(|b| f64::from_bits(*b))
        .sum();
    let expected: f64 = (0..8).map(|i| i as f64 * 0.5).sum();
    assert!((sum - expected).abs() < f64::EPSILON);
}

#[test]
fn column_rank_default() {
    use tape::ColumnRank;
    let r = ColumnRank::default();
    assert_eq!(r.pay_narrow, 0);
    assert_eq!(r.pay_wide, 0);
    assert_eq!(r.pay_agg, 0);
}

#[test]
fn cursor_with_rank_preserves_rank() {
    use tape::ColumnRank;
    let mut b = TapeBuilder::new();
    let off = b.push_leaf(TapeKind::Span, 0, 1, 0, 0);
    let tape = b.finish().unwrap();

    let rank = ColumnRank {
        pay_narrow: 5,
        pay_wide: 3,
        pay_agg: 1,
    };
    let cursor = TapeCursor::with_rank(&tape, off, rank);
    assert_eq!(cursor.rank().pay_narrow, 5);
    assert_eq!(cursor.rank().pay_wide, 3);
    assert_eq!(cursor.rank().pay_agg, 1);
}

// Compile-time witness: `Columns` can be constructed at `const` time
// via `Vec::new()`. Useful for static fixtures and tests.
const _COLUMNS_DEFAULT_WITNESS: fn() -> Columns = Columns::new;

// ── Tranche AV Phase 4 — PSI stream + stage-B fill ────────────────
//
// Verifies the AV.4.1 PayloadJob types and AV.4.2 stage-B rayon
// payload fill. The stream is a sidecar to the tape (stage A emits
// PayloadJobs alongside structural records); stage B drains the
// stream into the typed payload columns. The fingerprint gate
// `parallel_break_even_bytes` decides sequential vs. parallel
// dispatch. Both paths must produce identical column state — that
// is the regression invariant the tests pin.

#[test]
fn payload_job_size_and_alignment() {
    use tape::PayloadJob;
    // AW-III.W1 widened `column_idx: u8` to `arena_offset: u32` so
    // every job lands at a unique byte offset in `pay_agg`. Total
    // size is 20 bytes; each cache line holds 3 jobs.
    assert_eq!(std::mem::size_of::<PayloadJob>(), 20);
    assert_eq!(std::mem::align_of::<PayloadJob>(), 4);
}

#[test]
fn payload_kind_arena_widths() {
    use tape::PayloadKind;
    // AW-III.W1: every kind serialises into `pay_agg` at the
    // documented little-endian width. String / AggregateLarge are
    // variable-width (driven by the matched input slice), reported
    // as `0`.
    assert_eq!(PayloadKind::U8.arena_byte_width(), 1);
    assert_eq!(PayloadKind::Bool.arena_byte_width(), 1);
    assert_eq!(PayloadKind::HexU32.arena_byte_width(), 4);
    assert_eq!(PayloadKind::F64.arena_byte_width(), 8);
    assert_eq!(PayloadKind::I64.arena_byte_width(), 8);
    assert_eq!(PayloadKind::String.arena_byte_width(), 0);
    assert_eq!(PayloadKind::AggregateLarge.arena_byte_width(), 0);
    assert_eq!(PayloadKind::COUNT, 7);
    assert_eq!(PayloadKind::from_u8(0), Some(PayloadKind::F64));
    assert_eq!(PayloadKind::from_u8(6), Some(PayloadKind::AggregateLarge));
    assert_eq!(PayloadKind::from_u8(7), None);
}
