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

use bbnf_tape::{
    GrammarProfile, PayloadData, RuleId, Tape, TapeBuilder, TapeCursor, TapeKind, TapeOffset,
    TapeRec,
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

    // Cursor sibling walks read `sib_skip`, which Stage-C derives
    // from `child_off` + `frame_depth`. AW.0.1 gates Stage-C on the
    // DTA's inline-frame-depth flag; opt in so the legacy-API-built
    // tape is closed before the cursor reads it.
    b.enable_inline_frame_depth();
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
    let marked = b.mark_children();
    let compound_off = b.push_compound(TapeKind::Rule, marked, 4, 4, 0, 0);
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
    let marked = b.mark_children();
    b.push_leaf(TapeKind::Literal, 0, 4, 0, 0);
    b.push_leaf(TapeKind::Literal, 4, 8, 0, 0);
    let compound_off = b.push_compound(TapeKind::Seq, marked, 0, 8, 0, 0);
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
static _SAMPLE_DIGRAPHS: [[u8; 2]; 1] = [[b'/', b'*']];
static _SAMPLE_LIST_RULES: [RuleId; 2] = [RuleId(0), RuleId(7)];
const _SAMPLE_PROFILE: GrammarProfile = GrammarProfile {
    push_compound_count: 7,
    push_leaf_count: 2,
    push_leaf_with_count: 3,
    compounds_per_input_byte: 0.5,
    leaves_per_input_byte: 0.25,
    payload_bytes_per_input_byte: 0.125,
    expected_ns_per_byte: 5.0,
    parallel_break_even_bytes: 65_536,
    structural_alphabet: &_SAMPLE_ALPHABET,
    structural_digraphs: &_SAMPLE_DIGRAPHS,
    active_columns: &[],
    list_rules: &_SAMPLE_LIST_RULES,
    keyword_tables: &[],
    shape_dict: &[],
    branch_priors: &[],
    dedup_eligible_rules: &[],
    reorder_unroll_visitors: &[],
};

#[test]
fn grammar_profile_empty_is_zero_everywhere() {
    assert_eq!(GrammarProfile::EMPTY.total_push_sites(), 0);
    assert!(GrammarProfile::EMPTY.structural_alphabet.is_empty());
    assert!(GrammarProfile::EMPTY.structural_digraphs.is_empty());
    assert!(GrammarProfile::EMPTY.active_columns.is_empty());
    assert!(GrammarProfile::EMPTY.list_rules.is_empty());
    assert!(GrammarProfile::EMPTY.keyword_tables.is_empty());
    assert!(GrammarProfile::EMPTY.shape_dict.is_empty());
    assert!(GrammarProfile::EMPTY.branch_priors.is_empty());
    assert!(GrammarProfile::EMPTY.dedup_eligible_rules.is_empty());
    assert!(GrammarProfile::EMPTY.reorder_unroll_visitors.is_empty());
}

#[test]
fn grammar_profile_total_push_sites_sums_three_counts() {
    assert_eq!(_SAMPLE_PROFILE.total_push_sites(), 12);
}

#[test]
fn grammar_profile_capacity_for_scales_with_input_len() {
    // 0.5 + 0.25 = 0.75 records/byte; 1024 bytes ⇒ 768 records + 2
    assert_eq!(_SAMPLE_PROFILE.capacity_for(1024), 770);
    assert_eq!(_SAMPLE_PROFILE.capacity_for(0), 2);
}

#[test]
fn grammar_profile_slices_reference_rodata() {
    // Round-trip through the const profile to confirm the compiler
    // keeps the slice references and they read back equal to the
    // underlying static arrays.
    assert_eq!(_SAMPLE_PROFILE.structural_alphabet, &[b'{', b'}', b',']);
    assert_eq!(
        _SAMPLE_PROFILE.structural_digraphs,
        &[[b'/', b'*']] as &[[u8; 2]],
    );
    assert_eq!(_SAMPLE_PROFILE.list_rules.len(), 2);
    assert_eq!(_SAMPLE_PROFILE.list_rules[0].0, 0);
    assert_eq!(_SAMPLE_PROFILE.list_rules[1].0, 7);
}

// ── Tranche AV Phase 2 — Columns SoA substrate ────────────────────

use bbnf_tape::Columns;

#[test]
fn columns_struct_holds_soa_layout() {
    // Build a minimal tape and verify the six structural columns
    // grow in lockstep.
    let mut b = TapeBuilder::new();
    b.push_leaf(TapeKind::Span, 0, 1, 0, 0);
    b.push_leaf(TapeKind::Literal, 1, 2, 1, 0);
    let tape = b.finish().unwrap();

    let cols = tape.columns();
    assert_eq!(cols.len(), 2);
    assert_eq!(cols.kinds.len(), 2);
    assert_eq!(cols.flags.len(), 2);
    assert_eq!(cols.extra.len(), 2);
    assert_eq!(cols.span_lo.len(), 2);
    assert_eq!(cols.span_hi.len(), 2);
    assert_eq!(cols.sib_skip.len(), 2);
    assert_eq!(cols.child_off.len(), 2);

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
    let bc_children = b.mark_children();
    b.push_leaf(TapeKind::Span, 1, 2, 1, 0);
    b.push_leaf(TapeKind::Span, 2, 3, 2, 0);
    let _bc = b.push_compound(TapeKind::Seq, bc_children, 1, 3, 0, 0);
    // d
    b.push_leaf(TapeKind::Span, 3, 4, 3, 0);
    // outer compound (children at 0, 3, 4)
    let outer_children = TapeOffset(0);
    let outer = b.push_compound(TapeKind::Rule, outer_children, 0, 4, 0, 0);
    // AW.0.1: Stage-C populates `sib_skip`, which the assertions below
    // and the cursor's forward walk both depend on. Opt in.
    b.enable_inline_frame_depth();
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
    let xy_children = b.mark_children();
    b.push_leaf(TapeKind::Span, 1, 2, 1, 0);
    b.push_leaf(TapeKind::Span, 2, 3, 2, 0);
    let xy = b.push_compound(TapeKind::Seq, xy_children, 1, 3, 0, 0);
    // w
    b.push_leaf(TapeKind::Span, 3, 4, 3, 0);
    let outer_children = TapeOffset(0);
    let _outer = b.push_compound(TapeKind::Rule, outer_children, 0, 4, 0, 0);
    // AW.0.1: `sib_skip` is a Stage-C output; opt in to close it.
    b.enable_inline_frame_depth();
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
    let marked = b.mark_children();
    let empty = b.push_compound(TapeKind::Rule, marked, 0, 0, 0, 0);
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
    use bbnf_tape::ColumnRank;
    let r = ColumnRank::default();
    assert_eq!(r.pay_narrow, 0);
    assert_eq!(r.pay_wide, 0);
    assert_eq!(r.pay_agg, 0);
}

#[test]
fn cursor_with_rank_preserves_rank() {
    use bbnf_tape::ColumnRank;
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
    use bbnf_tape::PayloadJob;
    // 16 bytes = 4 jobs per 64 B cache line. Drives the rayon chunk
    // stride (`PayloadStream::CHUNK_RECS`).
    assert_eq!(std::mem::size_of::<PayloadJob>(), 16);
    assert_eq!(std::mem::align_of::<PayloadJob>(), 4);
}

#[test]
fn payload_kind_routing_helpers() {
    use bbnf_tape::PayloadKind;
    assert!(PayloadKind::U8.is_narrow());
    assert!(PayloadKind::Bool.is_narrow());
    assert!(PayloadKind::HexU32.is_narrow());
    assert!(PayloadKind::F64.is_wide());
    assert!(PayloadKind::I64.is_wide());
    assert!(PayloadKind::String.is_arena());
    assert!(PayloadKind::AggregateLarge.is_arena());
    assert!(!PayloadKind::F64.is_narrow());
    assert!(!PayloadKind::U8.is_wide());
    assert!(!PayloadKind::Bool.is_arena());
    assert_eq!(PayloadKind::COUNT, 7);
    assert_eq!(PayloadKind::from_u8(0), Some(PayloadKind::F64));
    assert_eq!(PayloadKind::from_u8(6), Some(PayloadKind::AggregateLarge));
    assert_eq!(PayloadKind::from_u8(7), None);
}

#[test]
fn payload_stream_capacity_from_profile() {
    use bbnf_tape::{GrammarProfile, PayloadStream};
    let mut profile = GrammarProfile::EMPTY;
    profile.leaves_per_input_byte = 0.05; // 5% of input bytes
    let stream = PayloadStream::with_capacity_for(&profile, 10_000);
    // The stream is empty on construction; capacity is the profile-
    // sized hint so subsequent pushes don't grow the Vec.
    assert!(stream.is_empty());
    assert_eq!(stream.len(), 0);
    // Capacity is observable via the underlying jobs() slice's
    // capacity through Vec semantics; we verify by pushing up to
    // estimate without expecting reallocation.
    let mut stream = stream;
    for i in 0..500u32 {
        stream.push(bbnf_tape::PayloadJob::new(
            i,
            i,
            i + 1,
            bbnf_tape::PayloadKind::U8,
            i as u8,
        ));
    }
    assert_eq!(stream.len(), 500);
}

#[test]
fn payload_stream_sequential_fill_round_trip() {
    use bbnf_tape::{Columns, GrammarProfile, PayloadJob, PayloadKind, PayloadStream};
    // Stage-A round-trip: build a PSI with two narrow scalars and
    // one wide scalar, drain into a fresh Columns, verify the slots.
    let input = b"42 17 3.14";
    let mut psi = PayloadStream::new();
    psi.push(PayloadJob::new(0, 0, 2, PayloadKind::U8, 0));
    psi.push(PayloadJob::new(1, 3, 5, PayloadKind::U8, 1));
    psi.push(PayloadJob::new(2, 6, 10, PayloadKind::F64, 0));

    let mut columns = Columns::new();
    let profile = GrammarProfile::EMPTY; // parallel_break_even_bytes = 0 → sequential
    let count = psi.fill_columns(input, &mut columns, &profile);
    assert_eq!(count, 3);
    // U8 takes the first byte of each slice.
    assert_eq!(columns.pay_narrow.len(), 2);
    assert_eq!(columns.pay_narrow[0], b'4' as u32);
    assert_eq!(columns.pay_narrow[1], b'1' as u32);
    // F64 round-trips bit-equivalent.
    assert_eq!(columns.pay_wide.len(), 1);
    assert!((f64::from_bits(columns.pay_wide[0]) - 3.14).abs() < f64::EPSILON);
}

#[test]
fn payload_stream_parallel_fill_matches_sequential() {
    use bbnf_tape::{Columns, GrammarProfile, PayloadJob, PayloadKind, PayloadStream};
    // Build a stream large enough to clear the rayon chunk threshold
    // and exercise both paths. Sequential and parallel must produce
    // bit-identical column state — that's the AV.4.2 invariant.
    let input: Vec<u8> = (0..1024u32)
        .flat_map(|i| {
            let s = format!("{:04} ", i % 10000);
            s.into_bytes()
        })
        .collect();
    let mut psi = PayloadStream::new();
    for i in 0..1024u32 {
        let lo = i * 5;
        let hi = lo + 4;
        let kind = match i % 4 {
            0 => PayloadKind::U8,
            1 => PayloadKind::F64,
            2 => PayloadKind::HexU32,
            _ => PayloadKind::I64,
        };
        let column_idx = (i / 4) as u8; // unique per kind
        psi.push(PayloadJob::new(i, lo, hi, kind, column_idx));
    }

    // Sequential — break-even = 0 → forced sequential.
    let mut seq_columns = Columns::new();
    let seq_profile = GrammarProfile::EMPTY;
    psi.fill_columns(&input, &mut seq_columns, &seq_profile);

    // Parallel — break-even small, input large, jobs ≥ 2 chunks.
    let mut par_columns = Columns::new();
    let mut par_profile = GrammarProfile::EMPTY;
    par_profile.parallel_break_even_bytes = 64;
    assert!(psi.should_parallelise(&par_profile, input.len()));
    psi.fill_columns(&input, &mut par_columns, &par_profile);

    assert_eq!(seq_columns.pay_narrow, par_columns.pay_narrow);
    assert_eq!(seq_columns.pay_wide, par_columns.pay_wide);
    assert_eq!(seq_columns.pay_agg, par_columns.pay_agg);
}

#[test]
fn payload_stream_parallel_threshold_gates() {
    use bbnf_tape::{GrammarProfile, PayloadJob, PayloadKind, PayloadStream};
    let mut psi = PayloadStream::new();
    for i in 0..16u32 {
        psi.push(PayloadJob::new(i, i, i + 1, PayloadKind::U8, i as u8));
    }
    // (1) `parallel_break_even_bytes == 0` always falls to sequential.
    let mut p = GrammarProfile::EMPTY;
    p.parallel_break_even_bytes = 0;
    assert!(!psi.should_parallelise(&p, usize::MAX));
    // (2) Below the threshold → sequential.
    p.parallel_break_even_bytes = 1024;
    assert!(!psi.should_parallelise(&p, 512));
    // (3) Above the threshold + enough jobs → parallel.
    p.parallel_break_even_bytes = 512;
    assert!(psi.should_parallelise(&p, 1024));
    // (4) Above the threshold but too few jobs → sequential.
    let small = PayloadStream::new();
    assert!(!small.should_parallelise(&p, 1024));
}

#[test]
fn payload_stream_arena_payload_round_trip() {
    use bbnf_tape::{Columns, GrammarProfile, PayloadJob, PayloadKind, PayloadStream};
    // Arena kinds (String, AggregateLarge) write `slice` into
    // `pay_agg[column_idx..column_idx+slice.len()]`. Verify byte
    // layout for both kinds.
    let input = b"hello world AABBCCDD";
    let mut psi = PayloadStream::new();
    psi.push(PayloadJob::new(0, 0, 5, PayloadKind::String, 0));
    psi.push(PayloadJob::new(1, 6, 11, PayloadKind::String, 5));
    psi.push(PayloadJob::new(
        2,
        12,
        20,
        PayloadKind::AggregateLarge,
        16,
    ));

    let mut columns = Columns::new();
    let profile = GrammarProfile::EMPTY;
    psi.fill_columns(input, &mut columns, &profile);
    assert!(columns.pay_agg.len() >= 24);
    assert_eq!(&columns.pay_agg[0..5], b"hello");
    assert_eq!(&columns.pay_agg[5..10], b"world");
    assert_eq!(&columns.pay_agg[16..24], b"AABBCCDD");
}

#[test]
fn payload_stream_hex_color_round_trip() {
    use bbnf_tape::{Columns, GrammarProfile, PayloadJob, PayloadKind, PayloadStream};
    let input = b"#ff0080 #abcdef12";
    let mut psi = PayloadStream::new();
    psi.push(PayloadJob::new(0, 0, 7, PayloadKind::HexU32, 0));
    psi.push(PayloadJob::new(1, 8, 17, PayloadKind::HexU32, 1));
    let mut columns = Columns::new();
    let profile = GrammarProfile::EMPTY;
    psi.fill_columns(input, &mut columns, &profile);
    // #ff0080 → 0xff0080ff (alpha defaulted)
    assert_eq!(columns.pay_narrow[0], 0xff_00_80_ff);
    // #abcdef12 → 0xabcdef12 (alpha given)
    assert_eq!(columns.pay_narrow[1], 0xab_cd_ef_12);
}

#[test]
fn payload_stream_bool_round_trip() {
    use bbnf_tape::{Columns, GrammarProfile, PayloadJob, PayloadKind, PayloadStream};
    let input = b"true false";
    let mut psi = PayloadStream::new();
    psi.push(PayloadJob::new(0, 0, 4, PayloadKind::Bool, 0));
    psi.push(PayloadJob::new(1, 5, 10, PayloadKind::Bool, 1));
    let mut columns = Columns::new();
    let profile = GrammarProfile::EMPTY;
    psi.fill_columns(input, &mut columns, &profile);
    assert_eq!(columns.pay_narrow[0], 1);
    assert_eq!(columns.pay_narrow[1], 0);
}

#[test]
fn payload_stream_i64_round_trip() {
    use bbnf_tape::{Columns, GrammarProfile, PayloadJob, PayloadKind, PayloadStream};
    let input = b"-9223372036854775808 9223372036854775807";
    let mut psi = PayloadStream::new();
    psi.push(PayloadJob::new(0, 0, 20, PayloadKind::I64, 0));
    psi.push(PayloadJob::new(1, 21, 40, PayloadKind::I64, 1));
    let mut columns = Columns::new();
    let profile = GrammarProfile::EMPTY;
    psi.fill_columns(input, &mut columns, &profile);
    assert_eq!(columns.pay_wide[0] as i64, i64::MIN);
    assert_eq!(columns.pay_wide[1] as i64, i64::MAX);
}

#[test]
fn payload_job_input_len_helper() {
    use bbnf_tape::{PayloadJob, PayloadKind};
    let job = PayloadJob::new(0, 100, 110, PayloadKind::F64, 0);
    assert_eq!(job.input_len(), 10);
    assert_eq!(job.kind, PayloadKind::F64);
    assert_eq!(job._pad, [0, 0]);
}

#[test]
fn payload_stream_chunk_recs_matches_cache_line() {
    use bbnf_tape::{PayloadJob, PayloadStream};
    // 4 jobs per 64 B cache line — the stride drives the rayon chunk
    // size so each worker owns a cache-coherent run on the read side.
    assert_eq!(
        PayloadStream::CHUNK_RECS,
        64 / std::mem::size_of::<PayloadJob>(),
    );
    assert_eq!(PayloadStream::CHUNK_RECS, 4);
}

// ── Tranche AV.4.4 — Stage-C finaliser bit-equality regression ─────
//
// `TapeBuilder::finish` routes through the Stage-C segmented prefix
// scan (`bbnf_tape::finaliser::finalise`) when
// `has_inline_frame_depth` is set; these tests pin the invariant
// that the scan's output matches a reference backward-walk
// implementation byte-for-byte on canonical post-order tapes.
//
// The reference implementation lives in this test file so the
// regression is a self-contained black-box check against the public
// API, with no dependence on crate-internal helpers.

use bbnf_tape::{derive_frame_depth, finalise, TapeOffset as Off};

/// Reference backward-walk sibling-skip computation.
///
/// Walks every compound's child run by following `child_off` pointers,
/// collecting direct-child roots in reverse emission order, and
/// stamping `next_root - this_root` into each non-last sibling's
/// slot. Operates on a `(child_off, has_children, sib_skip)` shape
/// extracted from a [`Columns`] snapshot.
fn reference_v2_sibling_skip(
    parent_count: usize,
    child_off: &[Off],
    has_children: &[bool],
) -> Vec<u32> {
    let mut sib_skip = vec![0u32; parent_count];
    for parent_idx in 0..parent_count as u32 {
        if !has_children[parent_idx as usize] {
            continue;
        }
        let child_start = child_off[parent_idx as usize];
        if child_start.is_none() {
            continue;
        }
        let start = child_start.0 as usize;
        let end = parent_idx as usize;
        if start >= end {
            continue;
        }
        let mut roots: Vec<u32> = Vec::new();
        let mut pos = end;
        while pos > start {
            let co = pos - 1;
            roots.push(co as u32);
            let co_has_children = has_children[co];
            let co_child_off = child_off[co];
            pos = if co_has_children && !co_child_off.is_none() {
                co_child_off.0 as usize
            } else {
                co
            };
        }
        for window in roots.windows(2) {
            let later = window[0];
            let earlier = window[1];
            sib_skip[earlier as usize] = later - earlier;
        }
    }
    sib_skip
}

/// Snapshot the structural shape of a finished tape's columns into
/// the small structural arrays the reference V2 implementation
/// consumes. Captures only the fields V2 reads — `child_off` and the
/// `has_children` bit per record — so the reference run is decoupled
/// from any other column movement.
fn snapshot_shape(cols: &Columns) -> (usize, Vec<Off>, Vec<bool>) {
    let n = cols.len();
    let child_off = cols.child_off.clone();
    let has_children: Vec<bool> = (0..n as u32).map(|i| cols.has_children_at(i)).collect();
    (n, child_off, has_children)
}

/// Construct a finished tape via the public `TapeBuilder` API and
/// assert that Stage-C's `sib_skip` column is bit-identical to the
/// reference V2 backward-walk implementation.
///
/// The reference V2 walk reads `child_off` to enumerate sibling
/// roots, so the snapshot is taken from the in-progress builder via
/// [`TapeBuilder::tape_snapshot`] *before* `finish()` runs Stage-C.
/// `tape_snapshot` clones columns without mutating `child_off`,
/// preserving the exact `child_off` values the parser wrote — which
/// is the input shape both algorithms were designed to consume.
fn assert_stage_c_matches_v2(mut b: TapeBuilder, label: &str) {
    let pre_snapshot = b.tape_snapshot();
    let (n, child_off, has_children) = snapshot_shape(pre_snapshot.columns());
    let v2 = reference_v2_sibling_skip(n, &child_off, &has_children);

    // The Stage-C bit-equality regression *is* the DTA-driven closure
    // path; AW.0.1 gates Stage-C on `has_inline_frame_depth`, so opt
    // in explicitly here. The reference V2 walk above still runs
    // against the pre-finalise column snapshot, keeping the
    // comparison honest.
    b.enable_inline_frame_depth();
    let tape = b.finish().unwrap();
    assert_eq!(
        tape.columns().sib_skip, v2,
        "Stage-C / V2 sib_skip mismatch on `{}`",
        label,
    );
}

#[test]
fn stage_c_matches_v2_flat_sibling_run() {
    // Three sibling leaves under a single root.
    let mut b = TapeBuilder::new();
    let cs = b.mark_children();
    b.push_leaf(TapeKind::Span, 0, 1, 0, 0);
    b.push_leaf(TapeKind::Span, 1, 2, 0, 0);
    b.push_leaf(TapeKind::Span, 2, 3, 0, 0);
    b.push_compound(TapeKind::Rule, cs, 0, 3, 0, 0);
    assert_stage_c_matches_v2(b, "flat_sibling_run");
}

#[test]
fn stage_c_matches_v2_nested_compound() {
    // (a (b c) d) outer Rule, mirroring `sibling_skip_walks_direct_children_forward`.
    let mut b = TapeBuilder::new();
    b.push_leaf(TapeKind::Span, 0, 1, 0, 0);
    let bc = b.mark_children();
    b.push_leaf(TapeKind::Span, 1, 2, 1, 0);
    b.push_leaf(TapeKind::Span, 2, 3, 2, 0);
    b.push_compound(TapeKind::Seq, bc, 1, 3, 0, 0);
    b.push_leaf(TapeKind::Span, 3, 4, 3, 0);
    let outer_children = TapeOffset(0);
    b.push_compound(TapeKind::Rule, outer_children, 0, 4, 0, 0);
    assert_stage_c_matches_v2(b, "nested_compound");
}

#[test]
fn stage_c_matches_v2_two_nested_siblings() {
    // (a (b) (c)) — two compound siblings, each with one inner leaf.
    // Exercises the per-depth `next_at_depth` invalidation when the
    // backward walk crosses the parent boundary between (b) and (c).
    let mut b = TapeBuilder::new();
    b.push_leaf(TapeKind::Span, 0, 1, 0, 0);
    let bc = b.mark_children();
    b.push_leaf(TapeKind::Span, 1, 2, 1, 0);
    b.push_compound(TapeKind::Seq, bc, 1, 2, 0, 0);
    let cc = b.mark_children();
    b.push_leaf(TapeKind::Span, 2, 3, 2, 0);
    b.push_compound(TapeKind::Seq, cc, 2, 3, 1, 0);
    let outer_children = TapeOffset(0);
    b.push_compound(TapeKind::Rule, outer_children, 0, 3, 0, 0);
    assert_stage_c_matches_v2(b, "two_nested_siblings");
}

#[test]
fn stage_c_matches_v2_deep_nesting() {
    // Deep right-spine ((((leaf))))) — exercises `tracked_depth`
    // high-water mark and the per-depth scratch growth.
    let mut b = TapeBuilder::new();
    let l1_children = b.mark_children();
    let l2_children = b.mark_children();
    let l3_children = b.mark_children();
    let l4_children = b.mark_children();
    b.push_leaf(TapeKind::Span, 0, 1, 0, 0);
    b.push_compound(TapeKind::Seq, l4_children, 0, 1, 0, 0);
    b.push_compound(TapeKind::Seq, l3_children, 0, 1, 0, 0);
    b.push_compound(TapeKind::Seq, l2_children, 0, 1, 0, 0);
    b.push_compound(TapeKind::Rule, l1_children, 0, 1, 0, 0);
    assert_stage_c_matches_v2(b, "deep_nesting");
}

#[test]
fn stage_c_matches_v2_empty_compound() {
    // An empty compound + a sibling leaf — empty compound's
    // `child_off` lands as NONE, skipping the child enumeration.
    let mut b = TapeBuilder::new();
    let leaf_off = b.push_leaf(TapeKind::Span, 0, 1, 0, 0);
    let _ = leaf_off;
    let empty_children = b.mark_children();
    b.push_compound(TapeKind::Seq, empty_children, 1, 1, 0, 0);
    b.push_leaf(TapeKind::Span, 1, 2, 0, 0);
    let outer_children = TapeOffset(0);
    b.push_compound(TapeKind::Rule, outer_children, 0, 2, 0, 0);
    assert_stage_c_matches_v2(b, "empty_compound");
}

#[test]
fn stage_c_matches_v2_wide_sibling_run() {
    // A wider sibling run (16 leaves) under a single root — checks
    // the linear scan against a non-trivial number of `sib_skip`
    // stamps in one frame.
    let mut b = TapeBuilder::new();
    let cs = b.mark_children();
    for i in 0..16u32 {
        b.push_leaf(TapeKind::Span, i, i + 1, 0, 0);
    }
    b.push_compound(TapeKind::Rule, cs, 0, 16, 0, 0);
    assert_stage_c_matches_v2(b, "wide_sibling_run");
}

#[test]
fn stage_c_matches_v2_mixed_compound_leaf_siblings() {
    // (leaf (compound leaf leaf) leaf (compound leaf) leaf)
    // — alternates compound and leaf siblings to stress the first-
    // child / last-child pointers across mixed shapes.
    let mut b = TapeBuilder::new();
    b.push_leaf(TapeKind::Span, 0, 1, 0, 0); // leaf 1
    let inner1 = b.mark_children();
    b.push_leaf(TapeKind::Span, 1, 2, 0, 0);
    b.push_leaf(TapeKind::Span, 2, 3, 0, 0);
    b.push_compound(TapeKind::Seq, inner1, 1, 3, 0, 0); // compound 1
    b.push_leaf(TapeKind::Span, 3, 4, 0, 0); // leaf 2
    let inner2 = b.mark_children();
    b.push_leaf(TapeKind::Span, 4, 5, 0, 0);
    b.push_compound(TapeKind::Seq, inner2, 4, 5, 0, 0); // compound 2
    b.push_leaf(TapeKind::Span, 5, 6, 0, 0); // leaf 3
    let outer_children = TapeOffset(0);
    b.push_compound(TapeKind::Rule, outer_children, 0, 6, 0, 0);
    assert_stage_c_matches_v2(b, "mixed_compound_leaf_siblings");
}

#[test]
fn stage_c_matches_v2_balanced_binary_tree() {
    // Balanced binary tree of 4 leaves, 3 internal compounds:
    //
    //               root
    //              /    \
    //          inner1  inner2
    //          /  \     /  \
    //         a   b    c   d
    //
    // Post-order: a, b, inner1, c, d, inner2, root.
    // Stresses two-level recursion + matched sibling pairs at every
    // depth.
    fn push_pair(b: &mut TapeBuilder, span_lo: u32, span_hi: u32) -> TapeOffset {
        let cs = b.mark_children();
        b.push_leaf(TapeKind::Span, span_lo, span_lo + 1, 0, 0);
        b.push_leaf(TapeKind::Span, span_lo + 1, span_hi, 0, 0);
        b.push_compound(TapeKind::Seq, cs, span_lo, span_hi, 0, 0)
    }
    let mut b = TapeBuilder::new();
    let outer_cs = b.mark_children();
    push_pair(&mut b, 0, 2);
    push_pair(&mut b, 2, 4);
    b.push_compound(TapeKind::Rule, outer_cs, 0, 4, 0, 0);
    assert_stage_c_matches_v2(b, "balanced_binary_tree");
}

#[test]
fn stage_c_matches_v2_recursive_three_level() {
    // Deterministic three-level shape:
    //
    //   root [
    //     leaf,
    //     mid [ leaf, leaf, mid' [ leaf, leaf ] ],
    //     leaf,
    //     mid'' [ leaf, leaf ],
    //   ]
    //
    // Mixes deep / shallow children at each layer; exercises the
    // tracked_depth high-water mark over multiple invalidate /
    // re-populate cycles.
    let mut b = TapeBuilder::new();
    let root_cs = b.mark_children();
    b.push_leaf(TapeKind::Span, 0, 1, 0, 0);
    let mid_cs = b.mark_children();
    b.push_leaf(TapeKind::Span, 1, 2, 0, 0);
    b.push_leaf(TapeKind::Span, 2, 3, 0, 0);
    let mid_inner_cs = b.mark_children();
    b.push_leaf(TapeKind::Span, 3, 4, 0, 0);
    b.push_leaf(TapeKind::Span, 4, 5, 0, 0);
    b.push_compound(TapeKind::Seq, mid_inner_cs, 3, 5, 0, 0);
    b.push_compound(TapeKind::Seq, mid_cs, 1, 5, 0, 0);
    b.push_leaf(TapeKind::Span, 5, 6, 0, 0);
    let mid2_cs = b.mark_children();
    b.push_leaf(TapeKind::Span, 6, 7, 0, 0);
    b.push_leaf(TapeKind::Span, 7, 8, 0, 0);
    b.push_compound(TapeKind::Seq, mid2_cs, 6, 8, 0, 0);
    b.push_compound(TapeKind::Rule, root_cs, 0, 8, 0, 0);
    assert_stage_c_matches_v2(b, "recursive_three_level");
}

// ── span_hi + child_off closure on canonical inputs ────────────────
//
// V2's path leaves `span_hi` and `child_off` exactly as the parser
// wrote them at `push_compound` time. Stage-C re-derives them from
// `frame_depth`; on canonical tapes the re-derivation reproduces the
// parser's values byte-for-byte.

#[test]
fn stage_c_compound_span_hi_and_child_off_match_parser() {
    // Build the same shape as `nested_compound` but assert directly
    // on the compound records — Stage-C's compound closure must
    // reproduce the parser-supplied `span_hi` / `child_off`.
    let mut b = TapeBuilder::new();
    b.push_leaf(TapeKind::Span, 0, 1, 0, 0);
    let bc = b.mark_children();
    b.push_leaf(TapeKind::Span, 1, 2, 1, 0);
    b.push_leaf(TapeKind::Span, 2, 3, 2, 0);
    let inner = b.push_compound(TapeKind::Seq, bc, 1, 3, 0, 0);
    b.push_leaf(TapeKind::Span, 3, 4, 3, 0);
    let outer_children = TapeOffset(0);
    let outer = b.push_compound(TapeKind::Rule, outer_children, 0, 4, 0, 0);
    let tape = b.finish().unwrap();

    let inner_rec = tape.get(inner);
    assert_eq!(inner_rec.span_hi, 3, "Stage-C inner span_hi mismatch");
    assert_eq!(inner_rec.child_off, TapeOffset(1), "Stage-C inner child_off mismatch");

    let outer_rec = tape.get(outer);
    assert_eq!(outer_rec.span_hi, 4, "Stage-C outer span_hi mismatch");
    assert_eq!(outer_rec.child_off, TapeOffset(0), "Stage-C outer child_off mismatch");
}

#[test]
fn stage_c_empty_compound_keeps_none_child_off() {
    // Stage-C must not synthesise a `child_off` for compounds whose
    // `has_children` bit is false — the empty-compound invariant
    // (`child_off == NONE`) carries forward unchanged.
    let mut b = TapeBuilder::new();
    let _ = b.push_leaf(TapeKind::Literal, 0, 4, 0, 0);
    let marked = b.mark_children();
    let empty = b.push_compound(TapeKind::Rule, marked, 4, 4, 0, 0);
    let tape = b.finish().unwrap();
    let rec = tape.get(empty);
    assert!(!rec.has_children());
    assert_eq!(rec.child_off, TapeOffset::NONE);
}

// ── derive_frame_depth round-trip ──────────────────────────────────

#[test]
fn derive_frame_depth_matches_post_order_layout() {
    // The transition-window `derive_frame_depth` helper must produce
    // depths that, when passed to `finalise`, recover the V2-equivalent
    // sibling-skip column. This test exercises the helper directly on
    // a finished tape (snapshot before `finalise` over-writes anything).
    let mut b = TapeBuilder::new();
    b.push_leaf(TapeKind::Span, 0, 1, 0, 0);
    let bc = b.mark_children();
    b.push_leaf(TapeKind::Span, 1, 2, 1, 0);
    b.push_leaf(TapeKind::Span, 2, 3, 2, 0);
    b.push_compound(TapeKind::Seq, bc, 1, 3, 0, 0);
    b.push_leaf(TapeKind::Span, 3, 4, 3, 0);
    let outer_children = TapeOffset(0);
    b.push_compound(TapeKind::Rule, outer_children, 0, 4, 0, 0);
    let tape = b.finish().unwrap();

    // Re-derive frame_depth from the post-finish columns.
    let depth = derive_frame_depth(tape.columns());
    // Layout: 0:a(d=1), 1:b(d=2), 2:c(d=2), 3:(bc)(d=1), 4:d(d=1), 5:outer(d=0)
    assert_eq!(depth, vec![1, 2, 2, 1, 1, 0]);
}

#[test]
fn finalise_independently_reproduces_v2_via_helper() {
    // Direct invocation of `finalise(columns, frame_depth)` on a
    // freshly-derived `frame_depth` must produce the same `sib_skip`
    // column as the V2 reference.
    //
    // Build a tape, snapshot its structural shape, zero out
    // `sib_skip`, run `finalise`, and compare against V2.
    let mut b = TapeBuilder::new();
    b.push_leaf(TapeKind::Span, 0, 1, 0, 0);
    let bc = b.mark_children();
    b.push_leaf(TapeKind::Span, 1, 2, 1, 0);
    b.push_leaf(TapeKind::Span, 2, 3, 2, 0);
    b.push_compound(TapeKind::Seq, bc, 1, 3, 0, 0);
    b.push_leaf(TapeKind::Span, 3, 4, 3, 0);
    let outer_children = TapeOffset(0);
    b.push_compound(TapeKind::Rule, outer_children, 0, 4, 0, 0);
    // `tape_snapshot()` clones columns WITHOUT running Stage-C, so
    // `sib_skip` is still default-zero and `child_off` / `span_hi`
    // hold the parser-written values. Perfect inputs for a direct
    // `finalise()` call.
    let snap = b.tape_snapshot();
    let mut cols = bbnf_tape::Columns {
        kinds: snap.columns().kinds.clone(),
        flags: snap.columns().flags.clone(),
        extra: snap.columns().extra.clone(),
        span_lo: snap.columns().span_lo.clone(),
        span_hi: snap.columns().span_hi.clone(),
        sib_skip: vec![0; snap.len()],
        child_off: snap.columns().child_off.clone(),
        pay_narrow: snap.columns().pay_narrow.clone(),
        pay_wide: snap.columns().pay_wide.clone(),
        pay_agg: snap.columns().pay_agg.clone(),
    };
    let depth = derive_frame_depth(&cols);
    finalise(&mut cols, &depth);

    let (n, child_off, has_children) = snapshot_shape(&cols);
    let v2 = reference_v2_sibling_skip(n, &child_off, &has_children);
    assert_eq!(cols.sib_skip, v2, "direct finalise / V2 sib_skip mismatch");
}

// ── ShapeRef round-trip (AV.5.1) ─────────────────────────────────

/// Build a ShapeRef record with no payload and verify round-trip.
#[test]
fn shape_ref_no_payload_round_trip() {
    let mut b = TapeBuilder::new();
    let off = b.push_shape_ref(10, 20, 7, &[]);
    assert_eq!(off, TapeOffset(0));

    let tape = b.finish().unwrap();
    assert_eq!(tape.len(), 1);

    let rec = tape.get(off);
    assert_eq!(rec.kind(), TapeKind::ShapeRef);
    assert_eq!(rec.span_lo, 10);
    assert_eq!(rec.span_hi, 20);
    assert_eq!(rec.shape_dict_idx(), 7);
    assert!(!rec.shape_ref_has_payload());
    assert_eq!(rec.child_off, TapeOffset::NONE);
    // ShapeRef is classified as a leaf (no structural children).
    assert!(!rec.has_children());
    assert!(TapeKind::ShapeRef.is_leaf());
    assert!(TapeKind::ShapeRef.is_shape_ref());
}

/// Build a ShapeRef with a packed payload and verify the packed
/// bytes round-trip through `pay_agg`.
#[test]
fn shape_ref_with_payload_round_trip() {
    let mut b = TapeBuilder::new();
    // Synthetic 3-leaf-hole payload: each leaf is (lo: u32, hi: u32) =
    // 8 bytes; total 24 bytes.
    let mut blob = Vec::new();
    blob.extend_from_slice(&100u32.to_le_bytes());
    blob.extend_from_slice(&110u32.to_le_bytes());
    blob.extend_from_slice(&110u32.to_le_bytes());
    blob.extend_from_slice(&115u32.to_le_bytes());
    blob.extend_from_slice(&115u32.to_le_bytes());
    blob.extend_from_slice(&130u32.to_le_bytes());

    let off = b.push_shape_ref(100, 130, 3, &blob);
    let tape = b.finish().unwrap();

    let rec = tape.get(off);
    assert_eq!(rec.kind(), TapeKind::ShapeRef);
    assert_eq!(rec.shape_dict_idx(), 3);
    assert!(rec.shape_ref_has_payload());
    let arena_offset = rec.child_off.0 as usize;
    let arena = tape.arena();
    assert!(arena_offset + 24 <= arena.len());
    assert_eq!(&arena[arena_offset..arena_offset + 24], blob.as_slice());
}

/// Lazy expansion via `TapeCursor::shape_ref_children` reconstructs
/// child cursors from the template.
#[test]
fn shape_ref_cursor_lazy_expansion() {
    use bbnf_tape::ShapeEntry;

    let mut b = TapeBuilder::new();
    // Template: 5 children. Positions 0, 2, 4 are leaf holes (Span);
    // positions 1 and 3 are structural (Literal).
    // Each leaf hole = 8 bytes (span lo, hi). Total payload = 24 bytes.
    let mut blob = Vec::new();
    // Hole 0 — child 0 — span [10, 18)
    blob.extend_from_slice(&10u32.to_le_bytes());
    blob.extend_from_slice(&18u32.to_le_bytes());
    // Hole 1 — child 2 — span [20, 25)
    blob.extend_from_slice(&20u32.to_le_bytes());
    blob.extend_from_slice(&25u32.to_le_bytes());
    // Hole 2 — child 4 — span [30, 42)
    blob.extend_from_slice(&30u32.to_le_bytes());
    blob.extend_from_slice(&42u32.to_le_bytes());

    let off = b.push_shape_ref(10, 42, 0, &blob);
    let tape = b.finish().unwrap();

    static CHILD_KINDS: [u8; 5] = [
        TapeKind::Span as u8,
        TapeKind::Literal as u8,
        TapeKind::Span as u8,
        TapeKind::Literal as u8,
        TapeKind::Span as u8,
    ];
    static LEAF_OFFSETS: [u16; 5] = [0, u16::MAX, 8, u16::MAX, 16];
    static ENTRY: ShapeEntry = ShapeEntry {
        shape_hash: 0xDEADBEEF,
        rule: RuleId(42),
        child_kinds: &CHILD_KINDS,
        leaf_payload_offsets: &LEAF_OFFSETS,
        payload_bytes: 24,
    };

    let cursor = TapeCursor::new(&tape, off);
    assert_eq!(cursor.shape_ref_child_count(&ENTRY), 5);

    let children: Vec<_> = cursor.shape_ref_children(&ENTRY).collect();
    assert_eq!(children.len(), 5);

    // Child 0: leaf hole, Span [10, 18).
    assert_eq!(children[0].kind, TapeKind::Span);
    assert!(children[0].is_leaf_hole);
    assert_eq!(children[0].span_lo, 10);
    assert_eq!(children[0].span_hi, 18);

    // Child 1: structural Literal, span inherited from parent.
    assert_eq!(children[1].kind, TapeKind::Literal);
    assert!(!children[1].is_leaf_hole);
    assert_eq!(children[1].span_lo, 10);
    assert_eq!(children[1].span_hi, 42);

    // Child 2: leaf hole, Span [20, 25).
    assert_eq!(children[2].kind, TapeKind::Span);
    assert!(children[2].is_leaf_hole);
    assert_eq!(children[2].span_lo, 20);
    assert_eq!(children[2].span_hi, 25);

    // Child 4: leaf hole, Span [30, 42).
    assert_eq!(children[4].kind, TapeKind::Span);
    assert!(children[4].is_leaf_hole);
    assert_eq!(children[4].span_lo, 30);
    assert_eq!(children[4].span_hi, 42);
}

/// On a non-ShapeRef record, `shape_ref_children` yields no items.
#[test]
fn shape_ref_children_empty_on_non_shape_ref() {
    use bbnf_tape::ShapeEntry;

    let mut b = TapeBuilder::new();
    let off = b.push_leaf(TapeKind::Span, 0, 10, 0, 0);
    let tape = b.finish().unwrap();

    static CHILD_KINDS: [u8; 1] = [TapeKind::Span as u8];
    static LEAF_OFFSETS: [u16; 1] = [0];
    static ENTRY: ShapeEntry = ShapeEntry {
        shape_hash: 0,
        rule: RuleId(0),
        child_kinds: &CHILD_KINDS,
        leaf_payload_offsets: &LEAF_OFFSETS,
        payload_bytes: 8,
    };

    let cursor = TapeCursor::new(&tape, off);
    assert_eq!(cursor.shape_ref_child_count(&ENTRY), 0);
    assert_eq!(cursor.shape_ref_children(&ENTRY).count(), 0);
}

/// Verify the `shape_dict_idx` 5-bit packing handles the boundary
/// values 0 and 31 correctly.
#[test]
fn shape_ref_dict_idx_boundary() {
    let mut b = TapeBuilder::new();
    let off_min = b.push_shape_ref(0, 1, 0, &[]);
    let off_max = b.push_shape_ref(1, 2, 31, &[]);
    let tape = b.finish().unwrap();

    assert_eq!(tape.get(off_min).shape_dict_idx(), 0);
    assert_eq!(tape.get(off_max).shape_dict_idx(), 31);
}

// ── AW.1.10 — cursor.child(0) O(1) under pre-order ───────────────

/// Run a minimal DTA table that emits a Seq with three empty-literal
/// children, producing a pre-order tape: `[Seq@0, Lit@1, Lit@2, Lit@3]`
/// with `child_off[0] == 1 == parent + 1`. The cursor's `child(0)`
/// accessor must return offset 1 without entering the backward walk.
#[test]
fn cursor_child_zero_is_o1_under_preorder() {
    use bbnf_tape::driver::dta_run;
    use bbnf_tape::dta::{
        DtaFrameKind, DtaRuleEntry, DtaRuleId, DtaState, DtaStateId, DtaTable,
    };
    use bbnf_tape::psi::PayloadStream;
    use bbnf_tape::Columns;

    // Three Literal states + one Seq state referencing them in order.
    // Static arrays keep lifetimes `'static` as the DTA contract
    // demands.
    static LIT_A: &str = "";
    static LIT_B: &str = "";
    static LIT_C: &str = "";
    static SEQ_CHILDREN: [DtaStateId; 3] = [
        DtaStateId(1),
        DtaStateId(2),
        DtaStateId(3),
    ];
    static STATES: [DtaState; 4] = [
        DtaState::Seq {
            children: &SEQ_CHILDREN,
            frame: DtaFrameKind::Seq,
        },
        DtaState::Literal { text: LIT_A },
        DtaState::Literal { text: LIT_B },
        DtaState::Literal { text: LIT_C },
    ];
    static RULE_ENTRIES: [DtaRuleEntry; 1] = [DtaRuleEntry {
        rule: DtaRuleId(0),
        state: DtaStateId(0),
    }];
    const TABLE: DtaTable = DtaTable {
        states: &STATES,
        rule_entries: &RULE_ENTRIES,
        shunting_yard_rules: &[],
        counter_optional_rules: &[],
        max_nesting_depth: 2,
    };

    // Stub regex scanner — not exercised by the literal-only DTA.
    struct NoScanner;
    impl bbnf_tape::RegexScanner for NoScanner {
        fn scan(&self, _: &str, _: &[u8], _: usize) -> Option<u32> {
            None
        }
    }

    let mut columns = Columns::new();
    let mut psi = PayloadStream::new();
    let mut frame_depth: Vec<u8> = Vec::new();
    let input: &[u8] = b"";
    dta_run(
        &TABLE,
        input,
        &NoScanner,
        &mut columns,
        &mut psi,
        &mut frame_depth,
    )
    .expect("empty-literal DTA run");

    assert_eq!(columns.len(), 4, "Seq + 3 literal leaves");
    // Pre-order layout: parent at 0, children at 1..=3.
    assert_eq!(columns.child_off_at(0).0, 1, "child_off = parent + 1");
    assert!(columns.has_children_at(0));

    // Finalise to populate sib_skip from the DTA-emitted frame_depth.
    bbnf_tape::finalise(&mut columns, &frame_depth);

    // Wrap in a Tape and drive a cursor through child(0). The
    // fast-path branch in `first_child_root` returns offset 1 directly
    // because `child_off == parent_idx + 1`.
    let tape = finalise_to_tape(columns);
    let root = TapeCursor::new(&tape, TapeOffset(0));
    let first = root.child(0).expect("first child present");
    assert_eq!(first.offset().0, 1);
}

/// Helper — wraps populated `Columns` into a `Tape` so pre-order
/// cursor tests can read through the public cursor API without the
/// `TapeBuilder`'s post-order emission path.
///
/// Uses the builder's `tape_snapshot` indirectly: construct a builder,
/// move the columns in by cloning every subcolumn, then call
/// `finish()` to obtain a `Tape`. The columns are already finalised
/// (sib_skip populated by the caller), so `finish` only wraps them;
/// the `has_inline_frame_depth` path skips the internal stage-C pass.
fn finalise_to_tape(columns: bbnf_tape::Columns) -> Tape {
    let mut b = TapeBuilder::new();
    b.enable_inline_frame_depth();
    // Move each column into the builder by swapping with an empty
    // scratch buffer — avoids a full clone.
    let mut scratch = columns;
    std::mem::swap(b.columns_mut(), &mut scratch);
    // Columns are already finalised; `finish` will run finalise over
    // an externally-derived frame_depth length of 0 (which no-ops on
    // an empty depth slice — but the columns already carry valid
    // sib_skip / child_off / span_hi). Use the direct snapshot path.
    b.tape_snapshot()
}
