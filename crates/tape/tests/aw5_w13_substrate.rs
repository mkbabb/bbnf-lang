//! AW-V.W1.3 — substrate-enabler regression suite.
//!
//! Three hard gates per AW-V.md §W1.3:
//!
//! 1. **`push_scalar_payload_*` writes to the right column offsets.**
//!    The five helpers (`f64` / `u8` / `bool` / `hex_u32` / `i64`)
//!    land bytes in `pay_agg` at the `child_off`-indicated offset
//!    with widths matching `PayloadKind::arena_byte_width`. Readers
//!    that consult `TapeRec::payload_in_arena()` + slice `pay_agg`
//!    at the record's `child_off` see the written scalar back.
//! 2. **`push_compound_fused_v32` emits one vector store.** The
//!    method body calls `vst1q_u8` (aarch64) or
//!    `_mm256_storeu_si256` (x86_64 AVX); the resulting record
//!    reads back via `materialize` / `kind_at` / `child_off_at` /
//!    `sib_skip_at` matching the values the caller passed in.
//! 3. **`TapeVisitor` produces tape-shape-identical output to the
//!    `dispatch_one` reference path on a JSON fixture.** A small
//!    object with mixed values drives the per-shape sub-trait
//!    surface; assertions cover record count, per-record kind, and
//!    the spanning-compound invariant.

use tape::{
    columns::Columns,
    kind::TapeKind,
    tape::{TapeOffset, TapeRec},
    ArrayVisitor, KeywordVisitor, NumberVisitor, ObjectVisitor, StringVisitor,
    TapeVisitor, Value, ValueVisitor,
};

// ─────────────────────────────────────────────────────────────────────
// 1. push_scalar_payload_* — per-kind round-trip through pay_agg
// ─────────────────────────────────────────────────────────────────────

/// Round-trip an `f64` payload through `push_scalar_payload_f64`. The
/// 8 bytes land at the supplied `child_off`; reading back via the
/// standard `u64::from_le_bytes` + `f64::from_bits` chain recovers
/// the original value bit-for-bit.
#[test]
fn push_scalar_payload_f64_round_trip() {
    let mut cols = Columns::new();
    // Pre-reserve 8 bytes in `pay_agg`; the caller's emitter arm
    // advances the arena cursor monotonically before calling this
    // method — mimic that here.
    let dst_off = cols.pay_agg.len() as u32;
    cols.pay_agg.resize(cols.pay_agg.len() + 8, 0);

    let value = std::f64::consts::PI;
    let idx = cols.push_scalar_payload_f64(
        TapeKind::Regex,
        10,
        20,
        0,
        dst_off,
        value,
    );
    assert_eq!(idx, 0);

    // Record decode — TapeKind, span, child_off, payload-in-arena bit.
    let rec = cols.materialize(idx);
    assert_eq!(rec.kind(), TapeKind::Regex);
    assert_eq!(rec.span_lo, 10);
    assert_eq!(rec.span_hi, 20);
    assert_eq!(rec.child_off, TapeOffset(dst_off));
    assert!(rec.payload_in_arena());

    // Byte-level readback — 8 bytes at dst_off, interpreted as LE u64
    // → f64::from_bits.
    let bytes: [u8; 8] = cols.pay_agg[dst_off as usize..dst_off as usize + 8]
        .try_into()
        .unwrap();
    let bits = u64::from_le_bytes(bytes);
    let got = f64::from_bits(bits);
    assert_eq!(got.to_bits(), value.to_bits());
}

#[test]
fn push_scalar_payload_u8_round_trip() {
    let mut cols = Columns::new();
    let dst_off = cols.pay_agg.len() as u32;
    cols.pay_agg.resize(cols.pay_agg.len() + 1, 0);

    let idx = cols.push_scalar_payload_u8(TapeKind::Literal, 0, 2, 0, dst_off, 42);
    assert_eq!(idx, 0);

    let rec = cols.materialize(idx);
    assert_eq!(rec.kind(), TapeKind::Literal);
    assert_eq!(rec.child_off, TapeOffset(dst_off));
    assert!(rec.payload_in_arena());
    assert_eq!(cols.pay_agg[dst_off as usize], 42);
}

#[test]
fn push_scalar_payload_bool_round_trip() {
    let mut cols = Columns::new();
    let t_off = cols.pay_agg.len() as u32;
    cols.pay_agg.resize(cols.pay_agg.len() + 1, 0);
    let idx_t =
        cols.push_scalar_payload_bool(TapeKind::Literal, 0, 4, 0, t_off, true);
    assert_eq!(idx_t, 0);
    assert_eq!(cols.pay_agg[t_off as usize], 1);

    let f_off = cols.pay_agg.len() as u32;
    cols.pay_agg.resize(cols.pay_agg.len() + 1, 0);
    let idx_f =
        cols.push_scalar_payload_bool(TapeKind::Literal, 5, 10, 0, f_off, false);
    assert_eq!(idx_f, 1);
    assert_eq!(cols.pay_agg[f_off as usize], 0);

    assert_eq!(cols.len(), 2);
    // Both records carry the arena-payload flag.
    assert!(cols.materialize(idx_t).payload_in_arena());
    assert!(cols.materialize(idx_f).payload_in_arena());
}

#[test]
fn push_scalar_payload_hex_u32_round_trip() {
    let mut cols = Columns::new();
    let dst_off = cols.pay_agg.len() as u32;
    cols.pay_agg.resize(cols.pay_agg.len() + 4, 0);

    let value: u32 = 0xDEAD_BEEF;
    let idx = cols.push_scalar_payload_hex_u32(
        TapeKind::Regex,
        0,
        7,
        0,
        dst_off,
        value,
    );
    assert_eq!(idx, 0);

    let rec = cols.materialize(idx);
    assert_eq!(rec.kind(), TapeKind::Regex);
    assert!(rec.payload_in_arena());
    let bytes: [u8; 4] = cols.pay_agg[dst_off as usize..dst_off as usize + 4]
        .try_into()
        .unwrap();
    assert_eq!(u32::from_le_bytes(bytes), value);
}

#[test]
fn push_scalar_payload_i64_round_trip() {
    let mut cols = Columns::new();
    let dst_off = cols.pay_agg.len() as u32;
    cols.pay_agg.resize(cols.pay_agg.len() + 8, 0);

    let value: i64 = -123_456_789_012_345;
    let idx = cols.push_scalar_payload_i64(
        TapeKind::Regex,
        0,
        16,
        0,
        dst_off,
        value,
    );
    assert_eq!(idx, 0);

    let rec = cols.materialize(idx);
    assert!(rec.payload_in_arena());
    let bytes: [u8; 8] = cols.pay_agg[dst_off as usize..dst_off as usize + 8]
        .try_into()
        .unwrap();
    let got = i64::from_le_bytes(bytes);
    assert_eq!(got, value);
}

#[test]
fn push_scalar_payload_sib_skip_overwrite() {
    // `push_leaf_fused` stamps sib_skip=0; the new methods must
    // overwrite when the caller supplies a non-zero stride (used by
    // mid-parse finalisation paths).
    let mut cols = Columns::new();
    let dst_off = cols.pay_agg.len() as u32;
    cols.pay_agg.resize(cols.pay_agg.len() + 8, 0);

    let idx = cols.push_scalar_payload_f64(
        TapeKind::Regex,
        0,
        8,
        7, // sib_skip
        dst_off,
        1.0,
    );
    assert_eq!(cols.sib_skip[idx as usize], 7);
}

#[test]
fn push_scalar_payload_interleaved_arena_growth() {
    // Multiple scalar payloads interleaved with monotonic arena
    // growth. Each write lands at the caller's pre-reserved offset;
    // columns stay in lockstep.
    let mut cols = Columns::new();

    // F64 #1.
    let off0 = cols.pay_agg.len() as u32;
    cols.pay_agg.resize(cols.pay_agg.len() + 8, 0);
    cols.push_scalar_payload_f64(TapeKind::Regex, 0, 3, 0, off0, 1.25);

    // U8.
    let off1 = cols.pay_agg.len() as u32;
    cols.pay_agg.resize(cols.pay_agg.len() + 1, 0);
    cols.push_scalar_payload_u8(TapeKind::Literal, 3, 5, 0, off1, 9);

    // I64.
    let off2 = cols.pay_agg.len() as u32;
    cols.pay_agg.resize(cols.pay_agg.len() + 8, 0);
    cols.push_scalar_payload_i64(TapeKind::Regex, 5, 10, 0, off2, -1);

    // F64 #2 at a later offset.
    let off3 = cols.pay_agg.len() as u32;
    cols.pay_agg.resize(cols.pay_agg.len() + 8, 0);
    cols.push_scalar_payload_f64(TapeKind::Regex, 10, 18, 0, off3, 0.5);

    assert_eq!(cols.len(), 4);
    assert_eq!(cols.pay_agg.len(), 25); // 8 + 1 + 8 + 8

    // Spot-check the scalar readbacks.
    let f64_bytes: [u8; 8] = cols.pay_agg[off0 as usize..off0 as usize + 8]
        .try_into()
        .unwrap();
    assert_eq!(f64::from_bits(u64::from_le_bytes(f64_bytes)), 1.25);
    assert_eq!(cols.pay_agg[off1 as usize], 9);
    let i64_bytes: [u8; 8] = cols.pay_agg[off2 as usize..off2 as usize + 8]
        .try_into()
        .unwrap();
    assert_eq!(i64::from_le_bytes(i64_bytes), -1);
    let f2_bytes: [u8; 8] = cols.pay_agg[off3 as usize..off3 as usize + 8]
        .try_into()
        .unwrap();
    assert_eq!(f64::from_bits(u64::from_le_bytes(f2_bytes)), 0.5);
}

// ─────────────────────────────────────────────────────────────────────
// 2. push_compound_fused_v32 — 32-byte vector store + readback
// ─────────────────────────────────────────────────────────────────────

/// A v32 compound push lands all seven column values at the expected
/// slot. The vector-store intrinsic (visible in `cargo asm` output per
/// the AW-V.W1.3 hard gate) is an implementation detail; the test
/// asserts the SoA reader contract end-to-end.
#[test]
fn push_compound_fused_v32_basic_round_trip() {
    let mut cols = Columns::new();
    let idx = cols.push_compound_fused_v32(
        TapeKind::Seq,
        100, // span_lo
        200, // span_hi
        3,   // sib_skip
        42,  // child_off (arbitrary — v32 doesn't interpret)
        7,   // variant_idx
    );
    assert_eq!(idx, 0);
    assert_eq!(cols.len(), 1);

    let rec = cols.materialize(idx);
    assert_eq!(rec.kind(), TapeKind::Seq);
    assert_eq!(rec.variant_idx(), 7);
    assert_eq!(rec.span_lo, 100);
    assert_eq!(rec.span_hi, 200);
    assert_eq!(rec.child_off, TapeOffset(42));
    // `extra` gets stamped 0 — no HAS_CHILDREN bit; compound still
    // bracket-closes via `push_compound` at the emitter level.
    assert_eq!(rec.extra, 0);

    // Direct column readers agree with the materialised view.
    assert_eq!(cols.kind_at(idx), TapeKind::Seq);
    assert_eq!(cols.child_off_at(idx), TapeOffset(42));
    assert_eq!(cols.sib_skip_at(idx), 3);
    let (lo, hi) = cols.span_at(idx);
    assert_eq!((lo, hi), (100, 200));
}

/// Multiple v32 pushes — columns stay in lockstep, each record's
/// values survive the vector-store-plus-scatter path.
#[test]
fn push_compound_fused_v32_many_pushes_stay_in_lockstep() {
    let mut cols = Columns::new();
    for i in 0..512u32 {
        let idx = cols.push_compound_fused_v32(
            TapeKind::Rule,
            i * 10,
            i * 10 + 5,
            0,
            i,
            i & 0xFF,
        );
        assert_eq!(idx, i);
    }
    assert_eq!(cols.len(), 512);
    assert_eq!(cols.kinds.len(), 512);
    assert_eq!(cols.flags.len(), 512);
    assert_eq!(cols.extra.len(), 512);
    assert_eq!(cols.span_lo.len(), 512);
    assert_eq!(cols.span_hi.len(), 512);
    assert_eq!(cols.sib_skip.len(), 512);
    assert_eq!(cols.child_off.len(), 512);

    // Spot-check across the sequence.
    let rec100 = cols.materialize(100);
    assert_eq!(rec100.kind(), TapeKind::Rule);
    assert_eq!(rec100.span_lo, 1000);
    assert_eq!(rec100.span_hi, 1005);
    assert_eq!(rec100.child_off, TapeOffset(100));
    assert_eq!(rec100.variant_idx(), 100);

    let rec511 = cols.materialize(511);
    assert_eq!(rec511.child_off, TapeOffset(511));
    assert_eq!(rec511.variant_idx(), 255); // 511 & 0xFF
}

/// Confirm the v32 output matches `push_compound_fused` on the fields
/// the scalar path writes — the vector-pack plus scatter cannot
/// diverge from the field-by-field baseline beyond the values v32
/// actually writes (v32 stamps `child_off` / `variant_idx` /
/// `sib_skip`; `push_compound_fused` defaults them to NONE / 0 / 0).
#[test]
fn push_compound_fused_v32_matches_scalar_baseline() {
    let mut a = Columns::new();
    let idx_a = a.push_compound_fused(TapeKind::Seq, 7);
    // Mirror the scalar baseline's writes: child_off = NONE,
    // variant_idx = 0, sib_skip = 0.
    let mut b = Columns::new();
    let idx_b = b.push_compound_fused_v32(TapeKind::Seq, 7, 7, 0, u32::MAX, 0);

    assert_eq!(idx_a, idx_b);
    let rec_a = a.materialize(idx_a);
    let rec_b = b.materialize(idx_b);
    assert_eq!(rec_a.kind(), rec_b.kind());
    assert_eq!(rec_a.span_lo, rec_b.span_lo);
    assert_eq!(rec_a.span_hi, rec_b.span_hi);
    assert_eq!(rec_a.child_off, rec_b.child_off);
    assert_eq!(rec_a.variant_idx(), rec_b.variant_idx());
}

/// v32 survives capacity growth — the `#[cold]` `grow_all` branch
/// catches the slow path and the post-grow stores still land in
/// lockstep. Mirrors `fused_writes::push_compound_fused_grows_with_min_cap_invariant`.
#[test]
fn push_compound_fused_v32_grows_with_min_cap_invariant() {
    let mut cols = Columns::with_capacity(3);
    for i in 0..1000u32 {
        let idx = cols.push_compound_fused_v32(
            TapeKind::Seq, i, i + 1, 0, i, 0,
        );
        assert_eq!(idx, i);
    }
    assert_eq!(cols.len(), 1000);
    assert_eq!(cols.kinds.len(), 1000);
    assert_eq!(cols.span_lo[500], 500);
    assert_eq!(cols.child_off[999], TapeOffset(999));
}

// ─────────────────────────────────────────────────────────────────────
// 3. TapeVisitor — per-shape sub-trait surface, JSON fixture
// ─────────────────────────────────────────────────────────────────────

/// The `TapeVisitor` drives through the per-shape sub-trait methods
/// and produces a well-formed tape. A small JSON-shaped event stream
/// (`{"key":42}`-equivalent) lands a compound + two leaves; the
/// record count and per-record kinds match the reference `dispatch_one`
/// shape.
#[test]
fn tape_visitor_simple_object_round_trip() {
    let mut visitor = TapeVisitor::new(b"{\"key\":42}");

    visitor.begin_object().unwrap();
    visitor.key(b"key").unwrap();
    visitor.number_f64(42.0).unwrap();
    visitor.end_object().unwrap();

    let tape = visitor.finish().unwrap();
    // Expected shape:
    //   [0] Span (key bytes arena-framed)
    //   [1] Regex (number f64)
    //   [2] Rule (compound bracketing the children at 0..=1)
    assert_eq!(tape.len(), 3);

    let key = tape.get(TapeOffset(0));
    assert_eq!(key.kind(), TapeKind::Span);

    let num = tape.get(TapeOffset(1));
    assert_eq!(num.kind(), TapeKind::Regex);

    let obj = tape.get(TapeOffset(2));
    assert_eq!(obj.kind(), TapeKind::Rule);
    assert!(obj.has_children());
    assert_eq!(obj.child_off, TapeOffset(0));
}

#[test]
fn tape_visitor_nested_array_round_trip() {
    // Event stream for `[true,null,"s"]`.
    let mut visitor = TapeVisitor::new(b"[true,null,\"s\"]");
    visitor.begin_array().unwrap();
    visitor.bool(true).unwrap();
    visitor.null().unwrap();
    visitor.string(b"s").unwrap();
    visitor.end_array().unwrap();

    let tape = visitor.finish().unwrap();
    assert_eq!(tape.len(), 4); // 3 leaves + 1 compound

    let bool_rec = tape.get(TapeOffset(0));
    assert_eq!(bool_rec.kind(), TapeKind::Literal);

    let null_rec = tape.get(TapeOffset(1));
    assert_eq!(null_rec.kind(), TapeKind::Literal);

    let str_rec = tape.get(TapeOffset(2));
    assert_eq!(str_rec.kind(), TapeKind::Span);

    let arr_rec = tape.get(TapeOffset(3));
    assert_eq!(arr_rec.kind(), TapeKind::Rule);
    assert!(arr_rec.has_children());
    assert_eq!(arr_rec.child_off, TapeOffset(0));
}

#[test]
fn tape_visitor_number_wide_scalar_payload() {
    // A number event lands as a Regex leaf with a WideScalar payload
    // column-rank in `pay_wide`. Verifies the f64 bits round-trip.
    let mut visitor = TapeVisitor::new(b"3.14");
    visitor.number_f64(std::f64::consts::PI).unwrap();
    let tape = visitor.finish().unwrap();
    assert_eq!(tape.len(), 1);
    let rec = tape.get(TapeOffset(0));
    assert_eq!(rec.kind(), TapeKind::Regex);
    // The prototype's TapeVisitor routes numbers through
    // `PayloadData::WideScalar`; reader recovers via `pay_wide`
    // indexed by the record's column-rank `child_off`.
    let col_rank = rec.child_off.as_u32() as usize;
    let bits = tape.columns().pay_wide[col_rank];
    assert_eq!(f64::from_bits(bits), std::f64::consts::PI);
}

#[test]
fn tape_visitor_key_arena_frame_contains_bytes() {
    let mut visitor = TapeVisitor::new(b"{\"k\":true}");
    visitor.begin_object().unwrap();
    visitor.key(b"k").unwrap();
    visitor.bool(true).unwrap();
    visitor.end_object().unwrap();

    let tape = visitor.finish().unwrap();
    // The key leaf's child_off is a pay_agg byte offset pointing at
    // the `(len: u32 LE, bytes)` frame.
    let key = tape.get(TapeOffset(0));
    let frame_off = key.child_off.as_u32() as usize;
    let arena = &tape.columns().pay_agg;
    let len_bytes: [u8; 4] = arena[frame_off..frame_off + 4]
        .try_into()
        .unwrap();
    let len = u32::from_le_bytes(len_bytes) as usize;
    assert_eq!(len, 1);
    assert_eq!(&arena[frame_off + 4..frame_off + 4 + len], b"k");
}

// ─────────────────────────────────────────────────────────────────────
// 4. ValueVisitor placeholder — compiles and materialises basic shapes
// ─────────────────────────────────────────────────────────────────────

/// The `ValueVisitor` placeholder compiles against the minimal JSON
/// enum + materialises a nested structure. W3.2 replaces this with
/// a per-grammar generated visitor; the placeholder exists so the
/// trait surface has a concrete consumer inside `tape`.
#[test]
fn value_visitor_placeholder_materialises_object() {
    let mut visitor = ValueVisitor::new();
    visitor.begin_object().unwrap();
    visitor.key(b"n").unwrap();
    visitor.number_f64(1.5).unwrap();
    visitor.key(b"b").unwrap();
    visitor.bool(false).unwrap();
    visitor.key(b"arr").unwrap();
    visitor.begin_array().unwrap();
    visitor.null().unwrap();
    visitor.string(b"hi").unwrap();
    visitor.end_array().unwrap();
    visitor.end_object().unwrap();

    let root = visitor.finish().expect("root populated");
    match root {
        Value::Object(pairs) => {
            assert_eq!(pairs.len(), 3);
            assert_eq!(pairs[0].0, b"n");
            assert!(matches!(pairs[0].1, Value::Number(x) if x == 1.5));
            assert_eq!(pairs[1].0, b"b");
            assert!(matches!(pairs[1].1, Value::Bool(false)));
            assert_eq!(pairs[2].0, b"arr");
            match &pairs[2].1 {
                Value::Array(arr) => {
                    assert_eq!(arr.len(), 2);
                    assert!(matches!(arr[0], Value::Null));
                    assert!(matches!(&arr[1], Value::String(s) if s == b"hi"));
                }
                _ => panic!("expected array"),
            }
        }
        _ => panic!("expected object root"),
    }
}

#[test]
fn value_visitor_placeholder_scalar_root() {
    let mut visitor = ValueVisitor::new();
    visitor.number_f64(42.0).unwrap();
    let root = visitor.finish().unwrap();
    assert!(matches!(root, Value::Number(x) if x == 42.0));
}

#[test]
fn tape_rec_and_visitor_both_in_scope() {
    // Smoke test: the crate-level re-exports of the visitor types
    // land; `TapeRec::PAYLOAD_IN_ARENA_BIT` is observable through the
    // existing tape surface.
    assert_eq!(TapeRec::PAYLOAD_IN_ARENA_BIT, 0x0002);
}
