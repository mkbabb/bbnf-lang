//! Aggregate payload layout planner — alignment, packing, rejection.
//!
//! Validates that [`bbnf_ir::passes::plan_layout`] computes correct
//! offsets for tuple-of-scalar payloads, respects per-scalar
//! alignment, and rejects layouts that exceed
//! [`MAX_PAYLOAD_BYTES`].

use bbnf_ir::passes::{plan_layout, MAX_PAYLOAD_BYTES};
use bbnf_ir::TypeDesc;

#[test]
fn single_f64_field_packs_8_bytes() {
    let layout = plan_layout(&[TypeDesc::F64]).expect("layout");
    assert_eq!(layout.total_bytes, 8);
    assert_eq!(layout.fields[0].offset, 0);
}

#[test]
fn f64_then_u8_packs_with_no_padding_at_end() {
    let layout = plan_layout(&[TypeDesc::F64, TypeDesc::U8]).expect("layout");
    // f64 at 0..8, u8 at 8..9.
    assert_eq!(layout.fields[0].offset, 0);
    assert_eq!(layout.fields[1].offset, 8);
    assert_eq!(layout.total_bytes, 9);
}

#[test]
fn u8_then_u32_inserts_alignment_padding() {
    let layout = plan_layout(&[TypeDesc::U8, TypeDesc::U32]).expect("layout");
    // u8 at 0..1, padding through 3, u32 at 4..8.
    assert_eq!(layout.fields[0].offset, 0);
    assert_eq!(layout.fields[1].offset, 4);
    assert_eq!(layout.total_bytes, 8);
}

#[test]
fn non_scalar_field_rejects_layout() {
    // Enum is not a scalar — layout should be rejected.
    let layout = plan_layout(&[TypeDesc::F64, TypeDesc::Enum]);
    assert!(layout.is_none());
}

#[test]
fn span_field_packs_as_8_bytes() {
    // AS.2: Span is now a scalar payload (two packed u32s = 8 bytes).
    let layout = plan_layout(&[TypeDesc::F64, TypeDesc::Span]).expect("layout");
    // f64 at 0..8, Span at 8..16.
    assert_eq!(layout.fields[0].offset, 0);
    assert_eq!(layout.fields[1].offset, 8);
    assert_eq!(layout.total_bytes, 16);
}

#[test]
fn over_max_bytes_rejects_layout() {
    // 2 x f64 = 16 bytes (== MAX_PAYLOAD_BYTES), fine. + 1 byte → reject.
    assert!(plan_layout(&[TypeDesc::F64, TypeDesc::F64]).is_some());
    assert!(plan_layout(&[TypeDesc::F64, TypeDesc::F64, TypeDesc::U8]).is_none());
    assert_eq!(MAX_PAYLOAD_BYTES, 16);
}
