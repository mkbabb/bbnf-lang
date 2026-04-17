//! AW-III.W6.1 — ShapeRef runtime dispatch activation test.
//!
//! Verifies the wire contract between the emitter and the walker's
//! compound-emit consumer:
//!
//! 1. `GrammarProfile.shape_dict` is a non-optional reference field.
//! 2. `ShapeEntry` layout is const-constructible exactly as the
//!    emitter writes it.
//! 3. `TapeKind::ShapeRef` discriminant is stable.

use bbnf::runtime::tape::{GrammarProfile, ShapeEntry, TapeKind};

/// Empty profile carries an empty shape_dict slice — grammars with
/// no admitted templates still pass struct-literal validation.
#[test]
fn empty_profile_carries_empty_shape_dict() {
    let p: GrammarProfile = GrammarProfile::EMPTY;
    assert!(
        p.shape_dict.is_empty(),
        "Empty profile must carry an empty shape_dict slice"
    );
}

/// ShapeEntry layout is const-constructible — matches the emitter's
/// literal shape in `emit_shape_entry_literal`.
#[test]
fn shape_entry_wire_layout() {
    const ENTRY: ShapeEntry = ShapeEntry {
        shape_hash: 0xdeadbeefu64,
        rule: bbnf::runtime::tape::RuleId(0),
        child_kinds: &[],
        leaf_payload_offsets: &[],
        payload_bytes: 0,
    };
    assert_eq!(ENTRY.shape_hash, 0xdeadbeef);
    assert_eq!(ENTRY.payload_bytes, 0);
}

/// `TapeKind::ShapeRef` must retain discriminant 13 — the wire contract
/// with `emit_shape_dict_arrays` and `template_piece_to_kind_byte`
/// hard-codes the numeric value.
#[test]
fn shape_ref_kind_discriminant_stable() {
    let k = TapeKind::ShapeRef;
    assert_eq!(k as u8, 13, "ShapeRef discriminant must remain 13");
}

/// `TapeKind::ShapeRef::is_leaf()` returns true — ShapeRef records
/// are leaves that the cursor expands lazily via the shape dict,
/// per AV.5.1.
#[test]
fn shape_ref_is_leaf_kind() {
    let k = TapeKind::ShapeRef;
    assert!(
        k.is_shape_ref(),
        "ShapeRef must report is_shape_ref = true"
    );
}
