//! AW-III.W6.3 / AW-IV.W3.3 — ClassifyByte dispatch activation test.
//!
//! Verifies that the `emit_classify_byte_arm` emitter produces the
//! inlined single-indexed-load shape (`__CLASSIFY_TABLE_<idx>[b]`)
//! that LLVM const-folds at the call site.

use bbnf::backend::rust::emitter::classify_byte::emit_classify_byte_arm;
use bbnf::runtime::tape::DtaStateId as TapeStateId;

// Re-use the IR-side StateId which the emitter consumes.
use bbnf_ir::passes::recognizers::dta::StateId;

/// The emitter lowers a ClassifyByte state to a literal
/// `const __CLASSIFY_TABLE_<idx>: [DtaStateId; 256]` plus an indexed
/// load — not a per-byte match cascade. Target state-ids appear as
/// literal entries in the const array; the fallback state-id
/// appears in the `if __next == NONE` branch.
#[test]
fn emits_indexed_load_over_literal_lut() {
    let mut table = vec![StateId::NONE; 256];
    // Bytes 'a'..='z' → state 10.
    for b in b'a'..=b'z' {
        table[b as usize] = StateId(10);
    }
    // Bytes '0'..='9' → state 20.
    for b in b'0'..=b'9' {
        table[b as usize] = StateId(20);
    }
    let arm = emit_classify_byte_arm(7, &table, Some(StateId(99)));
    let text = arm.to_string();
    // The arm body must include:
    // 1. The per-state const declaration `__CLASSIFY_TABLE_7`.
    // 2. The two mined target state-id literals (10, 20) as LUT
    //    entries.
    // 3. The fallback state-id (99) as the NONE-branch target.
    // 4. The indexed load `__CLASSIFY_TABLE_7 [__b as usize]`.
    assert!(
        text.contains("__CLASSIFY_TABLE_7"),
        "Missing per-state LUT ident: {}",
        text
    );
    assert!(
        text.contains("DtaStateId (10"),
        "Missing target 10 in LUT: {}",
        text
    );
    assert!(
        text.contains("DtaStateId (20"),
        "Missing target 20 in LUT: {}",
        text
    );
    assert!(
        text.contains("DtaStateId (99"),
        "Missing fallback 99 in NONE-branch: {}",
        text
    );
    assert!(
        text.contains("__CLASSIFY_TABLE_7 [__b as usize]"),
        "Emitted arm must load via indexed access: {}",
        text
    );
    assert!(
        text.contains("DtaStateId :: NONE"),
        "Emitted arm must branch on DtaStateId::NONE: {}",
        text
    );
}

/// Without a fallback, the arm emits a Syntax-error branch when the
/// indexed load returns NONE so the walker surfaces the failure
/// cleanly.
#[test]
fn no_fallback_raises_syntax() {
    let mut table = vec![StateId::NONE; 256];
    for b in b'a'..=b'z' {
        table[b as usize] = StateId(5);
    }
    let arm = emit_classify_byte_arm(3, &table, None);
    let text = arm.to_string();
    assert!(
        text.contains("__CLASSIFY_TABLE_3"),
        "Missing per-state LUT ident: {}",
        text
    );
    assert!(
        text.contains("DtaError :: Syntax"),
        "No-fallback arm must raise Syntax on NONE: {}",
        text
    );
    assert!(
        text.contains("failing_state"),
        "Syntax payload must carry failing_state: {}",
        text
    );
}

/// Empty table with a fallback routes unconditionally to the
/// fallback target — every LUT entry is `DtaStateId::NONE`, so the
/// NONE-branch fires every call.
#[test]
fn empty_table_routes_to_fallback() {
    let table = vec![StateId::NONE; 256];
    let arm = emit_classify_byte_arm(0, &table, Some(StateId(7)));
    let text = arm.to_string();
    // The fallback target appears as a DtaStateId(7) literal in
    // the NONE-branch (not the LUT — every LUT entry is NONE).
    assert!(
        text.contains("__CLASSIFY_TABLE_0"),
        "Missing per-state LUT ident: {}",
        text
    );
    assert!(
        text.contains("DtaStateId (7"),
        "Empty table must route to fallback 7: {}",
        text
    );
}

/// The tape-side DtaState::ClassifyByte variant is wire-construct
/// compatible with ByteDispatch: both hold a `&'static [DtaStateId;
/// 256]` + a fallback. Compile-time confirms the variant exists on
/// the runtime side.
#[test]
fn tape_side_variant_exists() {
    // Construct a no-op table to confirm the variant constructs.
    static TABLE: [TapeStateId; 256] = [TapeStateId::NONE; 256];
    let s = bbnf::runtime::tape::DtaState::ClassifyByte {
        table: &TABLE,
        fallback: TapeStateId::NONE,
    };
    match s {
        bbnf::runtime::tape::DtaState::ClassifyByte { .. } => {}
        _ => panic!("ClassifyByte variant not constructable"),
    }
}
