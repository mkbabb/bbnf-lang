//! AW-III.W6.3 — ClassifyByte dispatch activation test.
//!
//! Verifies that the `emit_classify_byte_arm` emitter produces the
//! inlined `match b { ... }` shape that LLVM lowers to a jumptable.

use bbnf::backend::rust::emitter::classify_byte::emit_classify_byte_arm;
use bbnf::runtime::tape::DtaStateId as TapeStateId;

// Re-use the IR-side StateId which the emitter consumes.
use bbnf_ir::passes::recognizers::dta::StateId;

/// The emitter groups 256 entries by target state — 2 targets + a
/// fallback produces a 3-way match.
#[test]
fn emits_match_over_mined_byte_classes() {
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
    // 1. The two mined target state-id literals (10, 20).
    // 2. The fallback state-id (99).
    // 3. A `match b {` inside the arm (lower_state's call site
    //    provides the `let b = ...` binding; the arm produces the
    //    match skeleton).
    assert!(text.contains("DtaStateId (10"),
        "Missing target 10 in emitted match: {}", text);
    assert!(text.contains("DtaStateId (20"),
        "Missing target 20 in emitted match: {}", text);
    assert!(text.contains("DtaStateId (99"),
        "Missing fallback 99 in emitted match: {}", text);
    assert!(text.contains("match b"),
        "Emitted arm must contain `match b`: {}", text);
}

/// Without a fallback, the arm emits a Syntax-error fallback so the
/// walker surfaces the failure cleanly.
#[test]
fn no_fallback_raises_syntax() {
    let mut table = vec![StateId::NONE; 256];
    for b in b'a'..=b'z' {
        table[b as usize] = StateId(5);
    }
    let arm = emit_classify_byte_arm(3, &table, None);
    let text = arm.to_string();
    assert!(text.contains("DtaError :: Syntax"),
        "No-fallback arm must raise Syntax: {}", text);
    assert!(text.contains("failing_state"),
        "Syntax payload must carry failing_state: {}", text);
}

/// Empty table with a fallback routes unconditionally to the
/// fallback target.
#[test]
fn empty_table_routes_to_fallback() {
    let table = vec![StateId::NONE; 256];
    let arm = emit_classify_byte_arm(0, &table, Some(StateId(7)));
    let text = arm.to_string();
    // Fallback is reachable via `_ =>`.
    assert!(text.contains("_ => :: bbnf :: runtime :: tape :: DtaStateId (7"),
        "Empty table must route to fallback: {}", text);
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
