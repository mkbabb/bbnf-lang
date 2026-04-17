//! AW-III.W1 — DTA lift inspection: literal/regex payloads.

use std::path::PathBuf;

use bbnf::pipeline::{
    compile_paths_request, CompileOutput, CompileRequest, CompileTarget, PipelineOptions,
};
use bbnf_ir::passes::{lift_dta, DtaState, LiteralPayload, RegexPayloadKind};

fn workspace_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(|p| p.parent())
        .expect("workspace root")
        .to_path_buf()
}

fn lift(relative: &str) -> (bbnf_ir::GrammarIR, bbnf_ir::passes::DtaTable) {
    let path = workspace_root().join(relative);
    let request = CompileRequest {
        target: CompileTarget::Vm,
        options: PipelineOptions::default(),
    };
    let output = compile_paths_request(&[path], &request).expect("compile");
    match output {
        CompileOutput::Vm(ir) => {
            let table = lift_dta(&ir);
            (ir, table)
        }
        _ => panic!("expected Vm IR"),
    }
}

/// AW-III.W1.7 — verify the Pratt `IrNode::Next` peel by asserting
/// the CSS L4 DTA table contains at least one ShuntingYard state.
///
/// CSS `calc()` / `min()` / `max()` / `clamp()` operator-chain
/// rules lower their operand chains through `IrNode::Next`, not
/// `IrNode::Seq`. Pre-W1 `match_operator_chain_rule` rejected
/// every Next-shaped rule and the lifter produced long
/// ByteDispatch chains. With the `strip_transparent_owned` peel
/// extended to Next/Skip alongside Map/OptionalWhitespace, the
/// chain detector synthesises a Seq view and the precedence chain
/// fires. ShuntingYard state count > 0 is the verifiable signal.
#[test]
fn css_l4_pratt_next_peel_emits_shunting_yard_state() {
    let (_ir, table) = lift("grammar/css/l4/stylesheet.bbnf");
    let sy_count = table
        .states
        .iter()
        .filter(|s| matches!(s, bbnf_ir::passes::DtaState::ShuntingYard { .. }))
        .count();
    eprintln!("CSS L4 ShuntingYard state count = {sy_count}");
    assert!(
        sy_count > 0,
        "AW-III.W1.7: CSS L4 DTA must contain at least one \
         ShuntingYard state after the Pratt `IrNode::Next` peel; \
         observed {sy_count}",
    );
}

#[test]
fn sheets_add_op_branches_carry_u8_payload() {
    let (ir, table) = lift("grammar/google-sheets/google-sheets.bbnf");
    let mut payloads: Vec<(String, LiteralPayload)> = Vec::new();
    for state in &table.states {
        if let DtaState::Literal { text, payload } = state {
            if !matches!(payload, LiteralPayload::None) {
                payloads.push((ir.get_string(*text).to_string(), *payload));
            }
        }
    }
    let plus = payloads.iter().find(|(t, _)| t == "+");
    let minus = payloads.iter().find(|(t, _)| t == "-");
    assert!(
        plus.is_some(),
        "Sheets add_op '+' must lift with payload; observed = {payloads:?}"
    );
    assert!(
        minus.is_some(),
        "Sheets add_op '-' must lift with payload; observed = {payloads:?}"
    );
    assert_eq!(plus.unwrap().1, LiteralPayload::U8(0));
    assert_eq!(minus.unwrap().1, LiteralPayload::U8(1));
}

#[test]
fn sheets_number_regex_carries_f64_payload() {
    let (_ir, table) = lift("grammar/google-sheets/google-sheets.bbnf");
    let f64_count = table
        .states
        .iter()
        .filter(|s| matches!(s, DtaState::Regex { payload: Some(RegexPayloadKind::F64), .. }))
        .count();
    assert!(
        f64_count >= 1,
        "Sheets `number = /regex/ -> f64` must lift with PayloadKind::F64; got {f64_count} F64-decoded regex states"
    );
}

#[test]
fn css_named_color_white_carries_u32_payload() {
    let (ir, table) = lift("grammar/css/l4/stylesheet.bbnf");
    let mut payloads: Vec<(String, LiteralPayload)> = Vec::new();
    for state in &table.states {
        if let DtaState::Literal { text, payload } = state {
            if !matches!(payload, LiteralPayload::None) {
                payloads.push((ir.get_string(*text).to_string(), *payload));
            }
        }
    }
    let white = payloads.iter().find(|(t, _)| t == "white");
    eprintln!("found white = {white:?}");
    eprintln!("First 5 U32 payloads: {:?}", payloads.iter().filter(|(_, p)| matches!(p, LiteralPayload::U32(_))).take(5).collect::<Vec<_>>());
    eprintln!("Last 5 U32 payloads: {:?}", payloads.iter().filter(|(_, p)| matches!(p, LiteralPayload::U32(_))).rev().take(5).collect::<Vec<_>>());
    eprintln!("payload count = {}", payloads.len());
    let mut u8_count = 0;
    let mut u32_count = 0;
    for (_, p) in &payloads {
        match p {
            LiteralPayload::U8(_) => u8_count += 1,
            LiteralPayload::U32(_) => u32_count += 1,
            _ => {}
        }
    }
    eprintln!("u8 count = {u8_count}, u32 count = {u32_count}");
    assert!(white.is_some(), "namedColor 'white' must lift with payload");
    assert_eq!(white.unwrap().1, LiteralPayload::U32(0xFFFFFFFF));
}

#[test]
fn css_dir_keyword_branches_carry_u8_payload() {
    let (ir, table) = lift("grammar/css/l4/stylesheet.bbnf");
    let mut payloads: Vec<(String, LiteralPayload)> = Vec::new();
    for state in &table.states {
        if let DtaState::Literal { text, payload } = state {
            if !matches!(payload, LiteralPayload::None) {
                payloads.push((ir.get_string(*text).to_string(), *payload));
            }
        }
    }
    let ltr = payloads.iter().find(|(t, _)| t == "ltr");
    let rtl = payloads.iter().find(|(t, _)| t == "rtl");
    assert!(
        ltr.is_some(),
        "CSS dirKeyword 'ltr' must lift with payload; observed payload count = {}",
        payloads.len()
    );
    assert!(rtl.is_some());
    assert_eq!(ltr.unwrap().1, LiteralPayload::U8(0));
    assert_eq!(rtl.unwrap().1, LiteralPayload::U8(1));
}
