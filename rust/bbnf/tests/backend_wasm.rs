//! WASM backend integration tests.
//!
//! Tests the full pipeline: .bbnf grammar → GrammarIR → WasmEmitter → WAT source.
//! Validates structural correctness of generated WebAssembly Text format.

use bbnf::pipeline::{
    CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_grammar_request,
};

/// Load the cross-backend JSON grammar from `grammar/json/json.bbnf`.
fn json_grammar() -> String {
    std::fs::read_to_string("../../grammar/json/json.bbnf")
        .expect("failed to read json.bbnf")
}

fn wasm_request() -> CompileRequest {
    CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Wasm,
    }
}

fn compile_wat(grammar: &str) -> String {
    match compile_grammar_request(grammar, &wasm_request()).unwrap() {
        CompileOutput::Wasm(bytes) => String::from_utf8(bytes).expect("WAT should be valid UTF-8"),
        other => panic!("expected WASM output, got {other:?}"),
    }
}

// ── Module structure ────────────────────────────────────────────────────────

#[test]
fn wat_emits_module_declaration() {
    let wat = compile_wat(r#"x = "a" ;"#);
    assert!(wat.contains("(module $"), "missing module: {wat}");
    assert!(wat.ends_with(")\n"), "module not properly closed: {wat}");
}

#[test]
fn wat_emits_memory_declaration() {
    let wat = compile_wat(r#"x = "a" ;"#);
    assert!(wat.contains("(memory"), "missing memory: {wat}");
    assert!(
        wat.contains("(export \"memory\""),
        "memory should be exported: {wat}"
    );
}

#[test]
fn wat_emits_host_imports() {
    let wat = compile_wat(r#"x = /[a-z]+/ ;"#);
    assert!(
        wat.contains("(import \"host\" \"match_regex\""),
        "missing regex import: {wat}"
    );
}

#[test]
fn wat_emits_parse_export() {
    let wat = compile_wat(r#"x = "a" ;"#);
    assert!(
        wat.contains("(export \"parse\""),
        "missing parse export: {wat}"
    );
}

// ── Function generation ─────────────────────────────────────────────────────

#[test]
fn wat_emits_functions_for_non_inlined_rules() {
    let wat = compile_wat(&json_grammar());
    // Entry + recursive rules get functions. Small rules may be inlined.
    assert!(wat.contains("(func $__value"), "missing __value function");
    assert!(
        wat.contains("(func $__object") || wat.contains("call $__object"),
        "missing object rule"
    );
    assert!(
        wat.contains("(func $__array") || wat.contains("call $__array"),
        "missing array rule"
    );
}

#[test]
fn wat_functions_take_off_and_len_params() {
    let wat = compile_wat(r#"x = "a" ;"#);
    assert!(
        wat.contains("(param $off i32)"),
        "missing $off param: {wat}"
    );
    assert!(
        wat.contains("(param $len i32)"),
        "missing $len param: {wat}"
    );
    assert!(
        wat.contains("(result i32)"),
        "missing i32 result type: {wat}"
    );
}

// ── Literal matching ────────────────────────────────────────────────────────

#[test]
fn wat_single_byte_literal_loads_byte() {
    let wat = compile_wat(r#"x = "a" ;"#);
    // 'a' = 97
    assert!(wat.contains("i32.const 97"), "missing byte 97 for 'a': {wat}");
    assert!(wat.contains("i32.load8_u"), "missing byte load: {wat}");
}

#[test]
fn wat_multi_byte_literal_checks_each_byte() {
    let wat = compile_wat(r#"x = "hello" ;"#);
    // h=104, e=101, l=108, l=108, o=111
    assert!(wat.contains("i32.const 104"), "missing 'h'=104: {wat}");
    assert!(wat.contains("i32.const 101"), "missing 'e'=101: {wat}");
    assert!(wat.contains("i32.const 108"), "missing 'l'=108: {wat}");
    assert!(wat.contains("i32.const 111"), "missing 'o'=111: {wat}");
}

#[test]
fn wat_literal_bounds_checks() {
    let wat = compile_wat(r#"x = "abc" ;"#);
    // Should check offset + len <= input.len before loading.
    assert!(
        wat.contains("i32.le_u") || wat.contains("i32.lt_u"),
        "missing bounds check: {wat}"
    );
}

// ── Alternation ─────────────────────────────────────────────────────────────

#[test]
fn wat_alternation_uses_blocks() {
    let wat = compile_wat(r#"x = "a" | "b" | "c" ;"#);
    // Should have checkpoint save/restore.
    assert!(
        wat.contains("i32.const -1"),
        "should return -1 on failure: {wat}"
    );
}

#[test]
fn wat_alternation_has_control_flow() {
    let wat = compile_wat(r#"
        digit = "0" | "1" | "2" ;
    "#);
    // Alternation should produce some control flow structure.
    let has_br = wat.contains("br_if") || wat.contains("br $");
    let has_neg1 = wat.contains("i32.const -1");
    assert!(
        has_br || has_neg1,
        "alternation should have control flow or failure: {wat}"
    );
}

// ── Repetition ──────────────────────────────────────────────────────────────

#[test]
fn wat_many_uses_loop() {
    let wat = compile_wat(r#"xs = { "a" } ;"#);
    assert!(wat.contains("(loop"), "many should use loop: {wat}");
    assert!(
        wat.contains("br $rep_loop") || wat.contains("br $"),
        "loop should branch back: {wat}"
    );
}

#[test]
fn wat_optional_uses_save_restore() {
    let wat = compile_wat(r#"x = [ "a" ] ;"#);
    // Optional: save offset, try, restore on failure.
    assert!(
        wat.contains("local.set") && wat.contains("local.get"),
        "optional should save/restore: {wat}"
    );
}

// ── Sequences ───────────────────────────────────────────────────────────────

#[test]
fn wat_sequence_chains_with_failure_checks() {
    let wat = compile_wat(r#"pair = "a", "b" ;"#);
    // Each step should check for -1 and return -1 on failure.
    assert!(
        wat.contains("i32.const -1"),
        "sequence should check failures: {wat}"
    );
}

// ── Skip / Next ─────────────────────────────────────────────────────────────

#[test]
fn wat_skip_preserves_kept_value() {
    let wat = compile_wat(r#"x = "a" << "b" ;"#);
    // Skip: parse left (keep), parse right (discard), return left offset.
    assert!(
        wat.contains("local.set") && wat.contains("local.get"),
        "skip should save kept value: {wat}"
    );
}

// ── Whitespace ──────────────────────────────────────────────────────────────

#[test]
fn wat_whitespace_trim_uses_loop() {
    let wat = compile_wat(r#"x = "a" ?w ;"#);
    // WS trim: loop checking space/tab/newline/CR.
    assert!(
        wat.contains("i32.const 32"),
        "should check space (32): {wat}"
    );
    assert!(
        wat.contains("i32.const 9"),
        "should check tab (9): {wat}"
    );
    assert!(
        wat.contains("i32.const 10"),
        "should check newline (10): {wat}"
    );
}

// ── Negate / Minus ──────────────────────────────────────────────────────────

#[test]
fn wat_minus_restores_offset() {
    // Minus is the set-difference operator: `a - b`.
    let wat = compile_wat(r#"x = /[a-z]+/ - "if" ;"#);
    assert!(
        wat.contains("local.set") && wat.contains("local.get"),
        "minus should save/restore offset: {wat}"
    );
}

#[test]
fn wat_minus_checks_exclusion() {
    let wat = compile_wat(r#"x = /[a-z]+/ - "if" ;"#);
    // Minus: try RHS, restore, fail if RHS matched, then try LHS.
    assert!(
        wat.contains("i32.const -1"),
        "minus should return -1 on exclusion: {wat}"
    );
}

// ── Full JSON grammar ───────────────────────────────────────────────────────

#[test]
fn wat_json_grammar_compiles_complete() {
    let wat = compile_wat(&json_grammar());

    assert!(wat.starts_with(";; Generated by BBNF"), "missing header: {wat}");
    assert!(wat.contains("(module $"), "missing module");
    assert!(wat.contains("(export \"parse\""), "missing parse export");
    assert!(wat.contains("(memory"), "missing memory");

    // Should be non-trivial.
    assert!(
        wat.len() > 500,
        "JSON WAT output suspiciously small: {} bytes",
        wat.len()
    );
}

// ── Regex ───────────────────────────────────────────────────────────────────

#[test]
fn wat_regex_calls_host_import() {
    let wat = compile_wat(r#"x = /[a-z]+/ ;"#);
    assert!(
        wat.contains("call $__match_regex"),
        "regex should call host import: {wat}"
    );
}

// ── Separator patterns ──────────────────────────────────────────────────────

#[test]
fn wat_sep_by_emits_loop() {
    let wat = compile_wat(r#"
        comma = "," ;
        xs = "a" << comma ? , { "a" << comma ? } ;
    "#);
    assert!(
        wat.contains("(loop") || wat.contains("loop"),
        "sep_by should emit loop: {wat}"
    );
}
