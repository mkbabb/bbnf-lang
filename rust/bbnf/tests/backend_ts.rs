//! TypeScript backend integration tests.
//!
//! Tests the full pipeline: .bbnf grammar → GrammarIR → TsEmitter → TS source.
//! Validates structural correctness of generated TypeScript.

use bbnf::pipeline::{
    CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_grammar_request,
};

/// Load the cross-backend JSON grammar from `grammar/json/json-pure.bbnf`.
fn json_grammar() -> String {
    std::fs::read_to_string("../../grammar/json/json-pure.bbnf")
        .expect("failed to read json-pure.bbnf")
}

fn ts_request() -> CompileRequest {
    CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Ts,
    }
}

fn compile_ts(grammar: &str) -> String {
    match compile_grammar_request(grammar, &ts_request()).unwrap() {
        CompileOutput::Ts(source) => source,
        other => panic!("expected TS output, got {other:?}"),
    }
}

// ── Runtime type definitions ────────────────────────────────────────────────

#[test]
fn ts_emits_parser_state_interface() {
    let source = compile_ts(r#"x = "a" ;"#);
    assert!(source.contains("interface ParserState"), "{source}");
    assert!(source.contains("input: string"), "{source}");
    assert!(source.contains("offset: number"), "{source}");
}

#[test]
fn ts_emits_span_interface() {
    let source = compile_ts(r#"x = "a" ;"#);
    assert!(source.contains("interface Span"), "{source}");
    assert!(source.contains("start: number"), "{source}");
    assert!(source.contains("end: number"), "{source}");
}

#[test]
fn ts_emits_create_state_helper() {
    let source = compile_ts(r#"x = "a" ;"#);
    assert!(source.contains("function createState"), "{source}");
}

#[test]
fn ts_emits_span_helper() {
    let source = compile_ts(r#"x = "a" ;"#);
    assert!(source.contains("function span("), "{source}");
}

// ── Discriminated union types ───────────────────────────────────────────────

#[test]
fn ts_emits_discriminated_union() {
    let source = compile_ts(r#"
        item = "x" | "y" ;
        list = item, { item } ;
    "#);
    assert!(source.contains("tag: \"item\""), "missing item variant: {source}");
    assert!(source.contains("tag: \"list\""), "missing list variant: {source}");
}

#[test]
fn ts_json_grammar_has_key_variants() {
    let source = compile_ts(&json_grammar());
    // After IR passes, some rules get inlined/fused. Check that the major
    // structural rules survive as union variants.
    assert!(source.contains("tag: \"value\"") || source.contains("function __value"),
        "missing value: {source}");
    // The entry rule must always exist.
    assert!(source.contains("function __value"), "missing __value function: {source}");
}

// ── Parser functions ────────────────────────────────────────────────────────

#[test]
fn ts_emits_function_per_rule() {
    let source = compile_ts(&json_grammar());
    for rule in &["value", "object", "array", "string", "number", "bool", "null", "pair", "comma", "colon"] {
        assert!(
            source.contains(&format!("function __{rule}")),
            "missing __{rule} function: {source}"
        );
    }
}

#[test]
fn ts_emits_public_parse_entry() {
    let source = compile_ts(&json_grammar());
    assert!(source.contains("export function parse"), "missing export: {source}");
    assert!(source.contains("__value(s)"), "entry should call __value: {source}");
}

// ── Literal matching ────────────────────────────────────────────────────────

#[test]
fn ts_single_byte_literal_uses_char_code() {
    let source = compile_ts(r#"x = "a" ;"#);
    assert!(
        source.contains("charCodeAt") || source.contains("startsWith"),
        "literal match should use charCodeAt or startsWith: {source}"
    );
}

#[test]
fn ts_multi_byte_literal_uses_starts_with() {
    let source = compile_ts(r#"x = "hello" ;"#);
    assert!(source.contains("startsWith"), "multi-byte should use startsWith: {source}");
    assert!(source.contains("\"hello\""), "literal value missing: {source}");
}

// ── Regex matching ──────────────────────────────────────────────────────────

#[test]
fn ts_regex_uses_sticky_flag() {
    let source = compile_ts(r#"x = /[a-z]+/ ;"#);
    assert!(source.contains("/y"), "regex should use sticky flag: {source}");
    assert!(source.contains(".exec("), "regex should use .exec(): {source}");
}

// ── Alternation ─────────────────────────────────────────────────────────────

#[test]
fn ts_alternation_emits_dispatch_or_checkpoint() {
    let source = compile_ts(r#"
        digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
    "#);
    let has_switch = source.contains("switch (s.input.charCodeAt");
    let has_checkpoint = source.contains("s.offset =");
    assert!(
        has_switch || has_checkpoint,
        "alternation should use dispatch or checkpoint: {source}"
    );
}

#[test]
fn ts_two_branch_alt_uses_checkpoint() {
    let source = compile_ts(r#"x = "foo" | "bar" ;"#);
    // Two branches: should checkpoint/restore.
    assert!(source.contains("s.offset"), "should track offset: {source}");
}

// ── Repetition ──────────────────────────────────────────────────────────────

#[test]
fn ts_many_emits_while_loop() {
    let source = compile_ts(r#"xs = { "a" } ;"#);
    assert!(
        source.contains("while (true)") || source.contains("while(true)"),
        "many should use while loop: {source}"
    );
}

#[test]
fn ts_optional_emits_checkpoint() {
    let source = compile_ts(r#"x = [ "a" ] ;"#);
    // Optional: try, restore on failure.
    assert!(
        source.contains("s.offset"),
        "optional should save/restore offset: {source}"
    );
}

// ── Sequences ───────────────────────────────────────────────────────────────

#[test]
fn ts_sequence_chains_parse_calls() {
    let source = compile_ts(r#"pair = "a", "b" ;"#);
    // Should have two literal match expressions chained.
    assert!(
        source.contains("null") && source.contains("return"),
        "sequence should chain and return: {source}"
    );
}

// ── Skip / Next ─────────────────────────────────────────────────────────────

#[test]
fn ts_skip_keeps_left() {
    let source = compile_ts(r#"x = "a" << "b" ;"#);
    assert!(source.contains("__kept"), "skip should keep left value: {source}");
}

#[test]
fn ts_next_keeps_right() {
    let source = compile_ts(r#"x = "a" >> "b" ;"#);
    // Next: discard left, return right.
    assert!(source.contains("null"), "next should check for failure: {source}");
}

// ── Whitespace ──────────────────────────────────────────────────────────────

#[test]
fn ts_optional_whitespace_trims() {
    let source = compile_ts(r#"x = "a" ?w ;"#);
    assert!(
        source.contains("\\t\\n\\r") || source.contains("trim") || source.contains("includes"),
        "should emit whitespace trimming: {source}"
    );
}

// ── Full JSON grammar structural test ───────────────────────────────────────

#[test]
fn ts_json_grammar_compiles_complete() {
    let source = compile_ts(&json_grammar());

    // Minimum structure checks.
    assert!(source.starts_with("// Generated by BBNF"), "missing header: {source}");
    assert!(source.contains("interface ParserState"), "missing ParserState");
    assert!(source.contains("interface Span"), "missing Span");
    assert!(source.contains("export function parse"), "missing parse export");

    // Should be non-trivial (JSON grammar produces substantial output).
    assert!(
        source.len() > 1000,
        "JSON TS output suspiciously small: {} bytes",
        source.len()
    );
}

// ── Negate / Minus ──────────────────────────────────────────────────────────

#[test]
fn ts_minus_emits_exclusion() {
    let source = compile_ts(r#"x = /[a-z]+/ - "if" ;"#);
    // Minus: save offset, try exclusion, restore, fail or continue.
    assert!(
        source.contains("__save") || source.contains("s.offset"),
        "minus should save/restore offset: {source}"
    );
}
