//! Round-trip tests for the emit codegen path.
//!
//! Parse → emit_compact → reparse → verify output is valid.

use bbnf_derive::Parser;

// ── JSON ─────────────────────────────────────────────────────────────────────

#[derive(Parser)]
#[parser(path = "../../grammar/json/json.bbnf", emit)]
struct JsonEmit;

// ── CSV ──────────────────────────────────────────────────────────────────────

#[derive(Parser)]
#[parser(path = "../../grammar/misc/csv.bbnf", emit)]
struct CsvEmit;

fn parse_and_emit(input: &str) -> String {
    let ctx = __JsonEmitEnumCtx::with_capacity(input.len() / 32);
    let parser = JsonEmit::value();
    let (result, _state) = parser.parse_return_state_with_context(input, &ctx);
    let value = result.expect("parse failed");
    JsonEmit::emit_compact(value)
}

fn round_trip(input: &str) {
    let emitted = parse_and_emit(input);
    // Re-parse the emitted output to verify it's valid.
    let ctx2 = __JsonEmitEnumCtx::with_capacity(emitted.len() / 32);
    let parser2 = JsonEmit::value();
    let (result2, state2) = parser2.parse_return_state_with_context(&emitted, &ctx2);
    assert!(
        result2.is_some(),
        "re-parse failed for emitted output: {:?}",
        emitted
    );
    assert!(
        state2.offset >= emitted.trim_end().len(),
        "incomplete re-parse ({}/{}): {:?}",
        state2.offset,
        emitted.len(),
        emitted
    );
}

#[test]
fn emit_null() {
    assert_eq!(parse_and_emit("null"), "null");
}

#[test]
fn emit_true() {
    assert_eq!(parse_and_emit("true"), "true");
}

#[test]
fn emit_false() {
    assert_eq!(parse_and_emit("false"), "false");
}

#[test]
fn emit_number() {
    let emitted = parse_and_emit("42");
    // ryu may produce "42.0" for integer inputs — both are valid.
    round_trip(&emitted);
}

#[test]
fn emit_string() {
    assert_eq!(parse_and_emit(r#""hello""#), r#""hello""#);
}

#[test]
fn emit_empty_array() {
    round_trip("[]");
}

#[test]
fn emit_array() {
    round_trip("[1, 2, 3]");
}

#[test]
fn emit_empty_object() {
    round_trip("{}");
}

#[test]
fn emit_object() {
    round_trip(r#"{"key": "value"}"#);
}

#[test]
fn emit_nested() {
    round_trip(r#"{"a": [1, 2], "b": {"c": true}}"#);
}

// ── CSV tests ────────────────────────────────────────────────────────────────

fn csv_parse_and_emit(input: &str) -> String {
    let ctx = __CsvEmitEnumCtx::with_capacity(input.len() / 8);
    let parser = CsvEmit::csv();
    let (result, _state) = parser.parse_return_state_with_context(input, &ctx);
    let value = result.expect("CSV parse failed");
    CsvEmit::emit_compact(&value)
}

fn csv_round_trip(input: &str) {
    let emitted = csv_parse_and_emit(input);
    let ctx2 = __CsvEmitEnumCtx::with_capacity(emitted.len() / 8);
    let parser2 = CsvEmit::csv();
    let (result2, state2) = parser2.parse_return_state_with_context(&emitted, &ctx2);
    assert!(result2.is_some(), "CSV re-parse failed: {:?}", emitted);
    assert!(
        state2.offset >= emitted.trim_end().len(),
        "CSV incomplete re-parse ({}/{}): {:?}",
        state2.offset, emitted.len(), emitted
    );
}

#[test]
fn csv_simple() {
    csv_round_trip("a,b,c");
}

#[test]
fn csv_multiline() {
    csv_round_trip("a,b,c\n1,2,3");
}

#[test]
fn csv_quoted() {
    csv_round_trip(r#""hello","world""#);
}

#[test]
fn csv_single_field() {
    csv_round_trip("hello");
}

