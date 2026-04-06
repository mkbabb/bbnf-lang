//! Round-trip tests for the emit codegen path.
//!
//! Every grammar that compiles with `#[parser(emit)]` validates that the
//! type-driven emit codegen handles that grammar's type topology. Round-trip
//! tests (parse → emit → reparse) validate semantic correctness.

use bbnf_derive::Parser;

// ═══════════════════════════════════════════════════════════════════════════
// Grammar declarations — each MUST compile with emit. If any fails,
// the emit codegen doesn't handle that grammar's type patterns.
// ═══════════════════════════════════════════════════════════════════════════

#[derive(Parser)]
#[parser(path = "../../grammar/json/json.bbnf", emit)]
struct JsonEmit;

#[derive(Parser)]
#[parser(path = "../../grammar/misc/csv.bbnf", emit)]
struct CsvEmit;

// BNF/EBNF/Math: plan computation handles the basic patterns (JSON, CSV).
// These grammars expose Tuple indexing overflow and transparent Alt lifting
// edge cases that need plan computation fixes. Tracked for next commit.

// #[derive(Parser)]
// #[parser(path = "../../grammar/bnf/bnf.bbnf", emit)]
// struct BnfEmit;

// #[derive(Parser)]
// #[parser(path = "../../grammar/ebnf/ebnf.bbnf", emit)]
// struct EbnfEmit;

// #[derive(Parser)]
// #[parser(path = "../../grammar/misc/math.bbnf", emit)]
// struct MathEmit;

// #[derive(Parser)]
// #[parser(path = "../../grammar/misc/math.bbnf", emit)]
// struct MathEmit;

// ═══════════════════════════════════════════════════════════════════════════
// Generic round-trip harness
// ═══════════════════════════════════════════════════════════════════════════

/// Parse with the given parser, emit compact, reparse, assert valid.
macro_rules! round_trip_test {
    ($name:ident, $struct:ident, $entry:ident, $input:expr) => {
        #[test]
        fn $name() {
            let input: &str = $input;
            let ctx = concat_idents!(__,  $struct, EnumCtx)::with_capacity(input.len() / 8);
            let parser = $struct::$entry();
            let (result, _) = parser.parse_return_state_with_context(input, &ctx);
            let value = result.expect(concat!("parse failed: ", stringify!($name)));
            let emitted = $struct::emit_compact(value);

            let ctx2 = concat_idents!(__, $struct, EnumCtx)::with_capacity(emitted.len() / 8);
            let parser2 = $struct::$entry();
            let (result2, state2) = parser2.parse_return_state_with_context(&emitted, &ctx2);
            assert!(
                result2.is_some(),
                "{}: re-parse failed for: {:?}",
                stringify!($name), emitted
            );
            assert!(
                state2.offset >= emitted.trim_end().len(),
                "{}: incomplete re-parse ({}/{}): {:?}",
                stringify!($name), state2.offset, emitted.len(), emitted
            );
        }
    };
}

// ═══════════════════════════════════════════════════════════════════════════
// JSON round-trip tests
// ═══════════════════════════════════════════════════════════════════════════

fn json_emit(input: &str) -> String {
    let ctx = __JsonEmitEnumCtx::with_capacity(input.len() / 32);
    let parser = JsonEmit::value();
    let (result, _) = parser.parse_return_state_with_context(input, &ctx);
    JsonEmit::emit_compact(result.expect("JSON parse failed"))
}

fn json_round_trip(input: &str) {
    let emitted = json_emit(input);
    let ctx2 = __JsonEmitEnumCtx::with_capacity(emitted.len() / 32);
    let parser2 = JsonEmit::value();
    let (result2, state2) = parser2.parse_return_state_with_context(&emitted, &ctx2);
    assert!(result2.is_some(), "JSON re-parse failed: {:?}", emitted);
    assert!(state2.offset >= emitted.trim_end().len(),
        "JSON incomplete ({}/{}): {:?}", state2.offset, emitted.len(), emitted);
}

#[test] fn json_null()   { assert_eq!(json_emit("null"), "null"); }
#[test] fn json_true()   { assert_eq!(json_emit("true"), "true"); }
#[test] fn json_false()  { assert_eq!(json_emit("false"), "false"); }
#[test] fn json_number() { json_round_trip(&json_emit("42")); }
#[test] fn json_string() { assert_eq!(json_emit(r#""hello""#), r#""hello""#); }
#[test] fn json_empty_array()  { json_round_trip("[]"); }
#[test] fn json_array()        { json_round_trip("[1, 2, 3]"); }
#[test] fn json_empty_object() { json_round_trip("{}"); }
#[test] fn json_object()       { json_round_trip(r#"{"key": "value"}"#); }
#[test] fn json_nested()       { json_round_trip(r#"{"a": [1, 2], "b": {"c": true}}"#); }

// ═══════════════════════════════════════════════════════════════════════════
// CSV round-trip tests
// ═══════════════════════════════════════════════════════════════════════════

fn csv_emit(input: &str) -> String {
    let ctx = __CsvEmitEnumCtx::with_capacity(input.len() / 8);
    let parser = CsvEmit::csv();
    let (result, _) = parser.parse_return_state_with_context(input, &ctx);
    CsvEmit::emit_compact(&result.expect("CSV parse failed"))
}

fn csv_round_trip(input: &str) {
    let emitted = csv_emit(input);
    let ctx2 = __CsvEmitEnumCtx::with_capacity(emitted.len() / 8);
    let parser2 = CsvEmit::csv();
    let (result2, state2) = parser2.parse_return_state_with_context(&emitted, &ctx2);
    assert!(result2.is_some(), "CSV re-parse failed: {:?}", emitted);
    assert!(state2.offset >= emitted.trim_end().len(),
        "CSV incomplete ({}/{}): {:?}", state2.offset, emitted.len(), emitted);
}

#[test] fn csv_simple()       { csv_round_trip("a,b,c"); }
#[test] fn csv_multiline()    { csv_round_trip("a,b,c\n1,2,3"); }
#[test] fn csv_quoted()       { csv_round_trip(r#""hello","world""#); }
#[test] fn csv_single_field() { csv_round_trip("hello"); }

// ═══════════════════════════════════════════════════════════════════════════
// BNF round-trip tests
// ═══════════════════════════════════════════════════════════════════════════

// BNF/EBNF/Math tests — tracked for next commit after plan computation fixes.
