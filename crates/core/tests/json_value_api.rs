//! AX.W1.A.4 — `bbnf::json::Value` API parity against `sonic_rs::Value`.
//!
//! Invariant 5 — binary parity, no tolerances. Every corpus fixture
//! in `data/json/` is parsed twice (once through BBNF's shape-
//! emission-authoritative tape walker, once through `sonic_rs`), and
//! the two resulting `Value` trees are asserted deep-equal via
//! `PartialEq<sonic_rs::Value>`.
//!
//! Covers:
//!
//! - The six primitive / compound cases (`null`, `true`, `false`, a
//!   positive number, a negative-fractional number, an escape-free
//!   string, an escape-bearing string, an empty array, an empty
//!   object) exercise every `Value` variant in isolation.
//! - The five declared JSON corpora (`data.json`, `twitter.json`,
//!   `citm_catalog.json`, `canada.json`, `data_xl.json`) exercise
//!   every corpus the downstream parity harness runs against.
//!
//! Each corpus test runs the BBNF parser, projects the root through
//! `Value::from_tape`, parses the same bytes through
//! `sonic_rs::from_str::<sonic_rs::Value>`, and asserts deep
//! equality in both directions (`bbnf_value == sonic_value` and
//! `sonic_value == bbnf_value`, verifying the symmetric `PartialEq`
//! bridges both land).

use std::borrow::Cow;
use std::path::PathBuf;

use bbnf::json::{JsonRuleIds, Number, Value};
use bbnf_derive::Parser;

#[derive(Parser, Debug)]
#[parser(path = "../../grammar/json/json.bbnf")]
struct JsonParser;

// ── Rule-id resolution (one compile per test-run) ─────────────────

/// Resolve the JSON grammar's rule ids from a freshly-compiled IR.
/// Cached once per process because compiling a grammar to IR is not
/// free and we run every corpus test.
fn rule_ids() -> &'static JsonRuleIds {
    static IDS: std::sync::OnceLock<JsonRuleIds> = std::sync::OnceLock::new();
    IDS.get_or_init(|| {
        let workspace = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .and_then(|p| p.parent())
            .expect("workspace root")
            .to_path_buf();
        let bbnf_path = workspace.join("grammar/json/json.bbnf");
        let req = bbnf::pipeline::CompileRequest {
            target: bbnf::pipeline::CompileTarget::Vm,
            options: bbnf::pipeline::PipelineOptions::default(),
        };
        let output =
            bbnf::pipeline::compile_paths_request(&[bbnf_path], &req).expect("json compile");
        let ir = match output {
            bbnf::pipeline::CompileOutput::Vm(ir) => ir,
            _ => panic!("expected Vm output for rule-id resolution"),
        };
        JsonRuleIds::from_ir(&ir)
    })
}

/// Load a corpus file by name from `data/json/`.
fn load(name: &str) -> String {
    let workspace = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(|p| p.parent())
        .expect("workspace root")
        .to_path_buf();
    let path = workspace.join("data").join("json").join(name);
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {}", path.display(), e))
}

/// Parse `input` through BBNF + hand the projected `Value` to `f`.
///
/// The returned `Value` borrows from `input` (for `Cow::Borrowed`
/// string leaves) and from the `Parsed<JsonParser>` tape's lifetime
/// (for its arena), so the callback keeps both alive across the
/// comparison. Closure-wrapped rather than leaked so the test suite
/// doesn't grow unbounded memory across runs.
fn with_bbnf_value<'input, R>(input: &'input str, f: impl FnOnce(Value<'_>) -> R) -> R {
    let parsed = JsonParser::parse(input).expect("json parse");
    let root = parsed.root_offset();
    let tape = parsed.tape();
    let value = Value::from_tape(tape, input, root, rule_ids());
    f(value)
}

fn sonic_value(input: &str) -> sonic_rs::Value {
    sonic_rs::from_str::<sonic_rs::Value>(input).expect("sonic-rs parse")
}

/// Deep equality check in both directions. The `PartialEq` bridges
/// land as two trait impls (`Value: PartialEq<sonic_rs::Value>` and
/// `sonic_rs::Value: PartialEq<Value>`) so both assertions hold.
#[track_caller]
fn assert_value_parity(input: &str) {
    let right = sonic_value(input);
    with_bbnf_value(input, |left| {
        assert_eq!(left, right, "bbnf → sonic deep-equality failed");
        assert_eq!(right, left, "sonic → bbnf deep-equality failed");
    });
}

// ── Primitive variant coverage ────────────────────────────────────

#[test]
fn value_null_matches_sonic() {
    assert_value_parity("null");
}

#[test]
fn value_true_matches_sonic() {
    assert_value_parity("true");
}

#[test]
fn value_false_matches_sonic() {
    assert_value_parity("false");
}

#[test]
fn value_integer_number_matches_sonic() {
    assert_value_parity("42");
}

#[test]
fn value_negative_fractional_number_matches_sonic() {
    assert_value_parity("-3.14");
}

#[test]
fn value_empty_string_matches_sonic() {
    assert_value_parity(r#""""#);
}

#[test]
fn value_plain_string_matches_sonic() {
    assert_value_parity(r#""hello world""#);
}

#[test]
fn value_escaped_string_matches_sonic() {
    assert_value_parity(r#""with\nnewline""#);
}

#[test]
fn value_unicode_escape_matches_sonic() {
    assert_value_parity(r#""\u00e9""#);
}

#[test]
fn value_empty_array_matches_sonic() {
    assert_value_parity("[]");
}

#[test]
fn value_flat_array_matches_sonic() {
    assert_value_parity("[1, 2, 3]");
}

#[test]
fn value_heterogeneous_array_matches_sonic() {
    assert_value_parity(r#"[null, true, false, 0, "x", [1], {"k": 2}]"#);
}

#[test]
fn value_empty_object_matches_sonic() {
    assert_value_parity("{}");
}

#[test]
fn value_flat_object_matches_sonic() {
    assert_value_parity(r#"{"a": 1, "b": "two", "c": null}"#);
}

#[test]
fn value_nested_object_matches_sonic() {
    assert_value_parity(r#"{"outer": {"inner": [1, null, true]}}"#);
}

// ── Corpus fixtures — five declared JSON datasets ─────────────────
//
// Brief specifies data/twitter/citm/canada/data_xl. The `citm`
// corpus is named `citm_catalog.json` in the workspace's `data/json`
// directory; the brief's shorthand maps to that file.

#[test]
fn corpus_data_matches_sonic() {
    let input = load("data.json");
    assert_value_parity(&input);
}

#[test]
fn corpus_twitter_matches_sonic() {
    let input = load("twitter.json");
    assert_value_parity(&input);
}

#[test]
fn corpus_citm_catalog_matches_sonic() {
    let input = load("citm_catalog.json");
    assert_value_parity(&input);
}

#[test]
fn corpus_canada_matches_sonic() {
    let input = load("canada.json");
    assert_value_parity(&input);
}

#[test]
fn corpus_data_xl_matches_sonic() {
    let input = load("data_xl.json");
    assert_value_parity(&input);
}

// ── From<sonic_rs::Value> conversion parity ──────────────────────

#[test]
fn from_sonic_roundtrips_against_bbnf() {
    // Build a sonic-rs value, convert to bbnf Value, and assert the
    // resulting bbnf value deep-equals the original sonic-rs value.
    // Exercises the `From<sonic_rs::Value> for Value<'static>` impl.
    for input in &[
        "null",
        "true",
        "false",
        "42",
        "-3.14",
        r#""hello""#,
        r#"[1, 2, 3]"#,
        r#"{"a": 1, "b": null}"#,
    ] {
        let sonic = sonic_value(input);
        let bbnf: Value<'static> = Value::from(&sonic);
        assert_eq!(bbnf, sonic, "From<sonic_rs::Value> for {input}");
    }
}

// ── Number variant coverage ──────────────────────────────────────

#[test]
fn number_integer_positive_from_sonic() {
    let sonic = sonic_value("42");
    let bbnf: Value<'static> = Value::from(&sonic);
    match bbnf {
        Value::Number(n) => {
            assert!(n.is_u64(), "positive integer should be u64");
            assert_eq!(n.as_u64(), Some(42));
        }
        other => panic!("expected Number, got {other:?}"),
    }
}

#[test]
fn number_integer_negative_from_sonic() {
    let sonic = sonic_value("-42");
    let bbnf: Value<'static> = Value::from(&sonic);
    match bbnf {
        Value::Number(n) => {
            assert!(n.is_i64(), "negative integer should be i64");
            assert_eq!(n.as_i64(), Some(-42));
        }
        other => panic!("expected Number, got {other:?}"),
    }
}

#[test]
fn number_float_from_sonic() {
    let sonic = sonic_value("3.14");
    let bbnf: Value<'static> = Value::from(&sonic);
    match bbnf {
        Value::Number(n) => {
            assert!(n.is_f64(), "fractional should be f64");
            assert_eq!(n.as_f64(), 3.14);
        }
        other => panic!("expected Number, got {other:?}"),
    }
}

#[test]
fn number_integer_vs_float_equal_when_same_value() {
    // Cross-variant equality: bbnf's Number::Float(42.0) should
    // compare equal to sonic-rs's Number::PosInt(42) when they
    // describe the same mathematical value. This is how the BBNF
    // tape (which projects numbers as f64) stays compatible with
    // sonic-rs's integer-preserving number representation.
    let bbnf_float = Number::from_f64_finite(42.0).expect("finite");
    let sonic_int = sonic_rs::from_str::<sonic_rs::Number>("42").expect("sonic int");
    assert_eq!(
        bbnf_float, sonic_int,
        "bbnf Float(42.0) should equal sonic PosInt(42) via as_f64"
    );
}

// ── String Cow storage ──────────────────────────────────────────

#[test]
fn string_borrow_safe_is_cow_borrowed() {
    // An escape-free string should borrow directly from the input
    // slice — `Cow::Borrowed` with a pointer that lives inside
    // `input`'s address range.
    let input: &str = r#""plain""#;
    with_bbnf_value(input, |v| match v {
        Value::String(Cow::Borrowed(s)) => {
            // Pointer-identity proof: the borrowed slice starts
            // inside `input`'s address range.
            let input_lo = input.as_ptr() as usize;
            let input_hi = input_lo + input.len();
            let slice_ptr = s.as_ptr() as usize;
            assert!(
                slice_ptr >= input_lo && slice_ptr < input_hi,
                "borrow-safe string must slice from input; got ptr={slice_ptr:#x}, \
                 input=[{input_lo:#x}, {input_hi:#x})"
            );
            assert_eq!(s, "plain");
        }
        Value::String(Cow::Owned(s)) => {
            panic!("plain string should be Cow::Borrowed, got Cow::Owned({s:?})");
        }
        other => panic!("expected String, got {other:?}"),
    });
}

#[test]
fn string_escaped_is_cow_owned() {
    // Escape-bearing string must materialise as `Cow::Owned` —
    // decoding through the arena requires a copy out so the value
    // doesn't carry a tape-borrowed pointer.
    let input = r#""with\nnewline""#;
    with_bbnf_value(input, |v| match v {
        Value::String(Cow::Owned(s)) => {
            assert_eq!(s, "with\nnewline");
        }
        Value::String(Cow::Borrowed(s)) => {
            panic!("escaped string should be Cow::Owned, got Cow::Borrowed({s:?})");
        }
        other => panic!("expected String, got {other:?}"),
    });
}
