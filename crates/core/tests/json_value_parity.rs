//! AZ-I.W2-act.B1 — JSON struct-direct value-tree parity vs
//! serde_json.
//!
//! Post-flip, `JsonParser::parse(input)` returns
//! `Result<JsonDocument<'_>, ParseErr>`. This harness compares the
//! struct-direct document tree against `serde_json::Value` node-for-
//! node — every typed `JsonValue` variant maps to the matching
//! `serde_json::Value` shape; nested compounds compare by ordered
//! walk; numbers compare via f64 conversion (the JSON grammar
//! projects through `-> f64`, matching serde's `as_f64()` accessor).
//!
//! Pre-W2-act this file walked the tape root span and re-parsed
//! through a hand-written reference; post-flip the tape substrate is
//! severed for JSON, so the harness recodes against `JsonDocument`'s
//! arena-backed compound resolution.

use ::bbnf::grammar::generated::json::*;
use bbnf::runtime::{JsonDocument, JsonValue};

/// Walk `JsonDocument` against `serde_json::Value`. The comparison is
/// structural — every variant on the bbnf side has a counterpart on
/// the serde side; mismatches surface with a path-to-divergence.
fn assert_doc_eq_serde(
    doc: &JsonDocument<'_>,
    bbnf_value: &JsonValue<'_>,
    oracle: &serde_json::Value,
    path: &str,
) {
    match (bbnf_value, oracle) {
        (JsonValue::Null, serde_json::Value::Null) => {}
        (JsonValue::Bool(b), serde_json::Value::Bool(o)) => {
            assert_eq!(b, o, "{path}: bool divergence");
        }
        (JsonValue::Number(n), serde_json::Value::Number(o)) => {
            let bbnf_f64 = n.as_f64();
            let oracle_f64 = o.as_f64().unwrap_or_else(|| {
                panic!("{path}: serde number {o:?} not f64-coercible")
            });
            if bbnf_f64.is_nan() {
                assert!(oracle_f64.is_nan(), "{path}: bbnf NaN, serde={oracle_f64}");
            } else {
                assert_eq!(bbnf_f64, oracle_f64, "{path}: number divergence");
            }
        }
        (JsonValue::String(s), serde_json::Value::String(o)) => {
            assert_eq!(*s, o.as_str(), "{path}: string divergence");
        }
        (JsonValue::Array(id), serde_json::Value::Array(o)) => {
            let items = doc.array(*id);
            assert_eq!(items.len(), o.len(), "{path}: array length divergence");
            for (i, (bbnf_elem, oracle_elem)) in items.iter().zip(o.iter()).enumerate() {
                let child_path = format!("{path}[{i}]");
                assert_doc_eq_serde(doc, bbnf_elem, oracle_elem, &child_path);
            }
        }
        (JsonValue::Object(id), serde_json::Value::Object(o)) => {
            let pairs = doc.object(*id);
            assert_eq!(
                pairs.len(),
                o.len(),
                "{path}: object pair-count divergence",
            );
            // serde_json's Map (with default features) preserves
            // insertion order; both sides walk source order, so the
            // pairwise comparison is total. If serde is built without
            // `preserve_order`, the keys still match by name lookup
            // below.
            for bbnf_pair in pairs {
                let oracle_value = o.get(bbnf_pair.key).unwrap_or_else(|| {
                    panic!(
                        "{path}: bbnf key {:?} missing in serde object",
                        bbnf_pair.key,
                    )
                });
                let child_path = format!("{path}.{}", bbnf_pair.key);
                assert_doc_eq_serde(doc, &bbnf_pair.value, oracle_value, &child_path);
            }
        }
        (bbnf, oracle) => panic!(
            "{path}: shape divergence — bbnf={bbnf:?}, serde={oracle:?}",
        ),
    }
}

fn parity_against_serde(input: &str) {
    let doc = JsonParser::parse(input).expect("bbnf JSON parse");
    let oracle: serde_json::Value =
        serde_json::from_str(input).expect("serde_json parse");
    assert_doc_eq_serde(&doc, doc.to_value(), &oracle, "$");
}

// ─── Parity tests ─────────────────────────────────────────────────────

#[test]
fn json_parses_null() {
    parity_against_serde("null");
}

#[test]
fn json_parses_bools() {
    parity_against_serde("true");
    parity_against_serde("false");
}

#[test]
fn json_parses_numbers() {
    for input in &["0", "3.14", "-42", "1e3", "2.5e-10"] {
        parity_against_serde(input);
    }
}

#[test]
fn json_parses_strings() {
    for input in &[r#""""#, r#""hello""#, r#""with spaces""#] {
        parity_against_serde(input);
    }
}

#[test]
fn json_parses_empty_array() {
    parity_against_serde("[]");
}

#[test]
fn json_parses_flat_array() {
    parity_against_serde("[1, 2, 3]");
}

#[test]
fn json_parses_nested_array() {
    parity_against_serde("[[1, 2], [3, 4]]");
}

#[test]
fn json_parses_empty_object() {
    parity_against_serde("{}");
}

#[test]
fn json_parses_flat_object() {
    parity_against_serde(r#"{"a": 1, "b": 2}"#);
}

#[test]
fn json_parses_nested_object() {
    parity_against_serde(r#"{"outer": {"inner": [1, null, true]}}"#);
}

#[test]
fn json_parses_mixed_value_tree() {
    let input = r#"[1, "two", null, {"k": 3}]"#;
    parity_against_serde(input);
}

// ─── simdjson OnDemand parity ─────────────────────────────────────────
//
// simd-json's borrowed Value carries the same shape family as
// serde_json::Value (Null / Bool / Number / String / Array / Object);
// the harness compares JsonDocument against simd-json's parsed tree
// using a thin adapter walking the simd-json sum. When the simd-json
// crate's value type closure is structurally compatible with serde's,
// we reuse the same walker; this test pins the simd-json integration
// surface so a regression on its parser surfaces here.

fn assert_doc_eq_simd(
    doc: &JsonDocument<'_>,
    bbnf_value: &JsonValue<'_>,
    oracle: &simd_json::owned::Value,
    path: &str,
) {
    use simd_json::prelude::*;
    match bbnf_value {
        JsonValue::Null => {
            assert!(
                oracle.is_null(),
                "{path}: bbnf=Null but simd-json={:?}",
                oracle.value_type(),
            );
        }
        JsonValue::Bool(b) => {
            let o = oracle.as_bool().unwrap_or_else(|| {
                panic!(
                    "{path}: bbnf=Bool({b}) but simd-json={:?}",
                    oracle.value_type(),
                )
            });
            assert_eq!(*b, o, "{path}: bool divergence");
        }
        JsonValue::Number(n) => {
            let oracle_f64 = oracle.as_f64().unwrap_or_else(|| {
                panic!(
                    "{path}: bbnf=Number but simd-json={:?}",
                    oracle.value_type(),
                )
            });
            let bbnf_f64 = n.as_f64();
            if !bbnf_f64.is_nan() {
                assert_eq!(bbnf_f64, oracle_f64, "{path}: number divergence");
            }
        }
        JsonValue::String(s) => {
            let o = oracle.as_str().unwrap_or_else(|| {
                panic!(
                    "{path}: bbnf=String but simd-json={:?}",
                    oracle.value_type(),
                )
            });
            assert_eq!(*s, o, "{path}: string divergence");
        }
        JsonValue::Array(id) => {
            let items = doc.array(*id);
            let oracle_arr = oracle.as_array().unwrap_or_else(|| {
                panic!(
                    "{path}: bbnf=Array but simd-json={:?}",
                    oracle.value_type(),
                )
            });
            assert_eq!(
                items.len(),
                oracle_arr.len(),
                "{path}: array length divergence",
            );
            for (i, (b, o)) in items.iter().zip(oracle_arr.iter()).enumerate() {
                assert_doc_eq_simd(doc, b, o, &format!("{path}[{i}]"));
            }
        }
        JsonValue::Object(id) => {
            let pairs = doc.object(*id);
            let oracle_obj = oracle.as_object().unwrap_or_else(|| {
                panic!(
                    "{path}: bbnf=Object but simd-json={:?}",
                    oracle.value_type(),
                )
            });
            assert_eq!(
                pairs.len(),
                oracle_obj.len(),
                "{path}: object length divergence",
            );
            for pair in pairs {
                let oracle_value = oracle_obj.get(pair.key).unwrap_or_else(|| {
                    panic!(
                        "{path}: bbnf key {:?} missing in simd-json object",
                        pair.key,
                    )
                });
                let child_path = format!("{path}.{}", pair.key);
                assert_doc_eq_simd(doc, &pair.value, oracle_value, &child_path);
            }
        }
    }
}

fn parity_against_simdjson(input: &str) {
    let doc = JsonParser::parse(input).expect("bbnf JSON parse");
    // simd-json mutates its input buffer; provide an owned copy.
    let mut bytes = input.as_bytes().to_vec();
    let oracle: simd_json::owned::Value =
        simd_json::to_owned_value(&mut bytes).expect("simd_json parse");
    assert_doc_eq_simd(&doc, doc.to_value(), &oracle, "$");
}

#[test]
fn simdjson_parity_flat_object() {
    parity_against_simdjson(r#"{"a": 1, "b": 2}"#);
}

#[test]
fn simdjson_parity_mixed_array() {
    parity_against_simdjson(r#"[1, "two", null, {"k": 3}]"#);
}

#[test]
fn simdjson_parity_scalars() {
    for input in &[
        "null",
        "true",
        "false",
        "42",
        "3.14",
        r#""hello""#,
        "[]",
        "{}",
    ] {
        parity_against_simdjson(input);
    }
}
