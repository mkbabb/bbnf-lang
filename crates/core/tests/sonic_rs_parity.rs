//! AW-IV.W5.2 — sonic-rs JSON parity harness (struct-direct).
//!
//! For each canonical JSON fixture, parse with bbnf and with sonic-rs
//! and assert the two parsers produce identical tree shapes. Equivalence
//! is measured at the `Value` abstraction: each parser's output is
//! projected to the same `RefValue` enum (Null / Bool / Number / String
//! / Array / Object) and compared node-for-node.
//!
//! # Migration history
//!
//! Pre-AZ-I.W2-act the bbnf side walked the parse output's tape
//! through a re-parse of the root record's span. AZ-I.W2-act flipped
//! JSON to the struct-direct path — `JsonParser::parse` returns a
//! [`JsonDocument`] whose typed value tree IS the parse output; no
//! tape, no cursor, no re-parse. The migrated bbnf-side projection
//! walks `JsonValue` directly, mapping each variant to the equivalent
//! `RefValue`. sonic-rs's projection (`sonic_rs::Value` →
//! `JsonValueTrait` accessors) is unchanged.
//!
//! The parity is a HARD GATE per AW-IV W5.2 — zero divergences on
//! canada / twitter / citm / data / data_xl. Divergence patterns that
//! MUST be fixed in bbnf rather than tolerated:
//!
//!   - dropped array / object entries
//!   - string mis-decodes (escape handling)
//!   - number precision drift (both parsers use f64; we tolerate
//!     bit-identical comparison via `to_bits` on finite non-NaN values)

use sonic_rs::{JsonContainerTrait, JsonType, JsonValueTrait};

use ::bbnf::grammar::generated::json::*;
use bbnf::runtime::{JsonNumber, JsonValue};

// ─── Shared projection type ──────────────────────────────────────────
//
// `RefValue` is the common denominator: both bbnf's tape projection
// and sonic-rs's `Value` reduce to this enum so the field-for-field
// equality reduces to a single `PartialEq` invocation.
//
// Numbers carry their `f64` bit-pattern so the comparison is exact for
// every finite value the JSON grammar admits; NaN inputs are out-of-
// scope (JSON does not admit NaN literals, both parsers reject them).

#[derive(Clone, Debug, PartialEq)]
enum RefValue {
    Null,
    Bool(bool),
    Number(f64),
    String(String),
    Array(Vec<RefValue>),
    Object(Vec<(String, RefValue)>),
}

// ─── bbnf-side projection (struct-direct) ────────────────────────────
//
// Parse with bbnf, walk the resulting `JsonDocument`'s typed value
// tree, mapping each variant to the equivalent `RefValue`. Pre-flip
// this re-parsed the root tape record's span text via the reference
// walker; struct-direct landed `JsonValue` directly so the projection
// is now a structural mapping. The text content is identical (the
// arena-decoded strings, the f64-decoded numbers); the variant
// alphabet collapses one-to-one.

fn bbnf_project(input: &str) -> RefValue {
    let doc = JsonParser::parse(input).expect("bbnf JSON parse");
    project_value(&doc, doc.root())
}

fn project_value<'p>(doc: &bbnf::runtime::JsonDocument<'p>, value: &JsonValue<'p>) -> RefValue {
    match value {
        JsonValue::Null => RefValue::Null,
        JsonValue::Bool(b) => RefValue::Bool(*b),
        JsonValue::Number(n) => match n {
            JsonNumber::Float(f) => RefValue::Number(*f),
            JsonNumber::Int(i) => RefValue::Number(*i as f64),
            JsonNumber::UInt(u) => RefValue::Number(*u as f64),
        },
        JsonValue::String(s) => RefValue::String((*s).to_string()),
        JsonValue::Array(id) => {
            let items = doc.array(*id);
            let mut out = Vec::with_capacity(items.len());
            for item in items {
                out.push(project_value(doc, item));
            }
            RefValue::Array(out)
        }
        JsonValue::Object(id) => {
            let pairs = doc.object(*id);
            let mut out = Vec::with_capacity(pairs.len());
            for pair in pairs {
                out.push((pair.key.to_string(), project_value(doc, &pair.value)));
            }
            RefValue::Object(out)
        }
    }
}

// ─── sonic-rs-side projection ────────────────────────────────────────
//
// `sonic_rs::Value` exposes `get_type()` (JsonType discriminant) + the
// JsonValueTrait accessors. Objects iterate as (&str, &Value); arrays
// as &[Value]. Both object key ordering and array ordering preserve
// source order in sonic-rs 0.5's non-sort_keys build (the default).

fn sonic_project(v: &sonic_rs::Value) -> RefValue {
    match v.get_type() {
        JsonType::Null => RefValue::Null,
        JsonType::Boolean => RefValue::Bool(v.as_bool().expect("bool accessor")),
        JsonType::Number => {
            // Every JSON number projects to f64 through the reference
            // walker; mirror that on the sonic side so the comparison
            // stays bit-identical for every finite value.
            let f = v.as_f64().expect("number accessor");
            RefValue::Number(f)
        }
        JsonType::String => RefValue::String(v.as_str().expect("string accessor").to_string()),
        JsonType::Array => {
            let arr = v.as_array().expect("array accessor");
            let mut out = Vec::with_capacity(arr.len());
            for item in arr.iter() {
                out.push(sonic_project(item));
            }
            RefValue::Array(out)
        }
        JsonType::Object => {
            let obj = v.as_object().expect("object accessor");
            let mut out = Vec::with_capacity(obj.len());
            for (k, val) in obj.iter() {
                out.push((k.to_string(), sonic_project(val)));
            }
            RefValue::Object(out)
        }
    }
}

// ─── Recursive node-for-node comparator ──────────────────────────────
//
// The comparator traverses both trees in lockstep, recording the
// divergence path so a failure pinpoints the exact element. Object
// comparison is ORDER-SENSITIVE by key list — both parsers must
// preserve the source-order key sequence, which is the contract JSON
// itself honours even though the spec permits key re-ordering.
// Numbers compare via `to_bits` on finite values; NaN never appears
// in either parser's output on these corpora.

fn assert_value_eq(bbnf: &RefValue, sonic: &RefValue, path: &str) {
    match (bbnf, sonic) {
        (RefValue::Null, RefValue::Null) => {}
        (RefValue::Bool(a), RefValue::Bool(b)) => {
            assert_eq!(a, b, "bool mismatch at {path}: bbnf={a} sonic={b}")
        }
        (RefValue::Number(a), RefValue::Number(b)) => {
            if a.is_nan() || b.is_nan() {
                panic!("NaN at {path}: bbnf={a} sonic={b} (JSON disallows NaN)");
            }
            assert_eq!(
                a.to_bits(),
                b.to_bits(),
                "number mismatch at {path}: bbnf={a} (0x{:016x}) sonic={b} (0x{:016x})",
                a.to_bits(),
                b.to_bits(),
            )
        }
        (RefValue::String(a), RefValue::String(b)) => {
            assert_eq!(a, b, "string mismatch at {path}: bbnf={a:?} sonic={b:?}")
        }
        (RefValue::Array(a), RefValue::Array(b)) => {
            assert_eq!(
                a.len(),
                b.len(),
                "array length mismatch at {path}: bbnf={} sonic={}",
                a.len(),
                b.len(),
            );
            for (i, (x, y)) in a.iter().zip(b.iter()).enumerate() {
                let sub = format!("{path}[{i}]");
                assert_value_eq(x, y, &sub);
            }
        }
        (RefValue::Object(a), RefValue::Object(b)) => {
            assert_eq!(
                a.len(),
                b.len(),
                "object key-count mismatch at {path}: bbnf={} sonic={}",
                a.len(),
                b.len(),
            );
            for (i, ((ak, av), (bk, bv))) in a.iter().zip(b.iter()).enumerate() {
                assert_eq!(
                    ak, bk,
                    "object key mismatch at {path}[{i}]: bbnf={ak:?} sonic={bk:?}",
                );
                let sub = format!("{path}.{ak}");
                assert_value_eq(av, bv, &sub);
            }
        }
        _ => panic!(
            "variant mismatch at {path}: bbnf={:?} sonic={:?}",
            std::mem::discriminant(bbnf),
            std::mem::discriminant(sonic),
        ),
    }
}

fn run_parity(fixture: &str) {
    let path = format!("../../data/json/{}", fixture);
    let input =
        std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{path}: read failed: {e}"));
    let bbnf_value = bbnf_project(&input);
    let sonic_raw: sonic_rs::Value = sonic_rs::from_str(&input)
        .unwrap_or_else(|e| panic!("{fixture}: sonic-rs parse failed: {e}"));
    let sonic_value = sonic_project(&sonic_raw);
    assert_value_eq(&bbnf_value, &sonic_value, fixture);
}

// ─── Per-fixture parity tests ────────────────────────────────────────

#[test]
fn sonic_rs_parity_canada() {
    run_parity("canada.json");
}

#[test]
fn sonic_rs_parity_twitter() {
    run_parity("twitter.json");
}

#[test]
fn sonic_rs_parity_citm() {
    run_parity("citm_catalog.json");
}

#[test]
fn sonic_rs_parity_data() {
    run_parity("data.json");
}

#[test]
fn sonic_rs_parity_data_xl() {
    run_parity("data_xl.json");
}
