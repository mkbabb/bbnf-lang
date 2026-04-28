//! AZ-I.W2-act.B1 — JSON struct-direct parity vs sonic-rs.
//!
//! Post-flip, `JsonParser::parse(input)` returns
//! `Result<JsonDocument<'_>, ParseErr>`. This harness compares the
//! struct-direct document tree against `sonic_rs::Value` node-for-node
//! across the canonical fixture corpus. Equality is structural — every
//! `JsonValue` variant maps to the matching `sonic_rs::Value` shape;
//! nested objects compare by ordered key/value walk; numbers compare
//! via f64 conversion (sonic-rs preserves arbitrary precision, but
//! the JSON grammar today projects through `-> f64` so equality at
//! the f64 layer is the canonical scalar comparison).
//!
//! Pre-W2-act this file walked tape spans + `payload_*` accessors;
//! post-flip the tape substrate is severed for JSON, so the harness
//! recodes against the `JsonDocument` accessor surface — no tape
//! symbols.

use ::bbnf::grammar::generated::json::*;
use bbnf::runtime::{JsonNumber, JsonPair, JsonValue};

/// Walk a `JsonDocument` against a `sonic_rs::Value` recursively.
/// Returns `Ok(())` on structural equality, `Err(path)` on any
/// divergence with the path-to-divergence as a string for diagnostic
/// surfacing.
fn assert_struct_eq_sonic(
    doc: &bbnf::runtime::JsonDocument<'_>,
    bbnf_value: &JsonValue<'_>,
    oracle: &sonic_rs::Value,
    path: &str,
) {
    use sonic_rs::JsonValueTrait;
    match bbnf_value {
        JsonValue::Null => {
            assert!(
                oracle.is_null(),
                "{path}: bbnf=Null but sonic-rs={:?}",
                oracle.get_type(),
            );
        }
        JsonValue::Bool(b) => {
            let o = oracle.as_bool().unwrap_or_else(|| {
                panic!("{path}: bbnf=Bool({b}) but sonic-rs={:?}", oracle.get_type())
            });
            assert_eq!(*b, o, "{path}: bool divergence");
        }
        JsonValue::Number(n) => {
            let oracle_f64 = oracle.as_f64().unwrap_or_else(|| {
                panic!(
                    "{path}: bbnf=Number({:?}) but sonic-rs={:?} (not numeric)",
                    n,
                    oracle.get_type(),
                )
            });
            let bbnf_f64 = n.as_f64();
            // Reduce to f64-equality. Both sides feed through f64 codecs
            // (sonic-rs's `as_f64` and bbnf's `JsonNumber::as_f64`); the
            // comparison is canonical at this layer.
            if bbnf_f64.is_nan() {
                assert!(
                    oracle_f64.is_nan(),
                    "{path}: bbnf NaN but sonic-rs={oracle_f64}",
                );
            } else {
                assert_eq!(bbnf_f64, oracle_f64, "{path}: number divergence");
            }
        }
        JsonValue::String(s) => {
            let o = oracle.as_str().unwrap_or_else(|| {
                panic!(
                    "{path}: bbnf=String({s:?}) but sonic-rs={:?}",
                    oracle.get_type()
                )
            });
            assert_eq!(*s, o, "{path}: string divergence");
        }
        JsonValue::Array(id) => {
            let items = doc.array(*id);
            let oracle_arr = oracle.as_array().unwrap_or_else(|| {
                panic!(
                    "{path}: bbnf=Array(len={}) but sonic-rs={:?}",
                    items.len(),
                    oracle.get_type(),
                )
            });
            assert_eq!(
                items.len(),
                oracle_arr.len(),
                "{path}: array length divergence",
            );
            for (i, (bbnf_elem, oracle_elem)) in items.iter().zip(oracle_arr.iter()).enumerate() {
                let child_path = format!("{path}[{i}]");
                assert_struct_eq_sonic(doc, bbnf_elem, oracle_elem, &child_path);
            }
        }
        JsonValue::Object(id) => {
            let pairs = doc.object(*id);
            let oracle_obj = oracle.as_object().unwrap_or_else(|| {
                panic!(
                    "{path}: bbnf=Object(len={}) but sonic-rs={:?}",
                    pairs.len(),
                    oracle.get_type(),
                )
            });
            assert_eq!(
                pairs.len(),
                oracle_obj.len(),
                "{path}: object length divergence",
            );
            // sonic-rs preserves insertion order on serialised parses;
            // walk both sides in order. JSON spec does not mandate
            // object-key ordering, but the tape-substrate-and-oracle
            // round-trip preserves source order on both sides for
            // every JSON parser that does not deduplicate keys, so
            // the comparison is total.
            for (i, (bbnf_pair, (oracle_key, oracle_value))) in
                pairs.iter().zip(oracle_obj.iter()).enumerate()
            {
                let child_path = format!("{path}.{}", bbnf_pair.key);
                assert_eq!(
                    bbnf_pair.key, oracle_key,
                    "{path}: pair[{i}] key divergence",
                );
                assert_struct_eq_sonic(doc, &bbnf_pair.value, oracle_value, &child_path);
            }
        }
    }
}

fn parity_against_sonic(fixture: &str) {
    let path = format!("../../data/json/{}", fixture);
    let src = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("{path}: read failed: {e}"));

    let doc = JsonParser::parse(&src)
        .unwrap_or_else(|e| panic!("{fixture}: bbnf JSON parse failed: {e:?}"));
    let oracle: sonic_rs::Value = sonic_rs::from_str(&src)
        .unwrap_or_else(|e| panic!("{fixture}: sonic-rs parse failed: {e}"));

    assert_struct_eq_sonic(&doc, doc.to_value(), &oracle, "$");
}

// ─── Per-fixture parity tests ────────────────────────────────────────

#[test]
fn parity_data_json() {
    parity_against_sonic("data.json");
}

#[test]
fn parity_twitter_json() {
    parity_against_sonic("twitter.json");
}

#[test]
fn parity_citm_catalog_json() {
    parity_against_sonic("citm_catalog.json");
}

#[test]
fn parity_canada_json() {
    parity_against_sonic("canada.json");
}

// ─── Typed-payload activation tests ──────────────────────────────────
//
// The typed-leaf assertions stay structural — every grammar `->` arm
// must reach a typed JsonValue variant. The oracle here is the
// post-flip `JsonDocument` itself; the test pins each arm by checking
// the expected variant lands at the expected path through the tree.

#[test]
fn null_materialises_to_null_value() {
    let doc = JsonParser::parse("null").expect("parse");
    assert!(matches!(doc.root, JsonValue::Null));
}

#[test]
fn bool_true_materialises_to_bool_true() {
    let doc = JsonParser::parse("true").expect("parse");
    assert!(matches!(doc.root, JsonValue::Bool(true)));
}

#[test]
fn bool_false_materialises_to_bool_false() {
    let doc = JsonParser::parse("false").expect("parse");
    assert!(matches!(doc.root, JsonValue::Bool(false)));
}

#[test]
fn number_materialises_to_f64() {
    // Mix integer, decimal, negative, and scientific to exercise the
    // full regex range covered by the `-> f64` annotation.
    let doc = JsonParser::parse("[0, 1, -2.5, 1e10, -1.5e-3]").expect("parse");
    let JsonValue::Array(id) = doc.root else {
        panic!("expected array root, got {:?}", doc.root);
    };
    let items = doc.array(id);
    assert_eq!(items.len(), 5, "every JSON number must materialise an f64");
    let nums: Vec<f64> = items
        .iter()
        .map(|v| match v {
            JsonValue::Number(n) => n.as_f64(),
            other => panic!("expected Number, got {other:?}"),
        })
        .collect();
    assert_eq!(nums[0], 0.0);
    assert_eq!(nums[1], 1.0);
    assert_eq!(nums[2], -2.5);
    assert_eq!(nums[3], 1e10);
    assert!((nums[4] - -1.5e-3).abs() < 1e-12);
}

#[test]
fn string_materialises_decoded_bytes() {
    // Plain + escape + unicode escapes exercise all three decode paths.
    let doc = JsonParser::parse(r#"["plain", "with\nnewline", "é"]"#).expect("parse");
    let JsonValue::Array(id) = doc.root else {
        panic!("expected array root, got {:?}", doc.root);
    };
    let items = doc.array(id);
    assert_eq!(items.len(), 3, "every JSON string must materialise");
    let strs: Vec<&str> = items
        .iter()
        .map(|v| match v {
            JsonValue::String(s) => *s,
            other => panic!("expected String, got {other:?}"),
        })
        .collect();
    assert_eq!(strs[0], "plain");
    assert_eq!(strs[1], "with\nnewline");
    assert_eq!(strs[2], "é");
}

#[test]
fn object_keys_and_values_decode() {
    let input = r#"{"a\n": 1, "b": "vA"}"#;
    let doc = JsonParser::parse(input).expect("parse");
    let JsonValue::Object(id) = doc.root else {
        panic!("expected object root, got {:?}", doc.root);
    };
    let pairs: &[JsonPair<'_>] = doc.object(id);
    assert_eq!(pairs.len(), 2, "two pairs");
    assert_eq!(pairs[0].key, "a\n", "first key with escape must decode");
    let JsonValue::Number(n0) = pairs[0].value else {
        panic!("expected Number, got {:?}", pairs[0].value);
    };
    assert_eq!(n0.as_f64(), 1.0);
    assert_eq!(pairs[1].key, "b", "plain key must round-trip");
    let JsonValue::String(s1) = pairs[1].value else {
        panic!("expected String, got {:?}", pairs[1].value);
    };
    assert_eq!(s1, "vA", "value with unicode escape must decode");
}

#[test]
fn nested_object_preserves_typed_payloads() {
    // A realistic nested JSON object exercises every typed annotation
    // in one input: null, booleans, numbers, strings.
    let input = r#"{"nulls":[null,null],"bools":[false,true],"nums":[0,1,-2.5],"strs":["a","b\t"]}"#;
    let doc = JsonParser::parse(input).expect("parse");

    // Walk the document recursively and collect every leaf's value.
    let mut nulls = 0usize;
    let mut bools = Vec::new();
    let mut nums = Vec::new();
    let mut strings: Vec<String> = Vec::new();
    walk_leaves(
        &doc,
        doc.to_value(),
        &mut nulls,
        &mut bools,
        &mut nums,
        &mut strings,
    );

    assert_eq!(nulls, 2, "two nulls must materialise");
    assert_eq!(bools, vec![false, true]);
    assert_eq!(nums, vec![0.0, 1.0, -2.5]);
    // Keys are also strings in this corpus; the walk yields each key
    // as a string leaf alongside the values.
    for key in ["nulls", "bools", "nums", "strs", "a", "b\t"] {
        assert!(
            strings.contains(&key.to_string()),
            "leaf {key:?} must round-trip; got {strings:?}",
        );
    }
}

fn walk_leaves(
    doc: &bbnf::runtime::JsonDocument<'_>,
    value: &JsonValue<'_>,
    nulls: &mut usize,
    bools: &mut Vec<bool>,
    nums: &mut Vec<f64>,
    strings: &mut Vec<String>,
) {
    match value {
        JsonValue::Null => *nulls += 1,
        JsonValue::Bool(b) => bools.push(*b),
        JsonValue::Number(n) => nums.push(n.as_f64()),
        JsonValue::String(s) => strings.push((*s).to_string()),
        JsonValue::Array(id) => {
            for item in doc.array(*id) {
                walk_leaves(doc, item, nulls, bools, nums, strings);
            }
        }
        JsonValue::Object(id) => {
            for pair in doc.object(*id) {
                strings.push(pair.key.to_string());
                walk_leaves(doc, &pair.value, nulls, bools, nums, strings);
            }
        }
    }
}

#[test]
fn every_declared_leaf_reaches_the_document() {
    // A value of each primitive type — every `->` annotation must
    // produce a typed materialisation in the JsonDocument tree.
    let doc = JsonParser::parse(r#"[null, true, false, 0, -1, 1.5, "s"]"#).expect("parse");
    let JsonValue::Array(id) = doc.root else {
        panic!("expected array root, got {:?}", doc.root);
    };
    let items = doc.array(id);
    assert_eq!(items.len(), 7, "all seven typed leaves must materialise");
    assert!(matches!(items[0], JsonValue::Null));
    assert!(matches!(items[1], JsonValue::Bool(true)));
    assert!(matches!(items[2], JsonValue::Bool(false)));
    assert!(matches!(items[3], JsonValue::Number(JsonNumber::Float(v)) if v == 0.0));
    assert!(matches!(items[4], JsonValue::Number(JsonNumber::Float(v)) if v == -1.0));
    assert!(matches!(items[5], JsonValue::Number(JsonNumber::Float(v)) if v == 1.5));
    assert!(matches!(items[6], JsonValue::String(s) if s == "s"));
}

#[test]
fn integer_witness_supported_in_struct_layer() {
    // The runtime JsonNumber sum carries `Int` / `UInt` / `Float`
    // variants; the grammar's `-> f64` annotation projects through
    // `Float` today, but the substrate must accept the integral
    // witnesses for future grammar refinements. Pin both shapes via
    // the builder API so the runtime contract stays stable.
    use bbnf::runtime::{JsonStructBuilder, StructBuilder};
    let mut b = JsonStructBuilder::new();
    b.push_leaf_with_i64(-42);
    let doc = b.finalise();
    assert!(matches!(doc.root, JsonValue::Number(JsonNumber::Int(-42))));

    let mut b = JsonStructBuilder::new();
    b.push_leaf_with_u64(99);
    let doc = b.finalise();
    assert!(matches!(doc.root, JsonValue::Number(JsonNumber::UInt(99))));
}
