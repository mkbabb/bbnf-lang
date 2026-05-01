//! AZ-I.W2-act.B1 — JSON struct-direct canonical-form parity vs
//! sonic-rs.
//!
//! Post-flip, `JsonParser::parse(input)` returns
//! `Result<JsonDocument<'_>, ParseErr>`. This harness renders the
//! struct-direct document through a local compact serializer and
//! compares its bytes against `sonic_rs::to_string(&Value)` after
//! both pass through the shared `strip_insignificant_ws` normalizer.
//! Symmetric byte-equality is the gate.
//!
//! The local serializer mirrors sonic-rs's compact form: no
//! whitespace outside string literals, escape sequences re-emitted
//! per RFC 8259 (the JSON parse round-trip preserves non-ASCII bytes
//! through the `decode_json_string_to_arena(input): String` shape).
//! Any byte divergence between the bbnf side and the sonic-rs side
//! after normalisation indicates a struct-tree shape regression.

mod common;

use common::json_normalize::strip_insignificant_ws;

use ::bbnf::grammar::generated::json::*;
use bbnf::runtime::{JsonDocument, JsonNumber, JsonValue};

/// Serialise a `JsonDocument` to a compact JSON byte form. The output
/// has no insignificant whitespace; numbers feed through Rust's
/// default `f64` Display per the f64 projection in the grammar; strings
/// re-emit per RFC 8259 with the canonical escape set.
fn serialize_compact_doc(doc: &JsonDocument<'_>) -> String {
    let mut buf = String::new();
    serialize_value(doc, doc.to_value(), &mut buf);
    buf
}

fn serialize_value(doc: &JsonDocument<'_>, value: &JsonValue<'_>, out: &mut String) {
    match value {
        JsonValue::Null => out.push_str("null"),
        JsonValue::Bool(true) => out.push_str("true"),
        JsonValue::Bool(false) => out.push_str("false"),
        JsonValue::Number(n) => serialize_number(*n, out),
        JsonValue::String(s) => serialize_string(s, out),
        JsonValue::Array(id) => {
            out.push('[');
            for (i, item) in doc.array(*id).iter().enumerate() {
                if i > 0 {
                    out.push(',');
                }
                serialize_value(doc, item, out);
            }
            out.push(']');
        }
        JsonValue::Object(id) => {
            out.push('{');
            for (i, pair) in doc.object(*id).iter().enumerate() {
                if i > 0 {
                    out.push(',');
                }
                serialize_string(pair.key, out);
                out.push(':');
                serialize_value(doc, &pair.value, out);
            }
            out.push('}');
        }
    }
}

fn serialize_number(n: JsonNumber, out: &mut String) {
    use std::fmt::Write;
    match n {
        JsonNumber::Int(v) => write!(out, "{v}").expect("write to string"),
        JsonNumber::UInt(v) => write!(out, "{v}").expect("write to string"),
        JsonNumber::Float(v) => {
            // Rust's default f64 Display matches sonic-rs's compact
            // float output for canonical inputs (both produce the
            // shortest round-trip representation modulo trailing
            // `.0` choice). The shared normalizer's number pass folds
            // both forms into a single canonical representation, so
            // the per-side rendering choice does not block parity.
            write!(out, "{v}").expect("write to string")
        }
    }
}

fn serialize_string(s: &str, out: &mut String) {
    out.push('"');
    for ch in s.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\x08' => out.push_str("\\b"),
            '\x0c' => out.push_str("\\f"),
            c if (c as u32) < 0x20 => {
                use std::fmt::Write;
                write!(out, "\\u{:04x}", c as u32).expect("write to string");
            }
            c => out.push(c),
        }
    }
    out.push('"');
}

fn canonical_parity(fixture: &str) {
    let path = format!("../../data/json/{}", fixture);
    let src = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{path}: read failed: {e}"));

    // bbnf side: parse through the struct-direct JsonDocument, render
    // compact, normalize.
    let doc = JsonParser::parse(&src)
        .unwrap_or_else(|e| panic!("{fixture}: bbnf JSON parse failed: {e:?}"));
    let bbnf_raw = serialize_compact_doc(&doc);
    let bbnf_canonical = strip_insignificant_ws(&bbnf_raw);

    // Oracle side: parse through sonic-rs, render via to_string,
    // normalize. The `strip_insignificant_ws` pass is byte-symmetric;
    // both sides feed through the same normalizer so equality is
    // structural.
    let oracle = sonic_rs::from_str::<sonic_rs::Value>(&src)
        .unwrap_or_else(|e| panic!("{fixture}: sonic-rs parse failed: {e}"));
    let oracle_raw = sonic_rs::to_string(&oracle)
        .unwrap_or_else(|e| panic!("{fixture}: sonic-rs to_string failed: {e}"));
    let oracle_canonical = strip_insignificant_ws(&oracle_raw);

    assert_eq!(
        bbnf_canonical, oracle_canonical,
        "canonical-form divergence on {fixture}",
    );
}

// ─── Per-fixture tests ──────────────────────────────────────────────

#[test]
fn canonical_parity_data() {
    canonical_parity("data.json");
}

#[test]
fn canonical_parity_twitter() {
    canonical_parity("twitter.json");
}

#[test]
fn canonical_parity_citm_catalog() {
    canonical_parity("citm_catalog.json");
}

#[test]
fn canonical_parity_canada() {
    canonical_parity("canada.json");
}

/// `data_xl.json` is ~21 MB; reading + two parses + two normalizes is
/// slow under the dev/ax-iter profile. Skip in debug builds; run only
/// under release to keep the iteration loop tight.
#[test]
#[cfg_attr(debug_assertions, ignore)]
fn canonical_parity_data_xl() {
    canonical_parity("data_xl.json");
}

// ─── Normalizer self-tests ──────────────────────────────────────────
//
// The normalizer is load-bearing: both sides of the parity comparison
// feed through it, so correctness is a prerequisite for any parity
// claim. Kept in the tests/ tree (no inline #[cfg(test)] in the helper
// module) per the workspace's test-placement invariant.

#[test]
fn normalize_strips_space_between_tokens() {
    assert_eq!(strip_insignificant_ws("{ }"), "{}");
    assert_eq!(strip_insignificant_ws("[ 1 , 2 ]"), "[1,2]");
    assert_eq!(
        strip_insignificant_ws(r#"{"a" : 1 , "b" : 2}"#),
        r#"{"a":1,"b":2}"#
    );
}

#[test]
fn normalize_preserves_whitespace_inside_strings() {
    assert_eq!(
        strip_insignificant_ws(r#""hello world""#),
        r#""hello world""#
    );
    assert_eq!(
        strip_insignificant_ws(r#"{"k" : "v 1 2"}"#),
        r#"{"k":"v 1 2"}"#
    );
}

#[test]
fn normalize_preserves_escaped_quote_inside_string() {
    let input = r#"{"k" : "a\"b"}"#;
    let expected = r#"{"k":"a\"b"}"#;
    assert_eq!(strip_insignificant_ws(input), expected);
}

#[test]
fn normalize_preserves_backslash_before_escaped_backslash() {
    let input = r#"{"k":"a\\b c"}"#;
    let expected = r#"{"k":"a\\b c"}"#;
    assert_eq!(strip_insignificant_ws(input), expected);
}

#[test]
fn normalize_identity_on_already_compact() {
    let input = r#"[{"a":1,"b":[2,3]},null,true,false,"x"]"#;
    assert_eq!(strip_insignificant_ws(input), input);
}

#[test]
fn normalize_strips_newlines_and_tabs() {
    let input = "{\n\t\"a\": 1\n}";
    assert_eq!(strip_insignificant_ws(input), r#"{"a":1}"#);
}
