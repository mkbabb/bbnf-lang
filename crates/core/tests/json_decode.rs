//! AZ-I.W2-act.B1 — JSON struct-direct string-decode round-trip tests.
//!
//! Asserts that a JSON string with escape sequences decodes through
//! the struct-direct parse path and surfaces as a borrowed UTF-8 slice
//! on the resulting [`JsonValue::String`]. The grammar's `string`
//! rule is annotated with `-> decode_json_string_to_arena(input) :
//! String`; the struct builder lands the decoded slice on the typed
//! tree directly — no tape walk required.
//!
//! Pre-W2-act this file walked tape spans + `payload_string_with_source`;
//! post-flip the tape substrate is severed for JSON, so the harness
//! recodes against the `JsonDocument` accessor surface.

use ::bbnf::grammar::generated::json::*;
use bbnf::runtime::{JsonDocument, JsonValue};

/// Walk the document recursively and collect every string leaf in
/// pre-order. Object keys are also strings; both keys and values
/// land in the returned vector in source order.
fn collect_strings(doc: &JsonDocument<'_>, value: &JsonValue<'_>, out: &mut Vec<String>) {
    match value {
        JsonValue::String(s) => out.push((*s).to_string()),
        JsonValue::Array(id) => {
            for item in doc.array(*id) {
                collect_strings(doc, item, out);
            }
        }
        JsonValue::Object(id) => {
            for pair in doc.object(*id) {
                out.push(pair.key.to_string());
                collect_strings(doc, &pair.value, out);
            }
        }
        _ => {}
    }
}

#[test]
fn decode_plain_string_round_trip() {
    // No escapes — borrow-safe leaf, the struct builder lands the
    // decoded body directly on the typed tree.
    let input = r#""hello""#;
    let doc = JsonParser::parse(input).expect("parse");
    let JsonValue::String(s) = doc.root else {
        panic!("expected String root, got {:?}", doc.root);
    };
    assert_eq!(s, "hello");
}

#[test]
fn decode_simple_escapes_round_trip() {
    // \n, \t, \r, \", \\, \/
    let input = r#""line1\nline2\t\"quoted\"\\\/""#;
    let doc = JsonParser::parse(input).expect("parse");
    let JsonValue::String(s) = doc.root else {
        panic!("expected String root, got {:?}", doc.root);
    };
    assert_eq!(s, "line1\nline2\t\"quoted\"\\/");
}

#[test]
fn decode_u_escape_round_trip() {
    // A = 'A'; é = 'é' (2-byte UTF-8).
    let input = r#""AAé""#;
    let doc = JsonParser::parse(input).expect("parse");
    let JsonValue::String(s) = doc.root else {
        panic!("expected String root, got {:?}", doc.root);
    };
    assert_eq!(s, "AAé");
}

#[test]
fn decode_surrogate_pair_round_trip() {
    // U+1F600 (grinning face emoji) encoded as 😀.
    let input = r#""😀""#;
    let doc = JsonParser::parse(input).expect("parse");
    let JsonValue::String(s) = doc.root else {
        panic!("expected String root, got {:?}", doc.root);
    };
    assert_eq!(s, "\u{1F600}");
}

#[test]
fn decode_json_object_string_keys_and_values() {
    // Nested structure — every string reachable via the document
    // walk resolves to its decoded UTF-8.
    let input = r#"{"key\n1": "valueé", "key2": "plain"}"#;
    let doc = JsonParser::parse(input).expect("parse");
    let mut collected = Vec::new();
    collect_strings(&doc, doc.to_value(), &mut collected);
    assert!(
        collected.contains(&"key\n1".to_string()),
        "key with escape must decode; got {collected:?}",
    );
    assert!(
        collected.contains(&"valueé".to_string()),
        "value with unicode escape must decode; got {collected:?}",
    );
    assert!(
        collected.contains(&"key2".to_string()),
        "plain key must round-trip; got {collected:?}",
    );
    assert!(
        collected.contains(&"plain".to_string()),
        "plain value must round-trip; got {collected:?}",
    );
}
