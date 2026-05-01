//! AZ-I.W2-act.close A.fix — multi-pair JSON object regression probe.
//!
//! Per `docs/tranches/AZ-I/PROGRESS.md` lines 935-989 (W2-act.recovery
//! scope-reveal), the JSON struct-direct emitter calls
//! `parse_string_JsonParser_string` directly against the open Object
//! frame (no enclosing Pair sub-frame): the key arrives as a deposited
//! `JsonValue::String` BEFORE the value, then the value lands on the
//! next deposit. The pre-fix `OpenFrame::Object::deposit` only
//! CONSUMED `pending_key`, never POPULATED it — so the second pair's
//! key push tripped the "Object value pushed without pending key"
//! assertion.
//!
//! The fix promotes `JsonValue::String` deposits into `pending_key`
//! when the slot is empty. This probe parses a 5-pair object literal
//! through the struct-direct path and asserts every pair lands in
//! source order with the correct keys + values.

use bbnf::grammar::generated::json::JsonParser;
use bbnf::runtime::{JsonNumber, JsonValue};

/// Five `(key, value)` pairs through the JSON struct-direct path.
///
/// The fix gate: pre-fix, this parse panicked at the second pair's
/// value-push (`b: 2`) because `pending_key` was None when the value
/// arrived. Post-fix, the string-key deposit promotes into
/// `pending_key` so the matching value lands cleanly.
#[test]
fn five_pair_object_resolves_in_order() {
    let input = r#"{"a":1,"b":2,"c":3,"d":4,"e":5}"#;
    let doc = JsonParser::parse(input).expect("parse 5-pair object");

    let JsonValue::Object(id) = doc.root else {
        panic!("expected object root, got {:?}", doc.root);
    };

    let pairs = doc.object(id);
    assert_eq!(pairs.len(), 5, "5-pair object must resolve to five pairs");

    let expected: [(&str, f64); 5] = [("a", 1.0), ("b", 2.0), ("c", 3.0), ("d", 4.0), ("e", 5.0)];
    for (i, (key, num)) in expected.iter().enumerate() {
        let pair = &pairs[i];
        assert_eq!(pair.key, *key, "pair #{i} key");
        match pair.value {
            JsonValue::Number(JsonNumber::Float(v)) => {
                assert!(
                    (v - num).abs() < f64::EPSILON,
                    "pair #{i} value: expected {num}, got {v}",
                );
            }
            other => panic!("pair #{i} value: expected Number, got {other:?}"),
        }
    }
}

/// Sanity probe: a single-pair object continues to parse correctly
/// post-fix. The fix only adds a new branch (string-deposit promoting
/// into `pending_key`) and must not regress the single-pair shape.
#[test]
fn single_pair_object_unchanged_by_fix() {
    let input = r#"{"k":42}"#;
    let doc = JsonParser::parse(input).expect("parse single-pair object");

    let JsonValue::Object(id) = doc.root else {
        panic!("expected object root, got {:?}", doc.root);
    };
    let pairs = doc.object(id);
    assert_eq!(pairs.len(), 1);
    assert_eq!(pairs[0].key, "k");
    assert!(matches!(
        pairs[0].value,
        JsonValue::Number(JsonNumber::Float(v)) if (v - 42.0).abs() < f64::EPSILON
    ));
}
