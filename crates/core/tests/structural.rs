//! AZ-I.W2-act.recovery — deep structural validation tests for the
//! JSON struct-direct parse path.
//!
//! These tests parse known JSON inputs via `JsonParser::parse`, walk
//! the resulting `JsonDocument`'s typed value tree, and assert specific
//! structural properties: typed kind discrimination, child counts,
//! span text recovery, and bounds. This fills the gap where
//! `tape_parity` (BBNF-only post-AZ-I.W2-act) checks only root record +
//! total count, and `grammar_roundtrip` checks only rule counts.
//!
//! # Migration history
//!
//! Pre-AZ-I.W2-act these tests asserted on `parsed.tape()` /
//! `view.cursor()` accessors against the tape substrate. AZ-I.W2-act
//! flipped JSON to the struct-direct path — `JsonParser::parse`
//! returns a `JsonDocument` with no tape, no cursor. The migrated
//! assertions walk the typed value tree directly: object pair lookups
//! by key, array element indexing, scalar shape kind-tags. Tests whose
//! pre-flip assertion was tape-shape-specific with no equivalent
//! struct-tree invariant (e.g. record-count thresholds) project to
//! the structural invariant the typed tree exposes (object pair count,
//! array element count, etc.).

use ::bbnf::grammar::generated::json::*;
use bbnf::runtime::{JsonValue, JsonNumber, JsonView};

// ── Helpers ────────────────────────────────────────────────────────────

/// Walk a `JsonValue` and count every reachable node (compound + leaf).
/// Mirrors the pre-flip `count_reachable(cursor)` walker shape, lifted
/// off the struct tree.
fn count_reachable<'p>(view: &JsonView<'_, 'p>, value: &JsonValue<'p>) -> usize {
    match value {
        JsonValue::Null
        | JsonValue::Bool(_)
        | JsonValue::Number(_)
        | JsonValue::String(_) => 1,
        JsonValue::Array(id) => {
            1 + view
                .array(*id)
                .iter()
                .map(|child| count_reachable(view, child))
                .sum::<usize>()
        }
        JsonValue::Object(id) => {
            1 + view
                .object(*id)
                .iter()
                .map(|pair| count_reachable(view, &pair.value))
                .sum::<usize>()
        }
    }
}

/// `true` if any reachable string scalar in the tree contains `needle`.
/// Replaces the pre-flip `tape_contains_substr` walker that searched
/// span-text on every record; the struct tree's strings are the
/// scalar-projection equivalent.
fn tree_contains_string<'p>(
    view: &JsonView<'_, 'p>,
    value: &JsonValue<'p>,
    needle: &str,
) -> bool {
    match value {
        JsonValue::String(s) => s.contains(needle),
        JsonValue::Array(id) => view
            .array(*id)
            .iter()
            .any(|child| tree_contains_string(view, child, needle)),
        JsonValue::Object(id) => view.object(*id).iter().any(|pair| {
            pair.key.contains(needle) || tree_contains_string(view, &pair.value, needle)
        }),
        _ => false,
    }
}

/// `true` if any reachable number scalar matches `target` exactly.
fn tree_contains_number<'p>(
    view: &JsonView<'_, 'p>,
    value: &JsonValue<'p>,
    target: f64,
) -> bool {
    match value {
        JsonValue::Number(n) => match n {
            JsonNumber::Float(f) => *f == target,
            JsonNumber::Int(i) => (*i as f64) == target,
            JsonNumber::UInt(u) => (*u as f64) == target,
        },
        JsonValue::Array(id) => view
            .array(*id)
            .iter()
            .any(|child| tree_contains_number(view, child, target)),
        JsonValue::Object(id) => view
            .object(*id)
            .iter()
            .any(|pair| tree_contains_number(view, &pair.value, target)),
        _ => false,
    }
}

/// Try to load a data file from standard candidate locations.
fn load_data(name: &str) -> Option<String> {
    let candidates = [
        format!("../../data/{}", name),
        format!("../data/{}", name),
        format!("data/{}", name),
    ];
    candidates
        .iter()
        .find_map(|p| std::fs::read_to_string(p).ok())
}

// ── Test: object with array value ──────────────────────────────────────

#[test]
fn structural_object_with_array() {
    let input = r#"{"key": [1, true, null]}"#;
    let doc = JsonParser::parse(input)
        .unwrap_or_else(|e| panic!("parse failed: {:?}", e));
    let view = doc.view();

    // Root must be an object compound, not a leaf.
    assert!(
        view.is_object(),
        "root should be an object compound, got {:?}",
        view.kind()
    );

    // The object should carry exactly one pair (the "key" entry).
    let pairs = match doc.root {
        JsonValue::Object(id) => view.object(id),
        _ => unreachable!(),
    };
    assert_eq!(pairs.len(), 1, "root object should have 1 pair");
    assert_eq!(pairs[0].key, "key");

    // The pair's value should be an array of three elements.
    let elements = match pairs[0].value {
        JsonValue::Array(id) => view.array(id),
        ref other => panic!("expected pair value to be array, got {:?}", other),
    };
    assert_eq!(elements.len(), 3, "expected 3 array elements");

    // Walk the tree and verify total node count matches the structure
    // (1 root + 1 array + 3 elements = 5 nodes, mirroring the pre-flip
    // ≥ 5 record-count invariant).
    let total = count_reachable(&view, view.root());
    assert!(
        total >= 5,
        "expected at least 5 nodes for '{}', got {}",
        input,
        total
    );
}

// ── Test: array with 3 numbers ─────────────────────────────────────────

#[test]
fn structural_array_three_numbers() {
    let input = "[1, 2, 3]";
    let doc = JsonParser::parse(input)
        .unwrap_or_else(|e| panic!("parse failed: {:?}", e));
    let view = doc.view();

    assert!(view.is_array(), "root should be an array, got {:?}", view.kind());

    let elements = match doc.root {
        JsonValue::Array(id) => view.array(id),
        _ => unreachable!(),
    };
    assert_eq!(elements.len(), 3, "expected 3 array elements");

    // Total reachable: array root + 3 number scalars = 4 nodes.
    let total = count_reachable(&view, view.root());
    assert!(
        total >= 4,
        "expected at least 4 nodes for '{}', got {}",
        input,
        total
    );
}

// ── Test: object with 2 pairs ──────────────────────────────────────────

#[test]
fn structural_object_two_pairs() {
    let input = r#"{"a": 1, "b": "hello"}"#;
    let doc = JsonParser::parse(input)
        .unwrap_or_else(|e| panic!("parse failed: {:?}", e));
    let view = doc.view();

    assert!(view.is_object(), "root should be an object");

    let pairs = match doc.root {
        JsonValue::Object(id) => view.object(id),
        _ => unreachable!(),
    };
    assert_eq!(pairs.len(), 2, "expected 2 object pairs");

    // Pair-by-pair shape verification.
    assert_eq!(pairs[0].key, "a");
    assert!(matches!(pairs[0].value, JsonValue::Number(_)));
    assert_eq!(pairs[1].key, "b");
    assert!(matches!(pairs[1].value, JsonValue::String("hello")));

    let total = count_reachable(&view, view.root());
    assert!(
        total >= 5,
        "expected at least 5 nodes for '{}', got {}",
        input,
        total
    );

    // Verify expected leaves appear in the tree. Pre-flip these were
    // span-substring lookups against tape records; the struct tree's
    // typed scalars surface the same content directly.
    assert!(
        tree_contains_string(&view, view.root(), "hello"),
        "tree should contain string 'hello'"
    );
    assert!(
        tree_contains_number(&view, view.root(), 1.0),
        "tree should contain number 1"
    );
}

// ── Test: nested objects ───────────────────────────────────────────────

#[test]
fn structural_nested_objects() {
    let input = r#"{"outer": {"inner": 42}}"#;
    let doc = JsonParser::parse(input)
        .unwrap_or_else(|e| panic!("parse failed: {:?}", e));
    let view = doc.view();

    assert!(view.is_object(), "root should be an object");

    let outer_pairs = match doc.root {
        JsonValue::Object(id) => view.object(id),
        _ => unreachable!(),
    };
    assert_eq!(outer_pairs.len(), 1);
    assert_eq!(outer_pairs[0].key, "outer");

    let inner_pairs = match outer_pairs[0].value {
        JsonValue::Object(id) => view.object(id),
        ref other => panic!("expected nested object, got {:?}", other),
    };
    assert_eq!(inner_pairs.len(), 1);
    assert_eq!(inner_pairs[0].key, "inner");
    assert!(matches!(inner_pairs[0].value, JsonValue::Number(_)));

    // 1 outer + 1 inner + 1 number = 3 distinct nodes; with pair edges
    // surfacing through the count_reachable walker (which recurses into
    // pair.value), we should see ≥ 3 nodes.
    let total = count_reachable(&view, view.root());
    assert!(
        total >= 3,
        "expected at least 3 nodes for nested object, got {}",
        total
    );

    // The number 42 should be present.
    assert!(
        tree_contains_number(&view, view.root(), 42.0),
        "nested tree should contain 42"
    );
}

// ── Test: empty containers ─────────────────────────────────────────────

#[test]
fn structural_empty_array() {
    let input = "[]";
    let doc = JsonParser::parse(input)
        .unwrap_or_else(|e| panic!("parse failed: {:?}", e));
    let view = doc.view();

    assert!(view.is_array(), "root should be an empty array");

    let elements = match doc.root {
        JsonValue::Array(id) => view.array(id),
        _ => unreachable!(),
    };
    assert_eq!(elements.len(), 0, "empty array should have 0 elements");
}

#[test]
fn structural_empty_object() {
    let input = "{}";
    let doc = JsonParser::parse(input)
        .unwrap_or_else(|e| panic!("parse failed: {:?}", e));
    let view = doc.view();

    assert!(view.is_object(), "root should be an empty object");

    let pairs = match doc.root {
        JsonValue::Object(id) => view.object(id),
        _ => unreachable!(),
    };
    assert_eq!(pairs.len(), 0, "empty object should have 0 pairs");
}

// ── Test: all scalar types ─────────────────────────────────────────────

#[test]
fn structural_scalar_null() {
    let input = "null";
    let doc = JsonParser::parse(input)
        .unwrap_or_else(|e| panic!("parse failed: {:?}", e));
    assert!(doc.view().is_null(), "expected null");
}

#[test]
fn structural_scalar_bool_true() {
    let input = "true";
    let doc = JsonParser::parse(input)
        .unwrap_or_else(|e| panic!("parse failed: {:?}", e));
    assert_eq!(doc.root, JsonValue::Bool(true));
}

#[test]
fn structural_scalar_bool_false() {
    let input = "false";
    let doc = JsonParser::parse(input)
        .unwrap_or_else(|e| panic!("parse failed: {:?}", e));
    assert_eq!(doc.root, JsonValue::Bool(false));
}

#[test]
fn structural_scalar_number_integer() {
    let input = "42";
    let doc = JsonParser::parse(input)
        .unwrap_or_else(|e| panic!("parse failed: {:?}", e));
    match doc.root {
        JsonValue::Number(n) => assert_eq!(n.as_f64(), 42.0),
        ref other => panic!("expected number, got {:?}", other),
    }
}

#[test]
fn structural_scalar_number_negative_float() {
    let input = "-3.14";
    let doc = JsonParser::parse(input)
        .unwrap_or_else(|e| panic!("parse failed: {:?}", e));
    match doc.root {
        JsonValue::Number(n) => assert!((n.as_f64() - (-3.14)).abs() < 1e-9),
        ref other => panic!("expected number, got {:?}", other),
    }
}

#[test]
fn structural_scalar_number_exponent() {
    let input = "1e10";
    let doc = JsonParser::parse(input)
        .unwrap_or_else(|e| panic!("parse failed: {:?}", e));
    match doc.root {
        JsonValue::Number(n) => assert_eq!(n.as_f64(), 1e10),
        ref other => panic!("expected number, got {:?}", other),
    }
}

#[test]
fn structural_scalar_string() {
    let input = r#""hello world""#;
    let doc = JsonParser::parse(input)
        .unwrap_or_else(|e| panic!("parse failed: {:?}", e));
    match doc.root {
        JsonValue::String(s) => assert_eq!(s, "hello world"),
        ref other => panic!("expected string, got {:?}", other),
    }
}

#[test]
fn structural_scalar_string_with_escapes() {
    let input = r#""line\nbreak""#;
    let doc = JsonParser::parse(input)
        .unwrap_or_else(|e| panic!("parse failed: {:?}", e));
    // The arena-decoded string should resolve the `\n` escape to a
    // literal newline byte. The struct-direct decoder runs the same
    // `decode_json_string_to_arena` kernel the pre-flip path used.
    match doc.root {
        JsonValue::String(s) => assert_eq!(s, "line\nbreak"),
        ref other => panic!("expected string, got {:?}", other),
    }
}

// ── Test: large file structural sanity ─────────────────────────────────

#[test]
fn structural_data_json_sanity() {
    let input = match load_data("json/data.json") {
        Some(s) => s,
        None => {
            eprintln!("[structural] skipping data.json sanity: data file not found");
            return;
        }
    };

    let doc = JsonParser::parse(&input)
        .unwrap_or_else(|e| panic!("data.json parse failed: {:?}", e));
    let view = doc.view();

    // Total reachable nodes — data.json should produce a substantial
    // tree (the pre-flip baseline asserted ≥ 10 records; the struct
    // tree projection retains every typed leaf + compound, so the
    // floor still holds).
    let total = count_reachable(&view, view.root());
    assert!(
        total >= 10,
        "data.json tree too small: {} nodes",
        total
    );

    // Root should be a compound (object or array), not a scalar leaf.
    assert!(
        view.is_object() || view.is_array(),
        "data.json root should be compound, got {:?}",
        view.kind()
    );

    // Non-empty children invariant — the root compound has at least
    // one child (data.json's top-level array carries entries).
    let child_count = match doc.root {
        JsonValue::Array(id) => view.array(id).len(),
        JsonValue::Object(id) => view.object(id).len(),
        _ => 0,
    };
    assert!(
        child_count >= 1,
        "data.json root should have at least 1 child"
    );
}

// ── Test: reject malformed input ───────────────────────────────────────

#[test]
fn structural_reject_malformed() {
    let malformed = [
        r#"{"key": [1, 2,"#,     // truncated array
        r#"{"key":"#,             // truncated object
        "",                       // empty input
    ];
    for input in malformed {
        let result = JsonParser::parse(input);
        assert!(
            result.is_err(),
            "expected parse failure for malformed input: {:?}",
            input
        );
    }
}
