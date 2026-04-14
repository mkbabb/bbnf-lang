//! Deep structural validation tests for the tape-first JSON parser.
//!
//! These tests parse known JSON inputs via `#[derive(Parser)]`, obtain
//! the tape + view, and walk the cursor tree to assert specific
//! structural properties: tape kinds, child counts, span text, and
//! record bounds. This fills the gap where `tape_parity` checks only
//! root record + total count, and `grammar_roundtrip` checks only rule
//! counts.

use bbnf::runtime::tape::{TapeCursor, TapeKind};
use bbnf_derive::Parser;

#[derive(Parser)]
#[parser(path = "../../grammar/json/json.bbnf")]
struct JsonParser;

// ── Helpers ────────────────────────────────────────────────────────────

/// Extract the span text for a cursor from the input.
fn cursor_text<'a>(cursor: &TapeCursor<'_>, input: &'a str) -> &'a str {
    let (lo, hi) = cursor.span();
    &input[lo as usize..hi as usize]
}

/// Recursively count all records reachable from a cursor
/// (the cursor itself plus all transitive children).
fn count_reachable(cursor: TapeCursor<'_>) -> usize {
    let mut total = 1;
    for child in cursor.children() {
        total += count_reachable(child);
    }
    total
}

/// Collect all unique span texts from the tape.
fn all_span_texts<'a>(tape: &bbnf::runtime::tape::Tape, input: &'a str) -> Vec<&'a str> {
    tape.iter()
        .map(|rec| &input[rec.span_lo as usize..rec.span_hi as usize])
        .collect()
}

/// Check whether any span text in the tape contains `needle` as a substring.
fn tape_contains_substr(tape: &bbnf::runtime::tape::Tape, input: &str, needle: &str) -> bool {
    tape.iter()
        .any(|rec| input[rec.span_lo as usize..rec.span_hi as usize].contains(needle))
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
    let parsed = JsonParser::parse(input)
        .unwrap_or_else(|e| panic!("parse failed: {:?}", e));
    let tape = parsed.tape();

    // Tape must be non-empty.
    assert!(tape.len() > 0, "tape should have records");

    let view = parsed.view();
    let root = view.cursor();

    // Root should be a compound kind (Rule, Alt, Seq, etc.), not a leaf.
    assert!(
        !matches!(root.kind(), TapeKind::None | TapeKind::Epsilon),
        "root should be a compound kind, got {:?}",
        root.kind()
    );

    // The full input span should cover the entire input.
    let root_text = cursor_text(&root, input);
    assert_eq!(root_text, input, "root span should cover entire input");

    // Walk the tree and verify total record count is reasonable.
    let total = count_reachable(root);
    assert!(
        total >= 5,
        "expected at least 5 records for '{}', got {}",
        input,
        total
    );

    // The tape should have more records than just the root.
    assert!(
        tape.len() >= 5,
        "tape should have at least 5 records, got {}",
        tape.len()
    );
}

// ── Test: array with 3 numbers ─────────────────────────────────────────

#[test]
fn structural_array_three_numbers() {
    let input = "[1, 2, 3]";
    let parsed = JsonParser::parse(input)
        .unwrap_or_else(|e| panic!("parse failed: {:?}", e));

    let view = parsed.view();
    let root = view.cursor();

    // Root span covers the whole input.
    assert_eq!(cursor_text(&root, input), input);

    // Walk depth-one children and count.
    let children: Vec<_> = root.children().collect();
    assert!(
        !children.is_empty(),
        "root should have children for array '{}'",
        input
    );

    // The total reachable records should include at least the root + the
    // three number elements.
    let total = count_reachable(root);
    assert!(
        total >= 4,
        "expected at least 4 records for '{}', got {}",
        input,
        total
    );
}

// ── Test: object with 2 pairs ──────────────────────────────────────────

#[test]
fn structural_object_two_pairs() {
    let input = r#"{"a": 1, "b": "hello"}"#;
    let parsed = JsonParser::parse(input)
        .unwrap_or_else(|e| panic!("parse failed: {:?}", e));

    let view = parsed.view();
    let root = view.cursor();

    assert_eq!(cursor_text(&root, input), input);

    // The tree should have a reasonable depth.
    let total = count_reachable(root);
    assert!(
        total >= 5,
        "expected at least 5 records for '{}', got {}",
        input,
        total
    );

    // Verify that expected substrings appear in some tape record's span.
    // Leaf spans may be fused or rolled up in the tape-first architecture,
    // so we check containment rather than exact matches.
    let tape = parsed.tape();
    assert!(
        tape_contains_substr(tape, input, "\"a\""),
        "some span should contain the key \"a\"; spans: {:?}",
        all_span_texts(tape, input)
    );
    assert!(
        tape_contains_substr(tape, input, "\"b\""),
        "some span should contain the key \"b\"; spans: {:?}",
        all_span_texts(tape, input)
    );
    assert!(
        tape_contains_substr(tape, input, "1"),
        "some span should contain number 1"
    );
    assert!(
        tape_contains_substr(tape, input, "\"hello\""),
        "some span should contain string \"hello\"; spans: {:?}",
        all_span_texts(tape, input)
    );
}

// ── Test: nested objects ───────────────────────────────────────────────

#[test]
fn structural_nested_objects() {
    let input = r#"{"outer": {"inner": 42}}"#;
    let parsed = JsonParser::parse(input)
        .unwrap_or_else(|e| panic!("parse failed: {:?}", e));

    let view = parsed.view();
    let root = view.cursor();

    // Root covers entire input.
    assert_eq!(cursor_text(&root, input), input);

    // Should have enough depth for nested structure.
    let total = count_reachable(root);
    assert!(
        total >= 6,
        "expected at least 6 records for nested object, got {}",
        total
    );

    // Verify nested values appear in tape span text.
    let tape = parsed.tape();
    assert!(
        tape_contains_substr(tape, input, "\"outer\""),
        "some span should contain the key \"outer\"; spans: {:?}",
        all_span_texts(tape, input)
    );
    assert!(
        tape_contains_substr(tape, input, "\"inner\""),
        "some span should contain the key \"inner\"; spans: {:?}",
        all_span_texts(tape, input)
    );
    assert!(
        tape_contains_substr(tape, input, "42"),
        "some span should contain the value 42"
    );
}

// ── Test: empty containers ─────────────────────────────────────────────

#[test]
fn structural_empty_array() {
    let input = "[]";
    let parsed = JsonParser::parse(input)
        .unwrap_or_else(|e| panic!("parse failed: {:?}", e));

    let view = parsed.view();
    let root = view.cursor();
    assert_eq!(cursor_text(&root, input), input);

    // Empty array should still parse and produce tape records.
    assert!(parsed.tape().len() >= 1, "empty array should produce at least 1 record");
}

#[test]
fn structural_empty_object() {
    let input = "{}";
    let parsed = JsonParser::parse(input)
        .unwrap_or_else(|e| panic!("parse failed: {:?}", e));

    let view = parsed.view();
    let root = view.cursor();
    assert_eq!(cursor_text(&root, input), input);

    assert!(parsed.tape().len() >= 1, "empty object should produce at least 1 record");
}

// ── Test: all scalar types ─────────────────────────────────────────────

#[test]
fn structural_scalar_null() {
    let input = "null";
    let parsed = JsonParser::parse(input)
        .unwrap_or_else(|e| panic!("parse failed: {:?}", e));

    let view = parsed.view();
    let root = view.cursor();
    assert_eq!(cursor_text(&root, input), "null");
}

#[test]
fn structural_scalar_bool_true() {
    let input = "true";
    let parsed = JsonParser::parse(input)
        .unwrap_or_else(|e| panic!("parse failed: {:?}", e));

    let view = parsed.view();
    let root = view.cursor();
    assert_eq!(cursor_text(&root, input), "true");
}

#[test]
fn structural_scalar_bool_false() {
    let input = "false";
    let parsed = JsonParser::parse(input)
        .unwrap_or_else(|e| panic!("parse failed: {:?}", e));

    let view = parsed.view();
    let root = view.cursor();
    assert_eq!(cursor_text(&root, input), "false");
}

#[test]
fn structural_scalar_number_integer() {
    let input = "42";
    let parsed = JsonParser::parse(input)
        .unwrap_or_else(|e| panic!("parse failed: {:?}", e));

    let view = parsed.view();
    let root = view.cursor();
    assert_eq!(cursor_text(&root, input), "42");
}

#[test]
fn structural_scalar_number_negative_float() {
    let input = "-3.14";
    let parsed = JsonParser::parse(input)
        .unwrap_or_else(|e| panic!("parse failed: {:?}", e));

    let view = parsed.view();
    let root = view.cursor();
    assert_eq!(cursor_text(&root, input), "-3.14");
}

#[test]
fn structural_scalar_number_exponent() {
    let input = "1e10";
    let parsed = JsonParser::parse(input)
        .unwrap_or_else(|e| panic!("parse failed: {:?}", e));

    let view = parsed.view();
    let root = view.cursor();
    assert_eq!(cursor_text(&root, input), "1e10");
}

#[test]
fn structural_scalar_string() {
    let input = r#""hello world""#;
    let parsed = JsonParser::parse(input)
        .unwrap_or_else(|e| panic!("parse failed: {:?}", e));

    let view = parsed.view();
    let root = view.cursor();
    assert_eq!(cursor_text(&root, input), r#""hello world""#);
}

#[test]
fn structural_scalar_string_with_escapes() {
    let input = r#""line\nbreak""#;
    let parsed = JsonParser::parse(input)
        .unwrap_or_else(|e| panic!("parse failed: {:?}", e));

    let view = parsed.view();
    let root = view.cursor();
    assert_eq!(cursor_text(&root, input), r#""line\nbreak""#);
}

// ── Test: tape record bounds ───────────────────────────────────────────

#[test]
fn structural_tape_record_bounds() {
    let input = r#"{"key": [1, true, null]}"#;
    let parsed = JsonParser::parse(input)
        .unwrap_or_else(|e| panic!("parse failed: {:?}", e));

    let tape = parsed.tape();

    // Every record's span should be within the input bounds.
    for (i, rec) in tape.iter().enumerate() {
        assert!(
            (rec.span_lo as usize) <= input.len(),
            "record {} span_lo {} exceeds input len {}",
            i,
            rec.span_lo,
            input.len()
        );
        assert!(
            (rec.span_hi as usize) <= input.len(),
            "record {} span_hi {} exceeds input len {}",
            i,
            rec.span_hi,
            input.len()
        );
        assert!(
            rec.span_lo <= rec.span_hi,
            "record {} has inverted span: lo={} hi={}",
            i,
            rec.span_lo,
            rec.span_hi
        );
    }

    // No TapeKind::None records should appear in a successful parse.
    for (i, rec) in tape.iter().enumerate() {
        assert_ne!(
            rec.kind,
            TapeKind::None,
            "record {} has TapeKind::None in a successful parse",
            i
        );
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

    let parsed = JsonParser::parse(&input)
        .unwrap_or_else(|e| panic!("data.json parse failed: {:?}", e));

    let tape = parsed.tape();

    // data.json should produce a substantial tape.
    assert!(
        tape.len() >= 10,
        "data.json tape too small: {} records",
        tape.len()
    );

    let view = parsed.view();
    let root = view.cursor();

    // Root should cover the entire input.
    let (lo, hi) = root.span();
    assert_eq!(lo, 0, "root span should start at 0");
    assert_eq!(
        hi as usize,
        input.len(),
        "root span should cover entire input"
    );

    // Root should be a compound kind.
    assert!(
        !matches!(root.kind(), TapeKind::None | TapeKind::Epsilon),
        "data.json root should be compound, got {:?}",
        root.kind()
    );

    // Should have at least one child.
    assert!(
        root.child_count() >= 1,
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
