//! End-to-end corpus tests — prototype parses every JSON fixture
//! without error.
//!
//! Each test loads a fixture from `../../data/json/`, parses it via
//! the prototype's [`parse_json`] with [`ValueVisitor`], and asserts
//! the [`Document::root`] is `Some`. Smaller smoke tests exercise
//! scalar roots + error paths.

use json_prototype::{parse_json, Document, TapeVisitor, Value, ValueVisitor};

fn load(name: &str) -> String {
    let path = format!("../../data/json/{}", name);
    std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("{}: {}", path, e))
}

fn parse_doc(bytes: &[u8]) -> Document {
    let mut visitor = ValueVisitor::with_input(bytes);
    parse_json(bytes, &mut visitor).unwrap_or_else(|e| {
        panic!("parse failed: {:?}", e);
    });
    visitor.finish()
}

#[test]
fn parses_data_s() {
    let input = load("data.json");
    let doc = parse_doc(input.as_bytes());
    assert!(doc.root.is_some());
}

#[test]
fn parses_twitter() {
    let input = load("twitter.json");
    let doc = parse_doc(input.as_bytes());
    assert!(doc.root.is_some());
}

#[test]
fn parses_citm() {
    let input = load("citm_catalog.json");
    let doc = parse_doc(input.as_bytes());
    assert!(doc.root.is_some());
}

#[test]
fn parses_canada() {
    let input = load("canada.json");
    let doc = parse_doc(input.as_bytes());
    assert!(doc.root.is_some());
}

#[test]
fn parses_data_xl() {
    let input = load("data_xl.json");
    let doc = parse_doc(input.as_bytes());
    assert!(doc.root.is_some());
}

// ── TapeVisitor corpus smoke ──────────────────────────────────────

#[test]
fn tape_visitor_data_s() {
    let input = load("data.json");
    let mut visitor = TapeVisitor::new(input.as_bytes());
    parse_json(input.as_bytes(), &mut visitor).expect("data.json parses via tape");
    let tape = visitor.finish().expect("tape finalises");
    assert!(!tape.is_empty());
}

#[test]
fn tape_visitor_twitter() {
    let input = load("twitter.json");
    let mut visitor = TapeVisitor::new(input.as_bytes());
    parse_json(input.as_bytes(), &mut visitor).expect("twitter.json parses via tape");
    let tape = visitor.finish().expect("tape finalises");
    assert!(!tape.is_empty());
}

// ── Scalar-root smoke tests ─────────────────────────────────────

#[test]
fn parses_simple_number() {
    let doc = parse_doc(b"42");
    assert_eq!(doc.root.unwrap().as_f64(), Some(42.0));
}

#[test]
fn parses_negative_float() {
    let doc = parse_doc(b"-3.14");
    assert!((doc.root.unwrap().as_f64().unwrap() - -3.14).abs() < 1e-10);
}

#[test]
fn parses_exponent() {
    let doc = parse_doc(b"1.5e10");
    assert!((doc.root.unwrap().as_f64().unwrap() - 1.5e10).abs() < 1e-3);
}

#[test]
fn parses_bool_true() {
    let doc = parse_doc(b"true");
    assert_eq!(doc.root.unwrap().as_bool(), Some(true));
}

#[test]
fn parses_bool_false() {
    let doc = parse_doc(b"false");
    assert_eq!(doc.root.unwrap().as_bool(), Some(false));
}

#[test]
fn parses_null() {
    let doc = parse_doc(b"null");
    assert!(doc.root.unwrap().is_null());
}

#[test]
fn parses_empty_object() {
    let doc = parse_doc(b"{}");
    let obj = doc.root.unwrap().as_object_span().unwrap();
    assert_eq!(obj.subtree_len, 0);
}

#[test]
fn parses_empty_array() {
    let doc = parse_doc(b"[]");
    let arr = doc.root.unwrap().as_array_span().unwrap();
    assert_eq!(arr.subtree_len, 0);
}

#[test]
fn parses_nested_object() {
    let doc = parse_doc(br#"{"a":1,"b":[2,3],"c":null}"#);
    let root = doc.root.unwrap();
    let obj = root.as_object_span().unwrap();
    let entries: Vec<(&[u8], Value)> = doc.object_entries(obj).collect();
    assert_eq!(entries.len(), 3);
    assert_eq!(entries[0].0, b"a");
    assert_eq!(entries[0].1.as_f64(), Some(1.0));
    assert_eq!(entries[1].0, b"b");
    // Inner array has 2 elements (scalars).
    let arr_span = entries[1].1.as_array_span().unwrap();
    let arr_entries: Vec<Value> = doc.children(arr_span).collect();
    assert_eq!(arr_entries.len(), 2);
    assert_eq!(entries[2].0, b"c");
    assert!(entries[2].1.is_null());
}

#[test]
fn parses_escapes() {
    let doc = parse_doc(br#""\n\t\"\\""#);
    let root = doc.root.unwrap();
    let span = root.as_string_span().unwrap();
    assert_eq!(doc.str(span).unwrap(), "\n\t\"\\");
}

#[test]
fn parses_unicode_escape() {
    let doc = parse_doc(br#""\u00e9""#);
    let root = doc.root.unwrap();
    let span = root.as_string_span().unwrap();
    assert_eq!(doc.str(span).unwrap(), "é");
}

#[test]
fn rejects_trailing() {
    let mut visitor = ValueVisitor::new();
    let res = parse_json(b"1 2", &mut visitor);
    assert!(res.is_err(), "trailing content must error");
}

#[test]
fn rejects_unterminated_string() {
    let mut visitor = ValueVisitor::new();
    let res = parse_json(br#""no close"#, &mut visitor);
    assert!(res.is_err(), "unterminated string must error");
}
