use gorgeous::PrinterConfig;
use gorgeous::json::prettify_json;

#[test]
fn test_prettify_null() {
    let config = PrinterConfig::default();
    let result = prettify_json("null", &config).unwrap();
    assert_eq!(result.trim(), "null");
}

#[test]
fn test_prettify_string() {
    let config = PrinterConfig::default();
    let result = prettify_json(r#""hello""#, &config).unwrap();
    assert_eq!(result.trim(), r#""hello""#);
}

#[test]
fn test_prettify_number() {
    let config = PrinterConfig::default();
    let result = prettify_json("42", &config).unwrap();
    assert_eq!(result.trim(), "42");
}

#[test]
fn test_prettify_simple_array() {
    let config = PrinterConfig::default();
    let result = prettify_json("[1, 2, 3]", &config).unwrap();
    assert!(result.contains("1"));
    assert!(result.contains("2"));
    assert!(result.contains("3"));
}

#[test]
fn test_prettify_simple_object() {
    let config = PrinterConfig::default();
    let result = prettify_json(r#"{"a": 1, "b": 2}"#, &config).unwrap();
    assert!(result.contains(r#""a""#));
    assert!(result.contains("1"));
}

#[test]
fn test_idempotent() {
    let config = PrinterConfig::default();
    let input = r#"{"key": [1, 2, {"nested": true}]}"#;
    let first = prettify_json(input, &config).unwrap();
    let second = prettify_json(&first, &config).unwrap();
    assert_eq!(first, second, "prettify should be idempotent");
}

#[test]
fn test_nested_json_output() {
    let config = PrinterConfig::default();
    let input = r#"{"users": [{"id": 1, "name": "Alice", "email": "alice@example.com", "active": true}, {"id": 2, "name": "Bob", "email": "bob@example.com", "active": false}], "total": 2, "page": 1}"#;
    let result = prettify_json(input, &config).unwrap();
    // Verify idempotency
    let second = prettify_json(&result, &config).unwrap();
    assert_eq!(result, second, "nested JSON prettify should be idempotent");
}

#[test]
fn test_minified_roundtrip() {
    let config = PrinterConfig::default();
    let minified = r#"{"a":1,"b":[2,3],"c":{"d":"e"}}"#;
    let result = prettify_json(minified, &config).unwrap();
    // Minified should parse and produce valid output.
    assert!(result.contains(r#""a""#));
    assert!(result.contains(r#""d""#));
    // Should be idempotent.
    let second = prettify_json(&result, &config).unwrap();
    assert_eq!(
        result, second,
        "minified JSON prettify should be idempotent"
    );
}

#[test]
fn test_pretty_preserves_spacing() {
    let config = PrinterConfig::default();
    let pretty = r#"{ "a": 1, "b": [2, 3], "c": { "d": "e" } }"#;
    let result = prettify_json(pretty, &config).unwrap();
    // Spaces around : and , should be preserved.
    assert!(result.contains(": 1"), "should preserve space after colon");
    assert!(result.contains(", "), "should preserve space after comma");
    // Should be idempotent.
    let second = prettify_json(&result, &config).unwrap();
    assert_eq!(result, second, "pretty JSON should be idempotent");
}
