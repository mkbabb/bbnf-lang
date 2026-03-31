//! Tests for structural regex classification.

use bbnf::generate::regex_ir::classify::{classify_regex, RegexClass};

#[test]
fn json_number() {
    assert!(matches!(
        classify_regex(r"-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?"),
        RegexClass::Numeric { allows_sign: true, allows_fraction: true, allows_exponent: true }
    ));
}

#[test]
fn css_number_optional_digits() {
    // CSS number with optional digits but required fraction: `[-+]?(\d+)?(\.\d+)?`
    // Classified as Numeric because the fraction group provides content.
    // (The fast path is only used when allows_sign=false, so this is safe.)
    assert!(matches!(
        classify_regex(r"[-+]?(\d+)?(\.\d+)?([eE][-+]?\d+)?"),
        RegexClass::Numeric { allows_sign: true, allows_fraction: true, allows_exponent: true }
    ));
}

#[test]
fn css_number_required_digits() {
    // CSS number with required digits: `[-+]?\d+(\.\d+)?` — can't match empty.
    assert!(matches!(
        classify_regex(r"[-+]?\d+(\.\d+)?([eE][-+]?\d+)?"),
        RegexClass::Numeric { allows_sign: true, allows_fraction: true, allows_exponent: true }
    ));
}

#[test]
fn css_number_non_nullable() {
    // CSS non-nullable number: `[-+]?(\d+(\.\d+)?|\.\d+)([eE][-+]?\d+)?`
    // The alternation group ensures at least one digit is always present.
    assert!(matches!(
        classify_regex(r"[-+]?(\d+(\.\d+)?|\.\d+)([eE][-+]?\d+)?"),
        RegexClass::Numeric { allows_sign: true, allows_fraction: true, allows_exponent: true }
    ));
}

#[test]
fn simple_integer() {
    assert!(matches!(
        classify_regex(r"[-+]?\d+"),
        RegexClass::Numeric { allows_sign: true, allows_fraction: false, allows_exponent: false }
    ));
}

#[test]
fn unsigned_integer() {
    assert!(matches!(
        classify_regex(r"\d+"),
        RegexClass::Numeric { allows_sign: false, allows_fraction: false, allows_exponent: false }
    ));
}

#[test]
fn simple_decimal() {
    assert!(matches!(
        classify_regex(r"[0-9]+(\.[0-9]+)?"),
        RegexClass::Numeric { allows_sign: false, allows_fraction: true, allows_exponent: false }
    ));
}

#[test]
fn hex_digits() {
    assert_eq!(classify_regex(r"[0-9a-fA-F]+"), RegexClass::HexDigits);
    assert_eq!(classify_regex(r"[0-9a-fA-F]{3,8}"), RegexClass::HexDigits);
}

#[test]
fn identifier() {
    assert_eq!(classify_regex(r"[a-zA-Z_][\w-]*"), RegexClass::Identifier);
    assert_eq!(classify_regex(r"[a-zA-Z][\w-]*"), RegexClass::Identifier);
}

#[test]
fn quoted_string() {
    assert!(matches!(
        classify_regex(r#""(?:[^"\\]|\\[\s\S])*""#),
        RegexClass::QuotedString { quote_char: b'"', .. }
    ));
}

#[test]
fn unknown_patterns() {
    assert_eq!(classify_regex(r"[^{};]+"), RegexClass::Unknown);
    assert_eq!(classify_regex(r"(?s)(?:\s|\/\*.*?\*\/)*"), RegexClass::Unknown);
}
