//! Tests for structural regex classification.

use bbnf::generate::regex_ir::classify::{RegexClass, classify_regex};

#[test]
fn json_number_known() {
    assert_eq!(
        classify_regex(r"-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?"),
        RegexClass::JsonNumber,
    );
}

#[test]
fn css_number_required_digits() {
    assert!(matches!(
        classify_regex(r"[-+]?\d+(\.\d+)?([eE][-+]?\d+)?"),
        RegexClass::Numeric {
            allows_sign: true,
            allows_fraction: true,
            allows_exponent: true
        }
    ));
}

#[test]
fn css_number_non_nullable() {
    assert!(matches!(
        classify_regex(r"[-+]?(\d+(\.\d+)?|\.\d+)([eE][-+]?\d+)?"),
        RegexClass::Numeric {
            allows_sign: true,
            allows_fraction: true,
            allows_exponent: true
        }
    ));
}

#[test]
fn simple_integer() {
    assert!(matches!(
        classify_regex(r"[-+]?\d+"),
        RegexClass::Numeric {
            allows_sign: true,
            allows_fraction: false,
            allows_exponent: false
        }
    ));
}

#[test]
fn unsigned_integer() {
    assert!(matches!(
        classify_regex(r"\d+"),
        RegexClass::Numeric {
            allows_sign: false,
            allows_fraction: false,
            allows_exponent: false
        }
    ));
}

#[test]
fn simple_decimal() {
    assert!(matches!(
        classify_regex(r"[0-9]+(\.[0-9]+)?"),
        RegexClass::Numeric {
            allows_sign: false,
            allows_fraction: true,
            allows_exponent: false
        }
    ));
}

#[test]
fn hex_digits() {
    assert_eq!(classify_regex(r"[0-9a-fA-F]+"), RegexClass::HexDigits);
    assert_eq!(classify_regex(r"[0-9a-fA-F]{3,8}"), RegexClass::HexDigits);
}

#[test]
fn identifier_known() {
    assert_eq!(classify_regex(r"[a-zA-Z_][\w-]*"), RegexClass::CssIdent);
    assert_eq!(classify_regex(r"[a-zA-Z][\w-]*"), RegexClass::CssIdent);
}

#[test]
fn identifier_structural() {
    assert_eq!(
        classify_regex(r"[a-zA-Z_][a-zA-Z0-9]*"),
        RegexClass::Identifier
    );
}

#[test]
fn quoted_string() {
    assert!(matches!(
        classify_regex(r#""(?:[^"\\]|\\[\s\S])*""#),
        RegexClass::QuotedString {
            quote_char: b'"',
            ..
        }
    ));
}

#[test]
fn known_patterns() {
    assert_eq!(
        classify_regex(r#""(?:[^"\\]|\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4}))*""#),
        RegexClass::JsonString,
    );
    assert_eq!(
        classify_regex(r"(?s)(?:\s|/\*.*?\*/)*"),
        RegexClass::WsBlockComment,
    );
    assert_eq!(
        classify_regex(r#""(?:[^"\\]|\\[\s\S])*"|'(?:[^'\\]|\\[\s\S])*'"#),
        RegexClass::CssQuotedString,
    );
}

#[test]
fn unknown_patterns() {
    assert_eq!(classify_regex(r"(?:foo|bar|baz)+"), RegexClass::Unknown);
}
