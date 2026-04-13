//! Tests for structural regex classification.

use parse_that::regex::classify::{RegexClass, classify_regex};

#[test]
fn json_number_known() {
    assert!(matches!(
        classify_regex(r"-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?"),
        RegexClass::Numeric {
            allows_sign: true,
            allows_fraction: true,
            allows_exponent: true,
            reject_leading_zero: true,
            allow_leading_dot: false,
        }
    ));
}

#[test]
fn css_number_required_digits() {
    assert!(matches!(
        classify_regex(r"[-+]?\d+(\.\d+)?([eE][-+]?\d+)?"),
        RegexClass::Numeric {
            allows_sign: true,
            allows_fraction: true,
            allows_exponent: true,
            ..
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
            allows_exponent: true,
            allow_leading_dot: true,
            ..
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
            allows_exponent: false,
            ..
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
            allows_exponent: false,
            ..
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
            allows_exponent: false,
            ..
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
    assert!(matches!(
        classify_regex(r"[a-zA-Z_][\w-]*"),
        RegexClass::Identifier {
            allows_leading_dash: false,
            allows_double_dash_prefix: false,
            allows_escapes: false,
        }
    ));
    assert!(matches!(
        classify_regex(r"[a-zA-Z][\w-]*"),
        RegexClass::Identifier {
            allows_leading_dash: false,
            allows_double_dash_prefix: false,
            allows_escapes: false,
        }
    ));
}

#[test]
fn identifier_structural() {
    assert!(matches!(
        classify_regex(r"[a-zA-Z_][a-zA-Z0-9]*"),
        RegexClass::Identifier { .. }
    ));
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
    assert!(matches!(
        classify_regex(r#""(?:[^"\\]|\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4}))*""#),
        RegexClass::QuotedString {
            quote_char: b'"',
            allows_escapes: true,
            allows_u_escapes: true,
        }
    ));
    assert_eq!(
        classify_regex(r"(?s)(?:\s|/\*.*?\*/)*"),
        RegexClass::WhitespaceWithBlockComment,
    );
    assert!(matches!(
        classify_regex(r#""(?:[^"\\]|\\[\s\S])*"|'(?:[^'\\]|\\[\s\S])*'"#),
        RegexClass::QuotedString {
            quote_char: b'"',
            allows_escapes: true,
            allows_u_escapes: false,
        }
    ));
}

#[test]
fn unknown_patterns() {
    assert_eq!(classify_regex(r"(?:foo|bar|baz)+"), RegexClass::Unknown);
}
