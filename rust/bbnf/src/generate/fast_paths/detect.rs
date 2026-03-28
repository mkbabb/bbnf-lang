//! Pattern detection functions for well-known regex patterns.
//!
//! Detects canonical JSON string/number and CSS ident/ws/string patterns
//! by exact match against known pattern arrays.

/// Known exact JSON string regex patterns that can be replaced with the
/// `sp_json_string_quoted()` SIMD fast-path.
const JSON_STRING_REGEX_PATTERNS: &[&str] = &[
    r#""(?:[^"\\]|\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4}))*""#,
    r#""(?:[^"\\]|\\(?:["\\\/bfnrt]|u[0-9A-Fa-f]{4}))*""#,
    r#""(?:[^"\\]|\\(?:["\\bfnrt]|u[0-9a-fA-F]{4}))*""#,
];

/// Known exact JSON number regex patterns that can be replaced with the
/// `sp_json_number()` monolithic byte-loop fast-path.
const JSON_NUMBER_REGEX_PATTERNS: &[&str] = &[
    r"-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?",
    r"-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?",
];

/// Detect the canonical JSON string regex pattern.
pub(crate) fn is_json_string_regex(pattern: &str) -> bool {
    JSON_STRING_REGEX_PATTERNS.contains(&pattern)
}

/// Detect the canonical JSON number regex.
pub(crate) fn is_json_number_regex(pattern: &str) -> bool {
    JSON_NUMBER_REGEX_PATTERNS.contains(&pattern)
}

/// Known CSS whitespace+comment regex patterns.
const CSS_WS_COMMENT_REGEX_PATTERNS: &[&str] =
    &[r"(?s)(?:\s|/\*.*?\*/)*", r"(?s)(?:\s|\/\*.*?\*\/)*"];

/// Detect the canonical CSS whitespace+comment regex.
pub(crate) fn is_css_ws_comment_regex(pattern: &str) -> bool {
    CSS_WS_COMMENT_REGEX_PATTERNS.contains(&pattern)
}

/// Known CSS identifier regex patterns.
const CSS_IDENT_REGEX_PATTERNS: &[&str] = &[
    r"[\-]?[a-zA-Z_][\w-]*|--[\w-]+",
    r"-?[a-zA-Z_][\w-]*|--[\w-]+",
    r"[a-zA-Z_][\w-]*|--[\w-]+|-[a-zA-Z][\w-]*",
    r"[a-zA-Z_][\w-]*",
    r"[a-zA-Z][\w-]*",
];

/// Detect a CSS identifier regex.
pub(crate) fn is_css_ident_regex(pattern: &str) -> bool {
    CSS_IDENT_REGEX_PATTERNS.contains(&pattern)
}

/// Known CSS string regex patterns.
const CSS_STRING_REGEX_PATTERNS: &[&str] = &[r#""(?:[^"\\]|\\[\s\S])*"|'(?:[^'\\]|\\[\s\S])*'"#];

/// Detect a CSS string regex.
pub(crate) fn is_css_string_regex(pattern: &str) -> bool {
    CSS_STRING_REGEX_PATTERNS.contains(&pattern)
}
