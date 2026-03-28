//! Pattern detection functions for well-known regex patterns.
//!
//! Detects canonical string/number, identifier, whitespace+comment, and
//! quoted-string patterns by exact match against known pattern arrays.

/// Known exact JSON string regex patterns → `scan_json_string` fast-path.
const JSON_STRING_PATTERNS: &[&str] = &[
    r#""(?:[^"\\]|\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4}))*""#,
    r#""(?:[^"\\]|\\(?:["\\\/bfnrt]|u[0-9A-Fa-f]{4}))*""#,
    r#""(?:[^"\\]|\\(?:["\\bfnrt]|u[0-9a-fA-F]{4}))*""#,
];

/// Known exact JSON number regex patterns → `scan_number_span_json` fast-path.
const JSON_NUMBER_PATTERNS: &[&str] = &[
    r"-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?",
    r"-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?",
];

/// Detect a JSON string regex pattern.
pub(crate) fn is_json_string_pattern(pattern: &str) -> bool {
    JSON_STRING_PATTERNS.contains(&pattern)
}

/// Detect a JSON number regex pattern.
pub(crate) fn is_json_number_pattern(pattern: &str) -> bool {
    JSON_NUMBER_PATTERNS.contains(&pattern)
}

/// Known whitespace + block-comment regex patterns → `scan_ws_block_comments`.
const WS_BLOCK_COMMENT_PATTERNS: &[&str] =
    &[r"(?s)(?:\s|/\*.*?\*/)*", r"(?s)(?:\s|\/\*.*?\*\/)*"];

/// Detect a whitespace+block-comment regex pattern.
pub(crate) fn is_ws_block_comment_pattern(pattern: &str) -> bool {
    WS_BLOCK_COMMENT_PATTERNS.contains(&pattern)
}

/// Known identifier regex patterns → `scan_ident`.
const IDENT_PATTERNS: &[&str] = &[
    r"[\-]?[a-zA-Z_][\w-]*|--[\w-]+",
    r"-?[a-zA-Z_][\w-]*|--[\w-]+",
    r"[a-zA-Z_][\w-]*|--[\w-]+|-[a-zA-Z][\w-]*",
    r"[a-zA-Z_][\w-]*",
    r"[a-zA-Z][\w-]*",
];

/// Detect an identifier regex pattern.
pub(crate) fn is_ident_pattern(pattern: &str) -> bool {
    IDENT_PATTERNS.contains(&pattern)
}

/// Known quoted-string regex patterns → `scan_string_quoted`.
const QUOTED_STRING_PATTERNS: &[&str] = &[r#""(?:[^"\\]|\\[\s\S])*"|'(?:[^'\\]|\\[\s\S])*'"#];

/// Detect a quoted-string regex pattern.
pub(crate) fn is_quoted_string_pattern(pattern: &str) -> bool {
    QUOTED_STRING_PATTERNS.contains(&pattern)
}
