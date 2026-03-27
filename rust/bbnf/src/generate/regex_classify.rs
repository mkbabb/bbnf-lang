//! Structural regex classification for during-parse value conversion.
//!
//! Analyzes regex pattern strings to determine their semantic class
//! (numeric, quoted string, hex value, identifier) without requiring
//! exact pattern matching. This enables grammar-agnostic fused scanners
//! that convert values during parsing instead of post-hoc.

/// Classification result for a regex pattern.
#[derive(Debug, Clone, PartialEq)]
pub enum RegexClass {
    /// Matches numeric values convertible to f64.
    /// Pattern structure: `[+-]? digits (. digits)? ([eE] [+-]? digits)?`
    Numeric {
        allows_sign: bool,
        allows_fraction: bool,
        allows_exponent: bool,
    },

    /// Matches quoted strings: `"content"` or `'content'`.
    /// The content may include escape sequences.
    QuotedString {
        quote_char: u8,
        allows_escapes: bool,
    },

    /// Matches hex digit runs: `[0-9a-fA-F]+` or similar.
    HexDigits,

    /// Matches identifier-class tokens: `[a-zA-Z_][\w-]*` or similar.
    Identifier,

    /// Not classifiable — use general regex engine.
    Unknown,
}

/// Classify a regex pattern structurally.
///
/// Analyzes the pattern's components to determine if it matches a known
/// value class (numeric, string, hex, identifier) without requiring
/// exact string matching against a pattern list.
pub fn classify_regex(pattern: &str) -> RegexClass {
    // Try each classifier in order of specificity.
    if let Some(class) = try_classify_numeric(pattern) {
        return class;
    }
    if let Some(class) = try_classify_quoted_string(pattern) {
        return class;
    }
    if let Some(class) = try_classify_hex(pattern) {
        return class;
    }
    if let Some(class) = try_classify_identifier(pattern) {
        return class;
    }
    RegexClass::Unknown
}

/// Detect numeric patterns structurally.
///
/// Recognizes patterns like:
/// - `-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?`  (JSON number)
/// - `[-+]?(\d+)?(\.\d+)?([eE][-+]?\d+)?`     (CSS number)
/// - `[0-9]+(\.[0-9]+)?`                        (simple decimal)
/// - `[-+]?\d+`                                  (integer)
/// - `\d+`                                       (unsigned integer)
///
/// The detection is structural: it looks for optional-sign + digit-run +
/// optional-fraction + optional-exponent patterns, not exact strings.
fn try_classify_numeric(pattern: &str) -> Option<RegexClass> {
    let mut rest = pattern;
    let mut allows_sign = false;
    let mut allows_fraction = false;
    let mut allows_exponent = false;

    // Optional sign: [-+]?, [+-]?, -?
    if let Some(r) = strip_optional_sign(rest) {
        allows_sign = true;
        rest = r;
    }

    // Required or optional digits: \d+, [0-9]+, (\d+)?, etc.
    let digits_required;
    if let Some(r) = strip_digit_run(rest) {
        digits_required = true;
        rest = r;
    } else if let Some(r) = strip_optional_group(rest, strip_digit_run) {
        digits_required = false; // digits are optional — pattern may match empty
        rest = r;
    } else {
        // Also handle patterns like (0|[1-9]\d*) — JSON integer part
        if let Some(r) = strip_json_integer_part(rest) {
            digits_required = true;
            rest = r;
        } else if let Some(r) = strip_css_number_body(rest) {
            // CSS non-nullable number body: (\d+(\.\d+)?|\.\d+)
            // This inherently includes optional fraction, so set both flags.
            digits_required = true;
            allows_fraction = true;
            rest = r;
        } else {
            return None; // No digits at all — not numeric.
        }
    }

    // Optional fraction: (\.\d+)?, (\.[0-9]+)?
    if let Some(r) = strip_optional_group(rest, strip_fraction) {
        allows_fraction = true;
        rest = r;
    } else if let Some(r) = strip_fraction(rest) {
        allows_fraction = true;
        rest = r;
    }

    // Optional exponent: ([eE][+-]?\d+)?, ([eE][-+]?\d+)?
    if let Some(r) = strip_optional_group(rest, strip_exponent) {
        allows_exponent = true;
        rest = r;
    } else if let Some(r) = strip_exponent(rest) {
        allows_exponent = true;
        rest = r;
    }

    // Must have consumed the entire pattern.
    if !rest.is_empty() {
        return None;
    }

    // If digits are optional but fraction is present, the pattern can still match
    // `.5` (dot + digits) — the CSS number scanner handles this. Only reject if
    // NEITHER digits nor fraction would provide any content.
    if !digits_required && !allows_fraction {
        return None;
    }

    Some(RegexClass::Numeric {
        allows_sign,
        allows_fraction,
        allows_exponent,
    })
}

/// Detect quoted string patterns.
///
/// Recognizes: `"(?:[^"\\]|\\[\s\S])*"`, `'...'`, and variations.
fn try_classify_quoted_string(pattern: &str) -> Option<RegexClass> {
    let bytes = pattern.as_bytes();
    if bytes.len() < 3 {
        return None;
    }

    let quote = bytes[0];
    if quote != b'"' && quote != b'\'' {
        return None;
    }

    // Must end with the same quote.
    if *bytes.last()? != quote {
        return None;
    }

    // Check for escape handling in the middle.
    let middle = &pattern[1..pattern.len() - 1];
    let allows_escapes = middle.contains("\\\\") || middle.contains("\\[");

    Some(RegexClass::QuotedString {
        quote_char: quote,
        allows_escapes,
    })
}

/// Detect hex digit patterns: `[0-9a-fA-F]+`, `[0-9a-fA-F]{3,8}`, etc.
fn try_classify_hex(pattern: &str) -> Option<RegexClass> {
    let rest = pattern;
    // Must start with [0-9a-fA-F] or similar hex class.
    if !rest.starts_with("[0-9a-fA-F]") && !rest.starts_with("[a-fA-F0-9]") {
        return None;
    }
    // Must have a quantifier after.
    let after_class = &rest[11..]; // len("[0-9a-fA-F]") = 11
    if after_class == "+"
        || after_class == "*"
        || after_class.starts_with('{')
        || after_class.is_empty()
    {
        Some(RegexClass::HexDigits)
    } else {
        None
    }
}

/// Detect identifier-class patterns: `[a-zA-Z_][\w-]*`, `[a-zA-Z][\w-]*`, etc.
fn try_classify_identifier(pattern: &str) -> Option<RegexClass> {
    if !pattern.starts_with('[') {
        return None;
    }
    let close = pattern.find(']')?;
    let class = &pattern[1..close];
    // Must contain letter ranges.
    if !(class.contains("a-z") || class.contains("A-Z")) {
        return None;
    }
    // Rest should be [\w-]* or similar repeat.
    let rest = &pattern[close + 1..];
    if rest.is_empty() || rest == "+" || rest == "*" || rest.starts_with("[\\w") {
        Some(RegexClass::Identifier)
    } else {
        None
    }
}

// ── Helper functions for numeric pattern detection ──────────────────────────

/// Strip optional sign prefix: `[-+]?`, `[+-]?`, `-?`
fn strip_optional_sign(s: &str) -> Option<&str> {
    for prefix in &["[-+]?", "[+-]?", "[+\\-]?", "[\\-+]?", "-?", "\\+?"] {
        if let Some(r) = s.strip_prefix(prefix) {
            return Some(r);
        }
    }
    None
}

/// Strip a digit run: `\d+`, `[0-9]+`, `\d*`, `[0-9]*`
fn strip_digit_run(s: &str) -> Option<&str> {
    for prefix in &["\\d+", "[0-9]+", "\\d*", "[0-9]*"] {
        if let Some(r) = s.strip_prefix(prefix) {
            return Some(r);
        }
    }
    None
}

/// Strip a JSON integer part: `(0|[1-9]\d*)`, `(0|[1-9][0-9]*)`
fn strip_json_integer_part(s: &str) -> Option<&str> {
    for prefix in &[
        "(0|[1-9]\\d*)",
        "(0|[1-9][0-9]*)",
    ] {
        if let Some(r) = s.strip_prefix(prefix) {
            return Some(r);
        }
    }
    // Also handle bare digit run as fallback.
    strip_digit_run(s)
}

/// Strip a CSS non-nullable number body: `(\d+(\.\d+)?|\.\d+)` and variants.
///
/// Recognizes the alternation group that ensures at least one digit is present:
/// either digits with optional fraction, or a leading-dot fraction.
fn strip_css_number_body(s: &str) -> Option<&str> {
    // Each entry is the literal pattern string as it appears in the regex source.
    // `\d` = two chars (backslash + d), `\.` = two chars (backslash + dot).
    for prefix in &[
        r"(\d+(\.\d+)?|\.\d+)",
        r"([0-9]+(\.[0-9]+)?|\.[0-9]+)",
    ] {
        if let Some(r) = s.strip_prefix(prefix) {
            return Some(r);
        }
    }
    None
}

/// Strip a fraction part: `\.\d+`, `\.[0-9]+`
fn strip_fraction(s: &str) -> Option<&str> {
    for prefix in &["\\.\\d+", "\\.[0-9]+"] {
        if let Some(r) = s.strip_prefix(prefix) {
            return Some(r);
        }
    }
    None
}

/// Strip an exponent part: `[eE][+-]?\d+`, `[eE][-+]?\d+`
fn strip_exponent(s: &str) -> Option<&str> {
    for prefix in &[
        "[eE][+-]?\\d+",
        "[eE][-+]?\\d+",
        "[eE][+-]?[0-9]+",
        "[eE][-+]?[0-9]+",
    ] {
        if let Some(r) = s.strip_prefix(prefix) {
            return Some(r);
        }
    }
    None
}

/// Strip an optional group: `(content)?` where content matches the given parser.
fn strip_optional_group<'a>(s: &'a str, parser: fn(&str) -> Option<&str>) -> Option<&'a str> {
    let rest = s.strip_prefix('(')?;
    let inner = parser(rest)?;
    let rest = inner.strip_prefix(')')?;
    let rest = rest.strip_prefix('?')?;
    Some(rest)
}

