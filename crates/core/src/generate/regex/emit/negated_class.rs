//! Negated character class detection and whitespace-padded literal detection.
//!
//! Handles patterns like `[^XYZ]+` / `[^XYZ]*` for `memchr`-based scanning,
//! and `\s*LITERAL\s*` for whitespace-padded literal matching.

/// Whether a negated character class uses `+` (one-or-more) or `*` (zero-or-more).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NegCharClassQuantifier {
    Plus,
    Star,
}

/// Detect a negated character class regex of the form `[^XYZ]+` or `[^XYZ]*`
/// and return the excluded bytes and quantifier. These patterns scan until any
/// excluded byte is found — suited for `take_until_any_span()` (256-byte LUT).
pub fn is_negated_char_class_regex(pattern: &str) -> Option<(String, NegCharClassQuantifier)> {
    let rest = pattern.strip_prefix("[^")?;

    let (inner, quantifier) = if let Some(inner) = rest.strip_suffix("]+") {
        (inner, NegCharClassQuantifier::Plus)
    } else if let Some(inner) = rest.strip_suffix("]*") {
        (inner, NegCharClassQuantifier::Star)
    } else {
        return None;
    };

    // Validate: only ASCII printable characters and simple backslash escapes.
    let mut chars = inner.chars().peekable();
    let mut excluded = String::new();
    while let Some(c) = chars.next() {
        if c == '\\' {
            let esc = chars.next()?;
            match esc {
                '\\' | '/' | ']' | '[' | '^' | '-' | '.' | '*' | '+' | '?' | '(' | ')' | '{'
                | '}' | '|' | 'n' | 'r' | 't' => {
                    let actual = match esc {
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        other => other,
                    };
                    excluded.push(actual);
                }
                _ => return None,
            }
        } else if c.is_ascii() && c != '[' && c != ']' {
            excluded.push(c);
        } else {
            return None;
        }
    }

    if excluded.is_empty() {
        return None;
    }

    Some((excluded, quantifier))
}

/// Detect `\s*LITERAL\s*` patterns — a fixed literal with optional whitespace padding.
/// Returns the inner literal string if detected. Handles single-char and multi-char literals.
/// Examples: `\s*,\s*` → Some(","), `\s*>\s*` → Some(">"), `\s*::\s*` → Some("::")
pub(crate) fn try_strip_ws_padded_literal(pattern: &str) -> Option<String> {
    let rest = pattern.strip_prefix(r"\s*")?;
    let (literal_end, _) = rest
        .char_indices()
        .find(|(_, c)| *c == '\\' || *c == '[' || *c == '(' || *c == '|')?;
    if literal_end == 0 {
        return None;
    }
    let literal = &rest[..literal_end];
    let after = &rest[literal_end..];
    if after == r"\s*" {
        // Verify the literal contains only plain ASCII (no regex metacharacters)
        if literal
            .chars()
            .all(|c| !r"\.+*?^${}[]|()/".contains(c) || c == '.' || c == '/')
        {
            // Only accept if the literal has no regex metacharacters
            if literal.chars().all(|c| matches!(c, ',' | '>' | '+' | '~' | ':' | ';' | '(' | ')' | '{' | '}' | '!' | '=' | '#' | '.' | '/' | '-' | '_' | 'a'..='z' | 'A'..='Z' | '0'..='9')) {
                return Some(literal.to_string());
            }
        }
    }
    None
}
