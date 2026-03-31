//! HIR-based regex classification for during-parse value conversion.
//!
//! Uses `regex_syntax` to parse patterns into HIR (High-level Intermediate
//! Representation) and classify structurally. More robust than string-level
//! matching: normalizes `\d` / `[0-9]`, class orderings, group nesting, etc.

use regex_syntax::hir::{Class, ClassBytesRange, Hir, HirKind};

/// Classification result for a regex pattern.
#[derive(Debug, Clone, PartialEq)]
pub enum RegexClass {
    /// Matches numeric values convertible to f64.
    Numeric {
        allows_sign: bool,
        allows_fraction: bool,
        allows_exponent: bool,
    },

    /// Matches quoted strings: `"content"` or `'content'`.
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

/// Classify a regex pattern structurally via HIR.
pub fn classify_regex(pattern: &str) -> RegexClass {
    let hir = match regex_syntax::ParserBuilder::new()
        .utf8(false)
        .unicode(false)
        .build()
        .parse(pattern)
    {
        Ok(h) => h,
        Err(_) => return RegexClass::Unknown,
    };

    if let Some(class) = try_classify_numeric(&hir) {
        return class;
    }
    if let Some(class) = try_classify_quoted_string(&hir) {
        return class;
    }
    if try_classify_hex(&hir) {
        return RegexClass::HexDigits;
    }
    if try_classify_identifier(&hir) {
        return RegexClass::Identifier;
    }
    RegexClass::Unknown
}

// ── Numeric ────────────────────────────────────────────────────────────────

fn try_classify_numeric(hir: &Hir) -> Option<RegexClass> {
    // Flatten the top-level concat (or treat a single node as a 1-element list).
    let parts = match hir.kind() {
        HirKind::Concat(parts) => parts.as_slice(),
        _ => std::slice::from_ref(hir),
    };

    let mut idx = 0;
    let mut allows_sign = false;
    let mut allows_fraction = false;
    let mut allows_exponent = false;

    // Optional sign: `[-+]?` or `-?`
    if idx < parts.len() && is_optional_sign_class(&parts[idx]) {
        allows_sign = true;
        idx += 1;
    }

    // Required digits (or alternation with optional fraction built in).
    if idx >= parts.len() {
        return None;
    }
    if is_digit_repetition(&parts[idx]) {
        idx += 1;
    } else if is_digit_class(&parts[idx]) {
        // Single digit character (no repetition) — could be numeric
        idx += 1;
    } else if is_json_integer_alternation(&parts[idx]) {
        // (0|[1-9]\d*) style
        idx += 1;
    } else if is_css_number_body(&parts[idx]) {
        // (\d+(\.\d+)?|\.\d+) style — inherently includes fraction
        allows_fraction = true;
        idx += 1;
    } else {
        return None;
    }

    // Optional fraction: `(\.\d+)?` or `\.\d+`
    if idx < parts.len() && is_fraction_part(&parts[idx]) {
        allows_fraction = true;
        idx += 1;
    }

    // Optional exponent: `([eE][+-]?\d+)?`
    if idx < parts.len() && is_exponent_part(&parts[idx]) {
        allows_exponent = true;
        idx += 1;
    }

    // Must have consumed everything.
    if idx != parts.len() {
        return None;
    }

    Some(RegexClass::Numeric {
        allows_sign,
        allows_fraction,
        allows_exponent,
    })
}

/// Check if HIR is an optional sign class: `[-+]?` or `-?`
fn is_optional_sign_class(hir: &Hir) -> bool {
    if let HirKind::Repetition(rep) = hir.kind() {
        if rep.min == 0 && rep.max == Some(1) {
            return is_sign_class(&rep.sub);
        }
    }
    // Also handle capture group wrapping.
    if let HirKind::Capture(cap) = hir.kind() {
        return is_optional_sign_class(&cap.sub);
    }
    false
}

fn is_sign_class(hir: &Hir) -> bool {
    if let HirKind::Class(Class::Bytes(bc)) = hir.kind() {
        let ranges = bc.ranges();
        // [-+] or [+-] → two single-byte ranges or one range
        let has_plus = ranges.iter().any(|r| r.start() <= b'+' && r.end() >= b'+');
        let has_minus = ranges.iter().any(|r| r.start() <= b'-' && r.end() >= b'-');
        return has_minus && (has_plus || ranges.len() == 1);
    }
    // Literal '-'
    if let HirKind::Literal(lit) = hir.kind() {
        return lit.0.as_ref() == b"-";
    }
    false
}

/// Check if HIR is a digit repetition: `\d+`, `[0-9]+`, etc.
fn is_digit_repetition(hir: &Hir) -> bool {
    if let HirKind::Repetition(rep) = hir.kind() {
        if rep.min >= 1 || (rep.min == 0 && rep.max.is_none()) {
            return is_digit_class(&rep.sub);
        }
    }
    if let HirKind::Capture(cap) = hir.kind() {
        return is_digit_repetition(&cap.sub);
    }
    false
}

/// Check if HIR is a digit class: `\d`, `[0-9]`
fn is_digit_class(hir: &Hir) -> bool {
    if let HirKind::Class(Class::Bytes(bc)) = hir.kind() {
        let ranges = bc.ranges();
        return ranges.len() == 1
            && ranges[0] == ClassBytesRange::new(b'0', b'9');
    }
    false
}

/// Check if HIR matches `(0|[1-9]\d*)` (JSON integer alternation).
fn is_json_integer_alternation(hir: &Hir) -> bool {
    let inner = unwrap_group(hir);
    if let HirKind::Alternation(alts) = inner.kind() {
        if alts.len() == 2 {
            let is_zero = is_literal_byte(&alts[0], b'0');
            let is_nonzero_seq = is_nonzero_digit_seq(&alts[1]);
            return is_zero && is_nonzero_seq;
        }
    }
    false
}

/// Check if HIR matches `[1-9]\d*`
fn is_nonzero_digit_seq(hir: &Hir) -> bool {
    let parts = match hir.kind() {
        HirKind::Concat(parts) => parts.as_slice(),
        _ => return false,
    };
    if parts.len() != 2 {
        return false;
    }
    // [1-9]
    if let HirKind::Class(Class::Bytes(bc)) = parts[0].kind() {
        let ranges = bc.ranges();
        if !(ranges.len() == 1 && ranges[0] == ClassBytesRange::new(b'1', b'9')) {
            return false;
        }
    } else {
        return false;
    }
    // \d*
    if let HirKind::Repetition(rep) = parts[1].kind() {
        return rep.min == 0 && rep.max.is_none() && is_digit_class(&rep.sub);
    }
    false
}

/// Check if HIR matches `(\d+(\.\d+)?|\.\d+)` (CSS non-nullable number body).
fn is_css_number_body(hir: &Hir) -> bool {
    let inner = unwrap_group(hir);
    if let HirKind::Alternation(alts) = inner.kind() {
        if alts.len() == 2 {
            // First alt: \d+(\.\d+)?
            // Second alt: \.\d+
            return is_digits_with_optional_fraction(&alts[0]) && is_dot_digits(&alts[1]);
        }
    }
    false
}

fn is_digits_with_optional_fraction(hir: &Hir) -> bool {
    let parts = match hir.kind() {
        HirKind::Concat(parts) => parts.as_slice(),
        _ => return is_digit_repetition(hir),
    };
    if parts.len() != 2 {
        return false;
    }
    is_digit_repetition(&parts[0]) && is_fraction_part(&parts[1])
}

fn is_dot_digits(hir: &Hir) -> bool {
    let parts = match hir.kind() {
        HirKind::Concat(parts) => parts.as_slice(),
        _ => return false,
    };
    if parts.len() != 2 {
        return false;
    }
    is_literal_byte(&parts[0], b'.') && is_digit_repetition(&parts[1])
}

/// Check if HIR is an optional fraction: `(\.\d+)?` or `\.\d+`.
fn is_fraction_part(hir: &Hir) -> bool {
    // Optional group: (\.\d+)?
    if let HirKind::Repetition(rep) = hir.kind() {
        if rep.min == 0 && rep.max == Some(1) {
            return is_dot_digits_inner(&rep.sub);
        }
    }
    // Direct: \.\d+
    is_dot_digits_inner(hir)
}

fn is_dot_digits_inner(hir: &Hir) -> bool {
    let inner = unwrap_group(hir);
    let parts = match inner.kind() {
        HirKind::Concat(parts) => parts.as_slice(),
        _ => return false,
    };
    if parts.len() != 2 {
        return false;
    }
    is_literal_byte(&parts[0], b'.') && is_digit_repetition(&parts[1])
}

/// Check if HIR is an optional exponent: `([eE][+-]?\d+)?`.
fn is_exponent_part(hir: &Hir) -> bool {
    if let HirKind::Repetition(rep) = hir.kind() {
        if rep.min == 0 && rep.max == Some(1) {
            return is_exponent_inner(&rep.sub);
        }
    }
    is_exponent_inner(hir)
}

fn is_exponent_inner(hir: &Hir) -> bool {
    let inner = unwrap_group(hir);
    let parts = match inner.kind() {
        HirKind::Concat(parts) => parts.as_slice(),
        _ => return false,
    };
    // [eE] [+-]? \d+  (2-3 parts)
    if parts.len() < 2 || parts.len() > 3 {
        return false;
    }
    if !is_exponent_letter_class(&parts[0]) {
        return false;
    }
    if parts.len() == 3 {
        is_optional_sign_class(&parts[1]) && is_digit_repetition(&parts[2])
    } else {
        is_digit_repetition(&parts[1])
    }
}

fn is_exponent_letter_class(hir: &Hir) -> bool {
    if let HirKind::Class(Class::Bytes(bc)) = hir.kind() {
        let ranges = bc.ranges();
        let has_e = ranges.iter().any(|r| r.start() <= b'e' && r.end() >= b'e');
        let has_upper_e = ranges.iter().any(|r| r.start() <= b'E' && r.end() >= b'E');
        return has_e && has_upper_e;
    }
    false
}

// ── QuotedString ───────────────────────────────────────────────────────────

fn try_classify_quoted_string(hir: &Hir) -> Option<RegexClass> {
    let parts = match hir.kind() {
        HirKind::Concat(parts) => parts.as_slice(),
        _ => return None,
    };
    if parts.len() < 3 {
        return None;
    }

    // First element: literal quote char.
    let quote_char = match parts[0].kind() {
        HirKind::Literal(lit) if lit.0.len() == 1 => {
            let b = lit.0[0];
            if b == b'"' || b == b'\'' {
                b
            } else {
                return None;
            }
        }
        _ => return None,
    };

    // Last element: literal closing quote (same char).
    let last = parts.last()?;
    match last.kind() {
        HirKind::Literal(lit) if lit.0.len() == 1 && lit.0[0] == quote_char => {}
        _ => return None,
    }

    // Middle: repetition containing the content pattern.
    // Check for escape handling.
    let middle = &parts[1..parts.len() - 1];
    let allows_escapes = middle.iter().any(contains_backslash_pattern);

    Some(RegexClass::QuotedString {
        quote_char,
        allows_escapes,
    })
}

fn contains_backslash_pattern(hir: &Hir) -> bool {
    match hir.kind() {
        HirKind::Literal(lit) => lit.0.contains(&b'\\'),
        HirKind::Concat(parts) => parts.iter().any(contains_backslash_pattern),
        HirKind::Alternation(alts) => alts.iter().any(contains_backslash_pattern),
        HirKind::Repetition(rep) => contains_backslash_pattern(&rep.sub),
        HirKind::Capture(cap) => contains_backslash_pattern(&cap.sub),
        _ => false,
    }
}

// ── HexDigits ──────────────────────────────────────────────────────────────

fn try_classify_hex(hir: &Hir) -> bool {
    // Match: [0-9a-fA-F]+ or [0-9a-fA-F]{n,m}
    if let HirKind::Repetition(rep) = hir.kind() {
        if rep.min >= 1 || rep.max.is_none() {
            return is_hex_class(&rep.sub);
        }
    }
    false
}

fn is_hex_class(hir: &Hir) -> bool {
    if let HirKind::Class(Class::Bytes(bc)) = hir.kind() {
        let ranges = bc.ranges();
        // Canonical hex: [0-9A-Fa-f] → 3 ranges after normalization.
        if ranges.len() == 3 {
            let has_digits = ranges.iter().any(|r| *r == ClassBytesRange::new(b'0', b'9'));
            let has_upper = ranges.iter().any(|r| *r == ClassBytesRange::new(b'A', b'F'));
            let has_lower = ranges.iter().any(|r| *r == ClassBytesRange::new(b'a', b'f'));
            return has_digits && has_upper && has_lower;
        }
    }
    false
}

// ── Identifier ─────────────────────────────────────────────────────────────

fn try_classify_identifier(hir: &Hir) -> bool {
    let parts = match hir.kind() {
        HirKind::Concat(parts) => parts.as_slice(),
        _ => {
            // Single class with repetition: [a-zA-Z]+
            if let HirKind::Repetition(rep) = hir.kind() {
                return is_letter_class(&rep.sub);
            }
            return false;
        }
    };

    if parts.is_empty() {
        return false;
    }

    // First part: must be a letter/underscore class (possibly with repetition).
    let first = unwrap_repetition(&parts[0]).unwrap_or(&parts[0]);
    if !is_letter_class(first) {
        return false;
    }

    // Remaining parts: word-class continuation ([\w-]*, [\w]*, etc.)
    for part in &parts[1..] {
        if !is_word_continuation(part) {
            return false;
        }
    }

    true
}

fn is_letter_class(hir: &Hir) -> bool {
    if let HirKind::Class(Class::Bytes(bc)) = hir.kind() {
        let ranges = bc.ranges();
        let has_lower = ranges.iter().any(|r| r.start() <= b'a' && r.end() >= b'z');
        let has_upper = ranges.iter().any(|r| r.start() <= b'A' && r.end() >= b'Z');
        return has_lower || has_upper;
    }
    false
}

fn is_word_continuation(hir: &Hir) -> bool {
    if let HirKind::Repetition(rep) = hir.kind() {
        return is_word_class(&rep.sub);
    }
    false
}

fn is_word_class(hir: &Hir) -> bool {
    if let HirKind::Class(Class::Bytes(bc)) = hir.kind() {
        let ranges = bc.ranges();
        // Word class contains at least alphanumeric ranges.
        let has_lower = ranges.iter().any(|r| r.start() <= b'a' && r.end() >= b'z');
        let has_digit = ranges.iter().any(|r| *r == ClassBytesRange::new(b'0', b'9'));
        return has_lower && has_digit;
    }
    false
}

// ── Utility ────────────────────────────────────────────────────────────────

fn is_literal_byte(hir: &Hir, byte: u8) -> bool {
    if let HirKind::Literal(lit) = hir.kind() {
        return lit.0.len() == 1 && lit.0[0] == byte;
    }
    // Escaped literal (e.g., \.)
    if let HirKind::Literal(lit) = hir.kind() {
        return lit.0.as_ref() == [byte];
    }
    false
}

fn unwrap_group(hir: &Hir) -> &Hir {
    match hir.kind() {
        HirKind::Capture(cap) => unwrap_group(&cap.sub),
        _ => hir,
    }
}

fn unwrap_repetition(hir: &Hir) -> Option<&Hir> {
    if let HirKind::Repetition(rep) = hir.kind() {
        Some(&rep.sub)
    } else {
        None
    }
}
