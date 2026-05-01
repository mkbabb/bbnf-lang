//! Negated character class detection and whitespace-padded literal detection.
//!
//! Handles patterns like `[^XYZ]+` / `[^XYZ]*` for `memchr`-based scanning,
//! and `\s*LITERAL\s*` for whitespace-padded literal matching.
//!
//! Detection uses `RegexClass::CharClassQuantified` from the structural
//! classifier rather than hand-rolled regex string parsing.

use parse_that::regex::classify::{ClassRangeInfo, RegexClass};

use crate::generate::regex::cost_model::EmitOpts;

/// Whether a negated character class uses `+` (one-or-more) or `*` (zero-or-more).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NegCharClassQuantifier {
    Plus,
    Star,
}

/// Detect a negated character class regex of the form `[^XYZ]+` or `[^XYZ]*`
/// via the structural classifier. Returns the excluded bytes (the complement
/// of the accepted set within ASCII) and the quantifier.
///
/// Uses `opts.classify_regex(pattern)` to resolve via the `ir.regex_info` cache
/// when available, avoiding redundant HIR parsing.
pub fn is_negated_char_class_regex(
    pattern: &str,
    opts: &EmitOpts,
) -> Option<(Vec<u8>, NegCharClassQuantifier)> {
    let class = opts.classify_regex(pattern);
    match class {
        RegexClass::CharClassQuantified(ClassRangeInfo {
            chars,
            negated: true,
            min,
            max,
        }) => {
            let quantifier = match (min, max) {
                (1, None) => NegCharClassQuantifier::Plus,
                (0, None) => NegCharClassQuantifier::Star,
                _ => return None,
            };

            // `chars` is the positive-form charset (bytes the class accepts).
            // For memchr/nibble-LUT, we need the excluded bytes — the ASCII
            // bytes NOT in the accepted set. These are the "needle" bytes
            // that terminate the scan.
            let excluded: Vec<u8> = (0u8..128).filter(|b| !chars.has(*b)).collect();

            if excluded.is_empty() {
                return None;
            }

            Some((excluded, quantifier))
        }
        _ => None,
    }
}

/// Detect `\s*LITERAL\s*` patterns via HIR inspection — a fixed literal
/// with optional whitespace padding. Returns the inner literal bytes if
/// detected.
///
/// Parses the pattern to HIR and checks for the structural shape
/// `Concat([Repetition(\s, 0..), Literal(bytes), Repetition(\s, 0..)])`.
pub(crate) fn try_strip_ws_padded_literal(pattern: &str) -> Option<Vec<u8>> {
    use parse_that::regex::hir::Hir;

    let hir = parse_that::regex::hir::parser::parse_with(
        pattern,
        &parse_that::regex::ParseOptions::byte_mode(),
    )
    .ok()?;

    let parts = match &hir {
        Hir::Concat(parts) if parts.len() == 3 => parts.as_slice(),
        _ => return None,
    };

    // First: \s* — optional whitespace repetition
    if !is_star_whitespace(&parts[0]) {
        return None;
    }

    // Middle: literal bytes
    let literal_bytes = match &parts[1] {
        Hir::Literal(bytes) if !bytes.is_empty() => bytes.clone(),
        _ => return None,
    };

    // Validate literal contains only safe ASCII (no regex metacharacters)
    if !literal_bytes.iter().all(|b| {
        matches!(b, b',' | b'>' | b'+' | b'~' | b':' | b';' | b'(' | b')' | b'{' | b'}' | b'!' | b'=' | b'#' | b'.' | b'/' | b'-' | b'_' | b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9')
    }) {
        return None;
    }

    // Last: \s* — optional whitespace repetition
    if !is_star_whitespace(&parts[2]) {
        return None;
    }

    Some(literal_bytes)
}

/// Check if an HIR node is `\s*` — a zero-or-more whitespace repetition.
fn is_star_whitespace(hir: &parse_that::regex::hir::Hir) -> bool {
    use parse_that::regex::hir::{ByteRange, CharClass, Hir, Repetition};

    if let Hir::Repetition(Repetition {
        sub,
        min: 0,
        max: None,
        ..
    }) = hir
    {
        // The sub must be a whitespace class: [\t\n\x0B\x0C\r ] (non-negated)
        if let Hir::Class(CharClass::Bytes { ranges, negated }) = sub.as_ref() {
            if *negated {
                return false;
            }
            let has_tab_to_cr = ranges.iter().any(|r| *r == ByteRange::new(0x09, 0x0D));
            let has_space = ranges.iter().any(|r| *r == ByteRange::new(b' ', b' '));
            return has_tab_to_cr && has_space;
        }
    }
    false
}
