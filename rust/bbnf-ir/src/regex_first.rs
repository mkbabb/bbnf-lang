//! FIRST set extraction from regex patterns via regex-syntax HIR.
//!
//! Uses `CharSet128` (128-bit ASCII bitset) for the IR crate.

use crate::CharSet128;

/// Extract the set of possible first bytes from a regex pattern.
///
/// Returns `None` for patterns containing wildcards (`.`) or unsupported constructs
/// whose FIRST set cannot be usefully constrained.
pub fn regex_first_chars(pattern: &str) -> Option<CharSet128> {
    let hir = regex_syntax::ParserBuilder::new()
        .utf8(false)
        .build()
        .parse(pattern)
        .ok()?;
    first_chars_from_hir(&hir)
}

/// Threshold: if a class covers this many or more of the 128 ASCII slots,
/// treat it as "any byte" and return `None` (equivalent to the old `.` bail-out).
const WILDCARD_THRESHOLD: usize = 126;

fn first_chars_from_hir(hir: &regex_syntax::hir::Hir) -> Option<CharSet128> {
    use regex_syntax::hir::HirKind;

    match hir.kind() {
        HirKind::Empty => Some(CharSet128::new()),

        HirKind::Literal(lit) => {
            let mut cs = CharSet128::new();
            if let Some(&b) = lit.0.first() {
                cs.add(b);
            }
            Some(cs)
        }

        HirKind::Class(class) => {
            let cs = class_to_charset(class);
            if cs.len() >= WILDCARD_THRESHOLD {
                None // wildcard-like class (e.g. `.`)
            } else {
                Some(cs)
            }
        }

        HirKind::Alternation(alts) => {
            let mut cs = CharSet128::new();
            for alt in alts {
                cs.union(&first_chars_from_hir(alt)?);
            }
            Some(cs)
        }

        HirKind::Concat(seq) => {
            if seq.is_empty() {
                return Some(CharSet128::new());
            }
            let first = first_chars_from_hir(&seq[0])?;
            if is_nullable(&seq[0]) {
                let mut cs = first;
                if seq.len() > 1 {
                    if let Some(next) = first_chars_from_hir(&seq[1]) {
                        cs.union(&next);
                    }
                }
                Some(cs)
            } else {
                Some(first)
            }
        }

        HirKind::Repetition(rep) => first_chars_from_hir(&rep.sub),

        HirKind::Capture(cap) => first_chars_from_hir(&cap.sub),

        HirKind::Look(_) => Some(CharSet128::new()),
    }
}

fn class_to_charset(class: &regex_syntax::hir::Class) -> CharSet128 {
    let mut cs = CharSet128::new();
    match class {
        regex_syntax::hir::Class::Bytes(bytes) => {
            for range in bytes.ranges() {
                let lo = range.start();
                let hi = range.end().min(127);
                if lo <= hi {
                    cs.add_range(lo, hi);
                }
            }
        }
        regex_syntax::hir::Class::Unicode(unicode) => {
            for range in unicode.ranges() {
                let start = range.start() as u32;
                let end = range.end() as u32;
                let lo = start.min(127) as u8;
                let hi = end.min(127) as u8;
                if lo <= hi {
                    cs.add_range(lo, hi);
                }
            }
        }
    }
    cs
}

fn is_nullable(hir: &regex_syntax::hir::Hir) -> bool {
    use regex_syntax::hir::HirKind;
    match hir.kind() {
        HirKind::Empty => true,
        HirKind::Literal(_) => false,
        HirKind::Class(_) => false,
        HirKind::Alternation(alts) => alts.iter().any(is_nullable),
        HirKind::Concat(seq) => seq.iter().all(is_nullable),
        HirKind::Repetition(rep) => rep.min == 0,
        HirKind::Capture(cap) => is_nullable(&cap.sub),
        HirKind::Look(_) => true,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_literal() {
        let cs = regex_first_chars("abc").unwrap();
        assert!(cs.has(b'a'));
        assert!(!cs.has(b'b'));
    }

    #[test]
    fn char_class() {
        let cs = regex_first_chars("[a-zA-Z_]").unwrap();
        assert!(cs.has(b'a'));
        assert!(cs.has(b'Z'));
        assert!(cs.has(b'_'));
        assert!(!cs.has(b'0'));
    }

    #[test]
    fn alternation() {
        let cs = regex_first_chars("abc|[0-9]").unwrap();
        assert!(cs.has(b'a'));
        assert!(cs.has(b'5'));
    }

    #[test]
    fn optional_prefix() {
        let cs = regex_first_chars("-?[0-9]").unwrap();
        assert!(cs.has(b'-'));
        assert!(cs.has(b'0'));
    }

    #[test]
    fn dot_returns_none() {
        assert!(regex_first_chars(".*").is_none());
    }

    #[test]
    fn escape_sequences() {
        let cs = regex_first_chars(r"\d+").unwrap();
        assert!(cs.has(b'0'));
        assert!(cs.has(b'9'));
        assert!(!cs.has(b'a'));
    }
}
