//! The `css_balanced_component_scan` named primitive (SK-V18 G2 §6).
//!
//! This is the irreducible balanced-delimiter scan ALGORITHM that the CSS L4
//! recognizer is built from — the 94.1% scalar-scan hot leaf. It is
//! grammar-INVOKED (the generated CSS provider calls it by name) and
//! grammar-PARAMETERIZED: the STOP-set passed at each call site (`b";{}"`,
//! `b"{};"`, `b";}"`, `b":{};"`) is DERIVED by `codegen::lower::css_scan` from
//! the CSS grammar's `ruleBlock`/declaration/`atRuleBody` literal shapes; the
//! structural SKIP family (`( ) [ ] { } ' " /`) is derived from the selector
//! `()`/`[]`, the `ruleBlock` `{}`, the `cssString` quotes, and the `@ws`
//! comment. None of those byte-sets is a literal here — they arrive as caller
//! data, so a mutation of the invoking `.bbnf` rule changes the emitted ARG
//! byte-set without touching this kernel (the arg-mutation falsifier).
//!
//! NEUTRALITY (CH6/s6): the balanced-recognizer SHELL is CSS-EXERCISED-ONLY —
//! it returns offsets and EMITS NOTHING, structurally incompatible with the
//! parse-WITH-emit `{}`/`[]` descents of JSON/Sheets. The CSS-scoped name is
//! therefore FORCED, not contingent. The INNER alphabet-scan sub-kernel
//! (`runtime_simd::find_css_significant`, the shared `bbnf-simd` eq-set scan)
//! stays genuinely neutral (caller-data alphabet) and is the G6 retarget seam.

use crate::runtime_simd::find_css_significant;

/// A position-only scan error (the CSS provider maps it to its `CssFactError`).
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct ScanError {
    pub offset: usize,
    pub message: &'static str,
}

#[inline]
fn scan_error(offset: usize, message: &'static str) -> ScanError {
    ScanError { offset, message }
}

/// The structural SKIP alphabet for the balanced component scan, as the
/// fixed-9 eq-set the neutral `find_css_significant` kernel consumes. These are
/// the bytes the inert-run skip must stop on so the scan can dispatch on them:
/// the open/close pairs `( ) [ ] { }` (selector `()`/`[]` + `ruleBlock` `{}`),
/// the `cssString` quotes `' "`, and the comment-opening `/`. The kernel treats
/// this set as caller data; the membership is grammar-derived (see
/// `codegen::lower::css_scan::ScanAlphabet`).
pub type SkipAlphabet = [u8; 9];

/// The balanced-component scan kernel: advance from `start`, skipping balanced
/// `()`/`[]`/`{}`, strings, and comments, and returning the first `delimiters`
/// member found at the TOP level (or `None` at end of input). The inert-run
/// skip rides the shared NEON eq-set classifier (`find_css_significant`); the
/// scalar dispatch handles each structural byte. Behaviorally identical to the
/// retired hand-written `find_component_delim`.
#[inline]
pub fn css_balanced_component_scan(
    bytes: &[u8],
    mut pos: usize,
    delimiters: &[u8],
    skip: &SkipAlphabet,
) -> Result<Option<(u8, usize)>, ScanError> {
    let len = bytes.len();
    while pos < len {
        // Skip the inert run to the next significant byte (a STOP delimiter or a
        // structural skip-family byte) via the shared neutral eq-set scan.
        pos = find_css_significant(bytes, pos, delimiters, skip);
        if pos >= len {
            return Ok(None);
        }
        let byte = bytes[pos];
        if delimiters.contains(&byte) {
            return Ok(Some((byte, pos)));
        }
        pos = dispatch_structural(bytes, pos, byte, skip)?;
    }
    Ok(None)
}

/// Consume a balanced component starting at the opener `start` (`bytes[start]`
/// is an open bracket), returning the index just past the matching `close`.
/// Recurses on nested `()`/`[]`/`{}`, skips strings/comments — the inner-recursive
/// half of the same scan machine.
#[inline]
pub fn css_consume_balanced(
    bytes: &[u8],
    start: usize,
    close: u8,
    skip: &SkipAlphabet,
) -> Result<usize, ScanError> {
    let len = bytes.len();
    let mut pos = start + 1;
    // The inner balance scan stops on the close byte plus the structural skip
    // family; the close byte is appended to the eq-set as caller data.
    let close_set = [close];
    while pos < len {
        pos = find_css_significant(bytes, pos, &close_set, skip);
        if pos >= len {
            break;
        }
        let byte = bytes[pos];
        if byte == close {
            return Ok(pos + 1);
        }
        pos = dispatch_structural(bytes, pos, byte, skip)?;
    }
    Err(scan_error(start, "unclosed component block"))
}

/// Dispatch on a structural byte the inert-run skip stopped on: recurse into a
/// nested balanced block, skip a string/comment, or reject an unexpected close.
/// A non-structural byte (e.g. the comment-`/` that is not `/*`) advances by one.
#[inline]
fn dispatch_structural(
    bytes: &[u8],
    pos: usize,
    byte: u8,
    skip: &SkipAlphabet,
) -> Result<usize, ScanError> {
    let next = bytes.get(pos + 1).copied();
    let consumed = match byte {
        b'\'' | b'"' => consume_string(bytes, pos)?,
        b'/' if next == Some(b'*') => consume_comment(bytes, pos)?,
        b'(' => css_consume_balanced(bytes, pos, b')', skip)?,
        b'[' => css_consume_balanced(bytes, pos, b']', skip)?,
        b'{' => css_consume_balanced(bytes, pos, b'}', skip)?,
        b')' | b']' | b'}' => {
            return Err(scan_error(pos, "unexpected closing delimiter"));
        }
        _ => pos + 1,
    };
    Ok(consumed)
}

/// Skip a `/* ... */` comment beginning at `start`, returning the index past
/// `*/`.
#[inline]
fn consume_comment(bytes: &[u8], start: usize) -> Result<usize, ScanError> {
    let mut pos = start + 2;
    while pos + 1 < bytes.len() {
        if bytes[pos] == b'*' && bytes[pos + 1] == b'/' {
            return Ok(pos + 2);
        }
        pos += 1;
    }
    Err(scan_error(start, "unclosed comment"))
}

/// Skip a CSS string literal beginning at `start` (a quote byte), honouring
/// escapes and rejecting unescaped newlines. Returns the index past the close.
#[inline]
fn consume_string(bytes: &[u8], start: usize) -> Result<usize, ScanError> {
    let quote = bytes[start];
    let mut pos = start + 1;
    while pos < bytes.len() {
        match bytes[pos] {
            byte if byte == quote => return Ok(pos + 1),
            b'\\' => {
                pos += 1;
                if pos >= bytes.len() {
                    return Err(scan_error(start, "unclosed string"));
                }
                if bytes[pos] == b'\r' && bytes.get(pos + 1).copied() == Some(b'\n') {
                    pos += 2;
                } else {
                    pos += 1;
                }
            }
            b'\n' | b'\r' | 0x0c => {
                return Err(scan_error(pos, "unescaped newline in string"));
            }
            _ => pos += 1,
        }
    }
    Err(scan_error(start, "unclosed string"))
}
