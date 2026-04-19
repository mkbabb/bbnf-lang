//! JSON string kernel — inline quoted-string scan + escape handling.
//!
//! # Hot path (escape-free)
//!
//! The prototype's string scanner is the branch-heavy hottest inner
//! loop on string-skewed inputs (twitter 64%, data_xl 42% strings).
//! Per B1 §4 + B1 Appendix A, the hot path is:
//!
//! 1. `first_quote_or_backslash` SIMD scan over the remaining input,
//!    returning `(offset, byte)` on hit.
//! 2. If the hit byte is `b'"'`: the full string body is the
//!    `[start, end)` slice — zero-copy. Emit to the visitor.
//! 3. If the hit byte is `b'\\'`: route into [`parse_string_escaped`]
//!    (cold path).
//!
//! # Escaped path (cold)
//!
//! Decodes JSON escape sequences per RFC 8259 §7. `\uXXXX` with a
//! surrogate pair `\uXXXX\uYYYY` combines to a single code point;
//! unpaired surrogates return [`crate::ParseError::InvalidEscape`].
//! The decoded bytes accumulate in a crate-internal buffer which the
//! visitor receives as a byte slice.

use crate::simd::first_quote_or_backslash;
use crate::{JsonVisitor, ParseError};

/// Parse a JSON string at `*p`, which must point at the opening `"`.
///
/// Advances `*p` past the closing `"`. On the escape-free fast path
/// the visitor receives a slice of `input` directly (borrow path). On
/// the escaped path the visitor receives a slice into a thread-local
/// decoded buffer.
#[inline(always)]
pub(crate) fn parse_string_body<V: JsonVisitor>(
    input: &[u8],
    p: &mut usize,
    visitor: &mut V,
    is_key: bool,
) -> Result<(), ParseError> {
    let open = *p;
    debug_assert_eq!(input.get(open), Some(&b'"'), "parse_string expects opening `\"`");
    let body_start = open + 1;
    let tail = input.get(body_start..).ok_or(ParseError::Eof(open))?;
    match first_quote_or_backslash(tail) {
        Some((off, b'"')) => {
            let end = body_start + off;
            let body = &input[body_start..end];
            *p = end + 1;
            emit(visitor, body, is_key, open)
        }
        Some((off, b'\\')) => {
            // Escape encountered at body_start + off. Route into the
            // cold path; the escape decoder takes over from here.
            let esc_start = body_start + off;
            parse_string_escaped(input, p, body_start, esc_start, visitor, is_key, open)
        }
        Some(_) => unreachable!("first_quote_or_backslash returned non-quote/non-escape byte"),
        None => Err(ParseError::UnterminatedString(open)),
    }
}

#[inline(always)]
fn emit<V: JsonVisitor>(
    visitor: &mut V,
    bytes: &[u8],
    is_key: bool,
    open: usize,
) -> Result<(), ParseError> {
    if is_key {
        visitor
            .key(bytes)
            .map_err(|_| ParseError::VisitorReject(open))
    } else {
        visitor
            .string(bytes)
            .map_err(|_| ParseError::VisitorReject(open))
    }
}

/// Cold path for strings containing at least one escape. Decodes
/// escapes per RFC 8259 §7 into a thread-local buffer; emits the
/// decoded byte slice to the visitor.
#[cold]
#[inline(never)]
fn parse_string_escaped<V: JsonVisitor>(
    input: &[u8],
    p: &mut usize,
    body_start: usize,
    mut cur: usize,
    visitor: &mut V,
    is_key: bool,
    open: usize,
) -> Result<(), ParseError> {
    // Accumulate the decoded body in a fresh Vec. The escape path is
    // cold enough that allocating here is negligible; reusing a
    // thread-local buffer would complicate borrow lifetimes without a
    // measurable win per the P1 profile (psi::write_decoded at 5.7%
    // twitter, 4.9% data_xl — escape path is a subset).
    let mut buf: Vec<u8> = Vec::with_capacity(cur - body_start + 16);
    buf.extend_from_slice(&input[body_start..cur]);

    while cur < input.len() {
        let b = input[cur];
        match b {
            b'"' => {
                *p = cur + 1;
                return emit(visitor, &buf, is_key, open);
            }
            b'\\' => {
                if cur + 1 >= input.len() {
                    return Err(ParseError::InvalidEscape(cur));
                }
                let esc = input[cur + 1];
                match esc {
                    b'"' => buf.push(b'"'),
                    b'\\' => buf.push(b'\\'),
                    b'/' => buf.push(b'/'),
                    b'b' => buf.push(0x08),
                    b'f' => buf.push(0x0C),
                    b'n' => buf.push(b'\n'),
                    b'r' => buf.push(b'\r'),
                    b't' => buf.push(b'\t'),
                    b'u' => {
                        if cur + 6 > input.len() {
                            return Err(ParseError::InvalidEscape(cur));
                        }
                        let cp = parse_u4(&input[cur + 2..cur + 6])
                            .ok_or(ParseError::InvalidEscape(cur))?;
                        let code = if (0xD800..=0xDBFF).contains(&cp) {
                            // High surrogate — expect matching low surrogate.
                            if cur + 12 > input.len()
                                || input[cur + 6] != b'\\'
                                || input[cur + 7] != b'u'
                            {
                                return Err(ParseError::InvalidEscape(cur));
                            }
                            let lo = parse_u4(&input[cur + 8..cur + 12])
                                .ok_or(ParseError::InvalidEscape(cur))?;
                            if !(0xDC00..=0xDFFF).contains(&lo) {
                                return Err(ParseError::InvalidEscape(cur));
                            }
                            cur += 6; // account for the low-surrogate escape too
                            0x10000 + (((cp - 0xD800) as u32) << 10) + (lo - 0xDC00) as u32
                        } else if (0xDC00..=0xDFFF).contains(&cp) {
                            return Err(ParseError::InvalidEscape(cur));
                        } else {
                            cp as u32
                        };
                        encode_utf8_to(&mut buf, code);
                        cur += 6;
                        continue;
                    }
                    _ => return Err(ParseError::InvalidEscape(cur)),
                }
                cur += 2;
            }
            _ => {
                // Scan forward for the next quote or backslash — use
                // the SIMD primitive to avoid a byte-by-byte tail.
                let tail = &input[cur..];
                match first_quote_or_backslash(tail) {
                    Some((off, _)) => {
                        buf.extend_from_slice(&tail[..off]);
                        cur += off;
                    }
                    None => return Err(ParseError::UnterminatedString(open)),
                }
            }
        }
    }
    Err(ParseError::UnterminatedString(open))
}

/// Parse a 4-hex-digit sequence into a `u16`. Returns `None` on any
/// non-hex byte.
#[inline]
fn parse_u4(bytes: &[u8]) -> Option<u16> {
    if bytes.len() != 4 {
        return None;
    }
    let mut out: u16 = 0;
    for &b in bytes {
        let v = match b {
            b'0'..=b'9' => b - b'0',
            b'a'..=b'f' => b - b'a' + 10,
            b'A'..=b'F' => b - b'A' + 10,
            _ => return None,
        };
        out = out.wrapping_shl(4) | (v as u16);
    }
    Some(out)
}

/// Encode a Unicode code point as UTF-8 into `buf`.
#[inline]
fn encode_utf8_to(buf: &mut Vec<u8>, code: u32) {
    if code < 0x80 {
        buf.push(code as u8);
    } else if code < 0x800 {
        buf.push(0xC0 | (code >> 6) as u8);
        buf.push(0x80 | (code & 0x3F) as u8);
    } else if code < 0x10000 {
        buf.push(0xE0 | (code >> 12) as u8);
        buf.push(0x80 | ((code >> 6) & 0x3F) as u8);
        buf.push(0x80 | (code & 0x3F) as u8);
    } else {
        buf.push(0xF0 | (code >> 18) as u8);
        buf.push(0x80 | ((code >> 12) & 0x3F) as u8);
        buf.push(0x80 | ((code >> 6) & 0x3F) as u8);
        buf.push(0x80 | (code & 0x3F) as u8);
    }
}
