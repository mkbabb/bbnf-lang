//! JSON string-escape decoder kernel.
//!
//! Decodes the matched bytes for a JSON `string` regex
//! (`"(?:[^"\\]|\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4}))*"`) into UTF-8
//! with escape sequences resolved:
//!
//! - `\"`, `\\`, `\/` → literal byte.
//! - `\b`, `\f`, `\n`, `\r`, `\t` → ASCII control byte.
//! - `\uXXXX` → BMP code point (1-3 byte UTF-8).
//! - `\uD8XX\uDCXX` (surrogate pair) → astral plane code point (4-byte
//!   UTF-8) per RFC 8259 §7.
//!
//! The kernel writes a `(len: u32 LE, bytes)` frame into the
//! caller-owned destination buffer at `dst_off`. The destination buffer
//! must be pre-sized for the worst case (4 bytes prefix + `slice.len()`
//! bytes) — every escape sequence shrinks the output, so the matched
//! length is an upper bound on decoded width.

/// Decode a JSON string slice (with surrounding quotes) into the
/// destination buffer at `dst_off`. Writes a 4-byte little-endian
/// length prefix followed by the decoded UTF-8 bytes.
///
/// `slice` is the full match including the leading and trailing `"`
/// (e.g. `b"\"hello\\n\""`). Caller is responsible for ensuring the
/// destination buffer contains at least `4 + slice.len()` bytes
/// starting at `dst_off`.
///
/// # Safety
///
/// - `dst` must point at a valid `[u8]` of length ≥ `dst_off + 4 +
///   slice.len()` — the worst case output (no escape sequences shrinks
///   the output).
/// - `dst_off + 4 + decoded_len` must remain in-bounds; the writer
///   stops at the last byte of the decoded body.
///
/// Returns the number of decoded bytes written (excluding the 4-byte
/// length prefix). The decoded length is back-stamped into the
/// length-prefix slot.
#[inline]
pub unsafe fn decode_into(slice: &[u8], dst: *mut u8, dst_off: usize, dst_len: usize) -> usize {
    let body = strip_quotes(slice);
    debug_assert!(
        dst_off + 4 + body.len() <= dst_len,
        "json_string::decode_into: destination short — need {} bytes at offset {}, have {}",
        4 + body.len(),
        dst_off,
        dst_len,
    );

    // SAFETY: caller-supplied `dst` + `dst_len` invariant covers
    // `dst_off + 4 + body.len()`; the decode loop never advances past
    // `body.len()` source bytes and never writes past `body.len()`
    // destination bytes (every escape sequence yields fewer output
    // bytes than its input source).
    let body_dst = unsafe { dst.add(dst_off + 4) };

    let mut src = 0usize;
    let mut out = 0usize;
    while src < body.len() {
        let b = body[src];
        if b != b'\\' {
            // Verbatim byte — fast path. JSON spec admits any UTF-8
            // continuation here; the regex matched the source so it's
            // valid UTF-8 already.
            unsafe { *body_dst.add(out) = b };
            out += 1;
            src += 1;
            continue;
        }
        // Escape sequence — inspect the byte after `\`.
        if src + 1 >= body.len() {
            // Truncated — should be impossible given the regex matched.
            // Fail soft: copy the lone backslash and advance.
            unsafe { *body_dst.add(out) = b };
            out += 1;
            src += 1;
            continue;
        }
        let esc = body[src + 1];
        match esc {
            b'"' | b'\\' | b'/' => {
                unsafe { *body_dst.add(out) = esc };
                out += 1;
                src += 2;
            }
            b'b' => {
                unsafe { *body_dst.add(out) = 0x08 };
                out += 1;
                src += 2;
            }
            b'f' => {
                unsafe { *body_dst.add(out) = 0x0C };
                out += 1;
                src += 2;
            }
            b'n' => {
                unsafe { *body_dst.add(out) = b'\n' };
                out += 1;
                src += 2;
            }
            b'r' => {
                unsafe { *body_dst.add(out) = b'\r' };
                out += 1;
                src += 2;
            }
            b't' => {
                unsafe { *body_dst.add(out) = b'\t' };
                out += 1;
                src += 2;
            }
            b'u' => {
                // \uXXXX — 4 hex digits. Possibly surrogate pair.
                if src + 6 > body.len() {
                    // Truncated — should be impossible. Copy verbatim.
                    unsafe { *body_dst.add(out) = b };
                    out += 1;
                    src += 1;
                    continue;
                }
                let cp1 = parse_hex4(&body[src + 2..src + 6]);
                src += 6;
                let scalar: u32 = if (0xD800..=0xDBFF).contains(&cp1) {
                    // High surrogate — expect a low-surrogate `\uXXXX`
                    // immediately following. Per RFC 8259 §7 the pair
                    // encodes one astral code point.
                    if src + 6 <= body.len() && body[src] == b'\\' && body[src + 1] == b'u' {
                        let cp2 = parse_hex4(&body[src + 2..src + 6]);
                        if (0xDC00..=0xDFFF).contains(&cp2) {
                            src += 6;
                            0x10000
                                + (((cp1 - 0xD800) as u32) << 10)
                                + ((cp2 - 0xDC00) as u32)
                        } else {
                            // Lone high surrogate followed by non-low-
                            // surrogate \u escape — encode the high
                            // surrogate as the U+FFFD replacement.
                            0xFFFD
                        }
                    } else {
                        // Lone high surrogate at end of string — emit
                        // the U+FFFD replacement.
                        0xFFFD
                    }
                } else if (0xDC00..=0xDFFF).contains(&cp1) {
                    // Lone low surrogate — invalid; emit replacement.
                    0xFFFD
                } else {
                    cp1 as u32
                };
                // Encode `scalar` as 1-4 byte UTF-8.
                // SAFETY: caller-supplied `dst_len` covers
                // `dst_off + 4 + body.len()`, and every encoded
                // scalar fits in ≤ 6 source bytes (`\uXXXX`) which
                // produces ≤ 4 output bytes — never expands.
                out += unsafe { encode_utf8(scalar, body_dst, out) };
            }
            other => {
                // Unrecognised escape — should be impossible given
                // the JSON regex restricts to the bytes above. Copy
                // both bytes verbatim.
                unsafe { *body_dst.add(out) = b };
                unsafe { *body_dst.add(out + 1) = other };
                out += 2;
                src += 2;
            }
        }
    }

    // Back-stamp the 4-byte length prefix.
    let len_bytes = (out as u32).to_le_bytes();
    let len_dst = unsafe { dst.add(dst_off) };
    for (i, &lb) in len_bytes.iter().enumerate() {
        unsafe { *len_dst.add(i) = lb };
    }

    out
}

/// Strip the leading and trailing `"` quotes from a matched JSON
/// string slice. Returns the body bytes; if the slice is too short to
/// contain quotes it is returned verbatim (defensive — should be
/// unreachable given the regex contract).
#[inline]
fn strip_quotes(slice: &[u8]) -> &[u8] {
    if slice.len() >= 2 && slice.first() == Some(&b'"') && slice.last() == Some(&b'"') {
        &slice[1..slice.len() - 1]
    } else {
        slice
    }
}

/// Parse a 4-digit hex value into u16. Caller guarantees `bytes` has
/// length 4 and contains only `[0-9a-fA-F]` bytes (the JSON regex
/// restricts).
#[inline]
fn parse_hex4(bytes: &[u8]) -> u16 {
    debug_assert!(bytes.len() == 4);
    let mut v: u16 = 0;
    for &b in bytes {
        let nibble = match b {
            b'0'..=b'9' => b - b'0',
            b'a'..=b'f' => b - b'a' + 10,
            b'A'..=b'F' => b - b'A' + 10,
            _ => 0,
        };
        v = (v << 4) | nibble as u16;
    }
    v
}

/// Encode a Unicode scalar value as 1-4 UTF-8 bytes into `dst[off..]`.
/// Returns the number of bytes written. Standard UTF-8 encoding rules.
///
/// # Safety
///
/// The destination must have at least 4 bytes available starting at
/// `dst.add(off)`.
#[inline]
unsafe fn encode_utf8(scalar: u32, dst: *mut u8, off: usize) -> usize {
    let p = unsafe { dst.add(off) };
    if scalar < 0x80 {
        unsafe { *p = scalar as u8 };
        1
    } else if scalar < 0x800 {
        unsafe {
            *p = 0xC0 | ((scalar >> 6) as u8 & 0x1F);
            *p.add(1) = 0x80 | (scalar as u8 & 0x3F);
        }
        2
    } else if scalar < 0x10000 {
        unsafe {
            *p = 0xE0 | ((scalar >> 12) as u8 & 0x0F);
            *p.add(1) = 0x80 | ((scalar >> 6) as u8 & 0x3F);
            *p.add(2) = 0x80 | (scalar as u8 & 0x3F);
        }
        3
    } else {
        unsafe {
            *p = 0xF0 | ((scalar >> 18) as u8 & 0x07);
            *p.add(1) = 0x80 | ((scalar >> 12) as u8 & 0x3F);
            *p.add(2) = 0x80 | ((scalar >> 6) as u8 & 0x3F);
            *p.add(3) = 0x80 | (scalar as u8 & 0x3F);
        }
        4
    }
}

