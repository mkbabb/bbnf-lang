use std::borrow::Cow;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RegexError {
    pub offset: usize,
    pub kind: RegexErrorKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RegexErrorKind {
    ExpectedString,
    UnterminatedString,
    ControlCharacter,
    InvalidEscape,
    InvalidUnicodeEscape,
    InvalidSurrogatePair,
}

impl fmt::Display for RegexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} at byte {}", self.kind, self.offset)
    }
}

impl std::error::Error for RegexError {}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct JsonStringMatch {
    pub raw_start: usize,
    pub raw_end: usize,
    pub content_start: usize,
    pub content_end: usize,
    pub needs_unescape: bool,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct JsonNumberMatch {
    pub start: usize,
    pub end: usize,
}

#[inline]
pub fn skip_json_whitespace(input: &[u8], mut offset: usize) -> usize {
    while offset < input.len() {
        match input[offset] {
            b' ' | b'\n' | b'\r' | b'\t' => offset += 1,
            _ => break,
        }
    }
    offset
}

#[inline]
pub fn match_json_number(input: &[u8], offset: usize) -> Option<JsonNumberMatch> {
    let len = input.len();
    let mut cursor = offset;

    if cursor >= len {
        return None;
    }

    if input[cursor] == b'-' {
        cursor += 1;
        if cursor >= len {
            return None;
        }
    }

    match input[cursor] {
        b'0' => cursor += 1,
        b'1'..=b'9' => {
            cursor += 1;
            while cursor < len && input[cursor].is_ascii_digit() {
                cursor += 1;
            }
        }
        _ => return None,
    }

    if cursor < len && input[cursor] == b'.' {
        cursor += 1;
        let digits_start = cursor;
        while cursor < len && input[cursor].is_ascii_digit() {
            cursor += 1;
        }
        if cursor == digits_start {
            return None;
        }
    }

    if cursor < len && matches!(input[cursor], b'e' | b'E') {
        cursor += 1;
        if cursor < len && matches!(input[cursor], b'+' | b'-') {
            cursor += 1;
        }
        let digits_start = cursor;
        while cursor < len && input[cursor].is_ascii_digit() {
            cursor += 1;
        }
        if cursor == digits_start {
            return None;
        }
    }

    Some(JsonNumberMatch {
        start: offset,
        end: cursor,
    })
}

#[inline]
pub fn validate_json_number(input: &str) -> bool {
    match match_json_number(input.as_bytes(), 0) {
        Some(number) => number.end == input.len(),
        None => false,
    }
}

pub fn match_json_string(input: &[u8], offset: usize) -> Result<JsonStringMatch, RegexError> {
    if input.get(offset) != Some(&b'"') {
        return Err(RegexError {
            offset,
            kind: RegexErrorKind::ExpectedString,
        });
    }

    let mut cursor = offset + 1;
    let mut needs_unescape = false;

    while let Some(byte) = input.get(cursor).copied() {
        match byte {
            b'"' => {
                return Ok(JsonStringMatch {
                    raw_start: offset,
                    raw_end: cursor + 1,
                    content_start: offset + 1,
                    content_end: cursor,
                    needs_unescape,
                });
            }
            b'\\' => {
                needs_unescape = true;
                cursor += 1;
                match input.get(cursor).copied() {
                    Some(b'"' | b'\\' | b'/' | b'b' | b'f' | b'n' | b'r' | b't') => {
                        cursor += 1;
                    }
                    Some(b'u') => {
                        let first_hex = cursor + 1;
                        let end = first_hex + 4;
                        if end > input.len()
                            || !input[first_hex..end].iter().all(|b| b.is_ascii_hexdigit())
                        {
                            return Err(RegexError {
                                offset: cursor.saturating_sub(1),
                                kind: RegexErrorKind::InvalidUnicodeEscape,
                            });
                        }
                        cursor = end;
                    }
                    Some(_) | None => {
                        return Err(RegexError {
                            offset: cursor.saturating_sub(1),
                            kind: RegexErrorKind::InvalidEscape,
                        });
                    }
                }
            }
            0x00..=0x1f => {
                return Err(RegexError {
                    offset: cursor,
                    kind: RegexErrorKind::ControlCharacter,
                });
            }
            _ => cursor += 1,
        }
    }

    Err(RegexError {
        offset,
        kind: RegexErrorKind::UnterminatedString,
    })
}

pub fn classify_json_string_content(
    input: &[u8],
    start: usize,
    end: usize,
) -> Result<bool, RegexError> {
    debug_assert!(start <= end && end <= input.len());
    #[cfg(target_arch = "aarch64")]
    {
        return unsafe { neon_classify_json_string_content(input, start, end) };
    }

    #[allow(unreachable_code)]
    scalar_classify_json_string_content(input, start, end)
}

fn scalar_classify_json_string_content(
    input: &[u8],
    start: usize,
    end: usize,
) -> Result<bool, RegexError> {
    for (relative, byte) in input[start..end].iter().copied().enumerate() {
        if byte == b'\\' {
            return Ok(true);
        }
        if byte < 0x20 {
            return Err(RegexError {
                offset: start + relative,
                kind: RegexErrorKind::ControlCharacter,
            });
        }
    }
    Ok(false)
}

#[cfg(target_arch = "aarch64")]
unsafe fn neon_classify_json_string_content(
    input: &[u8],
    start: usize,
    end: usize,
) -> Result<bool, RegexError> {
    use core::arch::aarch64::*;

    let mut cursor = start;
    unsafe {
        let slash = vdupq_n_u8(b'\\');
        let control_limit = vdupq_n_u8(0x20);
        while cursor + 16 <= end {
            let chunk = vld1q_u8(input.as_ptr().add(cursor));
            let control_mask = movemask_u8x16(vcltq_u8(chunk, control_limit));
            if control_mask != 0 {
                return Err(RegexError {
                    offset: cursor + control_mask.trailing_zeros() as usize,
                    kind: RegexErrorKind::ControlCharacter,
                });
            }
            let slash_mask = movemask_u8x16(vceqq_u8(chunk, slash));
            if slash_mask != 0 {
                return Ok(true);
            }
            cursor += 16;
        }
    }

    scalar_classify_json_string_content(input, cursor, end)
}

#[cfg(target_arch = "aarch64")]
#[inline(always)]
unsafe fn movemask_u8x16(value: core::arch::aarch64::uint8x16_t) -> u16 {
    use core::arch::aarch64::*;

    unsafe {
        let pattern: [u8; 16] = [1, 2, 4, 8, 16, 32, 64, 128, 1, 2, 4, 8, 16, 32, 64, 128];
        let bits = vandq_u8(value, vld1q_u8(pattern.as_ptr()));
        let lo = vaddv_u8(vget_low_u8(bits)) as u16;
        let hi = vaddv_u8(vget_high_u8(bits)) as u16;
        lo | (hi << 8)
    }
}

#[inline]
pub fn validate_json_string(input: &str) -> bool {
    match match_json_string(input.as_bytes(), 0) {
        Ok(span) => span.raw_end == input.len(),
        Err(_) => false,
    }
}

pub fn unescape_json_string(raw_content: &str) -> Result<Cow<'_, str>, RegexError> {
    if !raw_content.as_bytes().contains(&b'\\') {
        return Ok(Cow::Borrowed(raw_content));
    }

    let bytes = raw_content.as_bytes();
    let mut cursor = 0;
    let mut out = String::with_capacity(raw_content.len());

    while cursor < bytes.len() {
        match bytes[cursor] {
            b'\\' => {
                cursor += 1;
                match bytes.get(cursor).copied() {
                    Some(b'"') => {
                        out.push('"');
                        cursor += 1;
                    }
                    Some(b'\\') => {
                        out.push('\\');
                        cursor += 1;
                    }
                    Some(b'/') => {
                        out.push('/');
                        cursor += 1;
                    }
                    Some(b'b') => {
                        out.push('\u{0008}');
                        cursor += 1;
                    }
                    Some(b'f') => {
                        out.push('\u{000c}');
                        cursor += 1;
                    }
                    Some(b'n') => {
                        out.push('\n');
                        cursor += 1;
                    }
                    Some(b'r') => {
                        out.push('\r');
                        cursor += 1;
                    }
                    Some(b't') => {
                        out.push('\t');
                        cursor += 1;
                    }
                    Some(b'u') => {
                        cursor += 1;
                        let first = read_hex_unit(bytes, cursor)?;
                        cursor += 4;
                        let scalar = if is_high_surrogate(first) {
                            if bytes.get(cursor) != Some(&b'\\')
                                || bytes.get(cursor + 1) != Some(&b'u')
                            {
                                return Err(RegexError {
                                    offset: cursor.saturating_sub(6),
                                    kind: RegexErrorKind::InvalidSurrogatePair,
                                });
                            }
                            cursor += 2;
                            let second = read_hex_unit(bytes, cursor)?;
                            cursor += 4;
                            if !is_low_surrogate(second) {
                                return Err(RegexError {
                                    offset: cursor.saturating_sub(4),
                                    kind: RegexErrorKind::InvalidSurrogatePair,
                                });
                            }
                            0x10000 + (((first as u32 - 0xd800) << 10) | (second as u32 - 0xdc00))
                        } else if is_low_surrogate(first) {
                            return Err(RegexError {
                                offset: cursor.saturating_sub(4),
                                kind: RegexErrorKind::InvalidSurrogatePair,
                            });
                        } else {
                            first as u32
                        };

                        let ch = char::from_u32(scalar).ok_or(RegexError {
                            offset: cursor.saturating_sub(4),
                            kind: RegexErrorKind::InvalidUnicodeEscape,
                        })?;
                        out.push(ch);
                    }
                    Some(_) | None => {
                        return Err(RegexError {
                            offset: cursor.saturating_sub(1),
                            kind: RegexErrorKind::InvalidEscape,
                        });
                    }
                }
            }
            0x00..=0x1f => {
                return Err(RegexError {
                    offset: cursor,
                    kind: RegexErrorKind::ControlCharacter,
                });
            }
            _ => {
                let ch = raw_content[cursor..]
                    .chars()
                    .next()
                    .expect("cursor is in bounds");
                out.push(ch);
                cursor += ch.len_utf8();
            }
        }
    }

    Ok(Cow::Owned(out))
}

fn read_hex_unit(bytes: &[u8], offset: usize) -> Result<u16, RegexError> {
    let end = offset + 4;
    if end > bytes.len() || !bytes[offset..end].iter().all(|b| b.is_ascii_hexdigit()) {
        return Err(RegexError {
            offset,
            kind: RegexErrorKind::InvalidUnicodeEscape,
        });
    }

    let mut value = 0u16;
    for byte in &bytes[offset..end] {
        value = (value << 4) | hex_value(*byte) as u16;
    }
    Ok(value)
}

#[inline]
fn hex_value(byte: u8) -> u8 {
    match byte {
        b'0'..=b'9' => byte - b'0',
        b'a'..=b'f' => byte - b'a' + 10,
        b'A'..=b'F' => byte - b'A' + 10,
        _ => unreachable!("caller validates hex digits"),
    }
}

#[inline]
fn is_high_surrogate(unit: u16) -> bool {
    (0xd800..=0xdbff).contains(&unit)
}

#[inline]
fn is_low_surrogate(unit: u16) -> bool {
    (0xdc00..=0xdfff).contains(&unit)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn numbers_match_json_shape() {
        for valid in ["0", "-0", "12", "-12.34", "6.02e23", "1E-9"] {
            assert!(validate_json_number(valid), "{valid}");
        }

        for invalid in ["", "-", "01", "1.", "1e", "+1"] {
            assert!(!validate_json_number(invalid), "{invalid}");
        }
    }

    #[test]
    fn strings_report_escape_state() {
        let plain = match_json_string(br#""abc""#, 0).unwrap();
        assert_eq!(plain.content_start, 1);
        assert_eq!(plain.content_end, 4);
        assert!(!plain.needs_unescape);

        let escaped = match_json_string(br#""a\nb\u0041""#, 0).unwrap();
        assert!(escaped.needs_unescape);
    }

    #[test]
    fn string_content_prefilter_detects_escapes_and_controls() {
        assert!(!classify_json_string_content(b"abc", 0, 3).unwrap());
        assert!(classify_json_string_content(br#"a\nb"#, 0, 4).unwrap());
        assert!(matches!(
            classify_json_string_content(b"a\nb", 0, 3),
            Err(RegexError {
                kind: RegexErrorKind::ControlCharacter,
                ..
            })
        ));
    }

    #[test]
    fn unescape_handles_surrogate_pairs() {
        assert_eq!(unescape_json_string(r#"a\nb"#).unwrap().as_ref(), "a\nb");
        assert_eq!(
            unescape_json_string(r#"\uD834\uDD1E"#).unwrap().as_ref(),
            "\u{1d11e}"
        );
    }
}
