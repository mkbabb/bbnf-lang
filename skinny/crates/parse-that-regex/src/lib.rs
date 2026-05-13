use std::borrow::Cow;
use std::fmt;

pub mod integration;

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
            b' ' | b'\r' | b'\t' => offset += 1,
            b'\n' => {
                offset += 1;
                offset = skip_ascii_spaces(input, offset);
            }
            _ => break,
        }
    }
    offset
}

#[inline(always)]
fn skip_ascii_spaces(input: &[u8], mut cursor: usize) -> usize {
    const SPACES: u64 = 0x2020_2020_2020_2020;
    const ONES: u64 = 0x0101_0101_0101_0101;
    const HIGH_BITS: u64 = 0x8080_8080_8080_8080;

    while cursor + 8 <= input.len() {
        let block = unsafe { std::ptr::read_unaligned(input.as_ptr().add(cursor).cast::<u64>()) };
        let space_hits = zero_byte_mask(block ^ SPACES, ONES, HIGH_BITS);
        if space_hits != HIGH_BITS {
            let non_spaces = !space_hits & HIGH_BITS;
            return cursor + (non_spaces.trailing_zeros() as usize / 8);
        }
        cursor += 8;
    }

    while cursor < input.len() && input[cursor] == b' ' {
        cursor += 1;
    }
    cursor
}

#[inline(always)]
pub fn match_json_number(input: &[u8], offset: usize) -> Option<JsonNumberMatch> {
    let len = input.len();
    if offset >= len {
        return None;
    }

    match_json_number_from_first(input, offset, input[offset])
}

#[inline(always)]
pub fn match_json_number_from_first(
    input: &[u8],
    offset: usize,
    first: u8,
) -> Option<JsonNumberMatch> {
    let len = input.len();
    let mut cursor = offset;

    if first == b'-' {
        cursor += 1;
        if cursor >= len {
            return None;
        }
        match input[cursor] {
            b'0' => cursor += 1,
            b'1'..=b'9' => {
                cursor += 1;
                cursor = skip_ascii_digits(input, cursor);
            }
            _ => return None,
        }
    } else {
        match first {
            b'0' => cursor += 1,
            b'1'..=b'9' => {
                cursor += 1;
                cursor = skip_ascii_digits(input, cursor);
            }
            _ => return None,
        }
    }

    if cursor < len && input[cursor] == b'.' {
        cursor += 1;
        let digits_start = cursor;
        cursor = skip_ascii_digits(input, cursor);
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
        cursor = skip_ascii_digits(input, cursor);
        if cursor == digits_start {
            return None;
        }
    }

    Some(JsonNumberMatch {
        start: offset,
        end: cursor,
    })
}

#[inline(always)]
fn skip_ascii_digits(input: &[u8], mut cursor: usize) -> usize {
    const ZEROES: u64 = 0x3030_3030_3030_3030;
    const NINES: u64 = 0x3939_3939_3939_3939;
    const HIGH_BITS: u64 = 0x8080_8080_8080_8080;

    while cursor + 8 <= input.len() {
        let block = unsafe { std::ptr::read_unaligned(input.as_ptr().add(cursor).cast::<u64>()) };
        let below_zero = block.wrapping_sub(ZEROES);
        let above_nine = NINES.wrapping_sub(block);
        if ((below_zero | above_nine) & HIGH_BITS) != 0 {
            break;
        }
        cursor += 8;
    }

    while cursor < input.len() && input[cursor].is_ascii_digit() {
        cursor += 1;
    }
    cursor
}

#[inline]
pub fn validate_json_number(input: &str) -> bool {
    match match_json_number(input.as_bytes(), 0) {
        Some(number) => number.end == input.len(),
        None => false,
    }
}

#[inline]
pub fn match_json_string(input: &[u8], offset: usize) -> Result<JsonStringMatch, RegexError> {
    if input.get(offset) != Some(&b'"') {
        return Err(RegexError {
            offset,
            kind: RegexErrorKind::ExpectedString,
        });
    }

    match_json_string_at_quote(input, offset)
}

#[inline(always)]
pub fn match_json_string_at_quote(
    input: &[u8],
    offset: usize,
) -> Result<JsonStringMatch, RegexError> {
    debug_assert_eq!(input.get(offset), Some(&b'"'));
    let mut cursor = skip_json_string_plain(input, offset + 1);
    let mut needs_unescape = false;

    loop {
        let Some(byte) = input.get(cursor).copied() else {
            break;
        };
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
                cursor = validate_json_string_escape(input, cursor)?;
            }
            0x00..=0x1f => {
                return Err(RegexError {
                    offset: cursor,
                    kind: RegexErrorKind::ControlCharacter,
                });
            }
            _ => {
                cursor += 1;
                cursor = skip_json_string_plain(input, cursor);
            }
        }
    }

    Err(RegexError {
        offset,
        kind: RegexErrorKind::UnterminatedString,
    })
}

#[inline(always)]
fn validate_json_string_escape(input: &[u8], slash: usize) -> Result<usize, RegexError> {
    let escape = slash + 1;
    match input.get(escape).copied() {
        Some(b'"' | b'\\' | b'/' | b'b' | b'f' | b'n' | b'r' | b't') => Ok(escape + 1),
        Some(b'u') => validate_json_unicode_escape_run(input, slash),
        Some(_) | None => Err(RegexError {
            offset: slash,
            kind: RegexErrorKind::InvalidEscape,
        }),
    }
}

#[inline(always)]
fn validate_json_unicode_escape_run(input: &[u8], mut slash: usize) -> Result<usize, RegexError> {
    loop {
        let first_hex = slash + 2;
        let first = read_hex_unit_with_error_offset(input, first_hex, slash)?;
        let mut cursor = first_hex + 4;

        if is_high_surrogate(first) {
            if input.get(cursor) != Some(&b'\\') || input.get(cursor + 1) != Some(&b'u') {
                return Err(RegexError {
                    offset: slash,
                    kind: RegexErrorKind::InvalidSurrogatePair,
                });
            }
            let second_hex = cursor + 2;
            let second = read_hex_unit_with_error_offset(input, second_hex, second_hex)?;
            if !is_low_surrogate(second) {
                return Err(RegexError {
                    offset: second_hex,
                    kind: RegexErrorKind::InvalidSurrogatePair,
                });
            }
            cursor = second_hex + 4;
        } else if is_low_surrogate(first) {
            return Err(RegexError {
                offset: first_hex,
                kind: RegexErrorKind::InvalidSurrogatePair,
            });
        }

        if input.get(cursor) == Some(&b'\\') && input.get(cursor + 1) == Some(&b'u') {
            slash = cursor;
        } else {
            return Ok(cursor);
        }
    }
}

#[inline(always)]
fn skip_json_string_plain(input: &[u8], mut cursor: usize) -> usize {
    while cursor + 8 <= input.len() {
        let block = unsafe { std::ptr::read_unaligned(input.as_ptr().add(cursor).cast::<u64>()) };
        let interesting = json_string_interesting_mask(block);
        if interesting != 0 {
            return cursor + (interesting.trailing_zeros() as usize / 8);
        }
        cursor += 8;
    }
    cursor
}

#[inline(always)]
fn json_string_interesting_mask(block: u64) -> u64 {
    const QUOTES: u64 = 0x2222_2222_2222_2222;
    const SLASHES: u64 = 0x5c5c_5c5c_5c5c_5c5c;
    const CONTROL_LIMITS: u64 = 0x2020_2020_2020_2020;
    const ONES: u64 = 0x0101_0101_0101_0101;
    const HIGH_BITS: u64 = 0x8080_8080_8080_8080;

    let quote_hits = zero_byte_mask(block ^ QUOTES, ONES, HIGH_BITS);
    let slash_hits = zero_byte_mask(block ^ SLASHES, ONES, HIGH_BITS);
    let control_hits = block.wrapping_sub(CONTROL_LIMITS) & !block & HIGH_BITS;
    quote_hits | slash_hits | control_hits
}

#[inline(always)]
fn zero_byte_mask(block: u64, ones: u64, high_bits: u64) -> u64 {
    block.wrapping_sub(ones) & !block & high_bits
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
    read_hex_unit_with_error_offset(bytes, offset, offset)
}

#[inline(always)]
fn read_hex_unit_with_error_offset(
    bytes: &[u8],
    offset: usize,
    error_offset: usize,
) -> Result<u16, RegexError> {
    let end = offset + 4;
    let Some(hex) = bytes.get(offset..end) else {
        return Err(RegexError {
            offset: error_offset,
            kind: RegexErrorKind::InvalidUnicodeEscape,
        });
    };

    let n0 = hex_nibble(hex[0]);
    let n1 = hex_nibble(hex[1]);
    let n2 = hex_nibble(hex[2]);
    let n3 = hex_nibble(hex[3]);

    if (n0 | n1 | n2 | n3) & 0xf0 != 0 {
        return Err(RegexError {
            offset: error_offset,
            kind: RegexErrorKind::InvalidUnicodeEscape,
        });
    }

    Ok(((n0 as u16) << 12) | ((n1 as u16) << 8) | ((n2 as u16) << 4) | n3 as u16)
}

#[inline(always)]
fn hex_nibble(byte: u8) -> u8 {
    match byte {
        b'0'..=b'9' => byte - b'0',
        b'a'..=b'f' => byte - b'a' + 10,
        b'A'..=b'F' => byte - b'A' + 10,
        _ => 0xff,
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
    fn string_matcher_accepts_dense_unicode_escape_runs() {
        let input = br#""\u0041\u03a9\uD834\uDD1E""#;
        let escaped = match_json_string(input, 0).unwrap();
        assert_eq!(escaped.raw_end, input.len());
        assert!(escaped.needs_unescape);
    }

    #[test]
    fn string_matcher_rejects_bad_escapes_and_surrogate_pairs() {
        for (input, kind) in [
            (br#""\q""#.as_slice(), RegexErrorKind::InvalidEscape),
            (
                br#""\u12G4""#.as_slice(),
                RegexErrorKind::InvalidUnicodeEscape,
            ),
            (
                br#""\u123""#.as_slice(),
                RegexErrorKind::InvalidUnicodeEscape,
            ),
            (
                br#""\uD834x""#.as_slice(),
                RegexErrorKind::InvalidSurrogatePair,
            ),
            (
                br#""\uD834\u0041""#.as_slice(),
                RegexErrorKind::InvalidSurrogatePair,
            ),
            (
                br#""\uD834\uZZZZ""#.as_slice(),
                RegexErrorKind::InvalidUnicodeEscape,
            ),
        ] {
            let error = match_json_string(input, 0).unwrap_err();
            assert_eq!(error.kind, kind, "{input:?}");
        }
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

    #[test]
    fn string_matcher_rejects_lone_surrogates() {
        assert!(matches!(
            match_json_string(br#""\uD800""#, 0),
            Err(RegexError {
                kind: RegexErrorKind::InvalidSurrogatePair,
                ..
            })
        ));
        assert!(matches!(
            match_json_string(br#""\uDD1E""#, 0),
            Err(RegexError {
                kind: RegexErrorKind::InvalidSurrogatePair,
                ..
            })
        ));
    }

    #[test]
    fn unescape_accepts_unicode_noncharacters() {
        assert_eq!(
            unescape_json_string(r#"\uDBFF\uDFFE"#).unwrap().as_ref(),
            "\u{10fffe}"
        );
    }
}
