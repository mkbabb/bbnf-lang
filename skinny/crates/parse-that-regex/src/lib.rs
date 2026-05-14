use std::borrow::Cow;
use std::fmt;

pub mod integration;
pub mod number;
pub mod unicode;

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
    InvalidUtf8,
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
pub enum StringMode {
    StrictJson,
    StrictJsonTrustedUtf8,
    GrammarString,
    ByteString,
}

impl StringMode {
    #[inline]
    const fn validates_utf8(self) -> bool {
        matches!(self, Self::StrictJson | Self::GrammarString)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub struct StringFlags {
    bits: u8,
}

impl StringFlags {
    pub const HAS_ESC: Self = Self { bits: 1 << 0 };
    pub const HAS_CONTROL: Self = Self { bits: 1 << 1 };
    pub const HAS_NON_ASCII: Self = Self { bits: 1 << 2 };
    pub const NEEDS_DECODE: Self = Self { bits: 1 << 3 };
    pub const UTF8_VALIDATED: Self = Self { bits: 1 << 4 };

    #[inline]
    pub const fn empty() -> Self {
        Self { bits: 0 }
    }

    #[inline]
    pub const fn bits(self) -> u8 {
        self.bits
    }

    #[inline]
    pub const fn contains(self, flag: Self) -> bool {
        self.bits & flag.bits == flag.bits
    }

    #[inline]
    pub fn insert(&mut self, flag: Self) {
        self.bits |= flag.bits;
    }

    #[inline]
    pub fn union(&mut self, other: Self) {
        self.bits |= other.bits;
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct StringMatch {
    pub raw_start: usize,
    pub raw_end: usize,
    pub flags: StringFlags,
}

impl StringMatch {
    #[inline]
    pub const fn content_start(self) -> usize {
        self.raw_start + 1
    }

    #[inline]
    pub const fn content_end(self) -> usize {
        self.raw_end - 1
    }

    #[inline]
    pub const fn needs_decode(self) -> bool {
        self.flags.contains(StringFlags::NEEDS_DECODE)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct JsonNumberMatch {
    pub start: usize,
    pub end: usize,
    pub is_integer: bool,
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
    let mut is_integer = true;

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
        is_integer = false;
        cursor += 1;
        let digits_start = cursor;
        cursor = skip_ascii_digits(input, cursor);
        if cursor == digits_start {
            return None;
        }
    }

    if cursor < len && matches!(input[cursor], b'e' | b'E') {
        is_integer = false;
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
        is_integer,
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
    match_string_at_quote(input, offset, StringMode::StrictJson).map(|span| JsonStringMatch {
        raw_start: span.raw_start,
        raw_end: span.raw_end,
        content_start: span.content_start(),
        content_end: span.content_end(),
        needs_unescape: span.needs_decode(),
    })
}

#[inline(always)]
/// Matches a JSON string whose input buffer has already been validated as UTF-8.
///
/// This is for generated parsers that receive `&str` and therefore do not need
/// to repeat raw UTF-8 validation while scanning JSON string delimiters.
pub fn match_json_string_at_quote_trusted_utf8(
    input: &[u8],
    offset: usize,
) -> Result<JsonStringMatch, RegexError> {
    debug_assert_eq!(input.get(offset), Some(&b'"'));
    let mut cursor = skip_json_string_plain_trusted(input, offset + 1);
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
                cursor = skip_json_string_plain_trusted(input, cursor);
            }
        }
    }

    Err(RegexError {
        offset,
        kind: RegexErrorKind::UnterminatedString,
    })
}

#[inline]
pub fn match_string(
    input: &[u8],
    offset: usize,
    mode: StringMode,
) -> Result<StringMatch, RegexError> {
    if input.get(offset) != Some(&b'"') {
        return Err(RegexError {
            offset,
            kind: RegexErrorKind::ExpectedString,
        });
    }

    match_string_at_quote(input, offset, mode)
}

#[inline(always)]
pub fn match_string_at_quote(
    input: &[u8],
    offset: usize,
    mode: StringMode,
) -> Result<StringMatch, RegexError> {
    debug_assert_eq!(input.get(offset), Some(&b'"'));
    let mut scan = skip_json_string_plain(input, offset + 1, mode)?;
    let mut cursor = scan.cursor;
    let mut flags = scan.flags;

    loop {
        let Some(byte) = input.get(cursor).copied() else {
            break;
        };
        match byte {
            b'"' => {
                if mode != StringMode::ByteString {
                    flags.insert(StringFlags::UTF8_VALIDATED);
                }
                return Ok(StringMatch {
                    raw_start: offset,
                    raw_end: cursor + 1,
                    flags,
                });
            }
            b'\\' => {
                flags.insert(StringFlags::HAS_ESC);
                flags.insert(StringFlags::NEEDS_DECODE);
                cursor = validate_json_string_escape(input, cursor)?;
                scan = skip_json_string_plain(input, cursor, mode)?;
                flags.union(scan.flags);
                cursor = scan.cursor;
            }
            0x00..=0x1f => {
                flags.insert(StringFlags::HAS_CONTROL);
                return Err(RegexError {
                    offset: cursor,
                    kind: RegexErrorKind::ControlCharacter,
                });
            }
            _ => {
                cursor += 1;
                scan = skip_json_string_plain(input, cursor, mode)?;
                flags.union(scan.flags);
                cursor = scan.cursor;
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

#[derive(Debug, Clone, Copy)]
struct PlainStringScan {
    cursor: usize,
    flags: StringFlags,
}

pub fn decode_json_unicode_escape(input: &[u8], slash: usize) -> Result<(char, usize), RegexError> {
    if input.get(slash) != Some(&b'\\') || input.get(slash + 1) != Some(&b'u') {
        return Err(RegexError {
            offset: slash,
            kind: RegexErrorKind::InvalidUnicodeEscape,
        });
    }

    let first_hex = slash + 2;
    let first = read_hex_unit_with_error_offset(input, first_hex, slash)?;
    let mut cursor = first_hex + 4;
    let scalar = if is_high_surrogate(first) {
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
        0x10000 + (((first as u32 - 0xd800) << 10) | (second as u32 - 0xdc00))
    } else if is_low_surrogate(first) {
        return Err(RegexError {
            offset: first_hex,
            kind: RegexErrorKind::InvalidSurrogatePair,
        });
    } else {
        first as u32
    };

    let ch = char::from_u32(scalar).ok_or(RegexError {
        offset: first_hex,
        kind: RegexErrorKind::InvalidUnicodeEscape,
    })?;
    Ok((ch, cursor))
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

#[cfg(target_arch = "aarch64")]
#[inline]
fn unescape_four_unicode_escapes(
    input: &[u8],
    slash: usize,
    out: &mut String,
) -> Option<Result<usize, RegexError>> {
    let mut packed = [0u8; 16];
    for index in 0..4 {
        let escape = slash + index * 6;
        if input.get(escape) != Some(&b'\\') || input.get(escape + 1) != Some(&b'u') {
            return None;
        }
        let hex = input.get(escape + 2..escape + 6)?;
        packed[index * 4..index * 4 + 4].copy_from_slice(hex);
    }

    let Some(units) = (unsafe {
        bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_x4_neon(&packed)
    }) else {
        return Some(Err(RegexError {
            offset: slash,
            kind: RegexErrorKind::InvalidUnicodeEscape,
        }));
    };

    let mut chars = ['\0'; 4];
    let mut char_count = 0usize;
    let mut index = 0usize;
    while index < units.len() {
        let unit = units[index];
        if (0xd800..=0xdbff).contains(&unit) {
            if index + 1 >= units.len() {
                return None;
            }
            let Some(scalar) = bbnf_simd::aarch64::unescape_uxxxx::join_surrogate_pair_neon(
                unit,
                units[index + 1],
            ) else {
                return Some(Err(RegexError {
                    offset: slash + (index + 1) * 6 + 2,
                    kind: RegexErrorKind::InvalidSurrogatePair,
                }));
            };
            let Some(ch) = char::from_u32(scalar) else {
                return Some(Err(RegexError {
                    offset: slash + index * 6 + 2,
                    kind: RegexErrorKind::InvalidUnicodeEscape,
                }));
            };
            chars[char_count] = ch;
            char_count += 1;
            index += 2;
        } else if (0xdc00..=0xdfff).contains(&unit) {
            return Some(Err(RegexError {
                offset: slash + index * 6 + 2,
                kind: RegexErrorKind::InvalidSurrogatePair,
            }));
        } else {
            let Some(ch) = char::from_u32(unit) else {
                return Some(Err(RegexError {
                    offset: slash + index * 6 + 2,
                    kind: RegexErrorKind::InvalidUnicodeEscape,
                }));
            };
            chars[char_count] = ch;
            char_count += 1;
            index += 1;
        }
    }

    for ch in chars.iter().take(char_count) {
        out.push(*ch);
    }
    Some(Ok(slash + 24))
}

#[inline(always)]
fn skip_json_string_plain(
    input: &[u8],
    mut cursor: usize,
    mode: StringMode,
) -> Result<PlainStringScan, RegexError> {
    let mut flags = StringFlags::empty();

    #[cfg(target_arch = "aarch64")]
    unsafe {
        while cursor + 16 <= input.len() {
            let block = bbnf_simd::aarch64::string_block::scan_string_special_block(
                input.as_ptr().add(cursor),
                b'"',
                b'\\',
                0x20,
            );
            let special_mask = block.terminator_mask | block.escape_mask | block.control_mask;
            if special_mask != 0 {
                let special = special_mask.trailing_zeros() as usize;
                if mode.validates_utf8()
                    && first_non_ascii_before(block.non_ascii_mask, special)
                {
                    flags.union(validate_utf8_prefix(input, cursor, special, mode)?);
                }
                return Ok(PlainStringScan {
                    cursor: cursor + special,
                    flags,
                });
            }
            if block.non_ascii_mask != 0 && mode.validates_utf8() {
                flags.insert(StringFlags::HAS_NON_ASCII);
                let status = bbnf_simd::aarch64::utf8::validate_block(input.as_ptr().add(cursor));
                if status.is_valid_and_complete() {
                    cursor += 16;
                    continue;
                }
                if status.is_valid_but_continues() {
                    cursor += status.complete_bytes() as usize;
                    cursor = validate_utf8_codepoint(input, cursor)?;
                    continue;
                }
                return Err(RegexError {
                    offset: cursor + status.bad_byte_offset().unwrap_or(0) as usize,
                    kind: RegexErrorKind::InvalidUtf8,
                });
            }
            cursor += 16;
        }
    }

    while cursor + 8 <= input.len() {
        let block = unsafe { std::ptr::read_unaligned(input.as_ptr().add(cursor).cast::<u64>()) };
        let special = json_string_special_mask(block);
        let non_ascii = block & HIGH_BITS;
        if special != 0 {
            let first_special = special.trailing_zeros() as usize / 8;
            if mode.validates_utf8() && first_non_ascii_byte_before(non_ascii, first_special)
            {
                flags.union(validate_utf8_prefix(input, cursor, first_special, mode)?);
            }
            return Ok(PlainStringScan {
                cursor: cursor + first_special,
                flags,
            });
        }
        if non_ascii != 0 && mode.validates_utf8() {
            flags.union(validate_utf8_prefix(input, cursor, 8, mode)?);
            cursor += 8;
            continue;
        }
        cursor += 8;
    }

    while cursor < input.len() {
        match input[cursor] {
            b'"' | b'\\' | 0x00..=0x1f => break,
            0x80..=0xff if mode.validates_utf8() => {
                flags.insert(StringFlags::HAS_NON_ASCII);
                cursor = validate_utf8_codepoint(input, cursor)?;
            }
            _ => cursor += 1,
        }
    }

    Ok(PlainStringScan { cursor, flags })
}

#[inline(always)]
fn skip_json_string_plain_trusted(input: &[u8], mut cursor: usize) -> usize {
    #[cfg(target_arch = "aarch64")]
    unsafe {
        while cursor + 16 <= input.len() {
            let (quote_mask, backslash_mask, control_mask) =
                bbnf_simd::aarch64::string_block::quote_escape_control_masks(
                    input.as_ptr().add(cursor),
                );
            let special_mask = quote_mask | backslash_mask | control_mask;
            if special_mask != 0 {
                return cursor + special_mask.trailing_zeros() as usize;
            }
            cursor += 16;
        }
    }

    while cursor + 8 <= input.len() {
        let block = unsafe { std::ptr::read_unaligned(input.as_ptr().add(cursor).cast::<u64>()) };
        let special = json_string_special_mask(block);
        if special != 0 {
            return cursor + (special.trailing_zeros() as usize / 8);
        }
        cursor += 8;
    }
    cursor
}

#[inline(always)]
fn json_string_special_mask(block: u64) -> u64 {
    const QUOTES: u64 = 0x2222_2222_2222_2222;
    const SLASHES: u64 = 0x5c5c_5c5c_5c5c_5c5c;
    const CONTROL_LIMITS: u64 = 0x2020_2020_2020_2020;
    const ONES: u64 = 0x0101_0101_0101_0101;

    let quote_hits = zero_byte_mask(block ^ QUOTES, ONES, HIGH_BITS);
    let slash_hits = zero_byte_mask(block ^ SLASHES, ONES, HIGH_BITS);
    let control_hits = block.wrapping_sub(CONTROL_LIMITS) & !block & HIGH_BITS;
    quote_hits | slash_hits | control_hits
}

const HIGH_BITS: u64 = 0x8080_8080_8080_8080;

#[inline(always)]
fn first_non_ascii_before(mask: u16, byte_index: usize) -> bool {
    mask != 0 && (mask.trailing_zeros() as usize) < byte_index
}

#[inline(always)]
fn first_non_ascii_byte_before(mask: u64, byte_index: usize) -> bool {
    mask != 0 && (mask.trailing_zeros() as usize / 8) < byte_index
}

#[inline(always)]
fn validate_utf8_prefix(
    input: &[u8],
    start: usize,
    len: usize,
    mode: StringMode,
) -> Result<StringFlags, RegexError> {
    let mut cursor = start;
    let end = start + len;
    let mut flags = StringFlags::empty();
    while cursor < end {
        match input[cursor] {
            0x80..=0xff if mode.validates_utf8() => {
                flags.insert(StringFlags::HAS_NON_ASCII);
                cursor = validate_utf8_codepoint(input, cursor)?;
                if cursor > end {
                    return Err(RegexError {
                        offset: end,
                        kind: RegexErrorKind::InvalidUtf8,
                    });
                }
            }
            _ => cursor += 1,
        }
    }
    Ok(flags)
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
        classify_json_string_content(raw_content.as_bytes(), 0, raw_content.len())?;
        return Ok(Cow::Borrowed(raw_content));
    }

    let bytes = raw_content.as_bytes();
    let mut cursor = 0;
    let mut segment_start = 0;
    let mut out = String::with_capacity(raw_content.len());

    while cursor < bytes.len() {
        if !matches!(bytes[cursor], b'\\' | 0x00..=0x1f) {
            cursor = find_next_escape_or_control(bytes, cursor);
        }
        if cursor >= bytes.len() {
            break;
        }
        match bytes[cursor] {
            b'\\' => {
                if segment_start < cursor {
                    out.push_str(&raw_content[segment_start..cursor]);
                }
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
                        let slash = cursor - 1;
                        #[cfg(target_arch = "aarch64")]
                        if let Some(batch) =
                            unescape_four_unicode_escapes(bytes, slash, &mut out)
                        {
                            cursor = batch?;
                            segment_start = cursor;
                            continue;
                        }
                        let (ch, next) = decode_json_unicode_escape(bytes, slash)?;
                        out.push(ch);
                        cursor = next;
                    }
                    Some(_) | None => {
                        return Err(RegexError {
                            offset: cursor.saturating_sub(1),
                            kind: RegexErrorKind::InvalidEscape,
                        });
                    }
                }
                segment_start = cursor;
            }
            0x00..=0x1f => {
                return Err(RegexError {
                    offset: cursor,
                    kind: RegexErrorKind::ControlCharacter,
                });
            }
            _ => unreachable!("find_next_escape_or_control stops only on escape/control bytes"),
        }
    }

    if segment_start < raw_content.len() {
        out.push_str(&raw_content[segment_start..]);
    }
    Ok(Cow::Owned(out))
}

#[inline(always)]
fn find_next_escape_or_control(input: &[u8], mut cursor: usize) -> usize {
    while cursor + 8 <= input.len() {
        let block = unsafe { std::ptr::read_unaligned(input.as_ptr().add(cursor).cast::<u64>()) };
        let mask = json_string_escape_control_mask(block);
        if mask != 0 {
            return cursor + (mask.trailing_zeros() as usize / 8);
        }
        cursor += 8;
    }
    while cursor < input.len() {
        match input[cursor] {
            b'\\' | 0x00..=0x1f => break,
            _ => cursor += 1,
        }
    }
    cursor
}

#[inline(always)]
fn json_string_escape_control_mask(block: u64) -> u64 {
    const SLASHES: u64 = 0x5c5c_5c5c_5c5c_5c5c;
    const CONTROL_LIMITS: u64 = 0x2020_2020_2020_2020;
    const ONES: u64 = 0x0101_0101_0101_0101;

    let slash_hits = zero_byte_mask(block ^ SLASHES, ONES, HIGH_BITS);
    let control_hits = block.wrapping_sub(CONTROL_LIMITS) & !block & HIGH_BITS;
    slash_hits | control_hits
}

#[inline(always)]
fn validate_utf8_codepoint(input: &[u8], cursor: usize) -> Result<usize, RegexError> {
    let first = input[cursor];
    let valid = match first {
        0xc2..=0xdf => input
            .get(cursor + 1)
            .is_some_and(|&second| is_utf8_continuation(second))
            .then_some(2),
        0xe0 => (input
            .get(cursor + 1)
            .is_some_and(|&second| (0xa0..=0xbf).contains(&second))
            && input
                .get(cursor + 2)
                .is_some_and(|&third| is_utf8_continuation(third)))
        .then_some(3),
        0xe1..=0xec | 0xee..=0xef => (input
            .get(cursor + 1)
            .is_some_and(|&second| is_utf8_continuation(second))
            && input
                .get(cursor + 2)
                .is_some_and(|&third| is_utf8_continuation(third)))
        .then_some(3),
        0xed => (input
            .get(cursor + 1)
            .is_some_and(|&second| (0x80..=0x9f).contains(&second))
            && input
                .get(cursor + 2)
                .is_some_and(|&third| is_utf8_continuation(third)))
        .then_some(3),
        0xf0 => (input
            .get(cursor + 1)
            .is_some_and(|&second| (0x90..=0xbf).contains(&second))
            && input
                .get(cursor + 2)
                .is_some_and(|&third| is_utf8_continuation(third))
            && input
                .get(cursor + 3)
                .is_some_and(|&fourth| is_utf8_continuation(fourth)))
        .then_some(4),
        0xf1..=0xf3 => (input
            .get(cursor + 1)
            .is_some_and(|&second| is_utf8_continuation(second))
            && input
                .get(cursor + 2)
                .is_some_and(|&third| is_utf8_continuation(third))
            && input
                .get(cursor + 3)
                .is_some_and(|&fourth| is_utf8_continuation(fourth)))
        .then_some(4),
        0xf4 => (input
            .get(cursor + 1)
            .is_some_and(|&second| (0x80..=0x8f).contains(&second))
            && input
                .get(cursor + 2)
                .is_some_and(|&third| is_utf8_continuation(third))
            && input
                .get(cursor + 3)
                .is_some_and(|&fourth| is_utf8_continuation(fourth)))
        .then_some(4),
        _ => None,
    };

    match valid {
        Some(width) => Ok(cursor + width),
        None => Err(RegexError {
            offset: cursor,
            kind: RegexErrorKind::InvalidUtf8,
        }),
    }
}

#[inline(always)]
fn is_utf8_continuation(byte: u8) -> bool {
    (0x80..=0xbf).contains(&byte)
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

    let unit = read_hex_unit_scalar(hex);

    let Some(unit) = unit else {
        return Err(RegexError {
            offset: error_offset,
            kind: RegexErrorKind::InvalidUnicodeEscape,
        });
    };

    Ok(unit)
}

#[inline(always)]
fn read_hex_unit_scalar(hex: &[u8]) -> Option<u16> {
    let n0 = hex_nibble(hex[0]);
    let n1 = hex_nibble(hex[1]);
    let n2 = hex_nibble(hex[2]);
    let n3 = hex_nibble(hex[3]);

    if (n0 | n1 | n2 | n3) & 0xf0 != 0 {
        return None;
    }

    Some(((n0 as u16) << 12) | ((n1 as u16) << 8) | ((n2 as u16) << 4) | n3 as u16)
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
    fn string_primitive_reports_flags() {
        let plain = match_string("\"abc\"".as_bytes(), 0, StringMode::StrictJson).unwrap();
        assert_eq!(plain.raw_start, 0);
        assert_eq!(plain.raw_end, 5);
        assert!(plain.flags.contains(StringFlags::UTF8_VALIDATED));
        assert!(!plain.flags.contains(StringFlags::HAS_ESC));
        assert!(!plain.flags.contains(StringFlags::HAS_NON_ASCII));

        let unicode =
            match_string("\"cafe\u{00e9}\"".as_bytes(), 0, StringMode::StrictJson).unwrap();
        assert!(unicode.flags.contains(StringFlags::HAS_NON_ASCII));
        assert!(unicode.flags.contains(StringFlags::UTF8_VALIDATED));

        let escaped = match_string(br#""\u0041""#, 0, StringMode::GrammarString).unwrap();
        assert!(escaped.flags.contains(StringFlags::HAS_ESC));
        assert!(escaped.flags.contains(StringFlags::NEEDS_DECODE));
    }

    #[test]
    fn byte_string_mode_does_not_validate_raw_utf8() {
        let bytes = b"\"\x80\xff\"";
        let span = match_string(bytes, 0, StringMode::ByteString).unwrap();
        assert_eq!(span.raw_end, bytes.len());
        assert!(!span.flags.contains(StringFlags::UTF8_VALIDATED));
        assert!(!span.flags.contains(StringFlags::HAS_NON_ASCII));
    }

    #[test]
    fn string_matcher_accepts_dense_unicode_escape_runs() {
        let input = br#""\u0041\u03a9\uD834\uDD1E""#;
        let escaped = match_json_string(input, 0).unwrap();
        assert_eq!(escaped.raw_end, input.len());
        assert!(escaped.needs_unescape);
    }

    #[test]
    fn string_matcher_validates_raw_utf8() {
        for input in [
            b"\"\xc0\x80\"".as_slice(),
            b"\"\xe2\x82\"".as_slice(),
            b"\"\xed\xa0\x80\"".as_slice(),
            b"\"\xf4\x90\x80\x80\"".as_slice(),
            b"\"\x80\"".as_slice(),
        ] {
            let error = match_json_string(input, 0).unwrap_err();
            assert_eq!(error.kind, RegexErrorKind::InvalidUtf8, "{input:?}");
            assert_eq!(error.offset, 1, "{input:?}");
        }
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
    fn string_matcher_reports_error_offsets() {
        for (input, offset, kind) in [
            (br#"xx"a\q""#.as_slice(), 4, RegexErrorKind::InvalidEscape),
            (
                br#"xx"\u12G4""#.as_slice(),
                3,
                RegexErrorKind::InvalidUnicodeEscape,
            ),
            (
                br#"xx"\uD834x""#.as_slice(),
                3,
                RegexErrorKind::InvalidSurrogatePair,
            ),
            (
                br#"xx"\uD834\u0041""#.as_slice(),
                11,
                RegexErrorKind::InvalidSurrogatePair,
            ),
            (
                br#"xx"\uD834\uZZZZ""#.as_slice(),
                11,
                RegexErrorKind::InvalidUnicodeEscape,
            ),
            (b"xx\"\xe2\x82\"".as_slice(), 3, RegexErrorKind::InvalidUtf8),
        ] {
            let error = match_json_string(input, 2).unwrap_err();
            assert_eq!(error.kind, kind, "{input:?}");
            assert_eq!(error.offset, offset, "{input:?}");
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
    fn unescape_handles_all_json_escapes() {
        assert_eq!(
            unescape_json_string(r#"\"\\\/\b\f\n\r\t"#)
                .unwrap()
                .as_ref(),
            "\"\\/\u{0008}\u{000c}\n\r\t"
        );
    }

    #[test]
    fn unescape_rejects_unescaped_controls_on_fast_path() {
        let error = unescape_json_string("a\nb").unwrap_err();
        assert_eq!(error.kind, RegexErrorKind::ControlCharacter);
        assert_eq!(error.offset, 1);
    }

    #[test]
    fn unicode_escape_primitive_decodes_boundaries() {
        assert_eq!(
            decode_json_unicode_escape(br#"\u0000"#, 0).unwrap(),
            ('\u{0000}', 6)
        );
        assert_eq!(
            decode_json_unicode_escape(br#"\uFFFF"#, 0).unwrap(),
            ('\u{ffff}', 6)
        );
        assert_eq!(
            decode_json_unicode_escape(br#"\uD800\uDC00"#, 0).unwrap(),
            ('\u{10000}', 12)
        );
        assert_eq!(
            decode_json_unicode_escape(br#"\uDBFF\uDFFF"#, 0).unwrap(),
            ('\u{10ffff}', 12)
        );
    }

    #[test]
    fn unescape_rejects_invalid_unicode_with_offsets() {
        for (input, offset, kind) in [
            (r#"\u12G4"#, 0, RegexErrorKind::InvalidUnicodeEscape),
            (r#"\u123"#, 0, RegexErrorKind::InvalidUnicodeEscape),
            (r#"\uD800"#, 0, RegexErrorKind::InvalidSurrogatePair),
            (r#"\uDD1E"#, 2, RegexErrorKind::InvalidSurrogatePair),
            (r#"\uD834\u0041"#, 8, RegexErrorKind::InvalidSurrogatePair),
            (r#"\uD834\uZZZZ"#, 8, RegexErrorKind::InvalidUnicodeEscape),
        ] {
            let error = unescape_json_string(input).unwrap_err();
            assert_eq!(error.kind, kind, "{input}");
            assert_eq!(error.offset, offset, "{input}");
        }
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
