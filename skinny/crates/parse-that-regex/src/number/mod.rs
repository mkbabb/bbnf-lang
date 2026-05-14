pub mod eisel_lemire;
pub mod integer;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NumberSpan {
    pub start: usize,
    pub end: usize,
    pub is_integer: bool,
    pub negative: bool,
    pub digit_count: u32,
    pub decimal_exp: i32,
    pub mantissa: u64,
    pub mantissa_overflow: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumberError {
    Invalid,
    NotInteger,
    Overflow,
}

impl std::fmt::Display for NumberError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl std::error::Error for NumberError {}

#[inline(always)]
pub fn match_number_span(input: &[u8], offset: usize) -> Option<NumberSpan> {
    let first = *input.get(offset)?;
    match_number_span_from_first(input, offset, first)
}

#[inline(always)]
pub fn match_number_span_from_first(input: &[u8], offset: usize, first: u8) -> Option<NumberSpan> {
    let len = input.len();
    let mut cursor = offset;
    let mut parts = NumberParts::new(offset);

    if first == b'-' {
        parts.negative = true;
        cursor += 1;
        if cursor >= len {
            return None;
        }
    }

    match input.get(cursor).copied()? {
        b'0' => {
            parts.push_digit(0);
            cursor += 1;
        }
        b'1'..=b'9' => {
            cursor = scan_digit_run(input, cursor, len, &mut parts, false);
        }
        _ => return None,
    }

    if cursor < len && input[cursor] == b'.' {
        parts.is_integer = false;
        cursor += 1;
        let digits_start = cursor;
        cursor = scan_digit_run(input, cursor, len, &mut parts, true);
        if cursor == digits_start {
            return None;
        }
    }

    if cursor < len && matches!(input[cursor], b'e' | b'E') {
        parts.is_integer = false;
        cursor += 1;
        let exp_negative = if cursor < len && matches!(input[cursor], b'+' | b'-') {
            let negative = input[cursor] == b'-';
            cursor += 1;
            negative
        } else {
            false
        };
        let exp_start = cursor;
        let mut exponent = 0_i32;
        while cursor < len {
            let digit = input[cursor].wrapping_sub(b'0');
            if digit > 9 {
                break;
            }
            exponent = exponent.saturating_mul(10).saturating_add(digit as i32);
            cursor += 1;
        }
        if cursor == exp_start {
            return None;
        }
        if exp_negative {
            parts.decimal_exp = parts.decimal_exp.saturating_sub(exponent);
        } else {
            parts.decimal_exp = parts.decimal_exp.saturating_add(exponent);
        }
    }

    Some(parts.finish(cursor))
}

#[inline(always)]
fn scan_digit_run(
    input: &[u8],
    mut cursor: usize,
    len: usize,
    parts: &mut NumberParts,
    fractional: bool,
) -> usize {
    while cursor + 8 <= len && parts.can_push_eight_digits() {
        let block = unsafe { std::ptr::read_unaligned(input.as_ptr().add(cursor).cast::<u64>()) };
        if !is_eight_ascii_digits(block) {
            break;
        }
        parts.push_eight_digits(parse_eight_digits(block));
        if fractional {
            parts.decimal_exp -= 8;
        }
        cursor += 8;
    }

    while cursor + 4 <= len && parts.can_push_four_digits() {
        let block = unsafe { std::ptr::read_unaligned(input.as_ptr().add(cursor).cast::<u32>()) };
        if !is_four_ascii_digits(block) {
            break;
        }
        parts.push_four_digits(parse_four_digits(block));
        if fractional {
            parts.decimal_exp -= 4;
        }
        cursor += 4;
    }

    while cursor + 2 <= len && parts.can_push_two_digits() {
        let block = unsafe { std::ptr::read_unaligned(input.as_ptr().add(cursor).cast::<u16>()) };
        if !is_two_ascii_digits(block) {
            break;
        }
        parts.push_two_digits(parse_two_digits(block));
        if fractional {
            parts.decimal_exp -= 2;
        }
        cursor += 2;
    }

    while cursor < len {
        let digit = input[cursor].wrapping_sub(b'0');
        if digit > 9 {
            break;
        }
        parts.push_digit(digit);
        if fractional {
            parts.decimal_exp -= 1;
        }
        cursor += 1;
    }

    cursor
}

#[inline(always)]
fn is_two_ascii_digits(block: u16) -> bool {
    const ZEROES: u16 = 0x3030;
    const NINES: u16 = 0x3939;
    const HIGH_BITS: u16 = 0x8080;

    let below_zero = block.wrapping_sub(ZEROES);
    let above_nine = NINES.wrapping_sub(block);
    ((below_zero | above_nine) & HIGH_BITS) == 0
}

#[inline(always)]
fn is_four_ascii_digits(block: u32) -> bool {
    const ZEROES: u32 = 0x3030_3030;
    const NINES: u32 = 0x3939_3939;
    const HIGH_BITS: u32 = 0x8080_8080;

    let below_zero = block.wrapping_sub(ZEROES);
    let above_nine = NINES.wrapping_sub(block);
    ((below_zero | above_nine) & HIGH_BITS) == 0
}

#[inline(always)]
fn is_eight_ascii_digits(block: u64) -> bool {
    const ZEROES: u64 = 0x3030_3030_3030_3030;
    const NINES: u64 = 0x3939_3939_3939_3939;
    const HIGH_BITS: u64 = 0x8080_8080_8080_8080;

    let below_zero = block.wrapping_sub(ZEROES);
    let above_nine = NINES.wrapping_sub(block);
    ((below_zero | above_nine) & HIGH_BITS) == 0
}

#[inline(always)]
fn parse_two_digits(block: u16) -> u32 {
    let digits = block & 0x0f0f;
    let d0 = (digits & 0x00ff) as u32;
    let d1 = ((digits >> 8) & 0x00ff) as u32;

    d0 * 10 + d1
}

#[inline(always)]
fn parse_four_digits(block: u32) -> u32 {
    let digits = block & 0x0f0f_0f0f;
    let pairs = ((digits & 0x00ff_00ff) * 10).wrapping_add((digits >> 8) & 0x00ff_00ff);
    ((pairs & 0xffff) * 100).wrapping_add((pairs >> 16) & 0xffff)
}

#[inline(always)]
fn parse_eight_digits(block: u64) -> u32 {
    let digits = block & 0x0f0f_0f0f_0f0f_0f0f;
    let pairs =
        ((digits & 0x00ff_00ff_00ff_00ff) * 10).wrapping_add((digits >> 8) & 0x00ff_00ff_00ff_00ff);
    let quads =
        ((pairs & 0x0000_ffff_0000_ffff) * 100).wrapping_add((pairs >> 16) & 0x0000_ffff_0000_ffff);
    let lo = (quads & 0xffff) as u32;
    let hi = ((quads >> 32) & 0xffff) as u32;
    lo * 10_000 + hi
}

#[inline]
pub fn materialize_i64(input: &[u8], span: &NumberSpan) -> Result<i64, NumberError> {
    if !span.is_integer {
        return Err(NumberError::NotInteger);
    }
    if span.digit_count <= 19 && !span.mantissa_overflow {
        return if span.negative {
            if span.mantissa == (i64::MAX as u64) + 1 {
                Ok(i64::MIN)
            } else {
                i64::try_from(span.mantissa)
                    .map(|value| -value)
                    .map_err(|_| NumberError::Overflow)
            }
        } else {
            i64::try_from(span.mantissa).map_err(|_| NumberError::Overflow)
        };
    }
    integer::parse_i64(raw(input, span))
}

#[inline]
pub fn materialize_u64(input: &[u8], span: &NumberSpan) -> Result<u64, NumberError> {
    if !span.is_integer {
        return Err(NumberError::NotInteger);
    }
    if span.negative {
        return Err(NumberError::Overflow);
    }
    if span.digit_count <= 19 && !span.mantissa_overflow {
        return Ok(span.mantissa);
    }
    integer::parse_u64(raw(input, span))
}

#[inline]
pub fn materialize_f64(input: &[u8], span: &NumberSpan) -> Result<f64, NumberError> {
    if !span.mantissa_overflow {
        if let Some(value) =
            eisel_lemire::compute_f64(span.decimal_exp as i64, span.mantissa, span.negative)
        {
            return Ok(value);
        }
    }

    let text = std::str::from_utf8(raw(input, span)).map_err(|_| NumberError::Invalid)?;
    text.parse::<f64>().map_err(|_| NumberError::Invalid)
}

#[inline(always)]
fn raw<'a>(input: &'a [u8], span: &NumberSpan) -> &'a [u8] {
    &input[span.start..span.end]
}

#[derive(Debug, Clone, Copy)]
struct NumberParts {
    start: usize,
    is_integer: bool,
    negative: bool,
    digit_count: u32,
    decimal_exp: i32,
    mantissa: u64,
    mantissa_overflow: bool,
}

impl NumberParts {
    #[inline(always)]
    fn new(start: usize) -> Self {
        Self {
            start,
            is_integer: true,
            negative: false,
            digit_count: 0,
            decimal_exp: 0,
            mantissa: 0,
            mantissa_overflow: false,
        }
    }

    #[inline(always)]
    fn push_digit(&mut self, digit: u8) {
        self.digit_count += 1;
        if self.digit_count <= 19 {
            if let Some(next) = self
                .mantissa
                .checked_mul(10)
                .and_then(|value| value.checked_add(digit as u64))
            {
                self.mantissa = next;
            } else {
                self.mantissa_overflow = true;
            }
        } else if digit != 0 {
            self.mantissa_overflow = true;
        }
    }

    #[inline(always)]
    fn can_push_eight_digits(&self) -> bool {
        self.digit_count <= 11
    }

    #[inline(always)]
    fn can_push_four_digits(&self) -> bool {
        self.digit_count <= 15
    }

    #[inline(always)]
    fn can_push_two_digits(&self) -> bool {
        self.digit_count <= 17
    }

    #[inline(always)]
    fn push_eight_digits(&mut self, value: u32) {
        debug_assert!(self.can_push_eight_digits());
        self.digit_count += 8;
        self.mantissa = self
            .mantissa
            .wrapping_mul(100_000_000)
            .wrapping_add(value as u64);
    }

    #[inline(always)]
    fn push_four_digits(&mut self, value: u32) {
        debug_assert!(self.can_push_four_digits());
        self.digit_count += 4;
        self.mantissa = self
            .mantissa
            .wrapping_mul(10_000)
            .wrapping_add(value as u64);
    }

    #[inline(always)]
    fn push_two_digits(&mut self, value: u32) {
        debug_assert!(self.can_push_two_digits());
        self.digit_count += 2;
        self.mantissa = self.mantissa.wrapping_mul(100).wrapping_add(value as u64);
    }

    #[inline(always)]
    fn finish(self, end: usize) -> NumberSpan {
        NumberSpan {
            start: self.start,
            end,
            is_integer: self.is_integer,
            negative: self.negative,
            digit_count: self.digit_count,
            decimal_exp: self.decimal_exp,
            mantissa: self.mantissa,
            mantissa_overflow: self.mantissa_overflow,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scans_decimal_parts_without_second_walk() {
        let span = match_number_span(b"-123.45e2,", 0).unwrap();
        assert_eq!(span.end, 9);
        assert!(!span.is_integer);
        assert!(span.negative);
        assert_eq!(span.digit_count, 5);
        assert_eq!(span.decimal_exp, 0);
        assert_eq!(span.mantissa, 12345);
    }

    #[test]
    fn materializes_integer_widths() {
        let i = match_number_span(b"-9223372036854775808", 0).unwrap();
        assert_eq!(
            materialize_i64(b"-9223372036854775808", &i).unwrap(),
            i64::MIN
        );

        let u = match_number_span(b"18446744073709551615", 0).unwrap();
        assert_eq!(
            materialize_u64(b"18446744073709551615", &u).unwrap(),
            u64::MAX
        );
    }

    #[test]
    fn materializes_common_integers_from_span_facts() {
        let negative = match_number_span(b"-1234567890123456789,", 0).unwrap();
        assert_eq!(
            materialize_i64(b"-1234567890123456789,", &negative).unwrap(),
            -1_234_567_890_123_456_789
        );

        let positive = match_number_span(b"9999999999999999999]", 0).unwrap();
        assert_eq!(
            materialize_u64(b"9999999999999999999]", &positive).unwrap(),
            9_999_999_999_999_999_999
        );

        let overflow = match_number_span(b"9223372036854775808", 0).unwrap();
        assert_eq!(
            materialize_i64(b"9223372036854775808", &overflow),
            Err(NumberError::Overflow)
        );
    }

    #[test]
    fn eisel_materializer_matches_representative_bits() {
        for raw in [
            "0.0",
            "-0.0",
            "1.0",
            "1e0",
            "5e-324",
            "2.2250738585072014e-308",
            "1.7976931348623157e308",
            "43.474709000000125",
            "6.02214076e23",
        ] {
            let span = match_number_span(raw.as_bytes(), 0).unwrap();
            let actual = materialize_f64(raw.as_bytes(), &span).unwrap();
            let expected = raw.parse::<f64>().unwrap();
            assert_eq!(actual.to_bits(), expected.to_bits(), "{raw}");
        }
    }
}
