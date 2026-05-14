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
            while cursor < len {
                let digit = input[cursor].wrapping_sub(b'0');
                if digit > 9 {
                    break;
                }
                parts.push_digit(digit);
                cursor += 1;
            }
        }
        _ => return None,
    }

    if cursor < len && input[cursor] == b'.' {
        parts.is_integer = false;
        cursor += 1;
        let digits_start = cursor;
        while cursor < len {
            let digit = input[cursor].wrapping_sub(b'0');
            if digit > 9 {
                break;
            }
            parts.push_digit(digit);
            parts.decimal_exp -= 1;
            cursor += 1;
        }
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

#[inline]
pub fn materialize_i64(input: &[u8], span: &NumberSpan) -> Result<i64, NumberError> {
    if !span.is_integer {
        return Err(NumberError::NotInteger);
    }
    integer::parse_i64(raw(input, span))
}

#[inline]
pub fn materialize_u64(input: &[u8], span: &NumberSpan) -> Result<u64, NumberError> {
    if !span.is_integer {
        return Err(NumberError::NotInteger);
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
