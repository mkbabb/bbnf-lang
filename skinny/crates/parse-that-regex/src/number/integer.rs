use super::NumberError;

#[inline]
pub fn parse_integer(input: &[u8]) -> Result<i64, NumberError> {
    parse_i64(input)
}

#[inline]
pub fn parse_i64(input: &[u8]) -> Result<i64, NumberError> {
    let (negative, digits) = match input.first().copied() {
        Some(b'-') => (true, &input[1..]),
        Some(_) => (false, input),
        None => return Err(NumberError::Invalid),
    };
    if digits.is_empty() {
        return Err(NumberError::Invalid);
    }
    let value = parse_u64_digits(digits)?;
    if negative {
        if value == (i64::MAX as u64) + 1 {
            Ok(i64::MIN)
        } else {
            i64::try_from(value)
                .map(|value| -value)
                .map_err(|_| NumberError::Overflow)
        }
    } else {
        i64::try_from(value).map_err(|_| NumberError::Overflow)
    }
}

#[inline]
pub fn parse_u64(input: &[u8]) -> Result<u64, NumberError> {
    if input.first() == Some(&b'-') {
        return Err(NumberError::Overflow);
    }
    parse_u64_digits(input)
}

#[inline(always)]
fn parse_u64_digits(digits: &[u8]) -> Result<u64, NumberError> {
    let mut value = 0_u64;
    for byte in digits {
        let digit = byte.wrapping_sub(b'0');
        if digit > 9 {
            return Err(NumberError::Invalid);
        }
        value = value
            .checked_mul(10)
            .and_then(|value| value.checked_add(digit as u64))
            .ok_or(NumberError::Overflow)?;
    }
    Ok(value)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn preserves_i64_min() {
        assert_eq!(parse_i64(b"-9223372036854775808").unwrap(), i64::MIN);
        assert_eq!(
            parse_i64(b"-9223372036854775809"),
            Err(NumberError::Overflow)
        );
    }

    #[test]
    fn preserves_u64_max() {
        assert_eq!(parse_u64(b"18446744073709551615").unwrap(), u64::MAX);
        assert_eq!(
            parse_u64(b"18446744073709551616"),
            Err(NumberError::Overflow)
        );
    }
}
