//! Numeric literal parsing helpers — int / float literals, hex
//! prefix detection, and trailing-suffix splitting.

use bbnf_ir::{MapExpr, NumericDomain};

#[derive(Clone, Debug, PartialEq)]
pub(super) struct ParsedNumericLiteral {
    pub(super) expr: MapExpr,
    pub(super) domain: NumericDomain,
    pub(super) has_explicit_domain: bool,
}

pub(crate) fn parse_int_literal(text: &str) -> ParsedNumericLiteral {
    let (digits, suffix) = split_numeric_suffix(text);
    let (negative, unsigned_digits) = match digits.strip_prefix('-') {
        Some(digits) => (true, digits),
        None => (false, digits.strip_prefix('+').unwrap_or(digits)),
    };
    let radix = if unsigned_digits.starts_with("0x") || unsigned_digits.starts_with("0X") {
        16
    } else {
        10
    };
    let magnitude_digits = if radix == 16 {
        &unsigned_digits[2..]
    } else {
        unsigned_digits
    };
    let magnitude = u64::from_str_radix(magnitude_digits, radix)
        .unwrap_or_else(|error| panic!("invalid integer literal {text:?}: {error}"));
    if matches!(suffix, "u8" | "u16" | "u32" | "u64") {
        assert!(
            !negative,
            "unsigned integer literal {text:?} cannot have a negative sign"
        );
        let in_range = match suffix {
            "u8" => u8::try_from(magnitude).is_ok(),
            "u16" => u16::try_from(magnitude).is_ok(),
            "u32" => u32::try_from(magnitude).is_ok(),
            "u64" => true,
            _ => unreachable!(),
        };
        assert!(
            in_range,
            "unsigned integer literal {text:?} is out of range"
        );
        let domain = match suffix {
            "u8" => NumericDomain::U8,
            "u16" => NumericDomain::U16,
            "u32" => NumericDomain::U32,
            "u64" => NumericDomain::U64,
            _ => unreachable!(),
        };
        ParsedNumericLiteral {
            expr: MapExpr::UIntLit(magnitude),
            domain,
            has_explicit_domain: true,
        }
    } else {
        assert!(
            matches!(suffix, "" | "i8" | "i16" | "i32" | "i64"),
            "unsupported integer literal suffix in {text:?}"
        );
        let (positive_max, negative_max) = match suffix {
            "i8" => (i8::MAX as u64, (i8::MAX as u64) + 1),
            "i16" => (i16::MAX as u64, (i16::MAX as u64) + 1),
            "i32" => (i32::MAX as u64, (i32::MAX as u64) + 1),
            "" | "i64" => (i64::MAX as u64, (i64::MAX as u64) + 1),
            _ => unreachable!(),
        };
        let limit = if negative { negative_max } else { positive_max };
        assert!(
            magnitude <= limit,
            "signed integer literal {text:?} is out of range"
        );
        let value = if negative {
            if magnitude == (i64::MAX as u64) + 1 {
                i64::MIN
            } else {
                -(magnitude as i64)
            }
        } else {
            magnitude as i64
        };
        let domain = match suffix {
            "i8" => NumericDomain::I8,
            "i16" => NumericDomain::I16,
            "i32" => NumericDomain::I32,
            "" | "i64" => NumericDomain::I64,
            _ => unreachable!(),
        };
        ParsedNumericLiteral {
            expr: MapExpr::IntLit(value),
            domain,
            has_explicit_domain: !suffix.is_empty(),
        }
    }
}

pub(crate) fn parse_float_literal(text: &str) -> ParsedNumericLiteral {
    let (digits, suffix) = split_numeric_suffix(text);
    let value = match suffix {
        "" | "f64" => digits
            .parse::<f64>()
            .unwrap_or_else(|error| panic!("invalid f64 literal {text:?}: {error}")),
        "f32" => digits
            .parse::<f32>()
            .unwrap_or_else(|error| panic!("invalid f32 literal {text:?}: {error}"))
            as f64,
        _ => panic!("unsupported float literal suffix in {text:?}"),
    };
    assert!(value.is_finite(), "non-finite float literal {text:?}");
    ParsedNumericLiteral {
        expr: MapExpr::FloatLit(value),
        domain: NumericDomain::F64,
        has_explicit_domain: !suffix.is_empty(),
    }
}

/// Discriminate int vs float by inspecting whether the digit run
/// contains a `.`, `e`, or `E`. Used by `lower_value_atom` when
/// the source slice's leading byte is a digit.
pub(super) fn parse_numeric_literal_text(text: &str) -> ParsedNumericLiteral {
    let (digits, _) = split_numeric_suffix(text);
    if digits.starts_with("0x") || digits.starts_with("0X") {
        return parse_int_literal(text);
    }
    if digits.contains('.') || digits.contains('e') || digits.contains('E') {
        parse_float_literal(text)
    } else {
        parse_int_literal(text)
    }
}

pub(crate) fn split_numeric_suffix(text: &str) -> (&str, &str) {
    let bytes = text.as_bytes();
    let mut i = 0;
    if matches!(bytes.first(), Some(b'+' | b'-')) {
        i += 1;
    }
    let magnitude_start = i;
    if bytes.get(i) == Some(&b'0') && matches!(bytes.get(i + 1), Some(b'x' | b'X')) {
        i += 2;
        while i < bytes.len() && bytes[i].is_ascii_hexdigit() {
            i += 1;
        }
    } else {
        while i < bytes.len() && bytes[i].is_ascii_digit() {
            i += 1;
        }
        if bytes.get(i) == Some(&b'.') {
            i += 1;
            while i < bytes.len() && bytes[i].is_ascii_digit() {
                i += 1;
            }
        }
        if matches!(bytes.get(i), Some(b'e' | b'E')) {
            i += 1;
            if matches!(bytes.get(i), Some(b'+' | b'-')) {
                i += 1;
            }
            while i < bytes.len() && bytes[i].is_ascii_digit() {
                i += 1;
            }
        }
    }
    assert!(
        i > magnitude_start,
        "numeric literal {text:?} has no magnitude"
    );
    (&text[..i], &text[i..])
}
