//! Numeric literal parsing helpers — int / float literals, hex
//! prefix detection, and trailing-suffix splitting.

use bbnf_ir::MapExpr;

pub(crate) fn parse_int_literal(text: &str) -> MapExpr {
    let (digits, _suffix) = split_numeric_suffix(text);
    let value = if digits.starts_with("0x") || digits.starts_with("0X") {
        i64::from_str_radix(&digits[2..], 16).unwrap_or(0)
    } else {
        digits.parse::<i64>().unwrap_or(0)
    };
    MapExpr::IntLit(value)
}

pub(crate) fn parse_float_literal(text: &str) -> MapExpr {
    let (digits, _suffix) = split_numeric_suffix(text);
    let value = digits.parse::<f64>().unwrap_or(0.0);
    MapExpr::FloatLit(value)
}

/// Discriminate int vs float by inspecting whether the digit run
/// contains a `.`, `e`, or `E`. Used by `lower_value_atom` when
/// the source slice's leading byte is a digit.
pub(super) fn parse_numeric_literal_text(text: &str) -> MapExpr {
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
    if bytes.len() > 2 && bytes[0] == b'0' && (bytes[1] == b'x' || bytes[1] == b'X') {
        i = 2;
        while i < bytes.len() && bytes[i].is_ascii_hexdigit() {
            i += 1;
        }
    } else {
        while i < bytes.len()
            && (bytes[i].is_ascii_digit()
                || bytes[i] == b'.'
                || bytes[i] == b'e'
                || bytes[i] == b'E'
                || bytes[i] == b'+'
                || bytes[i] == b'-')
        {
            i += 1;
        }
    }
    (&text[..i], &text[i..])
}
