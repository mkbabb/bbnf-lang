use super::sink::JsonSink;
use parse_that_regex::number::{
    materialize_f64, materialize_i64, materialize_u64, match_number_span_from_first, NumberSpan,
};
use parse_that_regex::unescape_json_string;
use std::borrow::Cow;

pub fn parse_direct<'i, S: JsonSink>(input: &'i str, sink: &mut S) -> Result<(), ParseError<'i>> {
    let bytes = input.as_bytes();
    let mut cursor = 0;
    parse_value_direct(input, bytes, &mut cursor, sink)?;
    cursor = skip_json_whitespace(bytes, cursor);
    if cursor == bytes.len() {
        Ok(())
    } else {
        Err(direct_error(
            input,
            cursor,
            ParseErrorKind::TrailingCharacters,
        ))
    }
}

fn parse_value_direct<'i, S: JsonSink>(
    input: &'i str,
    bytes: &'i [u8],
    cursor: &mut usize,
    sink: &mut S,
) -> Result<(), ParseError<'i>> {
    *cursor = skip_json_whitespace(bytes, *cursor);
    let Some(byte) = bytes.get(*cursor).copied() else {
        return Err(direct_error(input, *cursor, ParseErrorKind::ExpectedValue));
    };
    match byte {
        b'{' => parse_object_direct(input, bytes, cursor, sink),
        b'[' => parse_array_direct(input, bytes, cursor, sink),
        b'"' => {
            let value = parse_string_direct(input, bytes, cursor)?;
            sink.string(value.as_ref());
            Ok(())
        }
        b'-' | b'0'..=b'9' => parse_number_direct(input, bytes, cursor, sink, byte),
        b't' => parse_literal_direct(input, bytes, cursor, b"true", sink, Some(true)),
        b'f' => parse_literal_direct(input, bytes, cursor, b"false", sink, Some(false)),
        b'n' => parse_literal_direct(input, bytes, cursor, b"null", sink, None),
        _ => Err(direct_error(input, *cursor, ParseErrorKind::ExpectedValue)),
    }
}

fn parse_object_direct<'i, S: JsonSink>(
    input: &'i str,
    bytes: &'i [u8],
    cursor: &mut usize,
    sink: &mut S,
) -> Result<(), ParseError<'i>> {
    consume_direct(input, bytes, cursor, b'{', ParseErrorKind::ExpectedValue)?;
    sink.begin_object();
    *cursor = skip_json_whitespace(bytes, *cursor);
    if take_direct(bytes, cursor, b'}') {
        sink.end_object();
        return Ok(());
    }
    loop {
        let key = parse_string_direct(input, bytes, cursor)?;
        sink.key(key.as_ref());
        *cursor = skip_json_whitespace(bytes, *cursor);
        consume_direct(input, bytes, cursor, b':', ParseErrorKind::ExpectedColon)?;
        parse_value_direct(input, bytes, cursor, sink)?;
        *cursor = skip_json_whitespace(bytes, *cursor);
        if take_direct(bytes, cursor, b',') {
            *cursor = skip_json_whitespace(bytes, *cursor);
            continue;
        }
        consume_direct(input, bytes, cursor, b'}', ParseErrorKind::ExpectedCommaOrObjectEnd)?;
        sink.end_object();
        return Ok(());
    }
}

fn parse_array_direct<'i, S: JsonSink>(
    input: &'i str,
    bytes: &'i [u8],
    cursor: &mut usize,
    sink: &mut S,
) -> Result<(), ParseError<'i>> {
    consume_direct(input, bytes, cursor, b'[', ParseErrorKind::ExpectedValue)?;
    sink.begin_array();
    *cursor = skip_json_whitespace(bytes, *cursor);
    if take_direct(bytes, cursor, b']') {
        sink.end_array();
        return Ok(());
    }
    loop {
        parse_value_direct(input, bytes, cursor, sink)?;
        *cursor = skip_json_whitespace(bytes, *cursor);
        if take_direct(bytes, cursor, b',') {
            continue;
        }
        consume_direct(input, bytes, cursor, b']', ParseErrorKind::ExpectedCommaOrArrayEnd)?;
        sink.end_array();
        return Ok(());
    }
}

fn parse_string_direct<'i>(
    input: &'i str,
    bytes: &'i [u8],
    cursor: &mut usize,
) -> Result<Cow<'i, str>, ParseError<'i>> {
    let span = match_json_string_at_quote(bytes, *cursor).map_err(|err| ParseError {
        input,
        offset: err.offset,
        kind: match err.kind {
            RegexErrorKind::ExpectedString => ParseErrorKind::ExpectedValue,
            _ => ParseErrorKind::InvalidString,
        },
    })?;
    let raw = unsafe { std::str::from_utf8_unchecked(&bytes[span.content_start..span.content_end]) };
    *cursor = span.raw_end;
    if span.needs_unescape {
        unescape_json_string(raw).map_err(|err| ParseError {
            input,
            offset: err.offset,
            kind: ParseErrorKind::InvalidString,
        })
    } else {
        Ok(Cow::Borrowed(raw))
    }
}

fn parse_number_direct<'i, S: JsonSink>(
    input: &'i str,
    bytes: &'i [u8],
    cursor: &mut usize,
    sink: &mut S,
    first: u8,
) -> Result<(), ParseError<'i>> {
    let span = match_number_span_from_first(bytes, *cursor, first)
        .ok_or_else(|| direct_error(input, *cursor, ParseErrorKind::InvalidNumber))?;
    *cursor = span.end;
    emit_number_direct(input, bytes, &span, sink)
}

fn emit_number_direct<'i, S: JsonSink>(
    input: &'i str,
    bytes: &'i [u8],
    span: &NumberSpan,
    sink: &mut S,
) -> Result<(), ParseError<'i>> {
    let raw = &bytes[span.start..span.end];
    if span.is_integer {
        if span.negative {
            if raw == b"-0" {
                sink.f64(-0.0);
                return Ok(());
            }
            if let Ok(value) = materialize_i64(bytes, span) {
                sink.i64(value);
                return Ok(());
            }
        } else if let Ok(value) = materialize_u64(bytes, span) {
            sink.u64(value);
            return Ok(());
        }
    }
    let value = materialize_f64(bytes, span)
        .map_err(|_| direct_error(input, span.start, ParseErrorKind::InvalidNumber))?;
    sink.f64(value);
    Ok(())
}

fn parse_literal_direct<'i, S: JsonSink>(
    input: &'i str,
    bytes: &'i [u8],
    cursor: &mut usize,
    literal: &'static [u8],
    sink: &mut S,
    boolean: Option<bool>,
) -> Result<(), ParseError<'i>> {
    let start = *cursor;
    if bytes.get(start..start + literal.len()) != Some(literal) {
        return Err(direct_error(
            input,
            start,
            ParseErrorKind::InvalidLiteral(
                std::str::from_utf8(literal).expect("literal is UTF-8"),
            ),
        ));
    }
    *cursor += literal.len();
    match boolean {
        Some(value) => sink.bool(value),
        None => sink.null(),
    }
    Ok(())
}

fn consume_direct<'i>(
    input: &'i str,
    bytes: &[u8],
    cursor: &mut usize,
    byte: u8,
    kind: ParseErrorKind,
) -> Result<(), ParseError<'i>> {
    if take_direct(bytes, cursor, byte) {
        Ok(())
    } else {
        Err(direct_error(input, *cursor, kind))
    }
}

fn take_direct(bytes: &[u8], cursor: &mut usize, byte: u8) -> bool {
    if bytes.get(*cursor) == Some(&byte) {
        *cursor += 1;
        true
    } else {
        false
    }
}

fn direct_error<'i>(input: &'i str, offset: usize, kind: ParseErrorKind) -> ParseError<'i> {
    ParseError {
        input,
        offset,
        kind,
    }
}
