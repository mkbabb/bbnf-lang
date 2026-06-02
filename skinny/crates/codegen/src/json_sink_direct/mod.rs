use crate::lower::sink_only::{SinkOnlyProgram, SinkOnlySpanKind};
use std::fmt::Write;

pub fn render(program: &SinkOnlyProgram) -> Result<String, String> {
    validate(program)?;

    let mut out = String::new();
    render_header(program, &mut out);
    render_entry(&mut out);
    render_value_dispatch(&mut out);
    render_container_rules(&mut out);
    render_string_rule(&mut out);
    render_number_rules(&mut out);
    render_utility_rules(&mut out);
    Ok(out)
}

fn validate(program: &SinkOnlyProgram) -> Result<(), String> {
    if !program.has_rule(&program.entry_rule) {
        return Err(format!(
            "sink-only renderer missing entry BIR rule `{}`",
            program.entry_rule
        ));
    }

    if program.direct_shapes.is_empty() {
        return Err("sink-only renderer requires DirectBuild shapes".to_string());
    }

    if program.literals.is_empty() {
        return Err("sink-only renderer requires literal recognizers".to_string());
    }

    for span in [
        SinkOnlySpanKind::String,
        SinkOnlySpanKind::Number,
        SinkOnlySpanKind::Whitespace,
    ] {
        if !program.span_kinds.contains(&span) {
            return Err(format!(
                "sink-only renderer missing {:?} RegexProgram in BIR",
                span
            ));
        }
    }

    for shape in program
        .rules
        .iter()
        .filter_map(|rule| rule.direct_shape.as_ref())
    {
        if shape.shape.is_empty() {
            return Err("sink-only renderer found empty DirectBuild shape".to_string());
        }
        for field in &shape.fields {
            if field.name.is_empty() {
                return Err(format!(
                    "sink-only renderer found empty DirectBuild field in `{}`",
                    shape.shape
                ));
            }
        }
    }

    Ok(())
}

fn render_header(program: &SinkOnlyProgram, out: &mut String) {
    let shapes = program
        .direct_shapes
        .iter()
        .map(String::as_str)
        .collect::<Vec<_>>()
        .join(",");
    let _ = writeln!(
        out,
        "// sink-only lowered from BackendIr: entry={} direct_shapes={} dispatch_alt_count={}",
        program.entry_rule, shapes, program.dispatch_alt_count
    );
    out.push_str(
        r#"
use super::sink::JsonSink;
use parse_that_regex::number::{
    materialize_f64, materialize_i64, materialize_u64,
};

struct ParsedString<'i> {
    raw: &'i str,
    needs_unescape: bool,
}

"#,
    );
}

fn render_entry(out: &mut String) {
    out.push_str(
        r#"#[cfg_attr(feature = "parse-attribution", inline(never))]
#[cfg_attr(not(feature = "parse-attribution"), inline(always))]
pub fn parse_direct<'i, S: JsonSink>(input: &'i str, sink: &mut S) -> Result<(), ParseError<'i>> {
    let _policy = config::w7_direct_policy_triad();
    debug_assert_eq!(config::W7_DIRECT_BACKEND_SHAPE, "SinkOnly");
    debug_assert_eq!(config::W7_SAME_SUBSTRATE_UNION, "pass");
    debug_assert_ne!(config::STRING_NEEDS_DECODE, 0);
    let bytes = input.as_bytes();
    let mut cursor = 0;
    parse_value_direct(input, bytes, &mut cursor, sink)?;
    cursor = skip_ascii_whitespace(bytes, cursor);
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

"#,
    );
}

fn render_value_dispatch(out: &mut String) {
    out.push_str(
        r#"#[cfg_attr(feature = "parse-attribution", inline(never))]
#[cfg_attr(not(feature = "parse-attribution"), inline(always))]
fn parse_value_direct<'i, S: JsonSink>(
    input: &'i str,
    bytes: &'i [u8],
    cursor: &mut usize,
    sink: &mut S,
) -> Result<(), ParseError<'i>> {
    *cursor = skip_ascii_whitespace(bytes, *cursor);
    let Some(byte) = bytes.get(*cursor).copied() else {
        return Err(direct_error(input, *cursor, ParseErrorKind::ExpectedValue));
    };
    match byte {
        b'{' => parse_object_direct(input, bytes, cursor, sink),
        b'[' => parse_array_direct(input, bytes, cursor, sink),
        b'"' => {
            let value = parse_string_direct(input, bytes, cursor)?;
            sink.string_source(value.raw, value.needs_unescape)
                .map_err(|err| string_error(input, err))?;
            Ok(())
        }
        b'-' | b'0'..=b'9' => parse_number_direct(input, bytes, cursor, sink, byte),
        b't' => {
            consume_literal_direct(input, bytes, cursor, config::TRUE_LITERAL)?;
            sink.bool(true);
            Ok(())
        }
        b'f' => {
            consume_literal_direct(input, bytes, cursor, config::FALSE_LITERAL)?;
            sink.bool(false);
            Ok(())
        }
        b'n' => {
            consume_literal_direct(input, bytes, cursor, config::NULL_LITERAL)?;
            sink.null();
            Ok(())
        }
        _ => Err(direct_error(input, *cursor, ParseErrorKind::ExpectedValue)),
    }
}

#[cfg_attr(feature = "parse-attribution", inline(never))]
#[cfg_attr(not(feature = "parse-attribution"), inline(always))]
fn parse_object_value_at_direct<'i, S: JsonSink>(
    input: &'i str,
    bytes: &'i [u8],
    cursor: &mut usize,
    sink: &mut S,
) -> Result<(), ParseError<'i>> {
    let Some(byte) = bytes.get(*cursor).copied() else {
        return Err(direct_error(input, *cursor, ParseErrorKind::ExpectedValue));
    };
    match byte {
        b'{' => parse_object_direct(input, bytes, cursor, sink),
        b'[' => parse_array_direct(input, bytes, cursor, sink),
        b'"' => {
            let value = parse_string_direct(input, bytes, cursor)?;
            sink.object_string_source(value.raw, value.needs_unescape)
                .map_err(|err| string_error(input, err))?;
            Ok(())
        }
        b'-' | b'0'..=b'9' => parse_number_object_direct(input, bytes, cursor, sink, byte),
        b't' => {
            consume_literal_direct(input, bytes, cursor, config::TRUE_LITERAL)?;
            sink.object_bool(true);
            Ok(())
        }
        b'f' => {
            consume_literal_direct(input, bytes, cursor, config::FALSE_LITERAL)?;
            sink.object_bool(false);
            Ok(())
        }
        b'n' => {
            consume_literal_direct(input, bytes, cursor, config::NULL_LITERAL)?;
            sink.object_null();
            Ok(())
        }
        _ => Err(direct_error(input, *cursor, ParseErrorKind::ExpectedValue)),
    }
}

#[cfg_attr(feature = "parse-attribution", inline(never))]
#[cfg_attr(not(feature = "parse-attribution"), inline(always))]
fn parse_array_element_at_direct<'i, S: JsonSink>(
    input: &'i str,
    bytes: &'i [u8],
    cursor: &mut usize,
    sink: &mut S,
) -> Result<(), ParseError<'i>> {
    let Some(byte) = bytes.get(*cursor).copied() else {
        return Err(direct_error(input, *cursor, ParseErrorKind::ExpectedValue));
    };
    match byte {
        b'{' => parse_object_direct(input, bytes, cursor, sink),
        b'[' => parse_array_direct(input, bytes, cursor, sink),
        b'"' => {
            let value = parse_string_direct(input, bytes, cursor)?;
            sink.array_string_source(value.raw, value.needs_unescape)
                .map_err(|err| string_error(input, err))?;
            Ok(())
        }
        b'-' | b'0'..=b'9' => parse_number_array_direct(input, bytes, cursor, sink, byte),
        b't' => {
            consume_literal_direct(input, bytes, cursor, config::TRUE_LITERAL)?;
            sink.array_bool(true);
            Ok(())
        }
        b'f' => {
            consume_literal_direct(input, bytes, cursor, config::FALSE_LITERAL)?;
            sink.array_bool(false);
            Ok(())
        }
        b'n' => {
            consume_literal_direct(input, bytes, cursor, config::NULL_LITERAL)?;
            sink.array_null();
            Ok(())
        }
        _ => Err(direct_error(input, *cursor, ParseErrorKind::ExpectedValue)),
    }
}

"#,
    );
}

fn render_container_rules(out: &mut String) {
    out.push_str(
        r#"#[cfg_attr(feature = "parse-attribution", inline(never))]
#[cfg_attr(not(feature = "parse-attribution"), inline(always))]
fn parse_object_direct<'i, S: JsonSink>(
    input: &'i str,
    bytes: &'i [u8],
    cursor: &mut usize,
    sink: &mut S,
) -> Result<(), ParseError<'i>> {
    consume_direct(input, bytes, cursor, b'{', ParseErrorKind::ExpectedValue)?;
    sink.begin_object();
    *cursor = skip_ascii_whitespace(bytes, *cursor);
    if take_direct(bytes, cursor, b'}') {
        sink.end_object();
        return Ok(());
    }
    loop {
        let key = parse_string_direct(input, bytes, cursor)?;
        sink.key_source(key.raw, key.needs_unescape)
            .map_err(|err| string_error(input, err))?;
        *cursor = skip_ascii_whitespace(bytes, *cursor);
        consume_direct(input, bytes, cursor, b':', ParseErrorKind::ExpectedColon)?;
        *cursor = skip_ascii_whitespace(bytes, *cursor);
        parse_object_value_at_direct(input, bytes, cursor, sink)?;
        *cursor = skip_ascii_whitespace(bytes, *cursor);
        if take_direct(bytes, cursor, b',') {
            *cursor = skip_ascii_whitespace(bytes, *cursor);
            continue;
        }
        consume_direct(input, bytes, cursor, b'}', ParseErrorKind::ExpectedCommaOrObjectEnd)?;
        sink.end_object();
        return Ok(());
    }
}

#[cfg_attr(feature = "parse-attribution", inline(never))]
#[cfg_attr(not(feature = "parse-attribution"), inline(always))]
fn parse_array_direct<'i, S: JsonSink>(
    input: &'i str,
    bytes: &'i [u8],
    cursor: &mut usize,
    sink: &mut S,
) -> Result<(), ParseError<'i>> {
    consume_direct(input, bytes, cursor, b'[', ParseErrorKind::ExpectedValue)?;
    sink.begin_array();
    *cursor = skip_ascii_whitespace(bytes, *cursor);
    if take_direct(bytes, cursor, b']') {
        sink.end_array();
        return Ok(());
    }
    loop {
        let Some(byte) = bytes.get(*cursor).copied() else {
            return Err(direct_error(input, *cursor, ParseErrorKind::ExpectedValue));
        };
        if matches!(byte, b'-' | b'0'..=b'9') {
            parse_number_array_direct(input, bytes, cursor, sink, byte)?;
        } else {
            parse_array_element_at_direct(input, bytes, cursor, sink)?;
        }
        *cursor = skip_ascii_whitespace(bytes, *cursor);
        if take_direct(bytes, cursor, b',') {
            *cursor = skip_ascii_whitespace(bytes, *cursor);
            continue;
        }
        consume_direct(input, bytes, cursor, b']', ParseErrorKind::ExpectedCommaOrArrayEnd)?;
        sink.end_array();
        return Ok(());
    }
}

"#,
    );
}

fn render_string_rule(out: &mut String) {
    out.push_str(
        r#"#[cfg_attr(feature = "parse-attribution", inline(never))]
#[cfg_attr(not(feature = "parse-attribution"), inline(always))]
fn parse_string_direct<'i>(
    input: &'i str,
    bytes: &'i [u8],
    cursor: &mut usize,
) -> Result<ParsedString<'i>, ParseError<'i>> {
    let start = *cursor;
    if let Some(raw_end) = match_tiny_plain_string_direct(bytes, start) {
        let raw = unsafe { std::str::from_utf8_unchecked(&bytes[start + 1..raw_end - 1]) };
        *cursor = raw_end;
        return Ok(ParsedString {
            raw,
            needs_unescape: false,
        });
    }
    let span =
        parse_that_regex::match_string_at_quote_trusted_utf8(bytes, start).map_err(|err| {
            ParseError {
                input,
                offset: err.offset,
                kind: match err.kind {
                    RegexErrorKind::ExpectedString => ParseErrorKind::ExpectedValue,
                    _ => ParseErrorKind::InvalidString,
                },
            }
        })?;
    let raw = unsafe { std::str::from_utf8_unchecked(&bytes[span.content_start()..span.content_end()]) };
    *cursor = span.raw_end;
    Ok(ParsedString {
        raw,
        needs_unescape: span.needs_decode(),
    })
}

"#,
    );
}

fn render_number_rules(out: &mut String) {
    out.push_str(
        r#"#[cfg_attr(feature = "parse-attribution", inline(never))]
#[cfg_attr(not(feature = "parse-attribution"), inline(always))]
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

#[cfg_attr(feature = "parse-attribution", inline(never))]
#[cfg_attr(not(feature = "parse-attribution"), inline(always))]
fn parse_number_object_direct<'i, S: JsonSink>(
    input: &'i str,
    bytes: &'i [u8],
    cursor: &mut usize,
    sink: &mut S,
    first: u8,
) -> Result<(), ParseError<'i>> {
    let span = match_number_span_from_first(bytes, *cursor, first)
        .ok_or_else(|| direct_error(input, *cursor, ParseErrorKind::InvalidNumber))?;
    *cursor = span.end;
    emit_number_object_direct(input, bytes, &span, sink)
}

#[cfg_attr(feature = "parse-attribution", inline(never))]
#[cfg_attr(not(feature = "parse-attribution"), inline(always))]
fn parse_number_array_direct<'i, S: JsonSink>(
    input: &'i str,
    bytes: &'i [u8],
    cursor: &mut usize,
    sink: &mut S,
    first: u8,
) -> Result<(), ParseError<'i>> {
    let span = match_number_span_from_first(bytes, *cursor, first)
        .ok_or_else(|| direct_error(input, *cursor, ParseErrorKind::InvalidNumber))?;
    *cursor = span.end;
    emit_number_array_direct(input, bytes, &span, sink)
}

"#,
    );
    render_number_emitter(out, "emit_number_direct", "sink.");
    render_number_emitter(out, "emit_number_object_direct", "sink.object_");
    render_number_emitter(out, "emit_number_array_direct", "sink.array_");
}

fn render_number_emitter(out: &mut String, name: &str, prefix: &str) {
    let i64_call = format!("{prefix}i64(value);");
    let u64_call = format!("{prefix}u64(value);");
    let f64_call = format!("{prefix}f64(value);");
    let f64_neg_zero = format!("{prefix}f64(-0.0);");
    let _ = writeln!(
        out,
        r#"#[cfg_attr(feature = "parse-attribution", inline(never))]
#[cfg_attr(not(feature = "parse-attribution"), inline(always))]
fn {name}<'i, S: JsonSink>(
    input: &'i str,
    bytes: &'i [u8],
    span: &NumberSpan,
    sink: &mut S,
) -> Result<(), ParseError<'i>> {{
    let raw = &bytes[span.start..span.end];
    if span.is_integer {{
        if span.negative {{
            if raw == b"-0" {{
                {f64_neg_zero}
                return Ok(());
            }}
            if let Ok(value) = materialize_i64(bytes, span) {{
                {i64_call}
                return Ok(());
            }}
        }} else if let Ok(value) = materialize_u64(bytes, span) {{
            {u64_call}
            return Ok(());
        }}
    }}
    let value = materialize_f64(bytes, span)
        .map_err(|_| direct_error(input, span.start, ParseErrorKind::InvalidNumber))?;
    {f64_call}
    Ok(())
}}
"#
    );
}

fn render_utility_rules(out: &mut String) {
    out.push_str(
        r#"#[cfg_attr(feature = "parse-attribution", inline(never))]
#[cfg_attr(not(feature = "parse-attribution"), inline(always))]
fn consume_literal_direct<'i>(
    input: &'i str,
    bytes: &'i [u8],
    cursor: &mut usize,
    literal: &'static [u8],
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

fn string_error<'i>(input: &'i str, err: parse_that_regex::RegexError) -> ParseError<'i> {
    ParseError {
        input,
        offset: err.offset,
        kind: ParseErrorKind::InvalidString,
    }
}
"#,
    );
}
