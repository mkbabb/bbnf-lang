//! The structural skeleton emitters: entry, container loops, string/number
//! shells, and the parser runtime-support utilities.
//!
//! Each parser fn is keyed off a rule in the `SinkOnlyProgram` (the `json`
//! entry rule, the `object`/`array` container rules, the `string`/`number`
//! leaf rules). The container open/close bytes and the value/key callees are
//! derived from each rule's `ByteLiteral` / `CallRule` / `RepeatLoop` nodes;
//! the body idiom (whitespace skips, comma handling, the `sink.*` verbs) is the
//! house convention, like `json_typed_direct.rs`'s hand-written parser idioms.

use super::dispatch::SinkPrefix;
use super::{ArmKind, SinkWalk, INLINE_CFG};
use std::fmt::Write;

impl SinkWalk<'_> {
    /// Emit `parse_direct` — the entry wrapper for the `json` rule
    /// (`Entry(Seq[ws, value, ws])`): skip ws, parse the value, trailing check.
    pub(crate) fn render_entry(&self, out: &mut String) {
        out.push_str(INLINE_CFG);
        out.push_str(
            r#"pub fn parse_direct<'i, S: JsonSink>(input: &'i str, sink: &mut S) -> Result<(), ParseError<'i>> {
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

    /// Emit the container parsers (`parse_object_direct` / `parse_array_direct`)
    /// derived from the `object`/`array` rules' open/close ByteLiterals + the
    /// member/element RepeatLoop.
    pub(crate) fn render_container_rules(&self, out: &mut String) {
        self.render_object(out);
        out.push('\n');
        self.render_array(out);
        out.push('\n');
    }

    fn render_object(&self, out: &mut String) {
        let (open, close) = self
            .container_bytes("object")
            .expect("object rule open/close bytes");
        let open_lit = super::byte_literal(open);
        let close_lit = super::byte_literal(close);
        out.push_str(INLINE_CFG);
        out.push_str(
            "fn parse_object_direct<'i, S: JsonSink>(\n    input: &'i str,\n    bytes: &'i [u8],\n    cursor: &mut usize,\n    sink: &mut S,\n) -> Result<(), ParseError<'i>> {\n",
        );
        out.push_str("    consume_direct(input, bytes, cursor, ");
        out.push_str(&open_lit);
        out.push_str(", ParseErrorKind::ExpectedValue)?;\n");
        out.push_str("    sink.begin_object();\n");
        out.push_str("    *cursor = skip_ascii_whitespace(bytes, *cursor);\n");
        out.push_str("    if take_direct(bytes, cursor, ");
        out.push_str(&close_lit);
        out.push_str(") {\n");
        out.push_str("        sink.end_object();\n        return Ok(());\n    }\n");
        out.push_str("    loop {\n");
        out.push_str("        let key = parse_string_direct(input, bytes, cursor)?;\n");
        out.push_str("        sink.key_source(key.raw, key.needs_unescape)\n");
        out.push_str("            .map_err(|err| string_error(input, err))?;\n");
        out.push_str("        *cursor = skip_ascii_whitespace(bytes, *cursor);\n");
        out.push_str("        consume_direct(input, bytes, cursor, b':', ParseErrorKind::ExpectedColon)?;\n");
        out.push_str("        *cursor = skip_ascii_whitespace(bytes, *cursor);\n");
        out.push_str("        parse_object_value_at_direct(input, bytes, cursor, sink)?;\n");
        out.push_str("        *cursor = skip_ascii_whitespace(bytes, *cursor);\n");
        out.push_str("        if take_direct(bytes, cursor, b',') {\n");
        out.push_str("            *cursor = skip_ascii_whitespace(bytes, *cursor);\n");
        out.push_str("            continue;\n        }\n");
        out.push_str("        consume_direct(input, bytes, cursor, ");
        out.push_str(&close_lit);
        out.push_str(", ParseErrorKind::ExpectedCommaOrObjectEnd)?;\n");
        out.push_str("        sink.end_object();\n        return Ok(());\n    }\n}\n");
    }

    fn render_array(&self, out: &mut String) {
        let (open, close) = self
            .container_bytes("array")
            .expect("array rule open/close bytes");
        let open_lit = super::byte_literal(open);
        let close_lit = super::byte_literal(close);
        // Derive the array element dispatch fast-path from the dispatch arms:
        // the number arm shares its callee with the array element variant.
        let number_arm = self
            .arms()
            .iter()
            .find(|arm| matches!(arm.kind, ArmKind::Number));
        out.push_str(INLINE_CFG);
        out.push_str(
            "fn parse_array_direct<'i, S: JsonSink>(\n    input: &'i str,\n    bytes: &'i [u8],\n    cursor: &mut usize,\n    sink: &mut S,\n) -> Result<(), ParseError<'i>> {\n",
        );
        out.push_str("    consume_direct(input, bytes, cursor, ");
        out.push_str(&open_lit);
        out.push_str(", ParseErrorKind::ExpectedValue)?;\n");
        out.push_str("    sink.begin_array();\n");
        out.push_str("    *cursor = skip_ascii_whitespace(bytes, *cursor);\n");
        out.push_str("    if take_direct(bytes, cursor, ");
        out.push_str(&close_lit);
        out.push_str(") {\n");
        out.push_str("        sink.end_array();\n        return Ok(());\n    }\n");
        out.push_str("    loop {\n");
        out.push_str("        let Some(byte) = bytes.get(*cursor).copied() else {\n");
        out.push_str("            return Err(direct_error(input, *cursor, ParseErrorKind::ExpectedValue));\n        };\n");
        if let Some(number_arm) = number_arm {
            let key = super::dispatch::number_match_pattern(&number_arm.keys);
            out.push_str("        if matches!(byte, ");
            out.push_str(&key);
            out.push_str(") {\n");
            out.push_str("            parse_number_array_direct(input, bytes, cursor, sink, byte)?;\n");
            out.push_str("        } else {\n");
            out.push_str("            parse_array_element_at_direct(input, bytes, cursor, sink)?;\n");
            out.push_str("        }\n");
        } else {
            out.push_str("        let _ = byte;\n");
            out.push_str("        parse_array_element_at_direct(input, bytes, cursor, sink)?;\n");
        }
        out.push_str("        *cursor = skip_ascii_whitespace(bytes, *cursor);\n");
        out.push_str("        if take_direct(bytes, cursor, b',') {\n");
        out.push_str("            *cursor = skip_ascii_whitespace(bytes, *cursor);\n");
        out.push_str("            continue;\n        }\n");
        out.push_str("        consume_direct(input, bytes, cursor, ");
        out.push_str(&close_lit);
        out.push_str(", ParseErrorKind::ExpectedCommaOrArrayEnd)?;\n");
        out.push_str("        sink.end_array();\n        return Ok(());\n    }\n}\n");
    }

    /// Emit `parse_string_direct` — the `string` rule's RegexProgram{String}
    /// shell: tiny-plain fast path + quote-scan fallback (the leaf scanners stay
    /// external named callees selected by span_kind).
    pub(crate) fn render_string_rule(&self, out: &mut String) {
        debug_assert!(self.rule("string").is_some(), "string rule present");
        out.push_str(INLINE_CFG);
        out.push_str(
            r#"fn parse_string_direct<'i>(
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

    /// Emit the `number` rule's scan shells (3 sink-prefix variants) + emitters.
    pub(crate) fn render_number_rules(&self, out: &mut String) {
        debug_assert!(self.rule("number").is_some(), "number rule present");
        for prefix in &super::PREFIXES {
            self.render_number_scan(out, prefix);
        }
        self.render_number_emitter(out, "emit_number_direct", "sink.");
        self.render_number_emitter(out, "emit_number_object_direct", "sink.object_");
        self.render_number_emitter(out, "emit_number_array_direct", "sink.array_");
    }

    fn render_number_scan(&self, out: &mut String, prefix: &SinkPrefix) {
        let emitter = prefix.number_fn.replace("parse_number", "emit_number");
        out.push_str(INLINE_CFG);
        let _ = writeln!(
            out,
            "fn {}<'i, S: JsonSink>(",
            prefix.number_fn
        );
        out.push_str(
            "    input: &'i str,\n    bytes: &'i [u8],\n    cursor: &mut usize,\n    sink: &mut S,\n    first: u8,\n) -> Result<(), ParseError<'i>> {\n",
        );
        out.push_str("    let span = match_number_span_from_first(bytes, *cursor, first)\n");
        out.push_str("        .ok_or_else(|| direct_error(input, *cursor, ParseErrorKind::InvalidNumber))?;\n");
        out.push_str("    *cursor = span.end;\n");
        let _ = writeln!(out, "    {emitter}(input, bytes, &span, sink)");
        out.push_str("}\n\n");
    }

    fn render_number_emitter(&self, out: &mut String, name: &str, prefix: &str) {
        let i64_call = format!("{prefix}i64(value);");
        let u64_call = format!("{prefix}u64(value);");
        let f64_call = format!("{prefix}f64(value);");
        let f64_neg_zero = format!("{prefix}f64(-0.0);");
        let _ = writeln!(
            out,
            r#"{INLINE_CFG}fn {name}<'i, S: JsonSink>(
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

    /// Emit the parser runtime-support utilities (fixed convention fragments —
    /// the parser's support code, like `json_typed_direct.rs`'s runtime block).
    pub(crate) fn render_utility_rules(&self, out: &mut String) {
        out.push_str(INLINE_CFG);
        out.push_str(
            r#"fn consume_literal_direct<'i>(
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

    /// The open/close ByteLiterals of a container rule (`object`/`array`).
    fn container_bytes(&self, name: &str) -> Option<(u8, u8)> {
        let rule = self.rule(name)?;
        let mut literals = Vec::new();
        super::dispatch::collect_container_literals(&rule.expr, &mut literals);
        let open = *literals.first()?;
        let close = *literals.last()?;
        Some((open, close))
    }
}
