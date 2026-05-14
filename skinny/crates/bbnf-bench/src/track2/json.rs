use parse_that_regex::{
    match_json_number_from_first, match_json_string_at_quote_trusted_utf8, skip_json_whitespace,
    RegexErrorKind,
};
use runtime::{
    grammars::json::{JsonRoot, ParseError, ParseErrorKind},
    tape::{OffsetFlags, TapeBuilder},
};

pub fn parse<'i>(input: &'i str) -> Result<JsonRoot<'i>, ParseError<'i>> {
    Parser::new(input).parse()
}

struct Parser<'i> {
    input: &'i str,
    bytes: &'i [u8],
    cursor: usize,
    tape: TapeBuilder<'i>,
}

impl<'i> Parser<'i> {
    #[inline(always)]
    fn new(input: &'i str) -> Self {
        let capacity = TapeBuilder::json_structural_capacity(input.as_bytes());
        Self {
            input,
            bytes: input.as_bytes(),
            cursor: 0,
            tape: TapeBuilder::new(input.as_bytes(), capacity),
        }
    }

    #[inline(always)]
    fn parse(mut self) -> Result<JsonRoot<'i>, ParseError<'i>> {
        self.parse_value()?;
        self.skip_ws();
        if self.cursor != self.bytes.len() {
            return Err(self.error(ParseErrorKind::TrailingCharacters));
        }
        Ok(JsonRoot::from_tape(self.input, self.tape.finish()))
    }

    #[inline(always)]
    fn parse_value(&mut self) -> Result<(), ParseError<'i>> {
        self.skip_ws();
        self.parse_value_at()
    }

    #[inline(always)]
    fn parse_value_at(&mut self) -> Result<(), ParseError<'i>> {
        if self.cursor >= self.bytes.len() {
            return Err(self.error(ParseErrorKind::ExpectedValue));
        }
        let byte = unsafe { *self.bytes.get_unchecked(self.cursor) };
        match byte {
            b'{' => self.parse_object(),
            b'[' => self.parse_array(),
            b'"' => self.parse_string(),
            b'-' | b'0'..=b'9' => self.parse_number(byte),
            b't' => self.parse_literal(b"true"),
            b'f' => self.parse_literal(b"false"),
            b'n' => self.parse_literal(b"null"),
            _ => Err(self.error(ParseErrorKind::ExpectedValue)),
        }
    }

    #[inline(always)]
    fn parse_object(&mut self) -> Result<(), ParseError<'i>> {
        if self.consume_structural(b'{').is_none() {
            return Err(self.error(ParseErrorKind::ExpectedValue));
        }
        self.skip_ws();

        if self.consume(b'}') {
            return Ok(());
        }

        loop {
            self.parse_pair()?;
            if self.consume_container_next(b'}', ParseErrorKind::ExpectedCommaOrObjectEnd)? {
                continue;
            }
            return Ok(());
        }
    }

    #[inline(always)]
    fn parse_pair(&mut self) -> Result<(), ParseError<'i>> {
        self.parse_key_colon()?;
        self.parse_value_at()
    }

    #[inline(always)]
    fn parse_key_colon(&mut self) -> Result<(), ParseError<'i>> {
        let start = self.cursor;
        let Some(open_cursor) = self.consume_quote_at_cursor() else {
            return Err(self.error(ParseErrorKind::ExpectedValue));
        };
        if let Some(raw_end) = match_tiny_plain_string(self.bytes, start) {
            self.cursor = raw_end;
        } else {
            let span =
                match_json_string_at_quote_trusted_utf8(self.bytes, start).map_err(|error| ParseError {
                    input: self.input,
                    offset: error.offset,
                    kind: match error.kind {
                        RegexErrorKind::ExpectedString => ParseErrorKind::ExpectedValue,
                        _ => ParseErrorKind::InvalidString,
                    },
                })?;
            if span.needs_unescape {
                self.tape
                    .patch_flags(open_cursor, OffsetFlags::NONE.with(OffsetFlags::HAS_ESC));
            }
            self.cursor = span.raw_end;
        }

        let colon = if self.cursor < self.bytes.len()
            && unsafe { *self.bytes.get_unchecked(self.cursor) } == b':'
        {
            self.cursor
        } else {
            skip_json_whitespace(self.bytes, self.cursor)
        };
        if colon >= self.bytes.len() || unsafe { *self.bytes.get_unchecked(colon) } != b':' {
            return Err(self.error(ParseErrorKind::ExpectedColon));
        }
        self.cursor = skip_json_whitespace(self.bytes, colon + 1);
        Ok(())
    }

    #[inline(always)]
    fn parse_array(&mut self) -> Result<(), ParseError<'i>> {
        if self.consume_structural(b'[').is_none() {
            return Err(self.error(ParseErrorKind::ExpectedValue));
        }
        self.skip_ws();

        if self.consume(b']') {
            return Ok(());
        }

        loop {
            self.parse_value_at()?;
            if self.consume_container_next(b']', ParseErrorKind::ExpectedCommaOrArrayEnd)? {
                continue;
            }
            return Ok(());
        }
    }

    #[inline(always)]
    fn parse_string(&mut self) -> Result<(), ParseError<'i>> {
        let start = self.cursor;
        let Some(open_cursor) = self.consume_quote_at_cursor() else {
            return Err(self.error(ParseErrorKind::ExpectedValue));
        };
        if let Some(raw_end) = match_tiny_plain_string(self.bytes, start) {
            self.cursor = raw_end;
            return Ok(());
        }
        let span = match_json_string_at_quote_trusted_utf8(self.bytes, start).map_err(|error| ParseError {
            input: self.input,
            offset: error.offset,
            kind: match error.kind {
                RegexErrorKind::ExpectedString => ParseErrorKind::ExpectedValue,
                _ => ParseErrorKind::InvalidString,
            },
        })?;
        if span.needs_unescape {
            self.tape
                .patch_flags(open_cursor, OffsetFlags::NONE.with(OffsetFlags::HAS_ESC));
        }
        self.cursor = span.raw_end;
        Ok(())
    }

    #[inline(always)]
    fn parse_number(&mut self, first: u8) -> Result<(), ParseError<'i>> {
        let number = match_json_number_from_first(self.bytes, self.cursor, first)
            .ok_or_else(|| self.error(ParseErrorKind::InvalidNumber))?;
        self.tape.push_plain_offset(number.start);
        self.cursor = number.end;
        Ok(())
    }

    #[inline(always)]
    fn parse_literal(&mut self, literal: &'static [u8]) -> Result<(), ParseError<'i>> {
        let start = self.cursor;
        if self.bytes.get(start..start + literal.len()) != Some(literal) {
            return Err(self.error(ParseErrorKind::InvalidLiteral(
                std::str::from_utf8(literal).expect("literal is UTF-8"),
            )));
        }
        self.tape.push_plain_offset(start);
        self.cursor += literal.len();
        Ok(())
    }

    #[inline(always)]
    fn skip_ws(&mut self) {
        self.cursor = skip_json_whitespace(self.bytes, self.cursor);
    }

    #[inline(always)]
    fn consume_delimiter(&mut self, byte: u8) -> bool {
        let offset = if self.cursor < self.bytes.len()
            && unsafe { *self.bytes.get_unchecked(self.cursor) } == byte
        {
            self.cursor
        } else {
            skip_json_whitespace(self.bytes, self.cursor)
        };
        if offset >= self.bytes.len() || unsafe { *self.bytes.get_unchecked(offset) } != byte {
            return false;
        }
        self.cursor = offset + 1;
        true
    }

    #[inline(always)]
    fn consume_quote_at_cursor(&mut self) -> Option<u32> {
        let offset = self.cursor;
        if offset >= self.bytes.len() || unsafe { *self.bytes.get_unchecked(offset) } != b'"' {
            return None;
        }
        let cursor = self.tape.push_plain_offset(offset);
        self.cursor = offset + 1;
        Some(cursor)
    }

    #[inline(always)]
    fn consume(&mut self, byte: u8) -> bool {
        if matches!(byte, b':' | b',') {
            return self.consume_delimiter(byte);
        }
        if matches!(byte, b'{' | b'}' | b'[' | b']' | b':' | b',' | b'"') {
            return self.consume_structural(byte).is_some();
        }
        if self.peek() == Some(byte) {
            self.cursor += 1;
            true
        } else {
            false
        }
    }

    #[inline(always)]
    fn consume_structural(&mut self, byte: u8) -> Option<u32> {
        let offset = if self.cursor < self.bytes.len()
            && unsafe { *self.bytes.get_unchecked(self.cursor) } == byte
        {
            self.cursor
        } else {
            skip_json_whitespace(self.bytes, self.cursor)
        };
        if offset >= self.bytes.len() || unsafe { *self.bytes.get_unchecked(offset) } != byte {
            return None;
        }
        let cursor = self.tape.push_plain_offset(offset);
        self.cursor = offset + 1;
        Some(cursor)
    }

    #[inline(always)]
    fn consume_container_next(
        &mut self,
        close: u8,
        error_kind: ParseErrorKind,
    ) -> Result<bool, ParseError<'i>> {
        let current = if self.cursor < self.bytes.len() {
            Some(unsafe { *self.bytes.get_unchecked(self.cursor) })
        } else {
            None
        };
        let offset = if current == Some(b',') || current == Some(close) {
            self.cursor
        } else {
            skip_json_whitespace(self.bytes, self.cursor)
        };
        if offset >= self.bytes.len() {
            return Err(self.error(error_kind));
        }
        let byte = unsafe { *self.bytes.get_unchecked(offset) };
        if byte == b',' {
            self.cursor = skip_json_whitespace(self.bytes, offset + 1);
            return Ok(true);
        }
        if byte == close {
            self.tape.push_plain_offset(offset);
            self.cursor = offset + 1;
            return Ok(false);
        }
        Err(self.error(error_kind))
    }

    #[inline(always)]
    fn peek(&self) -> Option<u8> {
        self.bytes.get(self.cursor).copied()
    }

    #[inline(always)]
    fn error(&self, kind: ParseErrorKind) -> ParseError<'i> {
        cold_error(self.input, self.cursor, kind)
    }
}

#[inline(always)]
fn match_tiny_plain_string(input: &[u8], offset: usize) -> Option<usize> {
    let mut cursor = offset + 1;
    let limit = (cursor + 8).min(input.len());
    while cursor < limit {
        match input[cursor] {
            b'"' => return Some(cursor + 1),
            b'\\' | 0x00..=0x1f => return None,
            _ => cursor += 1,
        }
    }
    None
}

#[cold]
#[inline(never)]
fn cold_error<'i>(input: &'i str, offset: usize, kind: ParseErrorKind) -> ParseError<'i> {
    ParseError {
        input,
        offset,
        kind,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_valid_json_without_payload_writes() {
        let root = parse(r#"{"b":2,"a":[true,null]}"#).unwrap();
        assert_eq!(root.tape().payloads().write_count(), 0);
        assert_eq!(root.tape().payloads().allocation_count(), 0);
        assert!(matches!(
            root.value(),
            runtime::grammars::json::JsonValue::Object(_)
        ));
    }

    #[test]
    fn rejects_invalid_json() {
        assert!(parse(r#"{"a":1,}"#).is_err());
    }

    #[test]
    fn emits_track1_compatible_offsets_without_calling_track1_parser() {
        let input = r#"{"a":["x",false],"b":{"c":null}}"#;
        let track1 = runtime::grammars::json::parse(input).unwrap();
        let track2 = parse(input).unwrap();
        assert_eq!(track2.offset_stream(), track1.offset_stream());
    }
}
