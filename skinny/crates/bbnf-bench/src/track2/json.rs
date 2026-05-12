use parse_that_regex::{
    match_json_number, match_json_string, skip_json_whitespace, RegexErrorKind,
};
use runtime::{
    grammars::json::{JsonNodeKind, JsonRoot, ParseError, ParseErrorKind},
    tape::{TapeBuilder, TokenFlags},
};

pub fn parse<'i>(input: &'i str) -> Result<JsonRoot<'i>, ParseError<'i>> {
    Parser::new(input).parse()
}

struct Parser<'i> {
    input: &'i str,
    bytes: &'i [u8],
    cursor: usize,
    structural_offsets: Vec<u32>,
    structural_cursor: usize,
    string_escape_offsets: Vec<u32>,
    string_escape_cursor: usize,
    string_control_offsets: Vec<u32>,
    string_control_cursor: usize,
    tape: TapeBuilder<'i>,
}

impl<'i> Parser<'i> {
    fn new(input: &'i str) -> Self {
        let scan = runtime::tape::scan_parse_index(input.as_bytes());
        let (structural_offsets, string_escape_offsets, string_control_offsets) = scan.into_parts();
        let tape = TapeBuilder::with_capacity(input.as_bytes(), structural_offsets.len() + 1);
        Self {
            input,
            bytes: input.as_bytes(),
            cursor: 0,
            structural_offsets,
            structural_cursor: 0,
            string_escape_offsets,
            string_escape_cursor: 0,
            string_control_offsets,
            string_control_cursor: 0,
            tape,
        }
    }

    fn parse(mut self) -> Result<JsonRoot<'i>, ParseError<'i>> {
        let root = self.tape.emit(
            JsonNodeKind::Root.into(),
            TokenFlags::new(TokenFlags::SIBLING_SKIP).with(TokenFlags::IS_STRUCTURAL_OPEN),
            0,
            self.bytes.len(),
            0,
        );

        self.skip_ws();
        self.parse_value()?;
        self.skip_ws();
        if self.cursor != self.bytes.len() {
            return Err(self.error(ParseErrorKind::TrailingCharacters));
        }
        self.tape.patch_skip_to_current_len(root);

        Ok(JsonRoot::from_tape(self.input, self.tape.finish()))
    }

    fn parse_value(&mut self) -> Result<u32, ParseError<'i>> {
        self.skip_ws();
        let index = match self.peek() {
            Some(b'{') => self.parse_object()?,
            Some(b'[') => self.parse_array()?,
            Some(b'"') => self.parse_string()?,
            Some(b'-' | b'0'..=b'9') => self.parse_number()?,
            Some(b't') => self.parse_literal(b"true", JsonNodeKind::True)?,
            Some(b'f') => self.parse_literal(b"false", JsonNodeKind::False)?,
            Some(b'n') => self.parse_literal(b"null", JsonNodeKind::Null)?,
            _ => return Err(self.error(ParseErrorKind::ExpectedValue)),
        };
        Ok(index)
    }

    fn parse_object(&mut self) -> Result<u32, ParseError<'i>> {
        let start = self.cursor;
        if !self.consume_structural(b'{') {
            return Err(self.error(ParseErrorKind::ExpectedValue));
        }
        let object = self.open(JsonNodeKind::ObjectOpen, start);
        self.skip_ws();

        if self.consume(b'}') {
            self.close(object, JsonNodeKind::ObjectClose);
            return Ok(object);
        }

        loop {
            if self.peek() != Some(b'"') {
                return Err(self.error(ParseErrorKind::ExpectedObjectKeyOrEnd));
            }
            self.parse_pair()?;
            self.skip_ws();
            if self.consume(b',') {
                self.skip_ws();
                continue;
            }
            if self.consume(b'}') {
                self.close(object, JsonNodeKind::ObjectClose);
                return Ok(object);
            }
            return Err(self.error(ParseErrorKind::ExpectedCommaOrObjectEnd));
        }
    }

    fn parse_pair(&mut self) -> Result<u32, ParseError<'i>> {
        let start = self.cursor;
        let pair = self.tape.emit(
            JsonNodeKind::Pair.into(),
            TokenFlags::new(TokenFlags::SIBLING_SKIP),
            start,
            start,
            0,
        );
        self.parse_string()?;
        self.skip_ws();
        if !self.consume(b':') {
            return Err(self.error(ParseErrorKind::ExpectedColon));
        }
        let value = self.parse_value()?;
        let end = self.tape.token(value).end as usize;
        self.tape.patch_end(pair, end);
        self.tape.patch_skip_to_current_len(pair);
        Ok(pair)
    }

    fn parse_array(&mut self) -> Result<u32, ParseError<'i>> {
        let start = self.cursor;
        if !self.consume_structural(b'[') {
            return Err(self.error(ParseErrorKind::ExpectedValue));
        }
        let array = self.open(JsonNodeKind::ArrayOpen, start);
        self.skip_ws();

        if self.consume(b']') {
            self.close(array, JsonNodeKind::ArrayClose);
            return Ok(array);
        }

        loop {
            if self.peek() == Some(b']') {
                return Err(self.error(ParseErrorKind::ExpectedArrayValueOrEnd));
            }
            self.parse_value()?;
            self.skip_ws();
            if self.consume(b',') {
                self.skip_ws();
                continue;
            }
            if self.consume(b']') {
                self.close(array, JsonNodeKind::ArrayClose);
                return Ok(array);
            }
            return Err(self.error(ParseErrorKind::ExpectedCommaOrArrayEnd));
        }
    }

    fn parse_string(&mut self) -> Result<u32, ParseError<'i>> {
        let start = self.cursor;
        if !self.consume_structural(b'"') {
            return Err(self.error(ParseErrorKind::ExpectedValue));
        }
        self.sync_structural();
        let Some(&close_offset) = self.structural_offsets.get(self.structural_cursor) else {
            return Err(self.error(ParseErrorKind::InvalidString));
        };
        let close = close_offset as usize;
        if self.bytes.get(close) != Some(&b'"') {
            return Err(self.error(ParseErrorKind::InvalidString));
        }
        self.structural_cursor += 1;
        self.cursor = close + 1;
        let content_start = start + 1;
        let content_end = close;
        if contains_indexed_offset(
            &self.string_control_offsets,
            &mut self.string_control_cursor,
            content_start,
            content_end,
        ) {
            return Err(ParseError {
                input: self.input,
                offset: self.string_control_offsets[self.string_control_cursor] as usize,
                kind: ParseErrorKind::InvalidString,
            });
        }
        let needs_unescape = contains_indexed_offset(
            &self.string_escape_offsets,
            &mut self.string_escape_cursor,
            content_start,
            content_end,
        );
        if needs_unescape {
            let span = match_json_string(self.bytes, start).map_err(|error| ParseError {
                input: self.input,
                offset: error.offset,
                kind: match error.kind {
                    RegexErrorKind::ExpectedString => ParseErrorKind::ExpectedValue,
                    _ => ParseErrorKind::InvalidString,
                },
            })?;
            if span.raw_end != self.cursor {
                return Err(self.error(ParseErrorKind::InvalidString));
            }
        }
        let mut flags = TokenFlags::new(TokenFlags::INLINE_STRING_BORROW)
            .with(TokenFlags::STRING_BORROWS_SOURCE);
        if needs_unescape {
            flags = flags.with(TokenFlags::STRING_NEEDS_UNESCAPE);
        }
        Ok(self.tape.emit(
            JsonNodeKind::String.into(),
            flags,
            content_start,
            content_end,
            0,
        ))
    }

    fn parse_number(&mut self) -> Result<u32, ParseError<'i>> {
        let number = match_json_number(self.bytes, self.cursor)
            .ok_or_else(|| self.error(ParseErrorKind::InvalidNumber))?;
        self.cursor = number.end;
        Ok(self.tape.emit(
            JsonNodeKind::Number.into(),
            TokenFlags::new(TokenFlags::INLINE_NUMBER_FAST),
            number.start,
            number.end,
            0,
        ))
    }

    fn parse_literal(
        &mut self,
        literal: &'static [u8],
        kind: JsonNodeKind,
    ) -> Result<u32, ParseError<'i>> {
        let start = self.cursor;
        if self.bytes.get(start..start + literal.len()) != Some(literal) {
            return Err(self.error(ParseErrorKind::InvalidLiteral(
                std::str::from_utf8(literal).expect("literal is UTF-8"),
            )));
        }
        self.cursor += literal.len();
        Ok(self.tape.emit(
            kind.into(),
            TokenFlags::new(TokenFlags::INLINE_BOOL_NULL),
            start,
            self.cursor,
            0,
        ))
    }

    fn open(&mut self, kind: JsonNodeKind, start: usize) -> u32 {
        self.tape.emit(
            kind.into(),
            TokenFlags::new(TokenFlags::SIBLING_SKIP).with(TokenFlags::IS_STRUCTURAL_OPEN),
            start,
            start + 1,
            0,
        )
    }

    fn close(&mut self, open: u32, _kind: JsonNodeKind) {
        let close = self.cursor - 1;
        self.tape.patch_end(open, close + 1);
        self.tape.patch_skip_to_current_len(open);
    }

    fn skip_ws(&mut self) {
        self.cursor = skip_json_whitespace(self.bytes, self.cursor);
    }

    fn consume(&mut self, byte: u8) -> bool {
        if matches!(byte, b'{' | b'}' | b'[' | b']' | b':' | b',' | b'"') {
            return self.consume_structural(byte);
        }
        if self.peek() == Some(byte) {
            self.cursor += 1;
            true
        } else {
            false
        }
    }

    fn consume_structural(&mut self, byte: u8) -> bool {
        self.sync_structural();
        let Some(&offset) = self.structural_offsets.get(self.structural_cursor) else {
            return false;
        };
        let offset = offset as usize;
        if self.cursor != offset && skip_json_whitespace(self.bytes, self.cursor) != offset {
            return false;
        }
        if self.bytes.get(offset) != Some(&byte) {
            return false;
        }
        self.structural_cursor += 1;
        self.cursor = offset + 1;
        true
    }

    fn sync_structural(&mut self) {
        while self
            .structural_offsets
            .get(self.structural_cursor)
            .is_some_and(|offset| (*offset as usize) < self.cursor)
        {
            self.structural_cursor += 1;
        }
    }

    fn peek(&self) -> Option<u8> {
        self.bytes.get(self.cursor).copied()
    }

    fn error(&self, kind: ParseErrorKind) -> ParseError<'i> {
        ParseError {
            input: self.input,
            offset: self.cursor,
            kind,
        }
    }
}

fn contains_indexed_offset(offsets: &[u32], cursor: &mut usize, start: usize, end: usize) -> bool {
    while offsets
        .get(*cursor)
        .is_some_and(|offset| (*offset as usize) < start)
    {
        *cursor += 1;
    }
    offsets
        .get(*cursor)
        .is_some_and(|offset| (*offset as usize) < end)
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
    fn emits_track1_compatible_tape_without_calling_track1_parser() {
        let input = r#"{"a":["x",false],"b":{"c":null}}"#;
        let track1: Vec<_> = runtime::grammars::json::parse(input)
            .unwrap()
            .token_stream()
            .collect();
        let track2: Vec<_> = parse(input).unwrap().token_stream().collect();

        assert_eq!(track2, track1);
    }
}
