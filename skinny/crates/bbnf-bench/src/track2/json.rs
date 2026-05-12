use parse_that_regex::{
    match_json_number, match_json_string, skip_json_whitespace, RegexErrorKind,
};
use runtime::{
    grammars::json::{JsonRoot, ParseError, ParseErrorKind},
    tape::TapeAssembler,
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
    tape: TapeAssembler<'i>,
}

impl<'i> Parser<'i> {
    fn new(input: &'i str) -> Self {
        let scan = runtime::tape::scan_parse_index(input.as_bytes());
        let (structural_offsets, string_escape_offsets, string_control_offsets) = scan.into_parts();
        let capacity = structural_offsets.len() + (structural_offsets.len() / 3) + 8;
        let tape = TapeAssembler::new(input.as_bytes(), capacity);
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
        self.skip_ws();
        self.parse_value()?;
        self.skip_ws();
        if self.cursor != self.bytes.len() {
            return Err(self.error(ParseErrorKind::TrailingCharacters));
        }
        Ok(JsonRoot::from_tape(
            self.input,
            self.tape
                .finish(self.string_escape_offsets, self.string_control_offsets),
        ))
    }

    fn parse_value(&mut self) -> Result<(), ParseError<'i>> {
        self.skip_ws();
        match self.peek() {
            Some(b'{') => self.parse_object(),
            Some(b'[') => self.parse_array(),
            Some(b'"') => self.parse_string(),
            Some(b'-' | b'0'..=b'9') => self.parse_number(),
            Some(b't') => self.parse_literal(b"true"),
            Some(b'f') => self.parse_literal(b"false"),
            Some(b'n') => self.parse_literal(b"null"),
            _ => Err(self.error(ParseErrorKind::ExpectedValue)),
        }
    }

    fn parse_object(&mut self) -> Result<(), ParseError<'i>> {
        if !self.consume_structural(b'{') {
            return Err(self.error(ParseErrorKind::ExpectedValue));
        }
        self.skip_ws();

        if self.consume(b'}') {
            return Ok(());
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
                return Ok(());
            }
            return Err(self.error(ParseErrorKind::ExpectedCommaOrObjectEnd));
        }
    }

    fn parse_pair(&mut self) -> Result<(), ParseError<'i>> {
        self.parse_string()?;
        self.skip_ws();
        if !self.consume(b':') {
            return Err(self.error(ParseErrorKind::ExpectedColon));
        }
        self.parse_value()
    }

    fn parse_array(&mut self) -> Result<(), ParseError<'i>> {
        if !self.consume_structural(b'[') {
            return Err(self.error(ParseErrorKind::ExpectedValue));
        }
        self.skip_ws();

        if self.consume(b']') {
            return Ok(());
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
                return Ok(());
            }
            return Err(self.error(ParseErrorKind::ExpectedCommaOrArrayEnd));
        }
    }

    fn parse_string(&mut self) -> Result<(), ParseError<'i>> {
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
        self.tape.push_offset(close);
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
        Ok(())
    }

    fn parse_number(&mut self) -> Result<(), ParseError<'i>> {
        let number = match_json_number(self.bytes, self.cursor)
            .ok_or_else(|| self.error(ParseErrorKind::InvalidNumber))?;
        self.tape.push_offset(number.start);
        self.cursor = number.end;
        Ok(())
    }

    fn parse_literal(&mut self, literal: &'static [u8]) -> Result<(), ParseError<'i>> {
        let start = self.cursor;
        if self.bytes.get(start..start + literal.len()) != Some(literal) {
            return Err(self.error(ParseErrorKind::InvalidLiteral(
                std::str::from_utf8(literal).expect("literal is UTF-8"),
            )));
        }
        self.tape.push_offset(start);
        self.cursor += literal.len();
        Ok(())
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
        if !matches!(byte, b':' | b',') {
            self.tape.push_offset(offset);
        }
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
    fn emits_track1_compatible_offsets_without_calling_track1_parser() {
        let input = r#"{"a":["x",false],"b":{"c":null}}"#;
        let track1 = runtime::grammars::json::parse(input).unwrap();
        let track2 = parse(input).unwrap();
        assert_eq!(track2.offset_stream(), track1.offset_stream());
    }
}
