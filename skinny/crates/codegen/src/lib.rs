use ir::{BackendIr, Recognizer};
use std::collections::BTreeMap;
use std::fmt::Write as _;
use std::path::Path;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum CodegenError {
    #[error(transparent)]
    Grammar(#[from] grammar::GrammarError),
    #[error(transparent)]
    Pass(#[from] passes::PassError),
    #[error("missing generated file `{0}`")]
    MissingFile(String),
    #[error("generated file `{0}` differs")]
    DifferentFile(String),
    #[error(transparent)]
    Io(#[from] std::io::Error),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EmittedSource {
    files: BTreeMap<String, String>,
}

impl EmittedSource {
    pub fn files(&self) -> impl Iterator<Item = (&str, &str)> {
        self.files
            .iter()
            .map(|(path, source)| (path.as_str(), source.as_str()))
    }

    pub fn get(&self, path: &str) -> Option<&str> {
        self.files.get(path).map(String::as_str)
    }

    pub fn write_to_dir(&self, output_dir: impl AsRef<Path>) -> Result<(), CodegenError> {
        let output_dir = output_dir.as_ref();
        std::fs::create_dir_all(output_dir)?;
        for (path, source) in &self.files {
            let file = output_dir.join(path);
            std::fs::write(file, source)?;
        }
        Ok(())
    }

    pub fn check_dir(&self, output_dir: impl AsRef<Path>) -> Result<(), CodegenError> {
        let output_dir = output_dir.as_ref();
        for (path, source) in &self.files {
            let file = output_dir.join(path);
            let actual = std::fs::read_to_string(&file)
                .map_err(|_| CodegenError::MissingFile(path.clone()))?;
            if actual != *source {
                return Err(CodegenError::DifferentFile(path.clone()));
            }
        }
        Ok(())
    }
}

pub fn emit_json_from_source(source: &str) -> Result<EmittedSource, CodegenError> {
    let grammar = grammar::parse_json_grammar(source)?;
    let output = passes::compile(&grammar)?;
    emit_json(&output.backend_ir)
}

pub fn emit_json(backend: &BackendIr) -> Result<EmittedSource, CodegenError> {
    let mut files = BTreeMap::new();
    files.insert("generated.rs".to_string(), generated_rs(backend));
    files.insert("host.rs".to_string(), host_rs());
    files.insert("mod.rs".to_string(), mod_rs());
    files.insert("parser.rs".to_string(), parser_rs(backend));
    files.insert("value.rs".to_string(), value_rs());
    files.insert("view.rs".to_string(), view_rs());
    files.insert("visitor.rs".to_string(), visitor_rs());
    Ok(EmittedSource { files })
}

fn mod_rs() -> String {
    normalize(
        r#"
        pub mod generated;
        pub mod host;
        pub mod parser;
        pub mod value;
        pub mod view;
        pub mod visitor;

        pub use parser::{parse, RECOGNIZER_COUNT};
        pub use value::{JsonNodeKind, JsonToken, JsonValue, ParseError, ParseErrorKind};
        pub use view::{
            JsonArray, JsonBool, JsonDocument, JsonNull, JsonNumber, JsonObject, JsonPair,
            JsonRoot, JsonString,
        };
        pub use visitor::JsonVisitor;
        "#,
    )
}

fn host_rs() -> String {
    normalize(
        r#"
        // JSON is host-fn-free in the skinny compiler slice.
        "#,
    )
}

fn parser_rs(backend: &BackendIr) -> String {
    let recognizer_count = backend.recognizers.len();
    normalize(&format!(
        r#"
        use super::generated;
        use super::value::ParseError;
        use super::view::JsonRoot;
        use crate::tape::TapeBuilder;

        pub(crate) struct ParserState<'i> {{
            pub input: &'i str,
            pub bytes: &'i [u8],
            pub cursor: usize,
            pub structural_offsets: Vec<u32>,
            pub structural_cursor: usize,
            pub string_escape_offsets: Vec<u32>,
            pub string_escape_cursor: usize,
            pub string_control_offsets: Vec<u32>,
            pub string_control_cursor: usize,
            pub tape: TapeBuilder<'i>,
        }}

        impl<'i> ParserState<'i> {{
            pub fn new(input: &'i str) -> Self {{
                Self {{
                    input,
                    bytes: input.as_bytes(),
                    cursor: 0,
                    structural_offsets: Vec::new(),
                    structural_cursor: 0,
                    string_escape_offsets: Vec::new(),
                    string_escape_cursor: 0,
                    string_control_offsets: Vec::new(),
                    string_control_cursor: 0,
                    tape: TapeBuilder::new(input.as_bytes()),
                }}
            }}

            pub fn finish(self) -> JsonRoot<'i> {{
                JsonRoot::from_tape(self.input, self.tape.finish())
            }}
        }}

        pub fn parse<'i>(input: &'i str) -> Result<JsonRoot<'i>, ParseError<'i>> {{
            let mut state = ParserState::new(input);
            generated::attach_structural_index(&mut state);
            generated::parse_json(&mut state)?;
            Ok(state.finish())
        }}

        pub const RECOGNIZER_COUNT: usize = {recognizer_count};
        "#,
    ))
}

fn generated_rs(backend: &BackendIr) -> String {
    let mut recognizer_comment = String::new();
    for recognizer in &backend.recognizers {
        match recognizer {
            Recognizer::SimdScan { alphabet, .. } => {
                let bytes = String::from_utf8_lossy(&alphabet.bytes);
                let _ = writeln!(
                    recognizer_comment,
                    "// recognizer: SimdScan Exact PreEntry alphabet={bytes:?}"
                );
            }
        }
    }

    normalize(&format!(
        r#"
        use super::parser::ParserState;
        use super::value::{{JsonNodeKind, ParseError, ParseErrorKind}};
        use crate::tape::TokenFlags;
        use parse_that_regex::{{
            match_json_number, match_json_string, skip_json_whitespace, RegexErrorKind,
        }};

        const STRUCTURAL_ALPHABET_JSON: &[u8] = b"{{}}[],:\"";

        {recognizer_comment}
        pub(crate) fn attach_structural_index(state: &mut ParserState<'_>) {{
            debug_assert_eq!(STRUCTURAL_ALPHABET_JSON, b"{{}}[],:\"");
            let scan = crate::tape::scan_parse_index(state.bytes);
            let (structural_offsets, string_escape_offsets, string_control_offsets) =
                scan.into_parts();
            state.tape.reserve_tokens(structural_offsets.len() + 1);
            state.structural_offsets = structural_offsets;
            state.structural_cursor = 0;
            state.string_escape_offsets = string_escape_offsets;
            state.string_escape_cursor = 0;
            state.string_control_offsets = string_control_offsets;
            state.string_control_cursor = 0;
        }}

        pub(crate) fn parse_json<'i>(state: &mut ParserState<'i>) -> Result<(), ParseError<'i>> {{
            let root = state.tape.emit(
                JsonNodeKind::Root.into(),
                TokenFlags::new(TokenFlags::SIBLING_SKIP).with(TokenFlags::IS_STRUCTURAL_OPEN),
                0,
                state.bytes.len(),
                0,
            );
            skip_ws(state);
            parse_value(state)?;
            skip_ws(state);
            if state.cursor != state.bytes.len() {{
                return Err(error(state, ParseErrorKind::TrailingCharacters));
            }}
            state.tape.patch_skip_to_current_len(root);
            Ok(())
        }}

        fn parse_value<'i>(state: &mut ParserState<'i>) -> Result<u32, ParseError<'i>> {{
            skip_ws(state);
            let index = match peek(state) {{
                Some(b'{{') => parse_object(state)?,
                Some(b'[') => parse_array(state)?,
                Some(b'"') => parse_string(state)?,
                Some(b'-' | b'0'..=b'9') => parse_number(state)?,
                Some(b't') => parse_literal(state, b"true", JsonNodeKind::True)?,
                Some(b'f') => parse_literal(state, b"false", JsonNodeKind::False)?,
                Some(b'n') => parse_literal(state, b"null", JsonNodeKind::Null)?,
                _ => return Err(error(state, ParseErrorKind::ExpectedValue)),
            }};
            Ok(index)
        }}

        fn parse_object<'i>(state: &mut ParserState<'i>) -> Result<u32, ParseError<'i>> {{
            let start = state.cursor;
            if !consume_structural(state, b'{{') {{
                return Err(error(state, ParseErrorKind::ExpectedValue));
            }}
            let object = open(state, JsonNodeKind::ObjectOpen, start);
            skip_ws(state);

            if consume(state, b'}}') {{
                close(state, object, JsonNodeKind::ObjectClose);
                return Ok(object);
            }}

            loop {{
                if peek(state) != Some(b'"') {{
                    return Err(error(state, ParseErrorKind::ExpectedObjectKeyOrEnd));
                }}
                parse_pair(state)?;
                skip_ws(state);
                if consume(state, b',') {{
                    skip_ws(state);
                    continue;
                }}
                if consume(state, b'}}') {{
                    close(state, object, JsonNodeKind::ObjectClose);
                    return Ok(object);
                }}
                return Err(error(state, ParseErrorKind::ExpectedCommaOrObjectEnd));
            }}
        }}

        fn parse_pair<'i>(state: &mut ParserState<'i>) -> Result<u32, ParseError<'i>> {{
            let start = state.cursor;
            let pair = state.tape.emit(
                JsonNodeKind::Pair.into(),
                TokenFlags::new(TokenFlags::SIBLING_SKIP),
                start,
                start,
                0,
            );
            parse_string(state)?;
            skip_ws(state);
            if !consume(state, b':') {{
                return Err(error(state, ParseErrorKind::ExpectedColon));
            }}
            let value = parse_value(state)?;
            let end = state.tape.token(value).end as usize;
            state.tape.patch_end(pair, end);
            state.tape.patch_skip_to_current_len(pair);
            Ok(pair)
        }}

        fn parse_array<'i>(state: &mut ParserState<'i>) -> Result<u32, ParseError<'i>> {{
            let start = state.cursor;
            if !consume_structural(state, b'[') {{
                return Err(error(state, ParseErrorKind::ExpectedValue));
            }}
            let array = open(state, JsonNodeKind::ArrayOpen, start);
            skip_ws(state);

            if consume(state, b']') {{
                close(state, array, JsonNodeKind::ArrayClose);
                return Ok(array);
            }}

            loop {{
                if peek(state) == Some(b']') {{
                    return Err(error(state, ParseErrorKind::ExpectedArrayValueOrEnd));
                }}
                parse_value(state)?;
                skip_ws(state);
                if consume(state, b',') {{
                    skip_ws(state);
                    continue;
                }}
                if consume(state, b']') {{
                    close(state, array, JsonNodeKind::ArrayClose);
                    return Ok(array);
                }}
                return Err(error(state, ParseErrorKind::ExpectedCommaOrArrayEnd));
            }}
        }}

        fn parse_string<'i>(state: &mut ParserState<'i>) -> Result<u32, ParseError<'i>> {{
            let start = state.cursor;
            if !consume_structural(state, b'"') {{
                return Err(error(state, ParseErrorKind::ExpectedValue));
            }}
            sync_structural(state);
            let Some(&close_offset) = state.structural_offsets.get(state.structural_cursor) else {{
                return Err(error(state, ParseErrorKind::InvalidString));
            }};
            let close = close_offset as usize;
            if state.bytes.get(close) != Some(&b'"') {{
                return Err(error(state, ParseErrorKind::InvalidString));
            }}
            state.structural_cursor += 1;
            state.cursor = close + 1;
            let content_start = start + 1;
            let content_end = close;
            if contains_indexed_offset(
                &state.string_control_offsets,
                &mut state.string_control_cursor,
                content_start,
                content_end,
            ) {{
                return Err(ParseError {{
                    input: state.input,
                    offset: state.string_control_offsets[state.string_control_cursor] as usize,
                    kind: ParseErrorKind::InvalidString,
                }});
            }}
            let needs_unescape = contains_indexed_offset(
                &state.string_escape_offsets,
                &mut state.string_escape_cursor,
                content_start,
                content_end,
            );
            if needs_unescape {{
                let span = match_json_string(state.bytes, start).map_err(|err| ParseError {{
                    input: state.input,
                    offset: err.offset,
                    kind: match err.kind {{
                        RegexErrorKind::ExpectedString => ParseErrorKind::ExpectedValue,
                        _ => ParseErrorKind::InvalidString,
                    }},
                }})?;
                if span.raw_end != state.cursor {{
                    return Err(error(state, ParseErrorKind::InvalidString));
                }}
            }}
            let mut flags = TokenFlags::new(TokenFlags::INLINE_STRING_BORROW)
                .with(TokenFlags::STRING_BORROWS_SOURCE);
            if needs_unescape {{
                flags = flags.with(TokenFlags::STRING_NEEDS_UNESCAPE);
            }}
            Ok(state.tape.emit(
                JsonNodeKind::String.into(),
                flags,
                content_start,
                content_end,
                0,
            ))
        }}

        fn parse_number<'i>(state: &mut ParserState<'i>) -> Result<u32, ParseError<'i>> {{
            let number = match_json_number(state.bytes, state.cursor)
                .ok_or_else(|| error(state, ParseErrorKind::InvalidNumber))?;
            state.cursor = number.end;
            Ok(state.tape.emit(
                JsonNodeKind::Number.into(),
                TokenFlags::new(TokenFlags::INLINE_NUMBER_FAST),
                number.start,
                number.end,
                0,
            ))
        }}

        fn parse_literal<'i>(
            state: &mut ParserState<'i>,
            literal: &'static [u8],
            kind: JsonNodeKind,
        ) -> Result<u32, ParseError<'i>> {{
            let start = state.cursor;
            if state.bytes.get(start..start + literal.len()) != Some(literal) {{
                return Err(error(
                    state,
                    ParseErrorKind::InvalidLiteral(
                        std::str::from_utf8(literal).expect("literal is UTF-8"),
                    ),
                ));
            }}
            state.cursor += literal.len();
            Ok(state.tape.emit(
                kind.into(),
                TokenFlags::new(TokenFlags::INLINE_BOOL_NULL),
                start,
                state.cursor,
                0,
            ))
        }}

        fn open(state: &mut ParserState<'_>, kind: JsonNodeKind, start: usize) -> u32 {{
            state.tape.emit(
                kind.into(),
                TokenFlags::new(TokenFlags::SIBLING_SKIP).with(TokenFlags::IS_STRUCTURAL_OPEN),
                start,
                start + 1,
                0,
            )
        }}

        fn close(state: &mut ParserState<'_>, open: u32, _kind: JsonNodeKind) {{
            let close = state.cursor - 1;
            state.tape.patch_end(open, close + 1);
            state.tape.patch_skip_to_current_len(open);
        }}

        fn skip_ws(state: &mut ParserState<'_>) {{
            state.cursor = skip_json_whitespace(state.bytes, state.cursor);
        }}

        fn consume(state: &mut ParserState<'_>, byte: u8) -> bool {{
            if matches!(byte, b'{{' | b'}}' | b'[' | b']' | b':' | b',' | b'"') {{
                return consume_structural(state, byte);
            }}
            if peek(state) == Some(byte) {{
                state.cursor += 1;
                true
            }} else {{
                false
            }}
        }}

        fn consume_structural(state: &mut ParserState<'_>, byte: u8) -> bool {{
            sync_structural(state);
            let Some(&offset) = state.structural_offsets.get(state.structural_cursor) else {{
                return false;
            }};
            let offset = offset as usize;
            if state.cursor != offset && skip_json_whitespace(state.bytes, state.cursor) != offset {{
                return false;
            }}
            if state.bytes.get(offset) != Some(&byte) {{
                return false;
            }}
            state.structural_cursor += 1;
            state.cursor = offset + 1;
            true
        }}

        fn sync_structural(state: &mut ParserState<'_>) {{
            while state
                .structural_offsets
                .get(state.structural_cursor)
                .is_some_and(|offset| (*offset as usize) < state.cursor)
            {{
                state.structural_cursor += 1;
            }}
        }}

        fn contains_indexed_offset(
            offsets: &[u32],
            cursor: &mut usize,
            start: usize,
            end: usize,
        ) -> bool {{
            while offsets
                .get(*cursor)
                .is_some_and(|offset| (*offset as usize) < start)
            {{
                *cursor += 1;
            }}
            offsets
                .get(*cursor)
                .is_some_and(|offset| (*offset as usize) < end)
        }}

        fn peek(state: &ParserState<'_>) -> Option<u8> {{
            state.bytes.get(state.cursor).copied()
        }}

        fn error<'i>(state: &ParserState<'i>, kind: ParseErrorKind) -> ParseError<'i> {{
            ParseError {{
                input: state.input,
                offset: state.cursor,
                kind,
            }}
        }}
        "#,
    ))
}

fn view_rs() -> String {
    normalize(include_str!("json_templates/view.rs"))
}

fn value_rs() -> String {
    normalize(include_str!("json_templates/value.rs"))
}

fn visitor_rs() -> String {
    normalize(include_str!("json_templates/visitor.rs"))
}

fn normalize(source: &str) -> String {
    let mut out = String::new();
    out.push_str("// @generated by skinny bbnf-codegen; do not edit by hand.\n");
    let trimmed = source.trim_matches('\n');
    let indent = trimmed
        .lines()
        .filter(|line| !line.trim().is_empty())
        .map(|line| line.chars().take_while(|ch| *ch == ' ').count())
        .min()
        .unwrap_or(0);
    for raw_line in trimmed.lines() {
        let line = raw_line.get(indent..).unwrap_or(raw_line).trim_end();
        if line.is_empty() {
            out.push('\n');
        } else {
            out.push_str(line);
            out.push('\n');
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    const JSON_GRAMMAR: &str = include_str!("../../../grammars/json.bbnf");

    #[test]
    fn emits_expected_file_set_in_order() {
        let emitted = emit_json_from_source(JSON_GRAMMAR).unwrap();
        let names: Vec<_> = emitted.files().map(|(path, _)| path).collect();

        assert_eq!(
            names,
            [
                "generated.rs",
                "host.rs",
                "mod.rs",
                "parser.rs",
                "value.rs",
                "view.rs",
                "visitor.rs"
            ]
        );
    }

    #[test]
    fn emission_is_deterministic() {
        let first = emit_json_from_source(JSON_GRAMMAR).unwrap();
        let second = emit_json_from_source(JSON_GRAMMAR).unwrap();

        assert_eq!(first, second);
        assert!(first
            .get("generated.rs")
            .unwrap()
            .contains("STRUCTURAL_ALPHABET_JSON"));
    }
}
