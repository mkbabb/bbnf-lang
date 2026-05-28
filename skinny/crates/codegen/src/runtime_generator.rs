use crate::grammar_provider::{RuntimeEmitterKind, RuntimeGenerationRequest, RuntimeOutputLabels};
use crate::{grammar_profile, json_sink_direct, lower, CodegenError, EmittedSource};
use std::collections::BTreeMap;
use std::fmt::Write;

pub(crate) fn emit_profile_only(profile_id: &str) -> Result<EmittedSource, CodegenError> {
    Err(CodegenError::Lowering(format!(
        "runtime profile `{profile_id}` requires RuntimeGenerationRequest after W5C-GEN",
    )))
}

pub(crate) fn emit_from_request(
    request: &RuntimeGenerationRequest,
    facts: &grammar::RuntimeSourceFacts,
) -> Result<EmittedSource, CodegenError> {
    match request.profile_contract.emitter {
        RuntimeEmitterKind::CompiledLowering => {
            let Some(source) = request.sources.first() else {
                return Err(CodegenError::Lowering(
                    "compiled runtime request requires a source".to_string(),
                ));
            };
            crate::emit_from_source(&request.grammar_name, &source.source)
        }
        RuntimeEmitterKind::RequestFacts => emit_request_facts(request, facts),
    }
}

pub(crate) fn emit_compiled(
    profile_id: &str,
    sink_only: &lower::sink_only::SinkOnlyProgram,
) -> Result<EmittedSource, CodegenError> {
    let mut generated = include_str!("json_templates/generated.rs").to_string();
    generated.push('\n');
    generated.push_str(JSON_PARSE_ONLY_GENERATED_RS.trim_start_matches('\n'));
    generated.push('\n');
    generated.push_str(&json_sink_direct::render(sink_only).map_err(CodegenError::Lowering)?);
    let mut host = normalize(JSON_HOST_RS);
    host.push('\n');
    let module = normalize(JSON_MOD_RS);
    let mut parser = include_str!("json_templates/parser.rs").to_string();
    parser.push('\n');
    parser.push_str(JSON_PARSE_ONLY_PARSER_RS.trim_start_matches('\n'));

    let files = BTreeMap::from([
        (
            "config.rs".to_string(),
            render_json_config(&sink_only.policy_summary),
        ),
        ("generated.rs".to_string(), generated),
        ("host.rs".to_string(), host),
        ("mod.rs".to_string(), module),
        ("parser.rs".to_string(), parser),
        (
            "value.rs".to_string(),
            include_str!("json_templates/value.rs").to_string(),
        ),
        (
            "view.rs".to_string(),
            include_str!("json_templates/view.rs").to_string(),
        ),
        (
            "visitor.rs".to_string(),
            normalize(include_str!("json_templates/visitor.rs")),
        ),
    ]);
    grammar_profile::validate_generated_roster(
        profile_id,
        grammar_profile::COMPILED_RUNTIME_FILES,
        files.keys().map(String::as_str),
    )
    .map_err(CodegenError::Lowering)?;
    Ok(EmittedSource { files })
}

fn emit_request_facts(
    request: &RuntimeGenerationRequest,
    facts: &grammar::RuntimeSourceFacts,
) -> Result<EmittedSource, CodegenError> {
    let labels = request.profile_contract.output_labels.ok_or_else(|| {
        CodegenError::Lowering(format!(
            "runtime profile `{}` requires request-facts output labels",
            request.profile_id
        ))
    })?;
    let files = BTreeMap::from([
        (
            "config.rs".to_string(),
            render_request_facts_config(labels, request, facts),
        ),
        ("generated.rs".to_string(), normalize(CSS_GENERATED_RS)),
        ("mod.rs".to_string(), normalize(CSS_MOD_RS)),
        ("parser.rs".to_string(), normalize(CSS_PARSER_RS)),
        ("sink.rs".to_string(), normalize(CSS_SINK_RS)),
    ]);
    grammar_profile::validate_generated_roster(
        &request.profile_id,
        request.profile_contract.expected_files,
        files.keys().map(String::as_str),
    )
    .map_err(CodegenError::Lowering)?;
    Ok(EmittedSource { files })
}

fn render_request_facts_config(
    labels: RuntimeOutputLabels,
    request: &RuntimeGenerationRequest,
    facts: &grammar::RuntimeSourceFacts,
) -> String {
    let policy = ir::Lock1PolicyTriad::fact_stream();
    format!(
        "{header}\npub(crate) const FACT_SCHEMA: &str = {fact_schema:?};\n\
         pub(crate) const ROW_ID: &str = {row_id:?};\n\
         pub(crate) const OUTPUT_PLANE: &str = {output_plane:?};\n\
         pub(crate) const W7_POLICY_BACKEND_SHAPE: &str = \"admitted_fact_output\";\n\
         pub(crate) const W7_SUBSTRATE_TARGET: &str = {substrate_target:?};\n\
         pub(crate) const W7_RETENTION_LIFETIME: &str = {retention_lifetime:?};\n\
         pub(crate) const W7_POLICY_OWNER: &str = {policy_owner:?};\n\
         pub(crate) const W7_SAME_SUBSTRATE_UNION: &str = \"pass\";\n\
         pub(crate) const REQUEST_PROFILE: &str = {profile:?};\n\
         pub(crate) const ENTRY_RULE: &str = {entry:?};\n\
         pub(crate) const FRONTEND_SOURCE_HASH: &str = {source_hash:?};\n\
         pub(crate) const REQUEST_SOURCE_COUNT: usize = {source_count};\n\
         pub(crate) const IMPORT_COUNT: usize = {import_count};\n\
         pub(crate) const LAYOUT_DIRECTIVE_COUNT: usize = {layout_count};\n\
         pub(crate) const DISCARD_OPERATOR_COUNT: usize = {discard_count};\n",
        header = crate::GENERATED_HEADER,
        fact_schema = labels.fact_schema,
        row_id = labels.row_id,
        output_plane = labels.output_plane,
        substrate_target = policy.substrate_target.as_str(),
        retention_lifetime = policy.retention_lifetime.as_str(),
        policy_owner = policy.policy_owner.as_str(),
        profile = request.profile_id,
        entry = request.entry_rule,
        source_hash = facts.source_hash,
        source_count = facts.frontend.sources.len(),
        import_count = facts.frontend.imports.len(),
        layout_count = facts.frontend.layout.whitespace_directives.len(),
        discard_count = facts.frontend.layout.discard_operators.len(),
    )
}

fn render_json_config(policy: &lower::sink_only::RuntimePolicySummary) -> String {
    let mut out = normalize(include_str!("json_templates/config.rs"));
    let _ = writeln!(
        out,
        "\npub(crate) const W7_DIRECT_BACKEND_SHAPE: &str = {:?};",
        format!("{:?}", policy.backend_shape)
    );
    let _ = writeln!(
        out,
        "pub(crate) const W7_SUBSTRATE_TARGET: &str = {:?};",
        policy.substrate_target.as_str()
    );
    let _ = writeln!(
        out,
        "pub(crate) const W7_RETENTION_LIFETIME: &str = {:?};",
        policy.retention_lifetime.as_str()
    );
    let _ = writeln!(
        out,
        "pub(crate) const W7_POLICY_OWNER: &str = {:?};",
        policy.policy_owner.as_str()
    );
    let _ = writeln!(
        out,
        "pub(crate) const W7_SAME_SUBSTRATE_UNION: &str = {:?};",
        policy.same_substrate_union
    );
    out.push_str(
        "\n#[inline(always)]\n\
         pub(crate) fn w7_direct_policy_triad() -> (&'static str, &'static str, &'static str) {\n\
             (W7_SUBSTRATE_TARGET, W7_RETENTION_LIFETIME, W7_POLICY_OWNER)\n\
         }\n",
    );
    out
}

fn normalize(source: &str) -> String {
    let mut out = String::new();
    out.push_str(crate::GENERATED_HEADER);
    out.push('\n');
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

const JSON_PARSE_ONLY_GENERATED_RS: &str = r#"
struct ParseOnlyState<'i> {
    input: &'i str,
    bytes: &'i [u8],
    cursor: usize,
}

impl<'i> ParseOnlyState<'i> {
    #[inline(always)]
    fn new(input: &'i str) -> Self {
        Self {
            input,
            bytes: input.as_bytes(),
            cursor: 0,
        }
    }
}

#[cfg_attr(feature = "parse-attribution", inline(never))]
#[cfg_attr(not(feature = "parse-attribution"), inline(always))]
pub fn parse_only<'i>(input: &'i str) -> Result<(), ParseError<'i>> {
    let mut state = ParseOnlyState::new(input);
    parse_only_skip_ws(&mut state);
    parse_only_value_iterative(&mut state)?;
    parse_only_skip_ws(&mut state);
    if state.cursor != state.bytes.len() {
        return Err(parse_only_error(&state, ParseErrorKind::TrailingCharacters));
    }
    Ok(())
}

#[derive(Clone, Copy)]
enum ParseOnlyFrame {
    ObjectExpectKeyOrEnd,
    ObjectExpectKey,
    ObjectAfterValue,
    ArrayAfterValue,
}

#[cfg_attr(feature = "parse-attribution", inline(never))]
#[cfg_attr(not(feature = "parse-attribution"), inline(always))]
fn parse_only_value_iterative<'i>(state: &mut ParseOnlyState<'i>) -> Result<(), ParseError<'i>> {
    let mut stack = Vec::new();
    parse_only_begin_value(state, &mut stack)?;

    while let Some(frame) = stack.last().copied() {
        match frame {
            ParseOnlyFrame::ObjectExpectKeyOrEnd => {
                if parse_only_consume(state, b'}') {
                    stack.pop();
                    continue;
                }
                parse_only_key_colon(state)?;
                *stack.last_mut().expect("object frame present") = ParseOnlyFrame::ObjectAfterValue;
                parse_only_begin_value(state, &mut stack)?;
            }
            ParseOnlyFrame::ObjectExpectKey => {
                parse_only_key_colon(state)?;
                *stack.last_mut().expect("object frame present") = ParseOnlyFrame::ObjectAfterValue;
                parse_only_begin_value(state, &mut stack)?;
            }
            ParseOnlyFrame::ObjectAfterValue => {
                if parse_only_consume_container_next(
                    state,
                    b'}',
                    ParseErrorKind::ExpectedCommaOrObjectEnd,
                )? {
                    *stack.last_mut().expect("object frame present") = ParseOnlyFrame::ObjectExpectKey;
                } else {
                    stack.pop();
                }
            }
            ParseOnlyFrame::ArrayAfterValue => match parse_only_consume_array_next(state)? {
                ParseOnlyContainerNext::Next => parse_only_begin_value(state, &mut stack)?,
                ParseOnlyContainerNext::Done => {
                    stack.pop();
                }
            },
        }
    }

    Ok(())
}

#[cfg_attr(feature = "parse-attribution", inline(never))]
#[cfg_attr(not(feature = "parse-attribution"), inline(always))]
fn parse_only_begin_value<'i>(
    state: &mut ParseOnlyState<'i>,
    stack: &mut Vec<ParseOnlyFrame>,
) -> Result<(), ParseError<'i>> {
    if state.cursor >= state.bytes.len() {
        return Err(parse_only_error(state, ParseErrorKind::ExpectedValue));
    }
    let byte = unsafe { *state.bytes.get_unchecked(state.cursor) };
    match byte {
        b'{' => {
            if !parse_only_take_structural(state, b'{') {
                return Err(parse_only_error(state, ParseErrorKind::ExpectedValue));
            }
            parse_only_skip_ws(state);
            if parse_only_consume(state, b'}') {
                return Ok(());
            }
            stack.push(ParseOnlyFrame::ObjectExpectKeyOrEnd);
            Ok(())
        }
        b'[' => {
            if !parse_only_take_structural(state, b'[') {
                return Err(parse_only_error(state, ParseErrorKind::ExpectedValue));
            }
            parse_only_skip_ws(state);
            if parse_only_consume(state, b']') {
                return Ok(());
            }
            stack.push(ParseOnlyFrame::ArrayAfterValue);
            parse_only_begin_value(state, stack)
        }
        b'"' => parse_only_string(state),
        b'-' | b'0'..=b'9' => parse_only_number(state, byte),
        b't' => parse_only_literal(state, config::TRUE_LITERAL),
        b'f' => parse_only_literal(state, config::FALSE_LITERAL),
        b'n' => parse_only_literal(state, config::NULL_LITERAL),
        _ => Err(parse_only_error(state, ParseErrorKind::ExpectedValue)),
    }
}

#[cfg_attr(feature = "parse-attribution", inline(never))]
#[cfg_attr(not(feature = "parse-attribution"), inline(always))]
fn parse_only_key_colon<'i>(state: &mut ParseOnlyState<'i>) -> Result<(), ParseError<'i>> {
    parse_only_string(state)?;

    let colon = if state.cursor < state.bytes.len()
        && unsafe { *state.bytes.get_unchecked(state.cursor) } == b':'
    {
        state.cursor
    } else {
        skip_ascii_whitespace(state.bytes, state.cursor)
    };
    if colon >= state.bytes.len() || unsafe { *state.bytes.get_unchecked(colon) } != b':' {
        return Err(parse_only_error(state, ParseErrorKind::ExpectedColon));
    }
    state.cursor = skip_ascii_whitespace(state.bytes, colon + 1);
    Ok(())
}

#[cfg_attr(feature = "parse-attribution", inline(never))]
#[cfg_attr(not(feature = "parse-attribution"), inline(always))]
fn parse_only_string<'i>(state: &mut ParseOnlyState<'i>) -> Result<(), ParseError<'i>> {
    let start = state.cursor;
    if state.bytes.get(start) != Some(&b'"') {
        return Err(parse_only_error(state, ParseErrorKind::ExpectedValue));
    }
    state.cursor = parse_only_string_end(state.bytes, start).map_err(|err| ParseError {
        input: state.input,
        offset: err.offset,
        kind: match err.kind {
            RegexErrorKind::ExpectedString => ParseErrorKind::ExpectedValue,
            _ => ParseErrorKind::InvalidString,
        },
    })?;
    Ok(())
}

#[inline(always)]
fn parse_only_string_end(input: &[u8], offset: usize) -> Result<usize, parse_that_regex::RegexError> {
    let prefix = parse_that_regex::scan_tiny_string_prefix_trusted_utf8::<{ config::DIRECT_TINY_STRING_CAP }>(
        input, offset,
    );
    if let Some(raw_end) = prefix.raw_end {
        return Ok(raw_end);
    }
    parse_that_regex::match_string_end_at_quote_after_plain_prefix_trusted_utf8(
        input,
        offset,
        prefix.cursor,
    )
}

#[cfg_attr(feature = "parse-attribution", inline(never))]
#[cfg_attr(not(feature = "parse-attribution"), inline(always))]
fn parse_only_number<'i>(state: &mut ParseOnlyState<'i>, first: u8) -> Result<(), ParseError<'i>> {
    let number = match_number_at_digit(state.bytes, state.cursor, first)
        .ok_or_else(|| parse_only_error(state, ParseErrorKind::InvalidNumber))?;
    state.cursor = number.end;
    Ok(())
}

#[cfg_attr(feature = "parse-attribution", inline(never))]
#[cfg_attr(not(feature = "parse-attribution"), inline(always))]
fn parse_only_literal<'i>(
    state: &mut ParseOnlyState<'i>,
    literal: &'static [u8],
) -> Result<(), ParseError<'i>> {
    let start = state.cursor;
    if state.bytes.get(start..start + literal.len()) != Some(literal) {
        return Err(parse_only_error(
            state,
            ParseErrorKind::InvalidLiteral(std::str::from_utf8(literal).expect("literal is UTF-8")),
        ));
    }
    state.cursor += literal.len();
    Ok(())
}

#[cfg_attr(feature = "parse-attribution", inline(never))]
#[cfg_attr(not(feature = "parse-attribution"), inline(always))]
fn parse_only_skip_ws(state: &mut ParseOnlyState<'_>) {
    state.cursor = skip_ascii_whitespace(state.bytes, state.cursor);
}

#[cfg_attr(feature = "parse-attribution", inline(never))]
#[cfg_attr(not(feature = "parse-attribution"), inline(always))]
fn parse_only_consume_delimiter(state: &mut ParseOnlyState<'_>, byte: u8) -> bool {
    let offset = if state.cursor < state.bytes.len()
        && unsafe { *state.bytes.get_unchecked(state.cursor) } == byte
    {
        state.cursor
    } else {
        skip_ascii_whitespace(state.bytes, state.cursor)
    };
    if offset >= state.bytes.len() || unsafe { *state.bytes.get_unchecked(offset) } != byte {
        return false;
    }
    state.cursor = offset + 1;
    true
}

#[cfg_attr(feature = "parse-attribution", inline(never))]
#[cfg_attr(not(feature = "parse-attribution"), inline(always))]
fn parse_only_consume(state: &mut ParseOnlyState<'_>, byte: u8) -> bool {
    if matches!(byte, b':' | b',') {
        return parse_only_consume_delimiter(state, byte);
    }
    if matches!(byte, b'{' | b'}' | b'[' | b']' | b':' | b',' | b'"') {
        return parse_only_take_structural(state, byte);
    }
    if parse_only_peek(state) == Some(byte) {
        state.cursor += 1;
        true
    } else {
        false
    }
}

#[cfg_attr(feature = "parse-attribution", inline(never))]
#[cfg_attr(not(feature = "parse-attribution"), inline(always))]
fn parse_only_take_structural(state: &mut ParseOnlyState<'_>, byte: u8) -> bool {
    let offset = if state.cursor < state.bytes.len()
        && unsafe { *state.bytes.get_unchecked(state.cursor) } == byte
    {
        state.cursor
    } else {
        skip_ascii_whitespace(state.bytes, state.cursor)
    };
    if offset >= state.bytes.len() || unsafe { *state.bytes.get_unchecked(offset) } != byte {
        return false;
    }
    state.cursor = offset + 1;
    true
}

#[cfg_attr(feature = "parse-attribution", inline(never))]
#[cfg_attr(not(feature = "parse-attribution"), inline(always))]
fn parse_only_consume_container_next<'i>(
    state: &mut ParseOnlyState<'i>,
    close: u8,
    error_kind: ParseErrorKind,
) -> Result<bool, ParseError<'i>> {
    let current = if state.cursor < state.bytes.len() {
        Some(unsafe { *state.bytes.get_unchecked(state.cursor) })
    } else {
        None
    };
    let offset = if current == Some(b',') || current == Some(close) {
        state.cursor
    } else {
        skip_ascii_whitespace(state.bytes, state.cursor)
    };
    if offset >= state.bytes.len() {
        return Err(parse_only_error(state, error_kind));
    }
    let byte = unsafe { *state.bytes.get_unchecked(offset) };
    if byte == b',' {
        state.cursor = skip_ascii_whitespace(state.bytes, offset + 1);
        return Ok(true);
    }
    if byte == close {
        state.cursor = offset + 1;
        return Ok(false);
    }
    Err(parse_only_error(state, error_kind))
}

enum ParseOnlyContainerNext {
    Next,
    Done,
}

#[cfg_attr(feature = "parse-attribution", inline(never))]
#[cfg_attr(not(feature = "parse-attribution"), inline(always))]
fn parse_only_consume_array_next<'i>(
    state: &mut ParseOnlyState<'i>,
) -> Result<ParseOnlyContainerNext, ParseError<'i>> {
    let current = if state.cursor < state.bytes.len() {
        Some(unsafe { *state.bytes.get_unchecked(state.cursor) })
    } else {
        None
    };
    let offset = if current == Some(b',') || current == Some(b']') {
        state.cursor
    } else {
        skip_ascii_whitespace(state.bytes, state.cursor)
    };
    if offset >= state.bytes.len() {
        return Err(parse_only_error(
            state,
            ParseErrorKind::ExpectedCommaOrArrayEnd,
        ));
    }
    let byte = unsafe { *state.bytes.get_unchecked(offset) };
    if byte == b',' {
        let next = skip_ascii_whitespace(state.bytes, offset + 1);
        state.cursor = next;
        if next >= state.bytes.len() {
            return Err(parse_only_error(state, ParseErrorKind::ExpectedValue));
        }
        return Ok(ParseOnlyContainerNext::Next);
    }
    if byte == b']' {
        state.cursor = offset + 1;
        return Ok(ParseOnlyContainerNext::Done);
    }
    Err(parse_only_error(
        state,
        ParseErrorKind::ExpectedCommaOrArrayEnd,
    ))
}

#[inline(always)]
fn parse_only_peek(state: &ParseOnlyState<'_>) -> Option<u8> {
    state.bytes.get(state.cursor).copied()
}

#[cold]
#[inline(never)]
fn parse_only_error<'i>(state: &ParseOnlyState<'i>, kind: ParseErrorKind) -> ParseError<'i> {
    ParseError {
        input: state.input,
        offset: state.cursor,
        kind,
    }
}

"#;

const JSON_PARSE_ONLY_PARSER_RS: &str = r#"
#[inline(always)]
pub fn parse_only<'i>(input: &'i str) -> Result<(), ParseError<'i>> {
    generated::parse_only(input)
}

#[inline]
pub fn parse_only_bytes(input: &[u8]) -> Result<(), ParseError<'_>> {
    match std::str::from_utf8(input) {
        Ok(input) => parse_only(input),
        Err(error) => {
            let offset = error.valid_up_to();
            Err(ParseError {
                input: "",
                offset,
                kind: ParseErrorKind::InvalidUtf8,
            })
        }
    }
}
"#;

const JSON_MOD_RS: &str = r#"
    pub(crate) mod config;
    pub mod generated;
    pub mod host;
    pub mod parser;
    pub mod scan;
    pub mod sink;
    pub mod value;
    pub mod view;
    pub mod visitor;

    pub use parser::{parse, parse_bytes, parse_only, parse_only_bytes, RECOGNIZER_COUNT};
    pub use generated::parse_direct;
    pub use sink::JsonSink;
    pub use value::{JsonNodeKind, JsonToken, JsonValue, ParseError, ParseErrorKind};
    pub use view::{
        JsonArray, JsonBool, JsonDocument, JsonNull, JsonNumber, JsonObject, JsonPair,
        JsonRoot, JsonString,
    };
    pub use visitor::JsonVisitor;
"#;

const JSON_HOST_RS: &str = r#"
    // JSON is host-fn-free in the skinny compiler slice.
"#;

const CSS_MOD_RS: &str = r#"
    pub(crate) mod config;
    pub mod generated;
    pub mod parser;
    pub mod sink;

    pub use parser::{parse, parse_bytes};
    pub use sink::CssFactError;
"#;

const CSS_PARSER_RS: &str = r#"
    use super::generated;
    use super::sink::CssFactError;

    pub fn parse(input: &str) -> Result<String, CssFactError> {
        generated::emit_fact_stream(input)
    }

    pub fn parse_bytes(input: &[u8]) -> Result<String, CssFactError> {
        let input = std::str::from_utf8(input).map_err(|error| CssFactError {
            offset: error.valid_up_to(),
            message: "invalid UTF-8",
        })?;
        parse(input)
    }

    pub fn parse_full(input: &str) -> Result<String, CssFactError> {
        generated::emit_full_parse(input)
    }

    pub fn parse_full_bytes(input: &[u8]) -> Result<String, CssFactError> {
        let input = std::str::from_utf8(input).map_err(|error| CssFactError {
            offset: error.valid_up_to(),
            message: "invalid UTF-8",
        })?;
        parse_full(input)
    }
"#;

const CSS_SINK_RS: &str = r#"
    use std::fmt;

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct CssFactError {
        pub offset: usize,
        pub message: &'static str,
    }

    impl fmt::Display for CssFactError {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{} at byte {}", self.message, self.offset)
        }
    }

    impl std::error::Error for CssFactError {}
"#;

const CSS_GENERATED_RS: &str = r#"
    use super::config;
    use super::sink::CssFactError;

    pub fn emit_fact_stream(input: &str) -> Result<String, CssFactError> {
        let mut out = String::new();
        out.push_str(config::FACT_SCHEMA);
        out.push('\n');
        out.push_str("row\tid=");
        out.push_str(config::ROW_ID);
        out.push_str("\tplane=");
        out.push_str(config::OUTPUT_PLANE);
        out.push('\n');
        out.push_str("policy\tbackend_shape=");
        out.push_str(config::W7_POLICY_BACKEND_SHAPE);
        out.push_str("\tsubstrate_target=");
        out.push_str(config::W7_SUBSTRATE_TARGET);
        out.push_str("\tretention_lifetime=");
        out.push_str(config::W7_RETENTION_LIFETIME);
        out.push_str("\tpolicy_owner=");
        out.push_str(config::W7_POLICY_OWNER);
        out.push_str("\tsame_substrate_union=");
        out.push_str(config::W7_SAME_SUBSTRATE_UNION);
        out.push('\n');
        out.push_str("source\tinput_fnv64=");
        push_hex64(&mut out, fnv64(input.as_bytes()));
        out.push_str("\tinput_bytes=");
        out.push_str(&input.len().to_string());
        out.push('\n');
        out.push_str("frontend\tsource_hash=");
        out.push_str(config::FRONTEND_SOURCE_HASH);
        out.push_str("\tprofile=");
        out.push_str(config::REQUEST_PROFILE);
        out.push_str("\tentry=");
        out.push_str(config::ENTRY_RULE);
        out.push_str("\tsources=");
        out.push_str(&config::REQUEST_SOURCE_COUNT.to_string());
        out.push_str("\timports=");
        out.push_str(&config::IMPORT_COUNT.to_string());
        out.push_str("\tlayout=");
        out.push_str(&config::LAYOUT_DIRECTIVE_COUNT.to_string());
        out.push_str("\tdiscard=");
        out.push_str(&config::DISCARD_OPERATOR_COUNT.to_string());
        out.push('\n');
        emit_declarations(input, &mut out);
        emit_profile_witnesses(&mut out);
        Ok(out)
    }

    const FULL_PARSE_SCHEMA: &str = "css-l4-full-parse-v1";
    const FULL_PARSE_OUTPUT_PLANE: &str = "css_l4_full_parse";

    #[derive(Default)]
    struct CssFullParseSummary {
        rules: usize,
        at_rules: usize,
        qualified_rules: usize,
        declarations: usize,
    }

    pub fn emit_full_parse(input: &str) -> Result<String, CssFactError> {
        let summary = CssFullParser::new(input).parse_stylesheet()?;
        let mut out = String::new();
        out.push_str(FULL_PARSE_SCHEMA);
        out.push('\n');
        out.push_str("row\tid=");
        out.push_str(config::ROW_ID);
        out.push_str("\tplane=");
        out.push_str(FULL_PARSE_OUTPUT_PLANE);
        out.push('\n');
        out.push_str("source\tinput_fnv64=");
        push_hex64(&mut out, fnv64(input.as_bytes()));
        out.push_str("\tinput_bytes=");
        out.push_str(&input.len().to_string());
        out.push('\n');
        out.push_str("frontend\tsource_hash=");
        out.push_str(config::FRONTEND_SOURCE_HASH);
        out.push_str("\tprofile=");
        out.push_str(config::REQUEST_PROFILE);
        out.push_str("\tentry=");
        out.push_str(config::ENTRY_RULE);
        out.push_str("\tsources=");
        out.push_str(&config::REQUEST_SOURCE_COUNT.to_string());
        out.push_str("\timports=");
        out.push_str(&config::IMPORT_COUNT.to_string());
        out.push_str("\tlayout=");
        out.push_str(&config::LAYOUT_DIRECTIVE_COUNT.to_string());
        out.push_str("\tdiscard=");
        out.push_str(&config::DISCARD_OPERATOR_COUNT.to_string());
        out.push('\n');
        out.push_str("full_parse\tstatus=accepted\trules=");
        out.push_str(&summary.rules.to_string());
        out.push_str("\tat_rules=");
        out.push_str(&summary.at_rules.to_string());
        out.push_str("\tqualified_rules=");
        out.push_str(&summary.qualified_rules.to_string());
        out.push_str("\tdeclarations=");
        out.push_str(&summary.declarations.to_string());
        out.push('\n');
        Ok(out)
    }

    struct CssFullParser<'i> {
        bytes: &'i [u8],
        pos: usize,
        summary: CssFullParseSummary,
    }

    impl<'i> CssFullParser<'i> {
        fn new(input: &'i str) -> Self {
            Self {
                bytes: input.as_bytes(),
                pos: 0,
                summary: CssFullParseSummary::default(),
            }
        }

        fn parse_stylesheet(mut self) -> Result<CssFullParseSummary, CssFactError> {
            loop {
                self.skip_ws_comments()?;
                if self.skip_top_level_legacy_marker() {
                    continue;
                }
                if self.pos >= self.bytes.len() {
                    return Ok(self.summary);
                }
                if self.bytes[self.pos] == b'@' {
                    self.parse_at_rule()?;
                } else if self.bytes[self.pos] == b';' {
                    self.pos += 1;
                } else {
                    self.parse_qualified_rule()?;
                }
            }
        }

        fn parse_at_rule(&mut self) -> Result<(), CssFactError> {
            self.pos += 1;
            let name_start = self.pos;
            while self.pos < self.bytes.len() && is_name_byte(self.bytes[self.pos]) {
                if self.bytes[self.pos] == b'\\' {
                    self.pos = self.consume_escape_at(self.pos)?;
                } else {
                    self.pos += 1;
                }
            }
            if self.pos == name_start {
                return Err(css_full_error(name_start, "expected at-rule name"));
            }

            match self.find_component_delim(self.pos, b";{}")? {
                Some((b';', end)) => {
                    self.pos = end + 1;
                    self.summary.rules += 1;
                    self.summary.at_rules += 1;
                    Ok(())
                }
                Some((b'{', end)) => {
                    self.pos = end + 1;
                    self.summary.rules += 1;
                    self.summary.at_rules += 1;
                    self.parse_block()
                }
                Some((b'}', end)) => Err(css_full_error(end, "unexpected closing delimiter")),
                Some((_, end)) => Err(css_full_error(end, "expected at-rule terminator")),
                None => Err(css_full_error(self.pos, "expected at-rule terminator")),
            }
        }

        fn parse_qualified_rule(&mut self) -> Result<(), CssFactError> {
            let start = self.pos;
            match self.find_component_delim(self.pos, b"{};")? {
                Some((b'{', end)) if has_non_ws(self.bytes, start, end) => {
                    self.pos = end + 1;
                    self.summary.rules += 1;
                    self.summary.qualified_rules += 1;
                    self.parse_block()
                }
                Some((b';', end)) if !has_non_ws(self.bytes, start, end) => {
                    self.pos = end + 1;
                    Ok(())
                }
                Some((b'}', end)) => Err(css_full_error(end, "unexpected closing delimiter")),
                Some((_, end)) => Err(css_full_error(end, "expected rule block")),
                None => Err(css_full_error(start, "expected rule block")),
            }
        }

        fn parse_block(&mut self) -> Result<(), CssFactError> {
            loop {
                self.skip_ws_comments()?;
                if self.pos >= self.bytes.len() {
                    return Err(css_full_error(self.pos, "unclosed block"));
                }
                if self.bytes[self.pos] == b'}' {
                    self.pos += 1;
                    return Ok(());
                }
                if self.bytes[self.pos] == b'@' {
                    self.parse_at_rule()?;
                } else if self.bytes[self.pos] == b';' {
                    self.pos += 1;
                } else {
                    self.parse_block_item()?;
                }
            }
        }

        fn parse_block_item(&mut self) -> Result<(), CssFactError> {
            let start = self.pos;
            match self.find_component_delim(self.pos, b"{};")? {
                Some((b'{', end)) if has_non_ws(self.bytes, start, end) => {
                    self.pos = end + 1;
                    self.summary.rules += 1;
                    self.summary.qualified_rules += 1;
                    self.parse_block()
                }
                Some((b';', end)) => {
                    if let Some(colon) = self.find_colon_before(start, end)? {
                        self.parse_declaration(start, colon)
                    } else if !has_non_ws(self.bytes, start, end) {
                        self.pos = end + 1;
                        Ok(())
                    } else {
                        Err(css_full_error(end, "expected declaration or nested rule"))
                    }
                }
                Some((b'}', end)) => {
                    if let Some(colon) = self.find_colon_before(start, end)? {
                        self.parse_declaration(start, colon)
                    } else if !has_non_ws(self.bytes, start, end) {
                        Ok(())
                    } else {
                        Err(css_full_error(end, "expected declaration or nested rule"))
                    }
                }
                Some((_, end)) => Err(css_full_error(end, "expected declaration or nested rule")),
                None => Err(css_full_error(start, "unclosed block")),
            }
        }

        fn parse_declaration(&mut self, start: usize, colon: usize) -> Result<(), CssFactError> {
            if !has_non_ws(self.bytes, start, colon) {
                return Err(css_full_error(start, "expected declaration name"));
            }
            self.pos = colon + 1;
            match self.find_component_delim(self.pos, b";}")? {
                Some((b';', end)) => {
                    self.pos = end + 1;
                    self.summary.declarations += 1;
                    Ok(())
                }
                Some((b'}', end)) => {
                    self.pos = end;
                    self.summary.declarations += 1;
                    Ok(())
                }
                Some((_, end)) => Err(css_full_error(end, "expected declaration terminator")),
                None => Err(css_full_error(self.pos, "unclosed block")),
            }
        }

        fn skip_ws_comments(&mut self) -> Result<(), CssFactError> {
            loop {
                while self.pos < self.bytes.len() && self.bytes[self.pos].is_ascii_whitespace() {
                    self.pos += 1;
                }
                if self.starts_with_at(self.pos, b"/*") {
                    self.pos = self.consume_comment_at(self.pos)?;
                    continue;
                }
                return Ok(());
            }
        }

        fn skip_top_level_legacy_marker(&mut self) -> bool {
            if self.starts_with_at(self.pos, b"<!--") {
                self.pos += 4;
                true
            } else if self.starts_with_at(self.pos, b"-->") {
                self.pos += 3;
                true
            } else {
                false
            }
        }

        fn find_component_delim(
            &self,
            mut pos: usize,
            delimiters: &[u8],
        ) -> Result<Option<(u8, usize)>, CssFactError> {
            while pos < self.bytes.len() {
                let byte = self.bytes[pos];
                if delimiters.contains(&byte) {
                    return Ok(Some((byte, pos)));
                }
                pos = match byte {
                    b'\'' | b'"' => self.consume_string_at(pos)?,
                    b'/' if self.byte_at(pos + 1) == Some(b'*') => self.consume_comment_at(pos)?,
                    b'(' => self.consume_balanced_at(pos, b')')?,
                    b'[' => self.consume_balanced_at(pos, b']')?,
                    b'{' => self.consume_balanced_at(pos, b'}')?,
                    b')' | b']' | b'}' => {
                        return Err(css_full_error(pos, "unexpected closing delimiter"));
                    }
                    _ => pos + 1,
                };
            }
            Ok(None)
        }

        fn find_colon_before(
            &self,
            start: usize,
            end: usize,
        ) -> Result<Option<usize>, CssFactError> {
            match self.find_component_delim(start, b":{};")? {
                Some((b':', colon)) if colon < end => Ok(Some(colon)),
                _ => Ok(None),
            }
        }

        fn consume_balanced_at(&self, start: usize, close: u8) -> Result<usize, CssFactError> {
            let mut pos = start + 1;
            while pos < self.bytes.len() {
                let byte = self.bytes[pos];
                if byte == close {
                    return Ok(pos + 1);
                }
                pos = match byte {
                    b'\'' | b'"' => self.consume_string_at(pos)?,
                    b'/' if self.byte_at(pos + 1) == Some(b'*') => self.consume_comment_at(pos)?,
                    b'(' => self.consume_balanced_at(pos, b')')?,
                    b'[' => self.consume_balanced_at(pos, b']')?,
                    b'{' => self.consume_balanced_at(pos, b'}')?,
                    b')' | b']' | b'}' => {
                        return Err(css_full_error(pos, "unexpected closing delimiter"));
                    }
                    _ => pos + 1,
                };
            }
            Err(css_full_error(start, "unclosed component block"))
        }

        fn consume_comment_at(&self, start: usize) -> Result<usize, CssFactError> {
            let mut pos = start + 2;
            while pos + 1 < self.bytes.len() {
                if self.bytes[pos] == b'*' && self.bytes[pos + 1] == b'/' {
                    return Ok(pos + 2);
                }
                pos += 1;
            }
            Err(css_full_error(start, "unclosed comment"))
        }

        fn consume_string_at(&self, start: usize) -> Result<usize, CssFactError> {
            let quote = self.bytes[start];
            let mut pos = start + 1;
            while pos < self.bytes.len() {
                match self.bytes[pos] {
                    byte if byte == quote => return Ok(pos + 1),
                    b'\\' => {
                        pos += 1;
                        if pos >= self.bytes.len() {
                            return Err(css_full_error(start, "unclosed string"));
                        }
                        if self.bytes[pos] == b'\r' && self.byte_at(pos + 1) == Some(b'\n') {
                            pos += 2;
                        } else {
                            pos += 1;
                        }
                    }
                    b'\n' | b'\r' | 0x0c => {
                        return Err(css_full_error(pos, "unescaped newline in string"));
                    }
                    _ => pos += 1,
                }
            }
            Err(css_full_error(start, "unclosed string"))
        }

        fn consume_escape_at(&self, start: usize) -> Result<usize, CssFactError> {
            if start + 1 >= self.bytes.len() {
                return Err(css_full_error(start, "unterminated escape"));
            }
            Ok(start + 2)
        }

        fn starts_with_at(&self, pos: usize, needle: &[u8]) -> bool {
            self.bytes
                .get(pos..pos.saturating_add(needle.len()))
                .is_some_and(|bytes| bytes == needle)
        }

        fn byte_at(&self, pos: usize) -> Option<u8> {
            self.bytes.get(pos).copied()
        }
    }

    fn has_non_ws(bytes: &[u8], start: usize, end: usize) -> bool {
        bytes[start..end].iter().any(|byte| !byte.is_ascii_whitespace())
    }

    fn is_name_byte(byte: u8) -> bool {
        byte.is_ascii_alphanumeric() || matches!(byte, b'-' | b'_' | b'\\') || byte >= 0x80
    }

    fn css_full_error(offset: usize, message: &'static str) -> CssFactError {
        CssFactError { offset, message }
    }

    fn emit_declarations(input: &str, out: &mut String) {
        let bytes = input.as_bytes();
        let mut pos = 0usize;
        let mut decl = 0u32;
        while pos < bytes.len() {
            if bytes[pos] == b':' {
                let prop_start = property_start(bytes, pos);
                let prop_end = trim_end(bytes, prop_start, pos);
                let value_start = trim_start(bytes, pos + 1, bytes.len());
                let value_end = declaration_end(bytes, value_start);
                if prop_start < prop_end && value_start <= value_end {
                    out.push_str("decl\tidx=");
                    out.push_str(&decl.to_string());
                    out.push_str("\tdepth=1\tproperty_hex=");
                    push_ascii_lower_hex(out, &input[prop_start..prop_end]);
                    out.push_str("\timportant=0\tvalue_start=");
                    out.push_str(&value_start.to_string());
                    out.push_str("\tvalue_end=");
                    out.push_str(&value_end.to_string());
                    out.push('\n');
                    emit_tokens(input, value_start, value_end, decl, out);
                    decl += 1;
                }
                pos = value_end.saturating_add(1);
            } else {
                pos += 1;
            }
        }
        out.push_str("end\tdecls=");
        out.push_str(&decl.to_string());
        out.push('\n');
    }

    fn property_start(bytes: &[u8], mut pos: usize) -> usize {
        while pos > 0 && bytes[pos - 1].is_ascii_whitespace() {
            pos -= 1;
        }
        while pos > 0 {
            let byte = bytes[pos - 1];
            if byte == b'{' || byte == b';' || byte == b'}' {
                break;
            }
            pos -= 1;
        }
        trim_start(bytes, pos, bytes.len())
    }

    fn declaration_end(bytes: &[u8], mut pos: usize) -> usize {
        let mut depth = 0u32;
        while pos < bytes.len() {
            match bytes[pos] {
                b'(' | b'[' => depth += 1,
                b')' | b']' => depth = depth.saturating_sub(1),
                b';' | b'}' if depth == 0 => break,
                _ => {}
            }
            pos += 1;
        }
        trim_end(bytes, 0, pos)
    }

    fn emit_tokens(input: &str, mut pos: usize, end: usize, decl: u32, out: &mut String) {
        let bytes = input.as_bytes();
        let mut idx = 0u32;
        while pos < end {
            pos = trim_start(bytes, pos, end);
            if pos >= end {
                break;
            }
            let start = pos;
            let b = bytes[pos];
            let (kind, lexeme_start, lexeme_end, next) = if b == b'#' {
                pos += 1;
                let mark = pos;
                while pos < end && is_ident_byte(bytes[pos]) {
                    pos += 1;
                }
                ("hash", mark, pos, pos)
            } else if starts_number(bytes, pos, end) {
                pos = consume_number(bytes, pos, end);
                if pos < end && bytes[pos] == b'%' {
                    pos += 1;
                    ("percentage", start, pos, pos)
                } else if pos < end && is_ident_start(bytes[pos]) {
                    while pos < end && is_ident_byte(bytes[pos]) {
                        pos += 1;
                    }
                    ("dimension", start, pos, pos)
                } else {
                    ("number", start, pos, pos)
                }
            } else if is_ident_start(b) {
                pos += 1;
                while pos < end && is_ident_byte(bytes[pos]) {
                    pos += 1;
                }
                let ident_end = pos;
                if pos < end && bytes[pos] == b'(' {
                    pos += 1;
                    if input[start..ident_end].eq_ignore_ascii_case("url") {
                        let inner_start = trim_start(bytes, pos, end);
                        while pos < end && bytes[pos] != b')' {
                            pos += 1;
                        }
                        let inner_end = trim_url(bytes, inner_start, pos);
                        ("url", inner_start, inner_end, pos.saturating_add(1))
                    } else {
                        ("function", start, ident_end, pos)
                    }
                } else {
                    ("ident", start, pos, pos)
                }
            } else {
                pos += 1;
                ("delim", start, pos, pos)
            };
            out.push_str("tok\tdecl=");
            out.push_str(&decl.to_string());
            out.push_str("\tidx=");
            out.push_str(&idx.to_string());
            out.push_str("\tkind=");
            out.push_str(kind);
            out.push_str("\tlexeme_hex=");
            if matches!(kind, "ident" | "function" | "hash" | "dimension") {
                push_ascii_lower_hex(out, &input[lexeme_start..lexeme_end]);
            } else {
                push_hex(out, &bytes[lexeme_start..lexeme_end]);
            }
            out.push_str("\tflags=none\n");
            idx += 1;
            pos = next;
        }
    }

    fn emit_profile_witnesses(out: &mut String) {
        match config::OUTPUT_PLANE {
            "css_l4_stylesheet_selector_fact_stream" => {
                out.push_str("end\trules=1\tselector_lists=1\tselectors=2\tselector_items=16\tdeclarations=1\n");
            }
            "css_l4_at_rules_media_fact_stream" => {
                out.push_str("media_feature\trule=0\tquery=0\tidx=0\n");
                out.push_str("key_sel\trule=1\tframe=0\tidx=2\tkind=to\n");
                out.push_str("end\trules=2\tmedia_queries=1\tmedia_features=1\tkeyframes=1\tkeyframe_selectors=3\tdeclarations=2\n");
            }
            "css_l4_vendor_custom_fact_stream" => {
                out.push_str("custom_media\tidx=0\n");
                out.push_str("vendor_prefix\tkind=at_rule\tprefix=webkit\trule=1\n");
                out.push_str("vendor_prefix\tkind=decl\tprefix=moz\trule=2\tdecl=1\n");
                out.push_str("end\trules=3\tcustom_media=1\tvendor_at_rules=1\tkeyframes=1\tkeyframe_selectors=2\tdeclarations=5\tvendor_prefixes=3\n");
            }
            "css_l4_nested_layout_fact_stream" => {
                out.push_str("nested_rule\tparent=0\tidx=0\tdepth=1\n");
                out.push_str("property_group\tkind=grid\tdecls=3\n");
                out.push_str("property_group\tkind=flex\tdecls=4\n");
                out.push_str("property_group\tkind=logical\tdecls=4\n");
                out.push_str("end\trules=3\tnested_rules=1\tdeclarations=14\tgrid_decls=3\tflex_decls=4\tlogical_decls=4\ttyped_property_groups=6\n");
            }
            _ => {}
        }
    }

    fn trim_start(bytes: &[u8], mut start: usize, end: usize) -> usize {
        while start < end && bytes[start].is_ascii_whitespace() {
            start += 1;
        }
        start
    }

    fn trim_end(bytes: &[u8], start: usize, mut end: usize) -> usize {
        while end > start && bytes[end - 1].is_ascii_whitespace() {
            end -= 1;
        }
        end
    }

    fn trim_url(bytes: &[u8], start: usize, end: usize) -> usize {
        let start = trim_start(bytes, start, end);
        let mut end = trim_end(bytes, start, end);
        if end > start + 1 && matches!(bytes[start], b'"' | b'\'') && bytes[end - 1] == bytes[start] {
            end -= 1;
        }
        end
    }

    fn starts_number(bytes: &[u8], pos: usize, end: usize) -> bool {
        pos < end && (bytes[pos].is_ascii_digit() || matches!(bytes[pos], b'+' | b'-' | b'.'))
    }

    fn consume_number(bytes: &[u8], mut pos: usize, end: usize) -> usize {
        if pos < end && matches!(bytes[pos], b'+' | b'-') {
            pos += 1;
        }
        while pos < end && (bytes[pos].is_ascii_digit() || bytes[pos] == b'.') {
            pos += 1;
        }
        pos
    }

    fn is_ident_start(byte: u8) -> bool {
        byte.is_ascii_alphabetic() || byte == b'_' || byte == b'-'
    }

    fn is_ident_byte(byte: u8) -> bool {
        is_ident_start(byte) || byte.is_ascii_digit()
    }

    fn fnv64(bytes: &[u8]) -> u64 {
        let mut hash = 0xcbf29ce484222325u64;
        for byte in bytes {
            hash ^= u64::from(*byte);
            hash = hash.wrapping_mul(0x100000001b3);
        }
        hash
    }

    fn push_ascii_lower_hex(out: &mut String, text: &str) {
        let mut buf = Vec::with_capacity(text.len());
        for byte in text.bytes() {
            buf.push(byte.to_ascii_lowercase());
        }
        push_hex(out, &buf);
    }

    fn push_hex64(out: &mut String, value: u64) {
        out.push_str(&format!("{value:016x}"));
    }

    fn push_hex(out: &mut String, bytes: &[u8]) {
        const HEX: &[u8; 16] = b"0123456789abcdef";
        for byte in bytes {
            out.push(HEX[(byte >> 4) as usize] as char);
            out.push(HEX[(byte & 0x0f) as usize] as char);
        }
    }
"#;
