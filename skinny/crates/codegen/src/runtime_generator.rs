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
    // SK-V17 W1: the fact-stream policy-plane constants (FACT_SCHEMA,
    // OUTPUT_PLANE, the W7_* fact-stream policy triad) are RETIRED with the
    // fact-stream emitter. The generated provider routes into the tape; the
    // diagnostic `emit_full_parse` rollup consumes only the request-identity
    // constants below.
    format!(
        "{header}\npub(crate) const ROW_ID: &str = {row_id:?};\n\
         pub(crate) const REQUEST_PROFILE: &str = {profile:?};\n\
         pub(crate) const ENTRY_RULE: &str = {entry:?};\n\
         pub(crate) const FRONTEND_SOURCE_HASH: &str = {source_hash:?};\n\
         pub(crate) const REQUEST_SOURCE_COUNT: usize = {source_count};\n\
         pub(crate) const IMPORT_COUNT: usize = {import_count};\n\
         pub(crate) const LAYOUT_DIRECTIVE_COUNT: usize = {layout_count};\n\
         pub(crate) const DISCARD_OPERATOR_COUNT: usize = {discard_count};\n",
        header = crate::GENERATED_HEADER,
        row_id = labels.row_id,
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

    pub use generated::{
        CssDeclaration, CssDocument, CssNode, CssNodeKind, CssRichSummary, CssRule, CssSummary,
        CssTypedNode, CssTypedValue,
    };
    pub use parser::{parse, parse_bytes, rich_summary, rich_summary_bytes, summary, summary_bytes};
    pub use sink::CssFactError;
"#;

const CSS_PARSER_RS: &str = r#"
    use super::generated::{self, CssDocument, CssRichSummary, CssSummary};
    use super::sink::CssFactError;

    /// Track-1 admission entry: recognize the stylesheet directly into the
    /// EXISTING skinny offset tape and return the lazy tape-backed document.
    /// The retired fact-stream `String` plane is gone from this path.
    pub fn parse(input: &str) -> Result<CssDocument<'_>, CssFactError> {
        generated::parse_into_tape(input)
    }

    pub fn parse_bytes(input: &[u8]) -> Result<CssDocument<'_>, CssFactError> {
        let input = std::str::from_utf8(input).map_err(|error| CssFactError {
            offset: error.valid_up_to(),
            message: "invalid UTF-8",
        })?;
        parse(input)
    }

    /// The lazy 4-field structural summary projected from the tape — the typed
    /// product the equality oracle checks (kind from source byte, zero payload).
    pub fn summary(input: &str) -> Result<CssSummary, CssFactError> {
        Ok(parse(input)?.summary())
    }

    pub fn summary_bytes(input: &[u8]) -> Result<CssSummary, CssFactError> {
        Ok(parse_bytes(input)?.summary())
    }

    /// The RICH typed-CSSOM summary projected lazily from the tape — the fair
    /// full-materialization product compared against lightningcss (selectors +
    /// typed value-node counts atop the 4 structural fields).
    pub fn rich_summary(input: &str) -> Result<CssRichSummary, CssFactError> {
        Ok(parse(input)?.rich_summary())
    }

    pub fn rich_summary_bytes(input: &[u8]) -> Result<CssRichSummary, CssFactError> {
        Ok(parse_bytes(input)?.rich_summary())
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

// SK-V17 W1 (PRUNE) — TAPE-ROUTED CSS L4 TRACK-1 PROVIDER.
//
// `CSS_GENERATED_RS` is the grammar-recognizer CSS provider. It no longer
// materializes a fact-stream `String` (`emit_fact_stream`, RETIRED): the
// `CssFullParser` recognizer appends every structural event into the EXISTING
// skinny offset tape (`crate::tape::TapeBuilder::push_plain_offset`), tagging
// at-rule openings with a single `BackendRule` branch-tag flag bit
// (`OffsetFlags::GRAMMAR_BIT0`). The typed CSS summary is then reconstructed
// LAZILY from the sealed `Tape` via `ValueRef` cursor reads — the node kind is
// recovered from the SOURCE BYTE at each offset (no stored tag, no eager tree,
// zero `PayloadArena` writes), isomorphic to JSON's `value_from_ref`
// (`grammars/json/value.rs`). `parse` returns a tape-backed `CssDocument`; the
// String fact-stream plane is GONE from the admission path.
//
// Lock 1: exactly one substrate (the existing `Tape`/`ValueRef`/`TapeBuilder`);
// no second cursor, no sidecar event vector. Lock 14: the only per-grammar datum
// is which positions are pushed (derived from the recognizer / `BackendRule`
// shape), never a hand-curated per-rule routing table.
const CSS_GENERATED_RS: &str = r#"
    use crate::tape::{OffsetFlags, Tape, TapeBuilder, ValueRef};
    use super::config;
    use super::sink::CssFactError;

    /// Structural-role flag: set on a rule cursor whose opener is an at-rule
    /// (`@media`, `@keyframes`, ...). A clear flag marks a qualified rule. This
    /// is a `BackendRule` branch-tag projection (at-rule branch vs qualified
    /// branch), stored in the EXISTING sparse `flag_cursors`/`flag_values` pair
    /// — paid only where non-zero — not a widened per-position record.
    const AT_RULE_FLAG: u8 = OffsetFlags::GRAMMAR_BIT0;

    /// The structural kind a tape cursor projects to, recovered lazily from the
    /// source byte at the cursor's offset plus the sparse at-rule flag. There is
    /// no stored tag: this is the CSS analogue of `JsonNodeKind::at_cursor`.
    #[derive(Copy, Clone, Eq, PartialEq, Debug)]
    pub enum CssNodeKind {
        QualifiedRule,
        AtRule,
        Declaration,
    }

    impl CssNodeKind {
        #[inline]
        pub fn at_cursor(tape: &Tape<'_>, cursor: u32) -> Self {
            let offset = tape
                .offset_at(cursor)
                .expect("CSS cursor must point into the sealed offset tape");
            match tape.source()[offset] {
                b':' => CssNodeKind::Declaration,
                _ if tape
                    .flags_at(cursor)
                    .is_some_and(|flags| flags.contains(AT_RULE_FLAG)) =>
                {
                    CssNodeKind::AtRule
                }
                _ => CssNodeKind::QualifiedRule,
            }
        }
    }

    /// A lazy view over one structural CSS node on the tape. 8 bytes of cursor;
    /// the kind is decoded on demand, never stored.
    #[derive(Copy, Clone)]
    pub struct CssNode<'doc, 'input> {
        node: ValueRef<'doc, 'input>,
    }

    impl<'doc, 'input> CssNode<'doc, 'input> {
        #[inline]
        pub fn kind(&self) -> CssNodeKind {
            CssNodeKind::at_cursor(self.node.tape(), self.node.cursor())
        }

        #[inline]
        pub fn offset(&self) -> usize {
            self.node.offset()
        }

        /// The left boundary of this node's prelude/property: the byte after the
        /// nearest structural delimiter (`{`, `}`, `;`) at or before `end`,
        /// floored by the previous tape node's offset. The previous offset bounds
        /// the backward scan to this node's own component (read from the EXISTING
        /// `offsets` vector); the delimiter scan resolves the exact left edge,
        /// lazily, with no retained span.
        #[inline]
        fn left_boundary(&self, end: usize) -> usize {
            let cursor = self.node.cursor();
            let floor = if cursor == 0 {
                0
            } else {
                self.node
                    .tape()
                    .offset_at(cursor - 1)
                    .map_or(0, |offset| offset + 1)
            };
            let source = self.node.tape().source();
            let mut pos = end.min(source.len());
            while pos > floor {
                match source[pos - 1] {
                    b'{' | b'}' | b';' => break,
                    _ => pos -= 1,
                }
            }
            pos
        }

        /// The rich typed view of this node: the `BackendRule` branch tag (kind)
        /// dispatches to the typed projection — at-rule / qualified-rule (carrying
        /// its lazy selector prelude) or declaration (carrying its lazy property +
        /// typed value). This is the CSS analogue of JSON's `value_from_ref`: the
        /// branch is recovered from `(source byte, at-rule flag)` and the typed
        /// payload is a lazy `ValueRef` span, materializing nothing.
        #[inline]
        pub fn value(&self) -> CssTypedNode<'doc, 'input> {
            match self.kind() {
                CssNodeKind::AtRule => CssTypedNode::Rule(CssRule {
                    node: *self,
                    at_rule: true,
                }),
                CssNodeKind::QualifiedRule => CssTypedNode::Rule(CssRule {
                    node: *self,
                    at_rule: false,
                }),
                CssNodeKind::Declaration => {
                    CssTypedNode::Declaration(CssDeclaration { node: *self })
                }
            }
        }
    }

    /// The lazily-projected rich CSS node: one `BackendRule` branch per variant,
    /// each a thin `ValueRef`-cursor view (8 bytes of cursor; no eager subtree).
    #[derive(Copy, Clone)]
    pub enum CssTypedNode<'doc, 'input> {
        Rule(CssRule<'doc, 'input>),
        Declaration(CssDeclaration<'doc, 'input>),
    }

    /// A lazy view over one CSS rule (at-rule or qualified). The selector prelude
    /// is the source slice `(prev_node_end .. brace_offset)`, recovered on demand
    /// from the neighbouring tape offsets — never stored.
    #[derive(Copy, Clone)]
    pub struct CssRule<'doc, 'input> {
        node: CssNode<'doc, 'input>,
        at_rule: bool,
    }

    impl<'doc, 'input> CssRule<'doc, 'input> {
        #[inline]
        pub fn is_at_rule(&self) -> bool {
            self.at_rule
        }

        /// The prelude span: for a qualified rule this is the selector list, for
        /// an at-rule it is the `@name <prelude>`. Lazily recovered from the tape.
        #[inline]
        pub fn prelude(&self) -> &'input [u8] {
            let source = self.node.node.tape().source();
            let end = self.node.offset();
            let start = self.node.left_boundary(end);
            trim_ascii(&source[start..end])
        }

        /// The selector-list entries of a qualified rule, split on top-level
        /// commas (the cssparser selector-count definition). At-rules yield zero
        /// selectors. Lazy: iterates the prelude bytes, allocating nothing.
        #[inline]
        pub fn selector_count(&self) -> usize {
            if self.at_rule {
                return 0;
            }
            let prelude = self.prelude();
            if prelude.is_empty() {
                return 0;
            }
            1 + count_top_level_commas(prelude)
        }
    }

    /// A lazy view over one CSS declaration. The property is the slice
    /// `(prev_node_end .. colon)`, the value is `(colon+1 .. terminator)`; both are
    /// recovered on demand from the recorded colon offset, materializing nothing.
    #[derive(Copy, Clone)]
    pub struct CssDeclaration<'doc, 'input> {
        node: CssNode<'doc, 'input>,
    }

    impl<'doc, 'input> CssDeclaration<'doc, 'input> {
        /// The declaration colon offset (the recorded tape position).
        #[inline]
        fn colon(&self) -> usize {
            self.node.offset()
        }

        /// The property name span `(prev_node_end .. colon)`, lazily recovered.
        #[inline]
        pub fn property(&self) -> &'input [u8] {
            let source = self.node.node.tape().source();
            let colon = self.colon();
            let start = self.node.left_boundary(colon);
            trim_ascii(&source[start..colon])
        }

        /// The value span `(colon+1 .. terminator)`, recovered by a lazy forward
        /// scan over balanced components from the colon — the same balanced scan
        /// the recognizer used, never a retained span.
        #[inline]
        pub fn value_span(&self) -> &'input [u8] {
            let source = self.node.node.tape().source();
            let colon = self.colon();
            let start = (colon + 1).min(source.len());
            let end = scan_value_end(source, start);
            trim_ascii(&source[start..end])
        }

        /// The typed value-node kind of the declaration's leading significant
        /// token, decoded from the source byte at the value head (no stored tag —
        /// the CSS analogue of `JsonNodeKind::at_cursor`).
        #[inline]
        pub fn typed_value(&self) -> CssTypedValue {
            CssTypedValue::classify(self.value_span())
        }
    }

    /// The typed value-node model the tape supports: the leading-token category of
    /// a declaration value, recovered lazily from the source bytes. Each variant is
    /// a `BackendRule` value-branch tag projection, decoded from the value head.
    #[derive(Copy, Clone, Eq, PartialEq, Debug)]
    pub enum CssTypedValue {
        /// `<number><unit>` or `<number>%` (e.g. `10px`, `50%`, `1.2rem`).
        Dimension,
        /// A bare numeric value (e.g. `0`, `1.5`, `.5`).
        Number,
        /// A `#rrggbb` / `#rgb` hash colour.
        Color,
        /// A `name(...)` functional value (e.g. `rgb(...)`, `calc(...)`).
        Function,
        /// A keyword / identifier-led value (e.g. `red`, `none`, `var`).
        Keyword,
        /// A quoted string, url, or otherwise non-categorised value head.
        Other,
    }

    impl CssTypedValue {
        /// Classify a value span by its leading significant byte and shape — the
        /// lazy typed-value decode (no stored tag, no payload).
        #[inline]
        pub fn classify(value: &[u8]) -> CssTypedValue {
            let value = trim_ascii(value);
            let Some(&head) = value.first() else {
                return CssTypedValue::Other;
            };
            match head {
                b'#' => CssTypedValue::Color,
                b'"' | b'\'' => CssTypedValue::Other,
                b'+' | b'-' | b'.' | b'0'..=b'9' => {
                    if number_has_unit(value) {
                        CssTypedValue::Dimension
                    } else {
                        CssTypedValue::Number
                    }
                }
                b'a'..=b'z' | b'A'..=b'Z' | b'_' | 0x80..=0xff | b'\\' => {
                    if leading_ident_is_function(value) {
                        CssTypedValue::Function
                    } else {
                        CssTypedValue::Keyword
                    }
                }
                _ => CssTypedValue::Other,
            }
        }
    }

    /// The sealed tape-backed CSS document: the retained product of a Track-1
    /// parse. Holds exactly the existing `Tape` — no second substrate.
    pub struct CssDocument<'input> {
        tape: Tape<'input>,
    }

    impl<'input> CssDocument<'input> {
        #[inline]
        pub fn tape(&self) -> &Tape<'input> {
            &self.tape
        }

        /// Iterate the structural nodes as lazy `ValueRef` views.
        pub fn nodes(&self) -> impl Iterator<Item = CssNode<'_, 'input>> + '_ {
            (0..self.tape.offsets().len() as u32).map(move |cursor| CssNode {
                node: ValueRef::new(&self.tape, cursor),
            })
        }

        /// Reconstruct the 4-field structural summary LAZILY from the tape —
        /// every count re-derived from `(source byte, at-rule flag)` via
        /// `ValueRef` reads, materializing nothing. This is the same summary the
        /// recognizer counts inline; equality is by construction.
        pub fn summary(&self) -> CssSummary {
            let mut summary = CssSummary::default();
            for node in self.nodes() {
                match node.kind() {
                    CssNodeKind::AtRule => {
                        summary.rules += 1;
                        summary.at_rules += 1;
                    }
                    CssNodeKind::QualifiedRule => {
                        summary.rules += 1;
                        summary.qualified_rules += 1;
                    }
                    CssNodeKind::Declaration => summary.declarations += 1,
                }
            }
            summary
        }

        /// Reconstruct the RICH typed CSSOM summary LAZILY from the tape — the
        /// fair full-materialization comparison vs lightningcss. Beyond the 4-field
        /// structural counts it materializes, per node, the rich typed values by
        /// `ValueRef` view: selector-list entries per qualified rule, and the typed
        /// value-node category of every declaration value head
        /// (dimension/number/color/function/keyword). Every field is re-derived
        /// from `(source, offset)` via lazy spans, writing nothing to the payload
        /// arena (preserve-rich-ast: rich, lazy, not eager, not flattened).
        pub fn rich_summary(&self) -> CssRichSummary {
            let mut summary = CssRichSummary::default();
            for node in self.nodes() {
                match node.value() {
                    CssTypedNode::Rule(rule) if rule.is_at_rule() => {
                        summary.rules += 1;
                        summary.at_rules += 1;
                    }
                    CssTypedNode::Rule(rule) => {
                        summary.rules += 1;
                        summary.qualified_rules += 1;
                        summary.selectors += rule.selector_count();
                    }
                    CssTypedNode::Declaration(decl) => {
                        summary.declarations += 1;
                        match decl.typed_value() {
                            CssTypedValue::Dimension => summary.dimensions += 1,
                            CssTypedValue::Number => summary.numbers += 1,
                            CssTypedValue::Color => summary.colors += 1,
                            CssTypedValue::Function => summary.functions += 1,
                            CssTypedValue::Keyword | CssTypedValue::Other => {}
                        }
                    }
                }
            }
            summary
        }
    }

    #[derive(Default, Copy, Clone, Eq, PartialEq, Debug)]
    pub struct CssSummary {
        pub rules: usize,
        pub at_rules: usize,
        pub qualified_rules: usize,
        pub declarations: usize,
    }

    /// The rich typed-CSSOM summary: the 4 structural fields plus the lazily
    /// materialized rich value plane (selectors + typed value-node counts). This
    /// is the population-parity surface the W2 equality oracle checks against the
    /// independent cssparser reference.
    #[derive(Default, Copy, Clone, Eq, PartialEq, Debug)]
    pub struct CssRichSummary {
        pub rules: usize,
        pub at_rules: usize,
        pub qualified_rules: usize,
        pub declarations: usize,
        pub selectors: usize,
        pub dimensions: usize,
        pub numbers: usize,
        pub colors: usize,
        pub functions: usize,
    }

    /// Track-1 entry: recognize the stylesheet and emit every structural event
    /// into the existing offset tape, returning the lazy tape-backed document.
    pub fn parse_into_tape(input: &str) -> Result<CssDocument<'_>, CssFactError> {
        let bytes = input.as_bytes();
        let mut builder = TapeBuilder::new(bytes, structural_reserve(bytes.len()));
        CssFullParser::new(input, &mut builder).parse_stylesheet()?;
        Ok(CssDocument {
            tape: builder.finish(),
        })
    }

    /// Conservative byte-proportional structural capacity bound (L7, scalar
    /// reserve until the W3 NEON scan count lands). Never a per-corpus literal.
    #[inline]
    fn structural_reserve(byte_len: usize) -> usize {
        (byte_len / 8).max(16)
    }

    const FULL_PARSE_SCHEMA: &str = "css-l4-full-parse-v1";
    const FULL_PARSE_OUTPUT_PLANE: &str = "css_l4_full_parse";

    pub fn emit_full_parse(input: &str) -> Result<String, CssFactError> {
        // Diagnostic-only roll-up: derive the summary LAZILY from the same tape
        // the admission path builds, proving tape-vs-recognizer equality by
        // construction (the tape is the single source of the counts).
        let summary = parse_into_tape(input)?.summary();
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

    struct CssFullParser<'i, 'b, 'input> {
        bytes: &'i [u8],
        pos: usize,
        tape: &'b mut TapeBuilder<'input>,
    }

    impl<'i, 'b, 'input> CssFullParser<'i, 'b, 'input> {
        fn new(input: &'i str, tape: &'b mut TapeBuilder<'input>) -> Self {
            Self {
                bytes: input.as_bytes(),
                pos: 0,
                tape,
            }
        }

        /// Append a rule opening at `offset` into the existing offset tape; the
        /// at-rule branch sets the sparse `GRAMMAR_BIT0` flag, the qualified
        /// branch leaves it clear (a `BackendRule` branch-tag projection).
        #[inline]
        fn push_rule(&mut self, offset: usize, at_rule: bool) {
            let flags = if at_rule {
                OffsetFlags::NONE.with(AT_RULE_FLAG)
            } else {
                OffsetFlags::NONE
            };
            self.tape.push_offset(offset, flags);
        }

        /// Append a declaration at its `:` offset; the source byte (`:`) is the
        /// lazy kind tag — no flag, no payload.
        #[inline]
        fn push_declaration(&mut self, colon: usize) {
            self.tape.push_plain_offset(colon);
        }

        fn parse_stylesheet(mut self) -> Result<(), CssFactError> {
            loop {
                self.skip_ws_comments()?;
                if self.skip_top_level_legacy_marker() {
                    continue;
                }
                if self.pos >= self.bytes.len() {
                    return Ok(());
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
                    self.push_rule(end, true);
                    self.pos = end + 1;
                    Ok(())
                }
                Some((b'{', end)) => {
                    self.push_rule(end, true);
                    self.pos = end + 1;
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
                    self.push_rule(end, false);
                    self.pos = end + 1;
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
                    self.push_rule(end, false);
                    self.pos = end + 1;
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
                    self.push_declaration(colon);
                    self.pos = end + 1;
                    Ok(())
                }
                Some((b'}', end)) => {
                    self.push_declaration(colon);
                    self.pos = end;
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
            } else if self.skip_charset_directive() {
                true
            } else {
                false
            }
        }

        /// `@charset "..."` is a tokenizer directive, not a counted at-rule (the
        /// CSS spec consumes it pre-tokenization; cssparser/lightningcss surface
        /// no rule for it). At the top level it is skipped here so the structural
        /// counts match the reference CSSOM — a top-level directive skip, the same
        /// class as the CDO/CDC markers above, not a per-rule routing table.
        #[inline]
        fn skip_charset_directive(&mut self) -> bool {
            if !self.starts_with_at(self.pos, b"@charset") {
                return false;
            }
            let after = self.pos + b"@charset".len();
            // Only a genuine `@charset` directive (terminated by `;`), never an
            // identifier whose prefix is `charset` (e.g. `@charset-like`).
            match self.bytes.get(after) {
                Some(byte) if byte.is_ascii_whitespace() => {}
                _ => return false,
            }
            let mut pos = after;
            while pos < self.bytes.len() && self.bytes[pos] != b';' && self.bytes[pos] != b'{' {
                pos += 1;
            }
            if pos < self.bytes.len() && self.bytes[pos] == b';' {
                self.pos = pos + 1;
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

    /// Trim leading/trailing ASCII whitespace from a span — the lazy span readers
    /// normalise prelude/property/value boundaries without allocating.
    #[inline]
    fn trim_ascii(mut bytes: &[u8]) -> &[u8] {
        while let [first, rest @ ..] = bytes {
            if first.is_ascii_whitespace() {
                bytes = rest;
            } else {
                break;
            }
        }
        while let [rest @ .., last] = bytes {
            if last.is_ascii_whitespace() {
                bytes = rest;
            } else {
                break;
            }
        }
        bytes
    }

    /// Count top-level commas in a selector prelude (commas not nested inside
    /// `()`/`[]`/`{}` or strings) — the selector-list separator count.
    #[inline]
    fn count_top_level_commas(bytes: &[u8]) -> usize {
        let mut depth: i32 = 0;
        let mut commas = 0usize;
        let mut pos = 0usize;
        while pos < bytes.len() {
            match bytes[pos] {
                b'(' | b'[' | b'{' => depth += 1,
                b')' | b']' | b'}' => depth = depth.saturating_sub(1),
                b'"' | b'\'' => {
                    pos = skip_string(bytes, pos);
                    continue;
                }
                b',' if depth == 0 => commas += 1,
                _ => {}
            }
            pos += 1;
        }
        commas
    }

    /// Forward-scan from a declaration value head to its terminator (`;` or `}` at
    /// top level), honouring balanced `()`/`[]`/`{}` and strings — recovers the
    /// value span lazily from the recorded colon offset.
    #[inline]
    fn scan_value_end(bytes: &[u8], start: usize) -> usize {
        let mut depth: i32 = 0;
        let mut pos = start;
        while pos < bytes.len() {
            match bytes[pos] {
                b'(' | b'[' | b'{' => depth += 1,
                b')' | b']' => depth = depth.saturating_sub(1),
                b'}' if depth == 0 => return pos,
                b'}' => depth -= 1,
                b';' if depth == 0 => return pos,
                b'"' | b'\'' => {
                    pos = skip_string(bytes, pos);
                    continue;
                }
                _ => {}
            }
            pos += 1;
        }
        bytes.len()
    }

    /// Skip a CSS string literal beginning at `pos` (`pos` points at the quote),
    /// returning the index just past the closing quote.
    #[inline]
    fn skip_string(bytes: &[u8], pos: usize) -> usize {
        let quote = bytes[pos];
        let mut cursor = pos + 1;
        while cursor < bytes.len() {
            match bytes[cursor] {
                b'\\' => cursor += 2,
                byte if byte == quote => return cursor + 1,
                _ => cursor += 1,
            }
        }
        bytes.len()
    }

    /// Whether a numeric value head carries a unit or `%` suffix (a dimension) as
    /// opposed to a bare number.
    #[inline]
    fn number_has_unit(value: &[u8]) -> bool {
        let mut pos = 0usize;
        if matches!(value.first(), Some(b'+' | b'-')) {
            pos += 1;
        }
        while pos < value.len() && matches!(value[pos], b'0'..=b'9' | b'.') {
            pos += 1;
        }
        if pos < value.len() && matches!(value[pos], b'e' | b'E') {
            let mut exp = pos + 1;
            if exp < value.len() && matches!(value[exp], b'+' | b'-') {
                exp += 1;
            }
            if exp < value.len() && value[exp].is_ascii_digit() {
                pos = exp;
                while pos < value.len() && value[pos].is_ascii_digit() {
                    pos += 1;
                }
            }
        }
        matches!(value.get(pos), Some(b'%') | Some(b'a'..=b'z') | Some(b'A'..=b'Z'))
    }

    /// Whether an identifier-led value head opens a function call (`name(`),
    /// scanning the leading ident run for an immediate `(`.
    #[inline]
    fn leading_ident_is_function(value: &[u8]) -> bool {
        let mut pos = 0usize;
        while pos < value.len() {
            match value[pos] {
                b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'-' | b'_' | 0x80..=0xff => pos += 1,
                b'\\' => pos += 2,
                _ => break,
            }
        }
        matches!(value.get(pos), Some(b'('))
    }

    fn css_full_error(offset: usize, message: &'static str) -> CssFactError {
        CssFactError { offset, message }
    }

    fn fnv64(bytes: &[u8]) -> u64 {
        let mut hash = 0xcbf29ce484222325u64;
        for byte in bytes {
            hash ^= u64::from(*byte);
            hash = hash.wrapping_mul(0x100000001b3);
        }
        hash
    }

    fn push_hex64(out: &mut String, value: u64) {
        out.push_str(&format!("{value:016x}"));
    }
"#;
