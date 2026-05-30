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

    pub use generated::{CssDocument, CssNode, CssNodeKind, CssSummary};
    pub use parser::{parse, parse_bytes, summary, summary_bytes};
    pub use sink::CssFactError;
"#;

const CSS_PARSER_RS: &str = r#"
    use super::generated::{self, CssDocument, CssSummary};
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
    }

    #[derive(Default, Copy, Clone, Eq, PartialEq, Debug)]
    pub struct CssSummary {
        pub rules: usize,
        pub at_rules: usize,
        pub qualified_rules: usize,
        pub declarations: usize,
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
