use crate::grammar_provider::{RuntimeEmitterKind, RuntimeGenerationRequest, RuntimeOutputLabels};
use crate::{css_scan_direct, grammar_profile, json_sink_direct, lower, CodegenError, EmittedSource};
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
    // SK-V18 G2: the CSS provider body is DERIVED from the raw grammar AST
    // (`lower::css_scan`) and emitted by `css_scan_direct::render` — the
    // verbatim raw-string courier is gone. The structural alphabet (the
    // per-call-site STOP byte-arrays + the fixed-9 SKIP set) is harvested from
    // `stylesheet.bbnf`'s `ruleBlock`/declaration/selector literals; the scan
    // ALGORITHM is the blob-free `css_balanced_component_scan` runtime primitive.
    let scan_sources = request
        .sources
        .iter()
        .map(|source| (source.rel_path.clone(), source.source.clone()))
        .collect::<Vec<_>>();
    let program = lower::css_scan::derive_program(
        &request.grammar_name,
        &request.entry_rule,
        &scan_sources,
    )?;
    let mut generated = String::new();
    generated.push_str(crate::GENERATED_HEADER);
    generated.push('\n');
    generated.push_str(&css_scan_direct::render(&program).map_err(CodegenError::Lowering)?);

    let files = BTreeMap::from([
        (
            "config.rs".to_string(),
            render_request_facts_config(labels, request, facts),
        ),
        ("generated.rs".to_string(), generated),
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

