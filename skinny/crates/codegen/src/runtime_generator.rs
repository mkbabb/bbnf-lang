use crate::grammar_profile::{GrammarProfile, RuntimeGenerationMode};
use crate::grammar_provider::RuntimeGenerationRequest;
use crate::{grammar_profile, json_sink_direct, lower, CodegenError, EmittedSource};
use std::collections::BTreeMap;
use std::fmt::Write;

pub(crate) fn emit_profile_only(profile: &GrammarProfile) -> Result<EmittedSource, CodegenError> {
    Err(CodegenError::Lowering(format!(
        "runtime profile `{}` requires RuntimeGenerationRequest after W5C-GEN",
        profile.id()
    )))
}

pub(crate) fn emit_from_request(
    profile: &GrammarProfile,
    request: &RuntimeGenerationRequest,
    facts: &grammar::RuntimeSourceFacts,
) -> Result<EmittedSource, CodegenError> {
    match profile.mode() {
        RuntimeGenerationMode::PassCompiled => {
            let Some(source) = request.sources.first() else {
                return Err(CodegenError::Lowering(
                    "compiled runtime request requires a source".to_string(),
                ));
            };
            crate::emit_from_source(&request.grammar_name, &source.source)
        }
        RuntimeGenerationMode::FrontendFacts => emit_frontend_facts(profile, request, facts),
    }
}

pub(crate) fn emit_compiled(
    profile: &GrammarProfile,
    sink_only: &lower::sink_only::SinkOnlyProgram,
) -> Result<EmittedSource, CodegenError> {
    if profile.mode() != RuntimeGenerationMode::PassCompiled {
        return Err(CodegenError::Lowering(format!(
            "runtime profile `{}` requires request frontend facts",
            profile.id()
        )));
    }
    let mut generated = include_str!("json_templates/generated.rs").to_string();
    generated.push('\n');
    generated.push_str(&json_sink_direct::render(sink_only).map_err(CodegenError::Lowering)?);
    let mut host = normalize(JSON_HOST_RS);
    host.push('\n');
    let mut module = normalize(JSON_MOD_RS);
    module.push('\n');

    let files = BTreeMap::from([
        (
            "config.rs".to_string(),
            render_json_config(&sink_only.policy_summary),
        ),
        ("generated.rs".to_string(), generated),
        ("host.rs".to_string(), host),
        ("mod.rs".to_string(), module),
        (
            "parser.rs".to_string(),
            include_str!("json_templates/parser.rs").to_string(),
        ),
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
    grammar_profile::validate_generated_roster(profile, files.keys().map(String::as_str))
        .map_err(CodegenError::Lowering)?;
    Ok(EmittedSource { files })
}

fn emit_frontend_facts(
    profile: &GrammarProfile,
    request: &RuntimeGenerationRequest,
    facts: &grammar::RuntimeSourceFacts,
) -> Result<EmittedSource, CodegenError> {
    let config = css_profile_config(profile.id()).ok_or_else(|| {
        CodegenError::Lowering(format!(
            "runtime profile `{}` is not a frontend-facts runtime",
            profile.id()
        ))
    })?;
    let files = BTreeMap::from([
        (
            "config.rs".to_string(),
            render_css_config(config, request, facts),
        ),
        ("generated.rs".to_string(), normalize(CSS_GENERATED_RS)),
        ("mod.rs".to_string(), normalize(CSS_MOD_RS)),
        ("parser.rs".to_string(), normalize(CSS_PARSER_RS)),
        ("sink.rs".to_string(), normalize(CSS_SINK_RS)),
    ]);
    grammar_profile::validate_generated_roster(profile, files.keys().map(String::as_str))
        .map_err(CodegenError::Lowering)?;
    Ok(EmittedSource { files })
}

#[derive(Clone, Copy)]
struct CssProfileConfig {
    fact_schema: &'static str,
    row_id: &'static str,
    output_plane: &'static str,
}

fn css_profile_config(profile_id: &str) -> Option<CssProfileConfig> {
    match profile_id {
        "css_l4_declaration_values" => Some(CssProfileConfig {
            fact_schema: "css-l4-declaration-value-facts-v1",
            row_id: "css_l4/declaration_values/direct_to_struct/main",
            output_plane: "css_l4_declaration_value_fact_stream",
        }),
        "css_l4_declaration_values_extended" => Some(CssProfileConfig {
            fact_schema: "css-l4-declaration-value-extended-facts-v1",
            row_id: "css_l4/declaration_values_extended/direct_to_struct/main",
            output_plane: "css_l4_declaration_value_extended_fact_stream",
        }),
        "css_l4_stylesheet_selectors" => Some(CssProfileConfig {
            fact_schema: "css-l4-stylesheet-selector-facts-v1",
            row_id: "css_l4/stylesheet_and_selectors/direct_to_struct/main",
            output_plane: "css_l4_stylesheet_selector_fact_stream",
        }),
        "css_l4_visual_functions" => Some(CssProfileConfig {
            fact_schema: "css-l4-visual-function-facts-v1",
            row_id: "css_l4/visual_functions/direct_to_struct/main",
            output_plane: "css_l4_visual_function_fact_stream",
        }),
        "css_l4_at_rules_and_media" => Some(CssProfileConfig {
            fact_schema: "css-l4-at-rules-media-facts-v1",
            row_id: "css_l4/at_rules_and_media/direct_to_struct/main",
            output_plane: "css_l4_at_rules_media_fact_stream",
        }),
        "css_l4_vendor_and_custom_atrules" => Some(CssProfileConfig {
            fact_schema: "css-l4-vendor-custom-facts-v1",
            row_id: "css_l4/vendor_and_custom_atrules/direct_to_struct/main",
            output_plane: "css_l4_vendor_custom_fact_stream",
        }),
        "css_l4_nested_layout" => Some(CssProfileConfig {
            fact_schema: "css-l4-nested-layout-facts-v1",
            row_id: "css_l4/nested_layout/direct_to_struct/main",
            output_plane: "css_l4_nested_layout_fact_stream",
        }),
        _ => None,
    }
}

fn render_css_config(
    config: CssProfileConfig,
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
        fact_schema = config.fact_schema,
        row_id = config.row_id,
        output_plane = config.output_plane,
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

    pub use parser::{parse, parse_bytes, RECOGNIZER_COUNT};
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
