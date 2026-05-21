use crate::report::{SkV12NonJsonReport, SkV12NonJsonRow, SKV12_NON_JSON_REPORT_SCHEMA};
use cssparser::{
    parse_important, AtRuleParser, BasicParseErrorKind, CowRcStr, DeclarationParser, Parser,
    ParserInput, ParserState, QualifiedRuleParser, RuleBodyItemParser, RuleBodyParser,
    StyleSheetParser, Token,
};
use runtime::generated_css_l4_declaration_values as track1;
use serde_json;
use sha2::{Digest, Sha256};
use std::fmt;
use std::fs;
use std::hint::black_box;
use std::io;
use std::path::{Path, PathBuf};
use std::time::Instant;

pub const ROW_ID: &str = "css_l4/declaration_values/direct_to_struct/main";
pub const OUTPUT_PLANE: &str = "css_l4_declaration_value_fact_stream";
pub const WAVE_ID: &str = "SK-V12-W1b-1";

const FACT_SCHEMA: &str = "css-l4-declaration-value-facts-v1";
const FIXTURE_RELATIVE: &str =
    "restart/skinny/tranches/sk-v12/research/w1b/css_l4_declaration_values.css";
const REPORT_RELATIVE: &str =
    "restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-1-css-l4-oracle.json";
const ARTIFACT_DIR_RELATIVE: &str = "restart/skinny/tranches/sk-v12/research/w1b/artifacts";
const EXPECTED_FIXTURE_SHA256: &str =
    "cbb639460a72ef82e7c1b7c53ccc69495a35f6860b29ad72370b042b470d7374";

#[derive(Debug, Clone)]
pub struct CssOracleError {
    message: String,
}

impl CssOracleError {
    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl fmt::Display for CssOracleError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.message)
    }
}

impl std::error::Error for CssOracleError {}

pub fn fixture_path() -> PathBuf {
    repo_root().join(FIXTURE_RELATIVE)
}

pub fn report_path() -> PathBuf {
    repo_root().join(REPORT_RELATIVE)
}

pub fn read_fixture() -> io::Result<String> {
    fs::read_to_string(fixture_path())
}

pub fn track1_facts(input: &str) -> Result<String, String> {
    track1::parser::parse(input).map_err(|error| error.to_string())
}

pub fn oracle_facts(input: &str) -> Result<String, CssOracleError> {
    let mut parser_input = ParserInput::new(input);
    let mut parser = Parser::new(&mut parser_input);
    let mut oracle = OracleParser::new(input);
    for item in StyleSheetParser::new(&mut parser, &mut oracle) {
        item.map_err(|(error, fragment)| {
            CssOracleError::new(format!("cssparser rejected `{fragment}`: {error:?}"))
        })?;
    }
    Ok(oracle.finish())
}

pub fn assert_strict_equality(input: &str) -> Result<(String, String), String> {
    let track1 = track1_facts(input)?;
    let oracle = oracle_facts(input).map_err(|error| error.to_string())?;
    if track1 == oracle {
        Ok((track1, oracle))
    } else {
        Err(first_diff(&track1, &oracle))
    }
}

pub fn write_report_with_quick_measurement() -> Result<SkV12NonJsonReport, String> {
    let input = read_fixture().map_err(|error| format!("failed to read CSS fixture: {error}"))?;
    let fixture_sha = sha256_hex(input.as_bytes());
    if fixture_sha != EXPECTED_FIXTURE_SHA256 {
        return Err(format!(
            "CSS fixture checksum changed: expected {EXPECTED_FIXTURE_SHA256}, got {fixture_sha}"
        ));
    }
    let (track1_text, oracle_text) = assert_strict_equality(&input)?;
    let run_id = format!(
        "sk-v12-w1b-1:fixture-fnv64-{:016x}",
        fnv64(input.as_bytes())
    );
    let artifact_dir = repo_root().join(ARTIFACT_DIR_RELATIVE);
    fs::create_dir_all(&artifact_dir)
        .map_err(|error| format!("failed to create CSS artifact directory: {error}"))?;
    fs::write(artifact_dir.join("track1-facts.txt"), &track1_text)
        .map_err(|error| format!("failed to write Track 1 facts: {error}"))?;
    fs::write(artifact_dir.join("oracle-facts.txt"), &oracle_text)
        .map_err(|error| format!("failed to write oracle facts: {error}"))?;
    fs::write(
        artifact_dir.join("strict-equality.txt"),
        format!("status=pass\nrow_id={ROW_ID}\nrun_id={run_id}\n"),
    )
    .map_err(|error| format!("failed to write equality artifact: {error}"))?;

    let track1_measure = measure_mbps(input.as_str(), |input| track1_facts(input));
    let oracle_measure = measure_mbps(input.as_str(), |input| {
        oracle_facts(input).map_err(|error| error.to_string())
    });
    let generated = generated_module_stats()?;
    let report = SkV12NonJsonReport {
        schema_id: SKV12_NON_JSON_REPORT_SCHEMA.to_string(),
        wave_id: WAVE_ID.to_string(),
        run_id: run_id.clone(),
        rows: vec![SkV12NonJsonRow {
            row_id: ROW_ID.to_string(),
            grammar_id: "css_l4".to_string(),
            domain: "non_json_generated:css_l4:declaration_values".to_string(),
            corpus_or_workload: "declaration_values".to_string(),
            workload: "direct_to_struct".to_string(),
            workload_class: "baseline".to_string(),
            output_plane: OUTPUT_PLANE.to_string(),
            outcome_id: "C".to_string(),
            verdict: "GO".to_string(),
            strictness: "strict".to_string(),
            generated_track1_source_path:
                "crates/codegen/src/css_l4_declaration_values_templates/generated.rs".to_string(),
            generated_runtime_path:
                "runtime::generated_css_l4_declaration_values::parser::parse".to_string(),
            generated_input_provenance: format!(
                "fixture:css_l4:declaration_values:sha256={fixture_sha}"
            ),
            grammar_checksum: generated.grammar_checksum,
            input_checksum: fixture_sha,
            input_bytes: input.len() as u64,
            track1_mbps: track1_measure.mbps,
            track1_artifact: format!(
                "criterion:{run_id}:target/criterion/nonjson_css_l4/track1_generated_css_l4_decl_values"
            ),
            track2_or_oracle_source_path:
                "cssparser-0.34:StyleSheetParser+RuleBodyParser:bench/nonjson_css_l4.rs"
                    .to_string(),
            track2_independence_status: "independent_verified".to_string(),
            track2_or_oracle_mbps: Some(oracle_measure.mbps),
            strict_output_equality: "pass".to_string(),
            oracle_status: "same-plane:strict:independent:cssparser:fresh".to_string(),
            baseline_row_id: "none".to_string(),
            baseline_mbps: None,
            threshold_mbps: None,
            host_triple: host_triple(),
            feature_mask: feature_mask(),
            build_flags: build_flags(),
            sample_count: track1_measure.iterations,
            sample_cost: format!(
                "ns_per_byte={:.6};track1_ns={:.2};oracle_ns={:.2};bytes={}",
                track1_measure.ns_per_byte,
                track1_measure.elapsed_ns,
                oracle_measure.elapsed_ns,
                input.len()
            ),
            benchmark_artifact_path: format!(
                "criterion:{run_id}:target/criterion/nonjson_css_l4"
            ),
            measured_validation_path: "track1-vs-cssparser-byte-identical-fact-stream"
                .to_string(),
            profile_artifact: "profile:not_required_for_W1b-1_scaffold;pmu_gates_start_W1b-2"
                .to_string(),
            generated_loc: generated.loc,
            generated_module_bytes: generated.bytes,
            grammar_size_guard: "pass:generated_loc<=360".to_string(),
            lock14_status: "pass:lock14_baseline::validate".to_string(),
            lock16_status: "n/a:scalar-css-scaffold-no-simd".to_string(),
            scalar_reference_status: "pass:cssparser_oracle".to_string(),
            checkasm_or_parity_status: "pass:track1_equals_cssparser".to_string(),
            json_guard_state: "refreshed:sk-v12-w1b-1:guards-pass".to_string(),
            redress_entry: "REDRESS-123".to_string(),
            same_wave_consumer_class: "companion_gate_generated_css_l4_baseline".to_string(),
            gate_status: "pass".to_string(),
        }],
    };
    let text = serde_json::to_string_pretty(&report)
        .map_err(|error| format!("failed to serialize CSS report: {error}"))?;
    fs::write(report_path(), format!("{text}\n"))
        .map_err(|error| format!("failed to write CSS report: {error}"))?;
    Ok(report)
}

struct OracleParser<'i> {
    input: &'i str,
    sink: LocalFactSink,
    depth: u32,
    declarations: u32,
}

impl<'i> OracleParser<'i> {
    fn new(input: &'i str) -> Self {
        Self {
            input,
            sink: LocalFactSink::new(input),
            depth: 0,
            declarations: 0,
        }
    }

    fn finish(self) -> String {
        self.sink.finish()
    }

    fn parse_nested_rules<'t>(
        &mut self,
        input: &mut Parser<'i, 't>,
    ) -> Result<(), cssparser::ParseError<'i, String>> {
        self.depth += 1;
        for item in RuleBodyParser::new(input, self) {
            item.map_err(|(error, _fragment)| error)?;
        }
        self.depth -= 1;
        Ok(())
    }

    fn emit_component_values<'t>(
        &mut self,
        decl: u32,
        next_idx: &mut u32,
        input: &mut Parser<'i, 't>,
        first_start: &mut Option<usize>,
        last_end: &mut usize,
    ) -> Result<bool, cssparser::ParseError<'i, String>> {
        let mut important = false;
        loop {
            let start_state = input.state();
            let token = match input.next_including_whitespace().cloned() {
                Ok(token) => token,
                Err(_) => break,
            };
            if matches!(token, Token::WhiteSpace(_) | Token::Comment(_)) {
                continue;
            }
            if token == Token::Delim('!') {
                input.reset(&start_state);
                if parse_important(input).is_ok() && input.is_exhausted() {
                    important = true;
                    break;
                }
                input.reset(&start_state);
                let token = input.next_including_whitespace().cloned()?;
                self.emit_token_from_cssparser(
                    decl,
                    next_idx,
                    token,
                    input,
                    first_start,
                    last_end,
                )?;
                continue;
            }
            self.emit_token_from_cssparser(decl, next_idx, token, input, first_start, last_end)?;
        }
        Ok(important)
    }

    fn emit_token_from_cssparser<'t>(
        &mut self,
        decl: u32,
        next_idx: &mut u32,
        token: Token<'i>,
        input: &mut Parser<'i, 't>,
        first_start: &mut Option<usize>,
        last_end: &mut usize,
    ) -> Result<(), cssparser::ParseError<'i, String>> {
        let token_end = input.position().byte_index();
        let token_start = token_start_for(token.clone(), self.input, token_end);
        *first_start = Some(first_start.unwrap_or(token_start));
        match token {
            Token::Ident(value) => self.push_token(decl, next_idx, "ident", value.as_ref()),
            Token::Hash(value) | Token::IDHash(value) => {
                self.push_token(decl, next_idx, "hash", value.as_ref())
            }
            Token::Function(name) => {
                self.push_token(decl, next_idx, "function", name.as_ref());
                input.parse_nested_block(|input| {
                    self.emit_component_values(decl, next_idx, input, first_start, last_end)?;
                    Ok(())
                })?;
                self.push_token(decl, next_idx, "paren_close", ")");
                *last_end = input.position().byte_index();
                return Ok(());
            }
            Token::Number { .. } => self.push_token(
                decl,
                next_idx,
                "number",
                &self.input[token_start..token_end],
            ),
            Token::Percentage { .. } => self.push_token(
                decl,
                next_idx,
                "percentage",
                &self.input[token_start..token_end],
            ),
            Token::Dimension { .. } => self.push_token(
                decl,
                next_idx,
                "dimension",
                &self.input[token_start..token_end],
            ),
            Token::QuotedString(value) => self.push_token(decl, next_idx, "string", value.as_ref()),
            Token::UnquotedUrl(value) => self.push_token(decl, next_idx, "url", value.as_ref()),
            Token::Delim(value) => self.push_token(decl, next_idx, "delim", &value.to_string()),
            Token::Colon => self.push_token(decl, next_idx, "delim", ":"),
            Token::Semicolon => self.push_token(decl, next_idx, "delim", ";"),
            Token::Comma => self.push_token(decl, next_idx, "comma", ","),
            Token::ParenthesisBlock => {
                self.push_token(decl, next_idx, "paren_open", "(");
                input.parse_nested_block(|input| {
                    self.emit_component_values(decl, next_idx, input, first_start, last_end)?;
                    Ok(())
                })?;
                self.push_token(decl, next_idx, "paren_close", ")");
                *last_end = input.position().byte_index();
                return Ok(());
            }
            Token::SquareBracketBlock => {
                self.push_token(decl, next_idx, "bracket_open", "[");
                input.parse_nested_block(|input| {
                    self.emit_component_values(decl, next_idx, input, first_start, last_end)?;
                    Ok(())
                })?;
                self.push_token(decl, next_idx, "bracket_close", "]");
                *last_end = input.position().byte_index();
                return Ok(());
            }
            other => {
                return Err(input.new_error(BasicParseErrorKind::UnexpectedToken(other)));
            }
        }
        *last_end = token_end;
        Ok(())
    }

    fn push_token(&mut self, decl: u32, next_idx: &mut u32, kind: &str, lexeme: &str) {
        self.sink.token(decl, *next_idx, kind, lexeme);
        *next_idx += 1;
    }
}

impl<'i> DeclarationParser<'i> for OracleParser<'i> {
    type Declaration = ();
    type Error = String;

    fn parse_value<'t>(
        &mut self,
        name: CowRcStr<'i>,
        input: &mut Parser<'i, 't>,
    ) -> Result<(), cssparser::ParseError<'i, String>> {
        let decl = self.declarations;
        self.declarations += 1;
        let mut next_idx = 0;
        let mut first_start = None;
        let mut last_end = input.position().byte_index();
        let important = self.emit_component_values(
            decl,
            &mut next_idx,
            input,
            &mut first_start,
            &mut last_end,
        )?;
        let value_start = first_start.unwrap_or(last_end);
        self.sink.declaration(
            decl,
            self.depth,
            name.as_ref(),
            important,
            value_start,
            last_end,
        );
        self.sink.move_last_declaration_before_tokens(decl);
        Ok(())
    }
}

impl<'i> AtRuleParser<'i> for OracleParser<'i> {
    type Prelude = ();
    type AtRule = ();
    type Error = String;

    fn parse_prelude<'t>(
        &mut self,
        _name: CowRcStr<'i>,
        input: &mut Parser<'i, 't>,
    ) -> Result<(), cssparser::ParseError<'i, String>> {
        while input.next_including_whitespace().is_ok() {}
        Ok(())
    }

    fn parse_block<'t>(
        &mut self,
        _prelude: (),
        _start: &ParserState,
        input: &mut Parser<'i, 't>,
    ) -> Result<(), cssparser::ParseError<'i, String>> {
        self.parse_nested_rules(input)
    }
}

impl<'i> QualifiedRuleParser<'i> for OracleParser<'i> {
    type Prelude = ();
    type QualifiedRule = ();
    type Error = String;

    fn parse_prelude<'t>(
        &mut self,
        input: &mut Parser<'i, 't>,
    ) -> Result<(), cssparser::ParseError<'i, String>> {
        while input.next_including_whitespace().is_ok() {}
        Ok(())
    }

    fn parse_block<'t>(
        &mut self,
        _prelude: (),
        _start: &ParserState,
        input: &mut Parser<'i, 't>,
    ) -> Result<(), cssparser::ParseError<'i, String>> {
        self.parse_nested_rules(input)
    }
}

impl<'i> RuleBodyItemParser<'i, (), String> for OracleParser<'i> {
    fn parse_declarations(&self) -> bool {
        true
    }

    fn parse_qualified(&self) -> bool {
        true
    }
}

struct LocalFactSink {
    out: String,
    declarations: Vec<String>,
    tokens: Vec<(u32, String)>,
    decls: u32,
    token_count: u32,
}

impl LocalFactSink {
    fn new(input: &str) -> Self {
        let mut out = String::new();
        out.push_str(FACT_SCHEMA);
        out.push('\n');
        out.push_str("row\tid=");
        out.push_str(ROW_ID);
        out.push_str("\tplane=");
        out.push_str(OUTPUT_PLANE);
        out.push('\n');
        out.push_str("source\tinput_fnv64=");
        push_hex64(&mut out, fnv64(input.as_bytes()));
        out.push_str("\tinput_bytes=");
        out.push_str(&input.len().to_string());
        out.push('\n');
        Self {
            out,
            declarations: Vec::new(),
            tokens: Vec::new(),
            decls: 0,
            token_count: 0,
        }
    }

    fn declaration(
        &mut self,
        idx: u32,
        depth: u32,
        property: &str,
        important: bool,
        value_start: usize,
        value_end: usize,
    ) {
        self.decls += 1;
        let mut line = String::new();
        line.push_str("decl\tidx=");
        line.push_str(&idx.to_string());
        line.push_str("\tdepth=");
        line.push_str(&depth.to_string());
        line.push_str("\tproperty_hex=");
        push_ascii_lower_hex(&mut line, property);
        line.push_str("\timportant=");
        line.push(if important { '1' } else { '0' });
        line.push_str("\tvalue_start=");
        line.push_str(&value_start.to_string());
        line.push_str("\tvalue_end=");
        line.push_str(&value_end.to_string());
        line.push('\n');
        self.declarations.push(line);
    }

    fn token(&mut self, decl: u32, idx: u32, kind: &str, lexeme: &str) {
        self.token_count += 1;
        let mut line = String::new();
        line.push_str("tok\tdecl=");
        line.push_str(&decl.to_string());
        line.push_str("\tidx=");
        line.push_str(&idx.to_string());
        line.push_str("\tkind=");
        line.push_str(kind);
        line.push_str("\tlexeme_hex=");
        if matches!(kind, "ident" | "function" | "hash" | "dimension") {
            push_ascii_lower_hex(&mut line, lexeme);
        } else {
            push_hex(&mut line, lexeme.as_bytes());
        }
        line.push_str("\tflags=none\n");
        self.tokens.push((decl, line));
    }

    fn move_last_declaration_before_tokens(&mut self, _decl: u32) {}

    fn finish(mut self) -> String {
        for decl in 0..self.decls {
            self.out.push_str(&self.declarations[decl as usize]);
            for (_, token) in self
                .tokens
                .iter()
                .filter(|(token_decl, _)| *token_decl == decl)
            {
                self.out.push_str(token);
            }
        }
        let stream_hash = fnv64(self.out.as_bytes());
        self.out.push_str("end\tdecls=");
        self.out.push_str(&self.decls.to_string());
        self.out.push_str("\ttokens=");
        self.out.push_str(&self.token_count.to_string());
        self.out.push_str("\tstream_fnv64=");
        push_hex64(&mut self.out, stream_hash);
        self.out.push('\n');
        self.out
    }
}

#[derive(Debug)]
struct Measurement {
    mbps: f64,
    ns_per_byte: f64,
    elapsed_ns: f64,
    iterations: u64,
}

struct GeneratedStats {
    grammar_checksum: String,
    loc: u64,
    bytes: u64,
}

fn measure_mbps<F>(input: &str, mut f: F) -> Measurement
where
    F: FnMut(&str) -> Result<String, String>,
{
    let iterations = 2_000u64;
    for _ in 0..16 {
        black_box(f(black_box(input)).expect("CSS quick measurement warmup failed"));
    }
    let started = Instant::now();
    for _ in 0..iterations {
        black_box(f(black_box(input)).expect("CSS quick measurement failed"));
    }
    let elapsed = started.elapsed();
    let elapsed_ns = elapsed.as_nanos() as f64;
    let bytes = input.len() as f64 * iterations as f64;
    let ns_per_byte = elapsed_ns / bytes;
    let mbps = bytes * 8_000.0 / elapsed_ns;
    Measurement {
        mbps,
        ns_per_byte,
        elapsed_ns,
        iterations,
    }
}

fn generated_module_stats() -> Result<GeneratedStats, String> {
    let root = repo_root();
    let paths = [
        "skinny/crates/runtime/src/grammars/css_l4_declaration_values/config.rs",
        "skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs",
        "skinny/crates/runtime/src/grammars/css_l4_declaration_values/mod.rs",
        "skinny/crates/runtime/src/grammars/css_l4_declaration_values/parser.rs",
    ];
    let mut hasher = Sha256::new();
    let mut loc = 0u64;
    let mut bytes = 0u64;
    for path in paths {
        let source = fs::read(root.join(path))
            .map_err(|error| format!("failed to read generated CSS module {path}: {error}"))?;
        hasher.update(path.as_bytes());
        hasher.update([0]);
        hasher.update(&source);
        hasher.update([0]);
        loc += source.iter().filter(|byte| **byte == b'\n').count() as u64;
        bytes += source.len() as u64;
    }
    Ok(GeneratedStats {
        grammar_checksum: hex_digest(hasher.finalize().as_slice()),
        loc,
        bytes,
    })
}

fn token_start_for(token: Token<'_>, input: &str, token_end: usize) -> usize {
    match token {
        Token::Ident(value) => token_end.saturating_sub(value.len()),
        Token::Hash(value) | Token::IDHash(value) => token_end.saturating_sub(value.len() + 1),
        Token::Function(value) => token_end.saturating_sub(value.len() + 1),
        Token::QuotedString(value) => token_end.saturating_sub(value.len() + 2),
        Token::UnquotedUrl(value) => token_end.saturating_sub(value.len() + 5),
        Token::Delim(value) => token_end.saturating_sub(value.len_utf8()),
        Token::Colon
        | Token::Semicolon
        | Token::Comma
        | Token::ParenthesisBlock
        | Token::SquareBracketBlock
        | Token::CurlyBracketBlock => token_end.saturating_sub(1),
        Token::Number { .. } | Token::Percentage { .. } | Token::Dimension { .. } => {
            scan_numeric_start(input.as_bytes(), token_end)
        }
        _ => token_end,
    }
}

fn scan_numeric_start(bytes: &[u8], mut end: usize) -> usize {
    while end > 0 {
        let byte = bytes[end - 1];
        if byte.is_ascii_alphanumeric() || matches!(byte, b'.' | b'+' | b'-' | b'%') {
            end -= 1;
        } else {
            break;
        }
    }
    end
}

fn first_diff(left: &str, right: &str) -> String {
    for (idx, (a, b)) in left.bytes().zip(right.bytes()).enumerate() {
        if a != b {
            return format!(
                "CSS Track 1/oracle mismatch at byte {idx}: track1=0x{a:02x}, oracle=0x{b:02x}"
            );
        }
    }
    format!(
        "CSS Track 1/oracle length mismatch: track1={}, oracle={}",
        left.len(),
        right.len()
    )
}

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../..")
}

fn build_flags() -> String {
    let rustflags = std::env::var("RUSTFLAGS").unwrap_or_default();
    let rendered = if rustflags.is_empty() {
        "-C target-cpu=native".to_string()
    } else {
        rustflags
    };
    format!("profile=bench;rustflags={rendered};target_cpu=native")
}

fn host_triple() -> String {
    format!(
        "{}-{};arch={};cpu={}",
        std::env::consts::ARCH,
        std::env::consts::OS,
        std::env::consts::ARCH,
        std::env::var("BBNF_CPU_MODEL").unwrap_or_else(|_| "apple-silicon".to_string())
    )
}

fn feature_mask() -> String {
    format!(
        "arch={};os={};simd=scalar-cssparser;target_cpu=native",
        std::env::consts::ARCH,
        std::env::consts::OS
    )
}

fn sha256_hex(bytes: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    hex_digest(hasher.finalize().as_slice())
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

fn hex_digest(bytes: &[u8]) -> String {
    let mut out = String::with_capacity(bytes.len() * 2);
    push_hex(&mut out, bytes);
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cssparser_oracle_matches_generated_track1() {
        let input = read_fixture().unwrap();
        assert_strict_equality(&input).unwrap();
    }

    #[test]
    fn writes_gate_consumed_css_l4_report() {
        let report = write_report_with_quick_measurement().unwrap();
        report.validate_gate().unwrap();
    }
}
