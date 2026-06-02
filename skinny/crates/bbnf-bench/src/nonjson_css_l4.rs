//! CSS L4 rich-CSSOM equality oracle (the 9-field `assert_rich_strict_equality`).
//!
//! The warm 2000-iter micro-fixture throughput harness — the iteration-loop Mbps
//! timer, the per-fixture lightningcss more-work comparators, the per-fixture
//! SHA256/byte scaffolding, and the 85–357-byte micro-fixture corpus — has been
//! retired
//! (SK-V18 P2, residual R13 / addendum 5 timed-plane-symmetry). The sole cold,
//! real-corpus CSS throughput harness is `css_canon_bench`; the load-bearing rich
//! typed-CSSOM parity oracle survives here so `track1_rich == cssparser_rich`
//! remains a genuine independent population-parity equality (selectors + typed
//! value-node counts atop the 4 structural fields).

use cssparser::{
    parse_important, AtRuleParser, BasicParseErrorKind, CowRcStr, DeclarationParser, Parser,
    ParserInput, ParserState, QualifiedRuleParser, RuleBodyItemParser, RuleBodyParser,
    StyleSheetParser, Token,
};
use std::fmt;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

pub const ROW_ID: &str = "css_l4/declaration_values/direct_to_struct/main";
pub const OUTPUT_PLANE: &str = "css_l4_declaration_value_fact_stream";

const FACT_SCHEMA: &str = "css-l4-declaration-value-facts-v1";
const FIXTURE_RELATIVE: &str =
    "restart/skinny/tranches/sk-v12/research/w1b/css_l4_declaration_values.css";

#[derive(Debug)]
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

pub fn read_fixture() -> io::Result<String> {
    fs::read_to_string(fixture_path())
}

/// SK-V17 W2 — the canonical RICH typed-CSSOM summary string. Both the rich
/// tape-projected Track-1 product and the independent cssparser reference render
/// to this form, so `track1_rich == cssparser_rich` is a genuine rich-CSSOM
/// population-parity equality (selectors + typed value-node counts atop the 4
/// structural fields).
#[allow(clippy::too_many_arguments)]
fn css_rich_summary_string(
    rules: usize,
    at_rules: usize,
    qualified_rules: usize,
    declarations: usize,
    selectors: usize,
    dimensions: usize,
    numbers: usize,
    colors: usize,
    functions: usize,
) -> String {
    format!(
        "css-l4-rich-summary-v1\nsummary\trules={rules}\tat_rules={at_rules}\tqualified_rules={qualified_rules}\tdeclarations={declarations}\tselectors={selectors}\tdimensions={dimensions}\tnumbers={numbers}\tcolors={colors}\tfunctions={functions}\n"
    )
}

/// Project a tape-backed Track-1 parse to the canonical RICH summary string. The
/// `rich_summary` fn reads the tape lazily via `ValueRef`, materializing nothing.
macro_rules! tape_rich_facts {
    ($module:path, $input:expr) => {{
        use $module as module;
        module::parser::rich_summary($input)
            .map(|s| {
                css_rich_summary_string(
                    s.rules,
                    s.at_rules,
                    s.qualified_rules,
                    s.declarations,
                    s.selectors,
                    s.dimensions,
                    s.numbers,
                    s.colors,
                    s.functions,
                )
            })
            .map_err(|error| error.to_string())
    }};
}

/// The rich tape-projected Track-1 summary for the declaration-values grammar.
pub fn track1_rich_facts(input: &str) -> Result<String, String> {
    tape_rich_facts!(runtime::generated_css_l4_declaration_values, input)
}

/// The independent cssparser RICH reference rendered to the canonical summary.
fn cssparser_rich_summary_facts(input: &str) -> Result<String, CssOracleError> {
    let mut parser_input = ParserInput::new(input);
    let mut parser = Parser::new(&mut parser_input);
    let mut oracle = OracleParser::new(input);
    for item in StyleSheetParser::new(&mut parser, &mut oracle) {
        item.map_err(|(error, fragment)| {
            CssOracleError::new(format!("cssparser rejected `{fragment}`: {error:?}"))
        })?;
    }
    Ok(oracle.rich_summary_string())
}

pub fn rich_oracle_facts(input: &str) -> Result<String, CssOracleError> {
    cssparser_rich_summary_facts(input)
}

/// W2 EQUALITY GATE: the rich tape-projected CSSOM summary equals the independent
/// cssparser rich reference (selectors + typed value-node population parity).
pub fn assert_rich_strict_equality(input: &str) -> Result<(String, String), String> {
    let track1 = track1_rich_facts(input)?;
    let oracle = rich_oracle_facts(input).map_err(|error| error.to_string())?;
    if track1 == oracle {
        Ok((track1, oracle))
    } else {
        Err(first_diff_named("track1_rich", &track1, "cssparser_rich", &oracle))
    }
}

struct OracleParser<'i> {
    input: &'i str,
    sink: LocalFactSink,
    depth: u32,
    declarations: u32,
    // SK-V17 W1: the canonical 4-field structural summary, counted independently
    // from cssparser to anchor tape-vs-cssparser typed equality (replaces the
    // retired byte-for-byte fact-stream comparison).
    rules: u32,
    at_rules: u32,
    qualified_rules: u32,
    // SK-V17 W2: the RICH typed-CSSOM population counts, counted independently
    // from cssparser to anchor tape-vs-cssparser RICH equality — selector-list
    // entries per qualified rule and the leading typed value-node category of
    // each declaration value (dimension/number/color/function).
    selectors: u32,
    dimensions: u32,
    numbers: u32,
    colors: u32,
    functions: u32,
    pending_selectors: u32,
}

impl<'i> OracleParser<'i> {
    fn new(input: &'i str) -> Self {
        Self {
            input,
            sink: LocalFactSink::new(input),
            depth: 0,
            declarations: 0,
            rules: 0,
            at_rules: 0,
            qualified_rules: 0,
            selectors: 0,
            dimensions: 0,
            numbers: 0,
            colors: 0,
            functions: 0,
            pending_selectors: 0,
        }
    }

    #[allow(dead_code)]
    fn finish(self) -> String {
        self.sink.finish()
    }

    /// The canonical RICH typed-CSSOM summary string — the W2 rich-equality
    /// anchor (selectors + typed value-node counts atop the 4 structural fields).
    fn rich_summary_string(&self) -> String {
        css_rich_summary_string(
            self.rules as usize,
            self.at_rules as usize,
            self.qualified_rules as usize,
            self.declarations as usize,
            self.selectors as usize,
            self.dimensions as usize,
            self.numbers as usize,
            self.colors as usize,
            self.functions as usize,
        )
    }

    /// Classify the leading significant token of a declaration value into the
    /// rich typed value-node category, by the same definition the tape projection
    /// uses (leading-byte/shape of the value head). Records the category count.
    fn record_leading_typed_value(&mut self, value: &str) {
        let value = value.trim();
        let Some(head) = value.as_bytes().first().copied() else {
            return;
        };
        match head {
            b'#' => self.colors += 1,
            b'"' | b'\'' => {}
            b'+' | b'-' | b'.' | b'0'..=b'9' => {
                if rich_number_has_unit(value.as_bytes()) {
                    self.dimensions += 1;
                } else {
                    self.numbers += 1;
                }
            }
            b'a'..=b'z' | b'A'..=b'Z' | b'_' | 0x80..=0xff | b'\\' => {
                if rich_leading_ident_is_function(value.as_bytes()) {
                    self.functions += 1;
                }
            }
            _ => {}
        }
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
        // SK-V17 W2: classify the leading typed value-node from the value span —
        // the rich population-parity anchor, matching the tape projection's
        // leading-byte/shape definition.
        if value_start <= last_end && last_end <= self.input.len() {
            self.record_leading_typed_value(&self.input[value_start..last_end]);
        }
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
        self.rules += 1;
        self.at_rules += 1;
        self.parse_nested_rules(input)
    }

    fn rule_without_block(&mut self, _prelude: (), _start: &ParserState) -> Result<(), ()> {
        // `;`-terminated at-rule (e.g. @import, @custom-media): a rule that
        // deposits no block — counted exactly as the tape recognizer does.
        self.rules += 1;
        self.at_rules += 1;
        Ok(())
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
        // SK-V17 W2: count the selector-list entries of this prelude by top-level
        // commas in the prelude source span — the independent reference for the
        // tape projection's `selector_count`. Held pending until the block (and
        // hence the rule) is accepted.
        let start = input.position().byte_index();
        while input.next_including_whitespace().is_ok() {}
        let end = input.position().byte_index();
        let prelude = self.input[start..end].trim();
        self.pending_selectors = if prelude.is_empty() {
            0
        } else {
            1 + rich_count_top_level_commas(prelude.as_bytes()) as u32
        };
        Ok(())
    }

    fn parse_block<'t>(
        &mut self,
        _prelude: (),
        _start: &ParserState,
        input: &mut Parser<'i, 't>,
    ) -> Result<(), cssparser::ParseError<'i, String>> {
        self.rules += 1;
        self.qualified_rules += 1;
        self.selectors += self.pending_selectors;
        self.pending_selectors = 0;
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

fn first_diff_named(left_name: &str, left: &str, right_name: &str, right: &str) -> String {
    for (idx, (a, b)) in left.bytes().zip(right.bytes()).enumerate() {
        if a != b {
            return format!(
                "CSS {left_name}/{right_name} mismatch at byte {idx}: {left_name}=0x{a:02x}, {right_name}=0x{b:02x}"
            );
        }
    }
    format!(
        "CSS {left_name}/{right_name} length mismatch: {left_name}={}, {right_name}={}",
        left.len(),
        right.len()
    )
}

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../..")
}

fn fnv64(bytes: &[u8]) -> u64 {
    let mut hash = 0xcbf29ce484222325u64;
    for byte in bytes {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(0x100000001b3);
    }
    hash
}

// SK-V17 W2 — independent reference helpers for the rich typed-value/selector
// counts, definitionally identical to the tape projection's lazy decode (the
// `CssTypedValue::classify` / `selector_count` recipe). Kept here as the
// cssparser-side reference so `track1_rich == cssparser_rich` is a genuine
// independent population-parity check.
fn rich_count_top_level_commas(bytes: &[u8]) -> usize {
    let mut depth: i32 = 0;
    let mut commas = 0usize;
    let mut pos = 0usize;
    while pos < bytes.len() {
        match bytes[pos] {
            b'(' | b'[' | b'{' => depth += 1,
            b')' | b']' | b'}' => depth = depth.saturating_sub(1),
            b'"' | b'\'' => {
                pos = rich_skip_string(bytes, pos);
                continue;
            }
            b',' if depth == 0 => commas += 1,
            _ => {}
        }
        pos += 1;
    }
    commas
}

fn rich_skip_string(bytes: &[u8], pos: usize) -> usize {
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

fn rich_number_has_unit(value: &[u8]) -> bool {
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

fn rich_leading_ident_is_function(value: &[u8]) -> bool {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rich_cssom_matches_cssparser_on_fixture() {
        // W2 EQUALITY GATE: the rich tape-projected typed CSSOM summary
        // (selectors + typed value-node counts) equals the independent cssparser
        // rich reference on the declaration-values fixture.
        let input = read_fixture().unwrap();
        assert_rich_strict_equality(&input).unwrap();
    }

    #[test]
    fn rich_cssom_matches_cssparser_on_real_corpora() {
        // W2 EQUALITY GATE at corpus scale: rich population parity vs cssparser on
        // every benched real-world corpus (bootstrap/tailwind/material/animate).
        for corpus in crate::css_l4_corpus::load_all().unwrap() {
            let source = std::str::from_utf8(&corpus.bytes).unwrap();
            assert_rich_strict_equality(source)
                .unwrap_or_else(|diff| panic!("rich CSSOM parity failed on {}: {diff}", corpus.spec.id));
        }
    }
}
