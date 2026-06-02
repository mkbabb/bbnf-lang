//! SK-V18 G2 — the CSS scan emitter (sibling of `json_sink_direct`).
//!
//! Renders the CSS `generated.rs` body from a [`CssScanProgram`] — the
//! grammar-derived provider description (`lower::css_scan`). The emitted
//! recognizer is the FLAT balanced-delimiter driver loop calling the
//! `css_balanced_component_scan` runtime primitive (the >SOTA guard: NOT
//! per-rule recursive descent), plus the lazy rich CSSOM projection.
//!
//! The structural alphabet (the per-call-site STOP byte-arrays + the fixed-9
//! SKIP array) is rendered FROM the [`ScanAlphabet`] the grammar harvest
//! produced — no delimiter byte is a literal in THIS source. A `.bbnf`
//! mutation that changes a derived byte changes the emitted constant
//! (`g2_balanced_scan_arg_mutation_fires`), and `CSS_GENERATED_RS` is gone
//! (`verbatim_blob_present == false`).
//!
//! The split (G2 AMENDMENT):
//! - HARD: the scan ALGORITHM is the blob-free named primitive; the emitted
//!   drivers INVOKE it with the grammar-derived ARGS.
//! - STRUCTURAL+BEHAVIORAL: the drivers + lazy projection are fact-keyed
//!   `push_str`-style emitted blocks, held to 9-field cssparser oracle parity
//!   (NOT byte-equivalence vs the retired `CssFullParser`).

use crate::lower::css_scan::{CssScanProgram, ScanAlphabet};
use std::fmt::Write;

mod drivers;
mod projection;

/// Render a structural byte as a Rust byte literal token (`b'{'`).
pub(crate) fn byte_literal(byte: u8) -> String {
    let inner = match byte {
        b'\\' => "\\\\".to_string(),
        b'\'' => "\\'".to_string(),
        0x20..=0x7e => (byte as char).to_string(),
        other => format!("\\x{other:02x}"),
    };
    format!("b'{inner}'")
}

/// Render a `&[u8]` byte-string literal from a slice (`b";{}"`).
pub(crate) fn byte_string_literal(bytes: &[u8]) -> String {
    let mut out = String::from("b\"");
    for &byte in bytes {
        match byte {
            b'"' => out.push_str("\\\""),
            b'\\' => out.push_str("\\\\"),
            0x20..=0x7e => out.push(byte as char),
            other => {
                let _ = write!(out, "\\x{other:02x}");
            }
        }
    }
    out.push('"');
    out
}

/// Render the CSS direct-scan `generated.rs` body for `program`.
pub fn render(program: &CssScanProgram) -> Result<String, String> {
    let mut out = String::new();
    render_header(program, &mut out);
    render_alphabet_consts(&program.alphabet, &mut out);
    projection::render_node_kind(&mut out);
    projection::render_node_view(&mut out);
    projection::render_typed_node(&mut out);
    projection::render_rule_view(&mut out);
    projection::render_declaration_view(&mut out);
    projection::render_typed_value(&mut out);
    projection::render_document(&mut out);
    projection::render_summaries(&mut out);
    render_parse_entry(&mut out);
    render_emit_full_parse(&mut out);
    drivers::render_full_parser(program, &mut out);
    projection::render_lazy_helpers(&program.alphabet, &mut out);
    Ok(out)
}

fn render_header(program: &CssScanProgram, out: &mut String) {
    let _ = writeln!(
        out,
        "// css scan lowered from grammar AST: grammar={} entry={} at_rule_branches={}",
        program.grammar_name,
        program.entry_rule,
        program.at_rule_branches.join("|"),
    );
    out.push_str(
        r#"use crate::tape::{OffsetFlags, Tape, TapeBuilder, ValueRef};
use crate::runtime_scan::{css_balanced_component_scan, ScanError};
use super::config;
use super::sink::CssFactError;

/// Structural-role flag: set on a rule cursor whose opener is an at-rule
/// (`@media`, `@keyframes`, ...). A clear flag marks a qualified rule. This is
/// a `BackendRule` branch-tag projection (at-rule branch vs qualified branch),
/// stored in the EXISTING sparse `flag_cursors`/`flag_values` pair.
const AT_RULE_FLAG: u8 = OffsetFlags::GRAMMAR_BIT0;

#[inline]
fn scan_error_into(err: ScanError) -> CssFactError {
    CssFactError {
        offset: err.offset,
        message: err.message,
    }
}

"#,
    );
}

/// The grammar-DERIVED structural alphabet constants. These are the ARGS the
/// `css_balanced_component_scan` primitive takes — every byte harvested from a
/// `.bbnf` rule literal, NONE a literal in the emitter source.
fn render_alphabet_consts(alphabet: &ScanAlphabet, out: &mut String) {
    out.push_str("// Grammar-derived structural alphabet (lower::css_scan::ScanAlphabet).\n");
    out.push_str("// Each byte-set is harvested from the CSS L4 rule literals — a `.bbnf`\n");
    out.push_str("// mutation of the invoking rule changes the emitted constant.\n");

    // The fixed-9 SKIP alphabet (quotes + comment lead + bracket pairs).
    let skip = alphabet.skip_alphabet();
    let _ = writeln!(
        out,
        "/// SKIP family from `cssString` quotes, the `@ws` comment lead, and the\n\
         /// `()`/`[]`/`ruleBlock` bracket pairs.\n\
         const SKIP_ALPHABET: [u8; 9] = {};",
        byte_string_literal_array(&skip),
    );

    // The per-call-site STOP sets.
    let _ = writeln!(
        out,
        "/// STOP at an at-rule prelude: `;` (`atRuleBody`) + `{{`/`}}` (`ruleBlock`).\n\
         const AT_RULE_STOP: &[u8] = {};",
        byte_string_literal(&alphabet.at_rule_stop()),
    );
    let _ = writeln!(
        out,
        "/// STOP at a qualified-rule / block-item: `{{` `}}` `;`.\n\
         const QUALIFIED_STOP: &[u8] = {};",
        byte_string_literal(&alphabet.qualified_stop()),
    );
    let _ = writeln!(
        out,
        "/// STOP at a declaration value: `;` `}}`.\n\
         const DECLARATION_STOP: &[u8] = {};",
        byte_string_literal(&alphabet.declaration_stop()),
    );
    let _ = writeln!(
        out,
        "/// STOP at the colon-before scan: `:` `{{` `}}` `;`.\n\
         const COLON_BEFORE_STOP: &[u8] = {};",
        byte_string_literal(&alphabet.colon_before_stop()),
    );
    // The individual structural bytes the drivers dispatch on.
    let _ = writeln!(out, "const BLOCK_OPEN: u8 = {};", byte_literal(alphabet.block_open));
    let _ = writeln!(out, "const BLOCK_CLOSE: u8 = {};", byte_literal(alphabet.block_close));
    let _ = writeln!(out, "const TERMINATOR: u8 = {};", byte_literal(alphabet.terminator));
    let _ = writeln!(out, "const COLON: u8 = {};", byte_literal(alphabet.colon));
    out.push('\n');
}

/// Render a fixed-size `[u8; N]` array literal.
fn byte_string_literal_array(bytes: &[u8]) -> String {
    let mut out = String::from("[");
    for (i, &byte) in bytes.iter().enumerate() {
        if i > 0 {
            out.push_str(", ");
        }
        out.push_str(&byte_literal(byte));
    }
    out.push(']');
    out
}

/// The track-1 parse entry + structural reserve (behavioral template).
fn render_parse_entry(out: &mut String) {
    out.push_str(
        r#"/// Track-1 entry: recognize the stylesheet and emit every structural event
/// into the existing offset tape, returning the lazy tape-backed document.
pub fn parse_into_tape(input: &str) -> Result<CssDocument<'_>, CssFactError> {
    let bytes = input.as_bytes();
    let mut builder = TapeBuilder::new(bytes, structural_reserve(bytes.len()));
    CssFullParser::new(input, &mut builder).parse_stylesheet()?;
    Ok(CssDocument {
        tape: builder.finish(),
    })
}

/// Conservative byte-proportional structural capacity bound.
#[inline]
fn structural_reserve(byte_len: usize) -> usize {
    (byte_len / 8).max(16)
}

"#,
    );
}

/// The diagnostic `emit_full_parse` roll-up (behavioral template).
fn render_emit_full_parse(out: &mut String) {
    out.push_str(
        r#"const FULL_PARSE_SCHEMA: &str = "css-l4-full-parse-v1";
const FULL_PARSE_OUTPUT_PLANE: &str = "css_l4_full_parse";

pub fn emit_full_parse(input: &str) -> Result<String, CssFactError> {
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

"#,
    );
}
