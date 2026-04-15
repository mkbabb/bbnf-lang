//! Grammar round-trip snapshot gate.
//!
//! For each production grammar under `grammar/`, parse the source
//! through `bbnf::grammar::parse` and lower it via
//! `bbnf::pipeline::compile_paths_request`, then assert the resulting
//! `GrammarIR`'s `rules.len()` matches a frozen snapshot constant.
//! Any deviation is a silent lowering regression.
//!
//! # Canonical grammar entry points
//!
//! The six production grammars covered by this gate mirror the
//! bench + prettify integration suites in `crates/core/tests/` and
//! `crates/core/benches/`:
//!
//! - `bbnf/bbnf.bbnf`          — the self-hosted BBNF grammar
//! - `json/json.bbnf`          — JSON
//! - `ebnf/ebnf.bbnf`          — EBNF
//! - `css/pretty.bbnf`         — CSS prettifier grammar
//! - `css/l4/stylesheet.bbnf`  — CSS L4 (multi-file via `@import`)
//! - `google-sheets/google-sheets.bbnf` — Google Sheets formula
//!
//! All six dispatch through the unified `compile_paths_request`
//! helper.

use std::path::PathBuf;

use bbnf::pipeline::{
    CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_paths_request,
};
use bbnf_ir::GrammarIR;

// ---------------------------------------------------------------------
// Frozen rule-count snapshots. Update any constant only when the
// corresponding grammar is intentionally edited.
// ---------------------------------------------------------------------

const BBNF_RULE_COUNT: usize = 52;
const JSON_RULE_COUNT: usize = 10;
const EBNF_RULE_COUNT: usize = 14;
const CSS_PRETTY_RULE_COUNT: usize = 20;
const CSS_L4_RULE_COUNT: usize = 195;
const GOOGLE_SHEETS_RULE_COUNT: usize = 38;

// ---------------------------------------------------------------------
// Shared helpers.
// ---------------------------------------------------------------------

/// Resolve a grammar path relative to the `grammar/` directory at the
/// repo root. `CARGO_MANIFEST_DIR` points at `crates/core/`, so we
/// climb two levels to reach the workspace root.
fn grammar_path(name: &str) -> PathBuf {
    let manifest = env!("CARGO_MANIFEST_DIR");
    PathBuf::from(manifest).join("../../grammar").join(name)
}

/// Build a VM-target compile request with default pipeline options.
fn vm_request() -> CompileRequest {
    CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Vm,
    }
}

/// Parse + lower a grammar through the universal `compile_paths_request`
/// entry point. This handles both single-file grammars and `@import`-
/// split module graphs uniformly — the loader only invokes the
/// `@import` resolver when the entry file actually contains import
/// directives, so there is no reason to fork the test helper across
/// two pipeline functions. Non-import grammars just take the trivial
/// loader path.
fn compile_entry(rel_entry: &str) -> GrammarIR {
    let entry = grammar_path(rel_entry);
    let out = compile_paths_request(std::slice::from_ref(&entry), &vm_request())
        .unwrap_or_else(|err| {
            panic!("grammar_roundtrip: compile_paths_request failed for {rel_entry}: {err}")
        });
    match out {
        CompileOutput::Vm(ir) => ir,
        other => panic!("grammar_roundtrip: expected Vm output for {rel_entry}, got {other:?}"),
    }
}

// ---------------------------------------------------------------------
// Round-trip gates (one per production grammar).
// ---------------------------------------------------------------------

#[test]
fn bbnf_grammar_roundtrip() {
    let ir = compile_entry("bbnf/bbnf.bbnf");
    assert_eq!(
        ir.rules.len(),
        BBNF_RULE_COUNT,
        "bbnf.bbnf rule count drifted from frozen snapshot — \
         update BBNF_RULE_COUNT in crates/core/tests/grammar_roundtrip.rs \
         only if the grammar itself was intentionally edited"
    );
}

#[test]
fn json_grammar_roundtrip() {
    let ir = compile_entry("json/json.bbnf");
    assert_eq!(
        ir.rules.len(),
        JSON_RULE_COUNT,
        "json.bbnf rule count drifted from frozen snapshot — \
         update JSON_RULE_COUNT in crates/core/tests/grammar_roundtrip.rs \
         only if the grammar itself was intentionally edited"
    );
}

#[test]
fn ebnf_grammar_roundtrip() {
    let ir = compile_entry("ebnf/ebnf.bbnf");
    assert_eq!(
        ir.rules.len(),
        EBNF_RULE_COUNT,
        "ebnf.bbnf rule count drifted from frozen snapshot — \
         update EBNF_RULE_COUNT in crates/core/tests/grammar_roundtrip.rs \
         only if the grammar itself was intentionally edited"
    );
}

#[test]
fn css_pretty_grammar_roundtrip() {
    let ir = compile_entry("css/pretty.bbnf");
    assert_eq!(
        ir.rules.len(),
        CSS_PRETTY_RULE_COUNT,
        "css/pretty.bbnf rule count drifted from frozen snapshot — \
         update CSS_PRETTY_RULE_COUNT in crates/core/tests/grammar_roundtrip.rs \
         only if the grammar itself was intentionally edited"
    );
}

#[test]
fn css_l4_grammar_roundtrip() {
    // CSS L4 is split across many modules under `grammar/css/l4/`
    // and `compile_paths_request` walks the `@import` graph from the
    // entry point.
    let ir = compile_entry("css/l4/stylesheet.bbnf");
    assert_eq!(
        ir.rules.len(),
        CSS_L4_RULE_COUNT,
        "css/l4/stylesheet.bbnf rule count drifted from frozen snapshot — \
         update CSS_L4_RULE_COUNT in crates/core/tests/grammar_roundtrip.rs \
         only if the grammar itself was intentionally edited"
    );
}

#[test]
fn google_sheets_grammar_roundtrip() {
    let ir = compile_entry("google-sheets/google-sheets.bbnf");
    assert_eq!(
        ir.rules.len(),
        GOOGLE_SHEETS_RULE_COUNT,
        "google-sheets.bbnf rule count drifted from frozen snapshot — \
         update GOOGLE_SHEETS_RULE_COUNT in crates/core/tests/grammar_roundtrip.rs \
         only if the grammar itself was intentionally edited"
    );
}
