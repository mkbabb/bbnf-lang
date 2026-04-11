//! Tranche AF.0 Wave 1 — grammar round-trip snapshot gate.
//!
//! For each production grammar under `grammar/`, parse the source
//! through `bbnf::grammar::parse` (the tape-first bootstrap entry
//! point) and lower it via `bbnf::pipeline::compile_paths_request`,
//! then assert the resulting `GrammarIR`'s `rules.len()` matches a
//! frozen snapshot constant.
//!
//! The snapshot is the substrate-break contract: once Wave 1A/1B/1C
//! rewrite the hand-patched sub-variant references in
//! `lower/expression.rs`, `lower/value_expr.rs`, and
//! `grammar/host.rs`+`graph/*`, and Wave 2 regenerates
//! `grammar/generated.rs` cleanly from `bbnf.bbnf`, the rule count
//! for every production grammar must match the pre-Tranche-AF
//! baseline exactly. Any deviation is a silent lowering regression.
//!
//! # Why this is `#[ignore]`-gated during Wave 1
//!
//! Wave 2 is the integration step where the clean regen lands. Until
//! Wave 2:
//!
//! - `generated.rs` in this worktree still embeds the old bootstrap
//!   numbering that the hand-patched sub-variant references depend on
//! - The rule-count snapshots below are placeholders
//!   (`usize::MAX`) — the actual canonical values will be frozen
//!   by the Wave 2 orchestrator once the clean regen lands and every
//!   grammar compiles successfully end-to-end
//!
//! Marking each test with `#[ignore]` rather than a `cfg` feature
//! gate keeps the test binary *compiled* — which is the minimum bar
//! the Wave 1D deliverable must meet — without running against
//! placeholder constants. The orchestrator will remove the
//! `#[ignore]` attributes and populate the `TODO_WAVE2_…_COUNT`
//! constants with real rule-count values in Wave 2.
//!
//! # Canonical grammar entry points
//!
//! The six production grammars covered by this gate mirror the
//! bench + prettify integration suites in `crates/core/tests/` and
//! `crates/core/benches/`:
//!
//! - `bbnf/bbnf.bbnf`          — the self-hosted BBNF grammar
//!   (bootstrap source of truth for `generated.rs`; pulls in
//!   `expressions.bbnf` and `types.bbnf` via `@import`)
//! - `json/json.bbnf`          — JSON
//! - `ebnf/ebnf.bbnf`          — EBNF
//! - `css/pretty.bbnf`         — CSS prettifier grammar
//! - `css/l4/stylesheet.bbnf`  — CSS L4 (multi-file via `@import`)
//! - `google-sheets/google-sheets.bbnf` — Google Sheets formula
//!
//! All six dispatch through the unified `compile_paths_request`
//! helper: its loader short-circuits on grammars without `@import`
//! directives, so the function is the universal pipeline entry and
//! the test helper has no reason to fork across two call sites.

use std::path::PathBuf;

use bbnf::pipeline::{
    CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_paths_request,
};
use bbnf_ir::GrammarIR;

// ---------------------------------------------------------------------
// Frozen snapshot constants — Wave 2 will replace these placeholders.
// ---------------------------------------------------------------------
//
// FIXME(AF.0 Wave 2): replace each `usize::MAX` sentinel with the
// actual `ir.rules.len()` produced by the post-regen pipeline. The
// Wave 2 orchestrator captures these values once the clean regen
// lands; until then, the assertions below are unreachable guards
// (any real count compared against `usize::MAX` fails deterministically
// — which is why the tests are `#[ignore]`d during Wave 1).

const TODO_WAVE2_BBNF_RULE_COUNT: usize = usize::MAX;
const TODO_WAVE2_JSON_RULE_COUNT: usize = usize::MAX;
const TODO_WAVE2_EBNF_RULE_COUNT: usize = usize::MAX;
const TODO_WAVE2_CSS_PRETTY_RULE_COUNT: usize = usize::MAX;
const TODO_WAVE2_CSS_L4_RULE_COUNT: usize = usize::MAX;
const TODO_WAVE2_GOOGLE_SHEETS_RULE_COUNT: usize = usize::MAX;

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
#[ignore = "Tranche AF.0 Wave 2: blocked on clean regen"]
fn bbnf_grammar_roundtrip() {
    let ir = compile_entry("bbnf/bbnf.bbnf");
    assert_eq!(
        ir.rules.len(),
        TODO_WAVE2_BBNF_RULE_COUNT,
        "bbnf.bbnf rule count drifted from frozen snapshot — \
         update TODO_WAVE2_BBNF_RULE_COUNT in crates/core/tests/grammar_roundtrip.rs \
         only if the grammar itself was intentionally edited"
    );
}

#[test]
#[ignore = "Tranche AF.0 Wave 2: blocked on clean regen"]
fn json_grammar_roundtrip() {
    let ir = compile_entry("json/json.bbnf");
    assert_eq!(
        ir.rules.len(),
        TODO_WAVE2_JSON_RULE_COUNT,
        "json.bbnf rule count drifted from frozen snapshot — \
         update TODO_WAVE2_JSON_RULE_COUNT in crates/core/tests/grammar_roundtrip.rs \
         only if the grammar itself was intentionally edited"
    );
}

#[test]
#[ignore = "Tranche AF.0 Wave 2: blocked on clean regen"]
fn ebnf_grammar_roundtrip() {
    let ir = compile_entry("ebnf/ebnf.bbnf");
    assert_eq!(
        ir.rules.len(),
        TODO_WAVE2_EBNF_RULE_COUNT,
        "ebnf.bbnf rule count drifted from frozen snapshot — \
         update TODO_WAVE2_EBNF_RULE_COUNT in crates/core/tests/grammar_roundtrip.rs \
         only if the grammar itself was intentionally edited"
    );
}

#[test]
#[ignore = "Tranche AF.0 Wave 2: blocked on clean regen"]
fn css_pretty_grammar_roundtrip() {
    let ir = compile_entry("css/pretty.bbnf");
    assert_eq!(
        ir.rules.len(),
        TODO_WAVE2_CSS_PRETTY_RULE_COUNT,
        "css/pretty.bbnf rule count drifted from frozen snapshot — \
         update TODO_WAVE2_CSS_PRETTY_RULE_COUNT in crates/core/tests/grammar_roundtrip.rs \
         only if the grammar itself was intentionally edited"
    );
}

#[test]
#[ignore = "Tranche AF.0 Wave 2: blocked on clean regen"]
fn css_l4_grammar_roundtrip() {
    // CSS L4 is split across many modules under `grammar/css/l4/`
    // and `compile_paths_request` walks the `@import` graph from the
    // entry point.
    let ir = compile_entry("css/l4/stylesheet.bbnf");
    assert_eq!(
        ir.rules.len(),
        TODO_WAVE2_CSS_L4_RULE_COUNT,
        "css/l4/stylesheet.bbnf rule count drifted from frozen snapshot — \
         update TODO_WAVE2_CSS_L4_RULE_COUNT in crates/core/tests/grammar_roundtrip.rs \
         only if the grammar itself was intentionally edited"
    );
}

#[test]
#[ignore = "Tranche AF.0 Wave 2: blocked on clean regen"]
fn google_sheets_grammar_roundtrip() {
    let ir = compile_entry("google-sheets/google-sheets.bbnf");
    assert_eq!(
        ir.rules.len(),
        TODO_WAVE2_GOOGLE_SHEETS_RULE_COUNT,
        "google-sheets.bbnf rule count drifted from frozen snapshot — \
         update TODO_WAVE2_GOOGLE_SHEETS_RULE_COUNT in crates/core/tests/grammar_roundtrip.rs \
         only if the grammar itself was intentionally edited"
    );
}
