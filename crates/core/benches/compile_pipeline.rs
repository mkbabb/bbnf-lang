//! Benchmarks the BBNF compile pipeline — grammar → IR → passes.
//!
//! Measures compile-time (CSP passes, lowering, optimization) not parse-time.
//! Covers simple grammars (JSON, regex), medium (@import: BBNF, Sheets),
//! and stress tests (CSS L4: 15 files, 973 lines, deep import chain).
//!
//! Each bench runs under a wall-clock guard (Tranche Y.-1.b) so that a
//! performance regression cannot hang CI indefinitely. See
//! `benches/common/timeout.rs` for the guard and its per-bench limits.

use bbnf::pipeline::{
    CompileRequest, CompileTarget, PipelineOptions, compile_grammar_request, compile_paths_request,
};

#[path = "common/timeout.rs"]
mod timeout;
use timeout::{bench_with_timeout, limits};

fn vm_request() -> CompileRequest {
    CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Vm,
    }
}

fn grammar_path(name: &str) -> std::path::PathBuf {
    // Resolve relative to the workspace root so profilers (samply) work
    // regardless of cwd.
    let manifest = std::env!("CARGO_MANIFEST_DIR");
    std::path::PathBuf::from(manifest)
        .join("../../grammar")
        .join(name)
}

fn load_grammar(name: &str) -> String {
    std::fs::read_to_string(grammar_path(name)).unwrap_or_else(|e| panic!("{name}: {e}"))
}

// ── Simple grammars (no @import) ────────────────────────────────────────────

// JSON: 9 rules, ~30 lines
#[divan::bench]
fn compile_json(b: divan::Bencher) {
    let source = load_grammar("json/json.bbnf");
    bench_with_timeout(
        b,
        limits::COMPILE_JSON,
        |source: String| compile_grammar_request(&source, &vm_request()).unwrap(),
        &source,
    );
}

// EBNF: 51 lines
#[divan::bench]
fn compile_ebnf(b: divan::Bencher) {
    let source = load_grammar("ebnf/ebnf.bbnf");
    bench_with_timeout(
        b,
        limits::COMPILE_EBNF,
        |source: String| compile_grammar_request(&source, &vm_request()).unwrap(),
        &source,
    );
}

// ── @import grammars ────────────────────────────────────────────────────────

// BBNF: 80 lines, self-describing, uses @import
#[divan::bench]
fn compile_bbnf(b: divan::Bencher) {
    let path = grammar_path("bbnf/bbnf.bbnf");
    bench_with_timeout(
        b,
        limits::COMPILE_BBNF,
        |path: std::path::PathBuf| compile_paths_request(&[path], &vm_request()).unwrap(),
        &path,
    );
}

// Google Sheets: 115 lines, uses @import
#[divan::bench]
fn compile_sheets(b: divan::Bencher) {
    let path = grammar_path("google-sheets/google-sheets.bbnf");
    bench_with_timeout(
        b,
        limits::COMPILE_SHEETS,
        |path: std::path::PathBuf| compile_paths_request(&[path], &vm_request()).unwrap(),
        &path,
    );
}

// CSS L4: 15 files, 973 lines, deep @import chain — THE stress test.
// Exercises type inference, FIRST/FOLLOW, dispatch tables across modules.
#[divan::bench]
fn compile_css_l4(b: divan::Bencher) {
    let path = grammar_path("css/l4/stylesheet.bbnf");
    bench_with_timeout(
        b,
        limits::COMPILE_CSS_L4,
        |path: std::path::PathBuf| compile_paths_request(&[path], &vm_request()).unwrap(),
        &path,
    );
}

fn main() {
    // Cold-per-parse (`sample_size = 1`) per workspace feedback
    // `no-warm-benches`. `skip_ext_time(true)` excludes the
    // `with_inputs` clone from the reported wall.
    divan::Divan::default()
        .sample_count(100)
        .sample_size(1)
        .skip_ext_time(true)
        .max_time(std::time::Duration::from_secs(30))
        .run_benches();
}
