//! Benchmarks the BBNF compile pipeline — grammar → IR → passes.
//!
//! Measures compile-time (CSP passes, lowering, optimization) not parse-time.
//! Covers simple grammars (JSON, regex), medium (@import: BBNF, Sheets),
//! and stress tests (CSS L4: 15 files, 973 lines, deep import chain).

use bencher::{Bencher, benchmark_group, benchmark_main, black_box};

use bbnf::pipeline::{
    CompileRequest, CompileTarget, PipelineOptions, compile_grammar_request,
    compile_paths_request,
};

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
    std::path::PathBuf::from(manifest).join("../../grammar").join(name)
}

fn load_grammar(name: &str) -> String {
    std::fs::read_to_string(grammar_path(name))
        .unwrap_or_else(|e| panic!("{name}: {e}"))
}

// ── Simple grammars (no @import) ────────────────────────────────────────────

// JSON: 9 rules, ~30 lines
fn compile_json(b: &mut Bencher) {
    let source = load_grammar("json/json.bbnf");
    b.iter(|| black_box(compile_grammar_request(&source, &vm_request()).unwrap()));
}

// EBNF: 51 lines
fn compile_ebnf(b: &mut Bencher) {
    let source = load_grammar("ebnf/ebnf.bbnf");
    b.iter(|| black_box(compile_grammar_request(&source, &vm_request()).unwrap()));
}

// CSS monolithic: 69 lines, single file (no imports), @pretty directives
fn compile_css_mono(b: &mut Bencher) {
    let source = load_grammar("css/pretty.bbnf");
    b.iter(|| black_box(compile_grammar_request(&source, &vm_request()).unwrap()));
}

// ── @import grammars ────────────────────────────────────────────────────────

// BBNF: 80 lines, self-describing, uses @import
fn compile_bbnf(b: &mut Bencher) {
    let path = grammar_path("bbnf/bbnf.bbnf");
    b.iter(|| black_box(compile_paths_request(&[path.clone()], &vm_request()).unwrap()));
}

// Google Sheets: 115 lines, uses @import
fn compile_sheets(b: &mut Bencher) {
    let path = grammar_path("google-sheets/google-sheets.bbnf");
    b.iter(|| black_box(compile_paths_request(&[path.clone()], &vm_request()).unwrap()));
}

// CSS L4: 15 files, 973 lines, deep @import chain — THE stress test.
// Exercises type inference, FIRST/FOLLOW, dispatch tables across modules.
fn compile_css_l4(b: &mut Bencher) {
    let path = grammar_path("css/l4/stylesheet.bbnf");
    b.iter(|| black_box(compile_paths_request(&[path.clone()], &vm_request()).unwrap()));
}

benchmark_group!(
    compile,
    compile_json,
    compile_ebnf,
    compile_css_mono,
    compile_bbnf,
    compile_sheets,
    compile_css_l4,
);
benchmark_main!(compile);
