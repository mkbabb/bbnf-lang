//! Read-only e-graph firing probe (AY-audit-3).
//!
//! Compiles each production grammar under `BBNF_EGRAPH_REPORT=1` +
//! `BBNF_HIR_EGRAPH_REPORT=1` and prints the scheduler's per-rule fire
//! counts for both the grammar-tier e-graph and the HIR e-graph.
//!
//! Invocation: `cargo run --example egraph_fire_probe --release`.
//!
//! This example is NOT a production binary and carries no other
//! behaviour. It is the firing-audit reproducer for
//! `docs/tranches/AY/audit/AYW-egraph-cost-firing.md`.

use bbnf::pipeline::{
    CompileRequest, CompileTarget, PipelineOptions, compile_grammar_request, compile_paths_request,
};

fn grammar_path(name: &str) -> std::path::PathBuf {
    let manifest = std::env!("CARGO_MANIFEST_DIR");
    std::path::PathBuf::from(manifest)
        .join("../../grammar")
        .join(name)
}

fn vm_request() -> CompileRequest {
    CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Vm,
    }
}

fn main() {
    // Force-emit per-rule reports from both tiers.
    // Each compile prints one "egraph saturation:" line + N
    // "  rule=<name> work=<n>" lines. The CSS L4 compile is the
    // stress case (15 files, 973 lines); JSON and BBNF are the
    // small end; EBNF and BNF are toy scales.

    // Single-file grammars via compile_grammar_request.
    for (label, rel_path) in &[
        ("json", "json/json.bbnf"),
        ("ebnf", "ebnf/ebnf.bbnf"),
        ("bnf", "bnf/bnf.bbnf"),
    ] {
        let source = std::fs::read_to_string(grammar_path(rel_path))
            .unwrap_or_else(|e| panic!("{label}: {e}"));
        eprintln!("==== {} ====", label);
        let _ = compile_grammar_request(&source, &vm_request()).unwrap();
    }

    // @import grammars via compile_paths_request.
    for (label, rel_path) in &[
        ("bbnf", "bbnf/bbnf.bbnf"),
        ("google-sheets", "google-sheets/google-sheets.bbnf"),
        ("css_l4", "css/l4/stylesheet.bbnf"),
    ] {
        let path = grammar_path(rel_path);
        eprintln!("==== {} ====", label);
        let _ = compile_paths_request(&[path], &vm_request()).unwrap();
    }
}
