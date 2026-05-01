//! Tranche Y.6b — authoritative `RegexEngine` consumption verification.
//!
//! Proves that the per-compile `GrammarIR::regex_engine_decisions`
//! sidecar (populated by `passes::csp_strategy::solve_strategy_decisions`)
//! contains entries for the production regex patterns that ship with
//! JSON and CSS L4 grammars. This is the sanity check the plan's
//! "decisions authoritative at emit sites" gate demands: if the
//! strategy CSP silently dropped a pattern's decision, the emit path
//! would fall through to `classify_regex` and drift from the CSP's
//! choice — exactly the authority gap X.11a was supposed to close.
//!
//! The test loads each grammar through the production pipeline and
//! walks `ir.regex_info` for all interned regex patterns. For every
//! pattern that has a `feasible_engines` mask, at least one engine
//! variant must be present in `ir.regex_engine_decisions` for the
//! same `StringId`. Patterns with empty feasible engines (e.g.,
//! literal-only constructions that never reached the CSP) are
//! allowed to have no decision.

use bbnf::pipeline::{
    CompileRequest, CompileTarget, PipelineOptions, compile_grammar_request, compile_paths_request,
};

fn vm_request() -> CompileRequest {
    CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Vm,
    }
}

fn grammar_path(name: &str) -> std::path::PathBuf {
    let manifest = std::env!("CARGO_MANIFEST_DIR");
    std::path::PathBuf::from(manifest)
        .join("../../grammar")
        .join(name)
}

#[test]
fn json_regex_decisions_are_populated() {
    let source = std::fs::read_to_string(grammar_path("json/json.bbnf")).unwrap();
    let out = compile_grammar_request(&source, &vm_request()).unwrap();
    let ir = match out {
        bbnf::pipeline::CompileOutput::Vm(ir) => ir,
        _ => panic!("expected Vm output"),
    };
    let ir = &ir;

    // The JSON grammar has two critical regex patterns: the string
    // body and the number body. Both must have CSP-decided engines.
    let mut seen_patterns = 0usize;
    let mut with_decision = 0usize;
    for (sid, _info) in &ir.regex_info {
        seen_patterns += 1;
        if ir.regex_engine_decisions.contains_key(sid) {
            with_decision += 1;
        }
    }

    assert!(
        seen_patterns > 0,
        "JSON grammar should have at least one regex pattern"
    );
    // Every regex pattern that reached the CSP must have a decision.
    // The test enforces the gate exactly: if a pattern had an entry
    // in `ir.regex_info`, the strategy CSP must have classified it.
    assert_eq!(
        with_decision, seen_patterns,
        "JSON: {with_decision}/{seen_patterns} regex patterns have CSP-decided engines — \
         Y.6b requires full authoritative coverage"
    );
}

#[test]
fn css_l4_regex_decisions_are_populated() {
    let path = grammar_path("css/l4/stylesheet.bbnf");
    let out = compile_paths_request(&[path], &vm_request()).unwrap();
    let ir = match out {
        bbnf::pipeline::CompileOutput::Vm(ir) => ir,
        _ => panic!("expected Vm output"),
    };
    let ir = &ir;

    let mut seen_patterns = 0usize;
    let mut with_decision = 0usize;
    for (sid, _info) in &ir.regex_info {
        seen_patterns += 1;
        if ir.regex_engine_decisions.contains_key(sid) {
            with_decision += 1;
        }
    }

    assert!(
        seen_patterns > 10,
        "CSS L4 should have many regex patterns (got {seen_patterns})"
    );
    // Tranche Y.6b gate: every regex pattern in a production grammar
    // has an authoritative CSP engine decision. This is what
    // `scanner_plan::plan_regex_scanner` consults on its primary
    // path — if the decision map is empty, the primary path returns
    // None and the fall-through `classify_regex` becomes the
    // authority instead, defeating the whole Tranche X.8d wiring.
    assert_eq!(
        with_decision, seen_patterns,
        "CSS L4: {with_decision}/{seen_patterns} regex patterns have CSP-decided engines — \
         Y.6b requires full authoritative coverage"
    );
}
