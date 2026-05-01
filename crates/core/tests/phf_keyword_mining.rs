//! AW-IV.W3.2 — Validation tests for PHF keyword mining recalibration
//! and AltLinear consumer wiring.
//!
//! Verifies that:
//! 1. The [`bbnf_ir::passes::recognizers::keyword_stats`] miner, post-
//!    W3.2 `Ref`-following extension of `leading_literal`, surfaces
//!    literal-led branches for rule-reference Alts such as BBNF's
//!    `directive` and JSON's `value`.
//! 2. The emitter's PHF activation gate
//!    ([`bbnf::backend::rust::emitter::keyword_dispatch::
//!    emit_keyword_phf`]) fires on those mined sets with the lowered
//!    threshold of 3.
//!
//! The wire contract for the AltLinear consumer itself is exercised by
//! `cargo expand` / `nm` against the bench binaries; the runtime
//! correctness of the consumer is covered by
//! `tests/sheets_parity::operator_branches_parse` and the grammar-
//! parity suites downstream.

use bbnf::pipeline::{
    CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_paths_request,
};
use bbnf_ir::GrammarIR;
use std::path::PathBuf;

fn grammar_path(relative: &str) -> PathBuf {
    let manifest = env!("CARGO_MANIFEST_DIR");
    PathBuf::from(manifest).join("../../grammar").join(relative)
}

fn compile(rel_entry: &str) -> GrammarIR {
    let entry = grammar_path(rel_entry);
    let req = CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Vm,
    };
    let out = compile_paths_request(std::slice::from_ref(&entry), &req)
        .unwrap_or_else(|e| panic!("compile {rel_entry}: {e}"));
    match out {
        CompileOutput::Vm(ir) => ir,
        other => panic!("expected Vm output, got {other:?}"),
    }
}

#[test]
fn bbnf_directive_mines_literal_branches() {
    let ir = compile("bbnf/bbnf.bbnf");
    let dag = ir.dag.as_ref().expect("bbnf grammar must have dag");
    let directive_rule = ir
        .rules
        .iter()
        .find(|r| ir.strings[r.name as usize] == "directive")
        .expect("bbnf grammar must have a 'directive' rule");
    let body_id = dag
        .node_for(&directive_rule.body)
        .expect("directive body must have a NodeId");
    let branches = ir.keyword_branches.get(&body_id).unwrap_or_else(|| {
        panic!(
            "AW-IV.W3.2: directive should mine literal branches via Ref; \
            got no entry in keyword_branches"
        )
    });
    assert!(
        branches.len() >= 3,
        "directive should mine >= 3 literal branches (post-Ref-follow); got {}",
        branches.len()
    );
    // Sanity: the mined keywords should be directive starts like
    // `@import`, `@pretty`, etc.
    let keyword_strs: Vec<String> = branches
        .iter()
        .filter_map(|b| std::str::from_utf8(&b.bytes).ok().map(String::from))
        .collect();
    for kw in &keyword_strs {
        assert!(
            kw.starts_with('@'),
            "every directive keyword should start with '@'; got {kw:?}"
        );
    }
}

#[test]
fn bbnf_directive_generates_emitter_phf_table() {
    // Verify the emitter actually emits a PHF table for directive
    // (downstream of the miner). The Alt's 7 branches cross the
    // threshold of 3 so we expect table + dispatch fn emitted.
    use bbnf::backend::rust::emitter::keyword_dispatch::{
        LiteralBranch, PHF_MIN_BRANCHES, emit_keyword_phf,
    };
    let ir = compile("bbnf/bbnf.bbnf");
    let dag = ir.dag.as_ref().expect("bbnf grammar must have dag");
    let directive_rule = ir
        .rules
        .iter()
        .find(|r| ir.strings[r.name as usize] == "directive")
        .expect("bbnf grammar must have a 'directive' rule");
    let body_id = dag.node_for(&directive_rule.body).unwrap();
    let mined = ir.keyword_branches.get(&body_id).expect("mined");
    assert!(
        mined.len() >= PHF_MIN_BRANCHES,
        "mined count {} must meet threshold {}",
        mined.len(),
        PHF_MIN_BRANCHES
    );
    let lits: Vec<LiteralBranch> = mined
        .iter()
        .map(|b| LiteralBranch {
            bytes: b.bytes.clone(),
            branch_idx: b.branch_idx,
        })
        .collect();
    let emitted = emit_keyword_phf("BbnfBootstrap", directive_rule.id, &lits);
    assert!(
        emitted.is_some(),
        "emit_keyword_phf returned None for directive with {} mined literal branches",
        lits.len()
    );
    let tokens = emitted.unwrap().to_string();
    assert!(
        tokens.contains("__PHF_BbnfBootstrap_"),
        "emitted stream must contain __PHF_BbnfBootstrap_<id>_KW: {}",
        tokens
    );
}

#[test]
fn json_value_mines_literal_branches_via_ref() {
    let ir = compile("json/json.bbnf");
    let dag = ir.dag.as_ref().expect("json grammar must have dag");
    let value_rule = ir
        .rules
        .iter()
        .find(|r| ir.strings[r.name as usize] == "value")
        .expect("json grammar must have a 'value' rule");
    let body_id = dag
        .node_for(&value_rule.body)
        .expect("value body must have a NodeId");
    let branches = ir.keyword_branches.get(&body_id).unwrap_or_else(|| {
        panic!(
            "AW-IV.W3.2: json value should mine literal branches via Ref; \
            got no entry in keyword_branches"
        )
    });
    // The literal-led branches are `object` → `{`, `array` → `[`,
    // `null` → `null`. `bool` is an Alt of literals but `leading_literal`
    // doesn't flatten Alts, so it contributes nothing via Ref.
    assert!(
        branches.len() >= 3,
        "json value should mine >= 3 literal branches (object/array/null); got {}",
        branches.len()
    );
}
