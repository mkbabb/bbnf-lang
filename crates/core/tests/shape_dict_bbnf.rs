//! BBNF shape-dictionary collapse tests — AV.5.6 / AV.6.1–6.3.
//!
//! Verifies that the BBNF self-hosting grammar mines two shape
//! templates into `GrammarProfile::bbnf_shape_templates`:
//!
//! 1. **`big_comment`**: single-hole template, three-record skeleton
//!    (Rule → Repeat → Span) collapses to one ShapeRef + 8-byte
//!    payload blob (Span lo/hi).
//!
//! 2. **`mapped_factor` empty branch**: empty optional `->` mapping,
//!    structurally just `factor`. Template captures the factor tape
//!    offset; payload blob is empty.
//!
//! Tests also confirm non-BBNF grammars (JSON, CSS L4) yield no BBNF
//! shape templates — the miner is name-gated on the canonical BBNF
//! rule names and produces an empty list elsewhere.

use std::path::PathBuf;

use bbnf::pipeline::{
    compile_paths_request, CompileOutput, CompileRequest, CompileTarget, PipelineOptions,
};
use bbnf_ir::passes::recognizers::shape_dict_bbnf::{
    compute_bbnf_shape_hash, mine_bbnf_shape_templates, BbnfShapeKind,
};
use bbnf_ir::GrammarIR;

// ─── Shared helpers ──────────────────────────────────────────────────

fn grammar_path(name: &str) -> PathBuf {
    let manifest = env!("CARGO_MANIFEST_DIR");
    PathBuf::from(manifest).join("../../grammar").join(name)
}

fn vm_request() -> CompileRequest {
    CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Vm,
    }
}

fn compile_entry(rel_entry: &str) -> GrammarIR {
    let entry = grammar_path(rel_entry);
    let out = compile_paths_request(std::slice::from_ref(&entry), &vm_request())
        .unwrap_or_else(|err| {
            panic!("shape_dict_bbnf: compile_paths_request failed for {rel_entry}: {err}")
        });
    match out {
        CompileOutput::Vm(ir) => ir,
        other => panic!("shape_dict_bbnf: expected Vm output for {rel_entry}, got {other:?}"),
    }
}

// ─── BBNF shape-dictionary mining ────────────────────────────────────

#[test]
fn bbnf_self_hosting_mines_big_comment_template() {
    let ir = compile_entry("bbnf/bbnf.bbnf");
    let templates = mine_bbnf_shape_templates(&ir);

    let big_comment = templates
        .iter()
        .find(|t| t.rule_name == "big_comment")
        .expect("big_comment shape template must be mined from BBNF self-hosting");

    assert_eq!(
        big_comment.kind,
        BbnfShapeKind::BigComment,
        "big_comment must classify as BigComment kind"
    );
    assert_eq!(
        big_comment.payload_bytes, 8,
        "big_comment payload is the inner Span's (u32 lo, u32 hi) — 8 bytes"
    );
    assert_eq!(
        big_comment.shape_hash,
        compute_bbnf_shape_hash(BbnfShapeKind::BigComment),
        "shape_hash must match the canonical BigComment hash"
    );
}

#[test]
fn bbnf_self_hosting_mines_mapped_factor_template() {
    let ir = compile_entry("bbnf/bbnf.bbnf");
    let templates = mine_bbnf_shape_templates(&ir);

    let mapped_factor = templates
        .iter()
        .find(|t| t.rule_name == "mapped_factor")
        .expect("mapped_factor shape template must be mined from BBNF self-hosting");

    assert_eq!(
        mapped_factor.kind,
        BbnfShapeKind::MappedFactorEmpty,
        "mapped_factor must classify as MappedFactorEmpty kind"
    );
    assert_eq!(
        mapped_factor.payload_bytes, 0,
        "mapped_factor empty branch carries no payload — 0 bytes"
    );
    assert_eq!(
        mapped_factor.shape_hash,
        compute_bbnf_shape_hash(BbnfShapeKind::MappedFactorEmpty),
        "shape_hash must match the canonical MappedFactorEmpty hash"
    );
}

#[test]
fn bbnf_self_hosting_mines_exactly_two_templates() {
    let ir = compile_entry("bbnf/bbnf.bbnf");
    let templates = mine_bbnf_shape_templates(&ir);

    assert_eq!(
        templates.len(),
        2,
        "BBNF self-hosting must mine exactly two shape templates \
         (big_comment + mapped_factor); found {}: {:?}",
        templates.len(),
        templates.iter().map(|t| &t.rule_name).collect::<Vec<_>>()
    );
}

#[test]
fn bbnf_shape_hashes_are_distinct() {
    let big_comment_hash = compute_bbnf_shape_hash(BbnfShapeKind::BigComment);
    let mapped_factor_hash = compute_bbnf_shape_hash(BbnfShapeKind::MappedFactorEmpty);

    assert_ne!(
        big_comment_hash, mapped_factor_hash,
        "Distinct BbnfShapeKind variants must produce distinct shape hashes"
    );
}

#[test]
fn bbnf_shape_hashes_are_stable() {
    // Two independent calls with the same kind must produce the same
    // hash. This is the contract that lets the dict be looked up by
    // hash at codegen time.
    let h1 = compute_bbnf_shape_hash(BbnfShapeKind::BigComment);
    let h2 = compute_bbnf_shape_hash(BbnfShapeKind::BigComment);
    assert_eq!(h1, h2, "compute_bbnf_shape_hash must be deterministic");

    let h3 = compute_bbnf_shape_hash(BbnfShapeKind::MappedFactorEmpty);
    let h4 = compute_bbnf_shape_hash(BbnfShapeKind::MappedFactorEmpty);
    assert_eq!(h3, h4, "compute_bbnf_shape_hash must be deterministic");
}

// ─── Non-BBNF grammars yield no BBNF shape templates ────────────────

#[test]
fn json_grammar_has_no_bbnf_templates() {
    let ir = compile_entry("json/json.bbnf");
    let templates = mine_bbnf_shape_templates(&ir);

    assert!(
        templates.is_empty(),
        "JSON grammar must produce zero BBNF shape templates \
         (the miner is name-gated on big_comment/mapped_factor); \
         found {} entries",
        templates.len()
    );
}

#[test]
fn css_pretty_grammar_has_no_bbnf_templates() {
    let ir = compile_entry("css/pretty.bbnf");
    let templates = mine_bbnf_shape_templates(&ir);

    assert!(
        templates.is_empty(),
        "CSS pretty grammar must produce zero BBNF shape templates; \
         found {} entries",
        templates.len()
    );
}

#[test]
fn ebnf_grammar_has_no_bbnf_templates() {
    let ir = compile_entry("ebnf/ebnf.bbnf");
    let templates = mine_bbnf_shape_templates(&ir);

    assert!(
        templates.is_empty(),
        "EBNF grammar must produce zero BBNF shape templates; \
         found {} entries",
        templates.len()
    );
}

// ─── GrammarProfile integration ──────────────────────────────────────

#[test]
fn bbnf_self_hosting_profile_carries_templates() {
    // The accessor `GrammarIR::profile()` must populate
    // `bbnf_shape_templates` from the same miner — single source of
    // truth, no orthogonal substrate.
    let ir = compile_entry("bbnf/bbnf.bbnf");
    let profile = ir.profile();

    let names: Vec<&str> = profile
        .bbnf_shape_templates
        .iter()
        .map(|t| t.rule_name.as_str())
        .collect();

    assert!(
        names.contains(&"big_comment"),
        "GrammarProfile must include big_comment; got: {:?}",
        names
    );
    assert!(
        names.contains(&"mapped_factor"),
        "GrammarProfile must include mapped_factor; got: {:?}",
        names
    );
}

#[test]
fn json_profile_has_empty_bbnf_templates() {
    let ir = compile_entry("json/json.bbnf");
    let profile = ir.profile();
    assert!(
        profile.bbnf_shape_templates.is_empty(),
        "JSON GrammarProfile must carry zero BBNF shape templates"
    );
}
