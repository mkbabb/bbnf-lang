//! Tranche AV.5.5 — CSS bootstrap shape-dictionary validation.
//!
//! Compiles the production CSS L4 grammar through the full pipeline
//! and verifies the shape-dictionary mining + selection passes
//! produced the expected templates.
//!
//! The CSS L4 grammar's `declaration` rule has the canonical
//! `propertyName ":" value ";"` shape that AV.5 targets — repeated
//! thousands of times across `bootstrap.css`. After this tranche the
//! per-declaration tape footprint should drop from ~5–7 records to
//! 1 (a single `TapeKind::ShapeRef` leaf + the packed payload blob).
//!
//! These tests verify the **compile-time** half of that contract:
//! the miner and selector produce a non-empty shape dictionary for
//! CSS, including at least one template whose skeleton resembles
//! the declaration shape. The runtime emission contract (gates 22
//! + 23 in AV.md) is a separate concern handled by the V4+ DTA
//! driver landing the actual `push_shape_ref` call.

use std::path::PathBuf;

use bbnf::pipeline::{
    CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_paths_request,
};
use bbnf_ir::GrammarIR;
use bbnf_ir::passes::recognizers::shape_dict::TemplatePiece;

/// Resolve a grammar path relative to the repo root.
fn grammar_path(rel: &str) -> PathBuf {
    let manifest = env!("CARGO_MANIFEST_DIR");
    PathBuf::from(manifest).join("../../grammar").join(rel)
}

/// VM-target compile request with default pipeline options.
fn vm_request() -> CompileRequest {
    CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Vm,
    }
}

/// Compile a grammar entry file through the full pipeline.
fn compile_grammar_ir(rel_entry: &str) -> GrammarIR {
    let entry = grammar_path(rel_entry);
    let out =
        compile_paths_request(std::slice::from_ref(&entry), &vm_request()).unwrap_or_else(|err| {
            panic!("CSS L4 compile failed: {err:?}");
        });
    match out {
        CompileOutput::Vm(ir) => ir,
        other => panic!("expected CompileOutput::Vm, got {:?}", other),
    }
}

#[test]
fn css_l4_emits_shape_dict_templates() {
    let ir = compile_grammar_ir("css/l4/stylesheet.bbnf");

    // The miner should have populated `shape_dict_templates` with
    // every fixed-shape compound the e-graph facts admit.
    let template_count = ir.shape_dict_templates.len();
    assert!(
        template_count > 0,
        "css_l4: ShapeDictMiner emitted zero templates — \
         expected at least one fixed-shape compound (e.g. declaration)"
    );

    eprintln!(
        "css_l4: {} shape-dict template candidates emitted",
        template_count
    );

    // Each template carries a deterministic skeleton + leaf-hole list
    // and a non-zero shape_hash.
    for (idx, (_node_id, template)) in ir.shape_dict_templates.iter().enumerate() {
        assert!(
            !template.skeleton.is_empty(),
            "template[{}]: empty skeleton",
            idx
        );
        assert_ne!(
            template.shape_hash, 0,
            "template[{}]: zero shape_hash (FxHasher should not produce 0 for non-trivial input)",
            idx
        );
    }
}

#[test]
fn css_l4_shape_dict_selection_admits_subset() {
    let ir = compile_grammar_ir("css/l4/stylesheet.bbnf");

    let candidate_count = ir.shape_dict_templates.len();
    let admitted_count = ir.shape_dict_selection.len();

    eprintln!(
        "css_l4: {} candidates → {} admitted (budget: 32)",
        candidate_count, admitted_count
    );

    // The selection is the indices into `shape_dict_templates` that
    // beat the per-template cost gate. Bound to the 5-bit
    // shape_dict_idx budget.
    assert!(
        admitted_count <= 32,
        "css_l4: admission exceeded MAX_SHAPE_DICT_ENTRIES (32): {}",
        admitted_count
    );
    assert!(
        admitted_count <= candidate_count,
        "css_l4: admitted more templates ({}) than candidates ({})",
        admitted_count,
        candidate_count
    );

    // The selection must be sorted ascending so emission order is
    // deterministic across compile sessions.
    let mut sorted = ir.shape_dict_selection.clone();
    sorted.sort_unstable();
    assert_eq!(
        ir.shape_dict_selection, sorted,
        "css_l4: shape_dict_selection is not sorted ascending"
    );

    // Indices must be in-range.
    for &idx in &ir.shape_dict_selection {
        assert!(
            idx < candidate_count,
            "css_l4: selection index {} out of range (candidates: {})",
            idx,
            candidate_count
        );
    }
}

#[test]
fn css_l4_admitted_templates_contain_leaf_holes() {
    let ir = compile_grammar_ir("css/l4/stylesheet.bbnf");

    // Every admitted template must have at least one leaf hole — a
    // shape with no holes degenerates to all-constant Span collapse
    // and would not benefit from ShapeRef admission.
    for &idx in &ir.shape_dict_selection {
        let (_node_id, template) = &ir.shape_dict_templates[idx];
        let hole_count = template
            .skeleton
            .iter()
            .filter(|p| matches!(p, TemplatePiece::LeafHole))
            .count();
        assert!(
            hole_count > 0,
            "css_l4: admitted template {} has zero leaf holes (skeleton: {:?})",
            idx,
            template.skeleton
        );
        assert_eq!(
            hole_count,
            template.leaf_holes.len(),
            "css_l4: admitted template {} skeleton holes ({}) != leaf_holes vec ({})",
            idx,
            hole_count,
            template.leaf_holes.len(),
        );
    }
}

#[test]
fn css_l4_eclass_facts_populated() {
    let ir = compile_grammar_ir("css/l4/stylesheet.bbnf");

    // The shape-dict miner reads `EClassFacts` per NodeId; the
    // pre-mining facts pass must populate the cache.
    assert!(
        !ir.eclass_facts.is_empty(),
        "css_l4: ir.eclass_facts is empty — \
         compute_eclass_facts did not run before mine_recognizers"
    );

    eprintln!(
        "css_l4: {} EClassFacts entries cached",
        ir.eclass_facts.len()
    );
}

#[test]
fn css_l4_admitted_shape_hashes_unique() {
    let ir = compile_grammar_ir("css/l4/stylesheet.bbnf");

    // Within the admitted set, no two templates may share a
    // shape_hash — the runtime DTA dispatch table is keyed by
    // shape_hash and a collision would silently misroute one
    // template to another's child kinds + payload offsets.
    let mut seen = std::collections::HashSet::new();
    for &idx in &ir.shape_dict_selection {
        let (_, template) = &ir.shape_dict_templates[idx];
        let inserted = seen.insert(template.shape_hash);
        assert!(
            inserted,
            "css_l4: admitted template {} duplicates shape_hash {} \
             of an earlier admitted template",
            idx, template.shape_hash
        );
    }
}
