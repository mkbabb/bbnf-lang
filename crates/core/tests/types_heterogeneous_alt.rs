//! AZ-III.W3a.3 — End-to-end heterogeneous Alt obligation surfacing.
//!
//! Loads the production JSON grammar through the full compile
//! pipeline and asserts that:
//!
//! 1. `GrammarIR::type_obligations` contains at least one
//!    `TypeObligation::HeterogeneousAltJoin` entry — the `value =
//!    object | array | string | number | bool | null` Alt is the
//!    canonical heterogeneous join, every JSON parse exercises it.
//! 2. The obligation's `branches` list is non-trivial (>= 2
//!    distinct types) and contains the deduplicated branch types
//!    the CSP saw at join time.
//! 3. The `value` rule's projected type is
//!    `TypeDesc::HeterogeneousAltJoin(_)` — not the historical
//!    silent `BoxedEnum` fallback.
//!
//! Per AZ-III invariant 7 (no silent fallback) and W3a hard gate 4
//! (`rg -n BoxedEnum crates/ir/src/passes/types/constraint/`
//! returns no live silent-fallback hit), this test is the
//! consumer-facing proof that the obligation surfaces in the
//! production diagnostic stream.

use std::path::PathBuf;

use bbnf::pipeline::{
    CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_paths_request,
};
use bbnf_ir::{GrammarIR, TypeDesc, TypeObligation};

fn json_grammar_path() -> PathBuf {
    let manifest = env!("CARGO_MANIFEST_DIR");
    PathBuf::from(manifest).join("../../grammar/json/json.bbnf")
}

fn compile_json_ir() -> GrammarIR {
    let entry = json_grammar_path();
    let request = CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Vm,
    };
    let out = compile_paths_request(std::slice::from_ref(&entry), &request)
        .expect("compile_paths_request failed for grammar/json/json.bbnf");
    match out {
        CompileOutput::Vm(ir) => ir,
        other => panic!("expected VM output for JSON, got {other:?}"),
    }
}

#[test]
fn json_value_alt_lifts_named_obligation() {
    let ir = compile_json_ir();

    assert!(
        !ir.type_obligations.is_empty(),
        "JSON `value = object | array | string | number | bool | null` is the \
         canonical heterogeneous Alt and must surface at least one \
         TypeObligation entry. Empty stream means the silent BoxedEnum \
         fallback at revise.rs:123 (or the W3a.2 reference.rs:74 site) has \
         resurrected.",
    );

    // At least one obligation must be a heterogeneous-Alt join.
    // Other obligations (UnresolvedCompoundRef from W3a.2) may also
    // surface for the JSON grammar's recursive `value -> array ->
    // value` cycle; we only care that the heterogeneous lane fires.
    let mut saw_heterogeneous = false;
    for ob in &ir.type_obligations {
        if let TypeObligation::HeterogeneousAltJoin { branches, .. } = ob {
            saw_heterogeneous = true;
            assert!(
                branches.len() >= 2,
                "obligation must carry >= 2 distinct branch types; got {:?}",
                branches,
            );
            // Dedup invariant: the lifted set is structurally distinct.
            let mut seen: Vec<&TypeDesc> = Vec::new();
            for b in branches {
                assert!(
                    !seen.contains(&b),
                    "obligation branch list must be deduplicated; \
                     saw {:?} twice in {:?}",
                    b,
                    branches,
                );
                seen.push(b);
            }
        }
    }
    assert!(
        saw_heterogeneous,
        "expected at least one TypeObligation::HeterogeneousAltJoin in stream; got {:?}",
        ir.type_obligations,
    );
}

#[test]
fn json_value_rule_projects_heterogeneous_alt_join() {
    let ir = compile_json_ir();
    let value_rule = ir
        .find_rule("value")
        .expect("JSON grammar must define `value`");
    let value_type = ir
        .types
        .iter()
        .find(|(rid, _)| *rid == value_rule.id)
        .map(|(_, t)| t)
        .expect("`value` rule must have a projected type");

    match value_type {
        TypeDesc::HeterogeneousAltJoin(branches) => {
            assert!(
                branches.len() >= 2,
                "JSON `value` must lift its distinct branch types into the \
                 obligation; got branches = {:?}",
                branches,
            );
        }
        other => panic!(
            "JSON `value` rule must project to TypeDesc::HeterogeneousAltJoin \
             — silent BoxedEnum fallback resurrected. Got: {:?}",
            other,
        ),
    }
}
