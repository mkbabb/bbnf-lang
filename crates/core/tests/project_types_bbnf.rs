//! AZ-II.cutover.A — BBNF `project_types` registry-closure wire
//! contract.
//!
//! Loads `grammar/bbnf/bbnf.bbnf` (with its `expressions.bbnf` /
//! `types.bbnf` imports) through the existing
//! `compile_paths_request(CompileTarget::Vm)` pipeline (which runs
//! `project_types` and populates `ir.struct_registry`), then asserts
//! the registry-shape gate the cutover.A close requires:
//!
//! 1. The `populate_struct_registry` closure produces ≥ 1 layout for
//!    BBNF — the cutover.md §AZ-II.cutover.A close sub-gate 4.
//! 2. Every surviving rule in `ir.rules` has a non-empty
//!    [`StructLayout`] — same hard-fail-and-block discipline JSON /
//!    CSS L4 / Sheets enforce post-W1.A.
//! 3. The audit pass run with the `&StructRegistry` probe reports
//!    zero `Missing` markers — `Pending` stays admissible until
//!    cutover.B regens BBNF onto the struct-direct path.
//!
//! Hard-fail-and-block per the cutover.A close discipline: if
//! `project_types` fails to close on BBNF post-typed-leaf
//! annotation, the test fails with a diagnostic naming the
//! unclosed rule.

use std::path::PathBuf;

use bbnf::pipeline::{
    CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_paths_request,
};
use bbnf_ir::GrammarIR;
use bbnf_ir::passes::{GrammarAuditTag, audit_payload_coverage};

/// Resolve the BBNF grammar path from `CARGO_MANIFEST_DIR`.
fn bbnf_grammar_path() -> PathBuf {
    let manifest = env!("CARGO_MANIFEST_DIR");
    PathBuf::from(manifest).join("../../grammar/bbnf/bbnf.bbnf")
}

/// Compile BBNF through the VM target. `CompileTarget::Vm` is the
/// minimal target that runs `project_types` end-to-end and returns
/// the populated `GrammarIR`.
fn compile_bbnf_ir() -> GrammarIR {
    let entry = bbnf_grammar_path();
    let request = CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Vm,
    };
    let out = compile_paths_request(std::slice::from_ref(&entry), &request)
        .expect("compile_paths_request failed for grammar/bbnf/bbnf.bbnf");
    match out {
        CompileOutput::Vm(ir) => ir,
        other => panic!("expected VM output for BBNF, got {other:?}"),
    }
}

#[test]
fn bbnf_struct_registry_is_non_empty() {
    let ir = compile_bbnf_ir();
    assert!(
        !ir.struct_registry.is_empty(),
        "AZ-II.cutover.A sub-gate 4: populate_struct_registry returned \
         zero layouts for BBNF. Expected ≥ 1 layout once typed-leaf \
         annotations on grammar/bbnf/bbnf.bbnf are authored."
    );
}

#[test]
fn every_named_bbnf_rule_has_a_struct_layout() {
    let ir = compile_bbnf_ir();

    for rule in &ir.rules {
        let rule_name = ir.get_string(rule.name).to_string();
        let layout = ir.struct_registry.layout(rule.id).unwrap_or_else(|| {
            panic!(
                "AZ-II.cutover.A: BBNF rule `{rule_name}` (id={}) has no \
                     registered StructLayout. `project_types` did not close \
                     on this rule.",
                rule.id
            )
        });
        assert!(
            layout.field_count() > 0,
            "AZ-II.cutover.A: BBNF rule `{rule_name}` has empty StructLayout \
             (kind: {:?}). Every surviving rule must have at least one field \
             after closure.",
            layout.kind
        );
    }
}

#[test]
fn bbnf_audit_coverage_reports_no_missing_markers() {
    let ir = compile_bbnf_ir();
    let probe = &ir.struct_registry;
    let coverage = audit_payload_coverage(&ir, GrammarAuditTag::Custom("bbnf"), &probe);

    assert_eq!(
        coverage.missing_markers, 0,
        "AZ-II.cutover.A: BBNF audit reports {} `Missing` markers — \
         registry layouts present but typed-leaf shapes mismatch. \
         Missing list: {:#?}",
        coverage.missing_markers, coverage.missing,
    );
}
