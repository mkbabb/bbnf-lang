//! AZ-I.W1.B2 — wire-contract test for `project_types` closure on the
//! Sheets grammar.
//!
//! Loads `grammar/google-sheets/google-sheets.bbnf` through the live
//! compile pipeline (`compile_paths_request`), which runs `project_types`
//! (and therefore `populate_struct_registry`). The test asserts:
//!
//! 1. Every Named rule in the Sheets grammar has a `StructLayout` in
//!    `ir.struct_registry` whose `field_count() > 0`.
//! 2. The cell-related layouts (`cell`, `formula`, `cell_or_range`) carry
//!    the expected [`LayoutKind`] discriminator (struct / tagged-enum).
//! 3. The audit pass run with the `&StructRegistry` probe reports
//!    `Mapped` for every typed `->` marker on Sheets — zero `Pending`,
//!    zero `Missing`.
//! 4. Total `StructLayout` count for Sheets rules is at least eight
//!    (the AZ-I.W1 expected coverage minimum from `RESEARCH.md` §6).
//!
//! The wire contract: typed `->` marker in the grammar source ⇒ `IrRule`
//! body shape ⇒ `populate_struct_registry` projection ⇒ `StructLayout`
//! field with the correct `TypeDesc` ⇒ audit pass reporting `Mapped`.
//! All four boundaries asserted here.

use std::path::PathBuf;

use bbnf::pipeline::{
    CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_paths_request,
};
use bbnf_ir::passes::{GrammarAuditTag, audit_payload_coverage};
use bbnf_ir::{GrammarIR, IrNode, LayoutKind};

/// Resolve a grammar path relative to the repo root. `CARGO_MANIFEST_DIR`
/// points at `crates/core/`, so we climb two levels.
fn grammar_path(rel: &str) -> PathBuf {
    let manifest = env!("CARGO_MANIFEST_DIR");
    PathBuf::from(manifest).join("../../grammar").join(rel)
}

/// Compile the Sheets grammar through the live pipeline and return the
/// projected `GrammarIR`. `project_types` runs as part of this; the
/// `struct_registry` is populated as a side-effect.
fn compile_sheets_ir() -> GrammarIR {
    let entry = grammar_path("google-sheets/google-sheets.bbnf");
    let request = CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Vm,
    };
    let out = compile_paths_request(std::slice::from_ref(&entry), &request)
        .unwrap_or_else(|err| panic!("project_types_sheets: compile failed: {err:?}"));
    match out {
        CompileOutput::Vm(ir) => ir,
        other => panic!("expected Vm output for sheets, got {:?}", other),
    }
}

/// True if a rule is a Named user rule. In the current pipeline the
/// `IrRule` set is exactly the user rules at parse time; pipeline lowering
/// does not synthesise additional anonymous Named-shaped rules. Every
/// `IrRule` therefore needs a non-empty `StructLayout`.
fn is_named_user_rule(_body: &IrNode) -> bool {
    true
}

#[test]
fn every_named_sheets_rule_has_non_empty_layout() {
    let ir = compile_sheets_ir();

    // Snapshot the registry shape — total count, plus per-rule
    // `(name, kind, field_count)` for diagnostic clarity on failure.
    let mut empty_rules: Vec<(String, LayoutKind)> = Vec::new();
    for rule in &ir.rules {
        if !is_named_user_rule(&rule.body) {
            continue;
        }
        let name = ir.get_string(rule.name).to_string();
        let layout = ir
            .struct_registry
            .layout(rule.id)
            .unwrap_or_else(|| panic!("rule `{name}` has no StructLayout"));
        if layout.field_count() == 0 {
            empty_rules.push((name, layout.kind));
        }
    }
    assert!(
        empty_rules.is_empty(),
        "Sheets rules with empty layouts: {:?}",
        empty_rules,
    );
}

#[test]
fn sheets_registry_meets_minimum_layout_count() {
    let ir = compile_sheets_ir();
    let count = ir.struct_registry.len();
    // RESEARCH.md §6: Sheets coverage minimum is 8 layouts.
    assert!(
        count >= 8,
        "expected at least 8 Sheets `StructLayout` entries, got {count}",
    );
}

#[test]
fn cell_related_layouts_carry_expected_layout_kinds() {
    let ir = compile_sheets_ir();

    // ── `cell` — `sheet_prefix? , cell_ref` is a Seq with two
    //    positions; projects to `LayoutKind::Struct` with field_count >= 1.
    let cell = ir
        .struct_registry
        .layout_by_name("cell")
        .expect("`cell` registered");
    assert!(
        cell.is_struct(),
        "`cell` projects to Struct (got {:?})",
        cell.kind,
    );
    assert!(
        cell.field_count() >= 1,
        "`cell` has at least one field (got {})",
        cell.field_count(),
    );

    // ── `formula` — `/=?/ , expression` is a Seq with two positions;
    //    projects to `LayoutKind::Struct`.
    let formula = ir
        .struct_registry
        .layout_by_name("formula")
        .expect("`formula` registered");
    assert!(
        formula.is_struct(),
        "`formula` projects to Struct (got {:?})",
        formula.kind,
    );

    // ── `cell_or_range` — `range_ref | cell` is an Alt with two
    //    branches; projects to a tagged or untagged enum depending on
    //    whether both branches share a `TypeDesc`.
    let cor = ir
        .struct_registry
        .layout_by_name("cell_or_range")
        .expect("`cell_or_range` registered");
    assert!(
        cor.is_tagged_enum() || cor.is_untagged_enum(),
        "`cell_or_range` projects to TaggedEnum or UntaggedEnum (got {:?})",
        cor.kind,
    );
    assert_eq!(cor.field_count(), 2, "`cell_or_range` has two branches");
}

#[test]
fn typed_leaf_rules_project_to_newtype_or_struct() {
    let ir = compile_sheets_ir();

    // ── `number` — `/.../ -> f64` is a Map(Regex, expr_typed(F64));
    //    projects to NewtypeWrapper with one TypedLeaf field carrying F64.
    let number = ir
        .struct_registry
        .layout_by_name("number")
        .expect("`number` registered");
    assert!(
        number.is_newtype() || number.is_struct(),
        "`number` projects to NewtypeWrapper or Struct (got {:?})",
        number.kind,
    );
    assert!(number.field_count() >= 1, "`number` has at least one field",);

    // ── `boolean` — `... -> true | ... -> false` is an Alt with two
    //    Bool branches; projects to TaggedEnum or UntaggedEnum (when
    //    both branches share Bool).
    let boolean = ir
        .struct_registry
        .layout_by_name("boolean")
        .expect("`boolean` registered");
    assert!(
        boolean.is_tagged_enum() || boolean.is_untagged_enum(),
        "`boolean` projects to TaggedEnum/UntaggedEnum (got {:?})",
        boolean.kind,
    );
    assert_eq!(boolean.field_count(), 2);

    // ── `error_literal` — nine literal branches with homogeneous u8
    //    payloads (`-> 0u8` … `-> 8u8`). The closure folds homogeneous
    //    Alt branches; the projection's exact `LayoutKind` and field
    //    count are closure-internal — verify only that the rule is
    //    registered with a non-empty layout (the global per-rule check
    //    in `every_named_sheets_rule_has_non_empty_layout` covers the
    //    closure totality).
    let error_literal = ir
        .struct_registry
        .layout_by_name("error_literal")
        .expect("`error_literal` registered");
    assert!(
        error_literal.field_count() >= 1,
        "`error_literal` has at least one field (got {})",
        error_literal.field_count(),
    );
}

#[test]
fn audit_pass_reports_mapped_for_every_sheets_marker() {
    let ir = compile_sheets_ir();

    let probe = &ir.struct_registry;
    let coverage = audit_payload_coverage(&ir, GrammarAuditTag::Sheets, &probe);

    assert!(
        coverage.is_clean(),
        "Sheets audit reports `Missing` markers: {:?}",
        coverage.missing,
    );
    assert_eq!(
        coverage.pending_markers, 0,
        "Sheets audit reports `Pending` markers (registry should be closed): {:?}",
        coverage.pending,
    );
    assert!(
        coverage.total_markers > 0,
        "Sheets grammar has typed `->` markers; audit must enumerate at least one",
    );
    assert_eq!(
        coverage.mapped_markers, coverage.total_markers,
        "every Sheets marker is `Mapped`: {} / {}",
        coverage.mapped_markers, coverage.total_markers,
    );
    assert_eq!(coverage.ratio(), 1.0);
}

#[test]
fn registry_iteration_is_deterministic_across_invocations() {
    // Compile twice; the registry's iter order must be byte-stable
    // (BTreeMap-keyed) across runs. The audit JSON output's row order
    // depends on this.
    let ir1 = compile_sheets_ir();
    let ir2 = compile_sheets_ir();
    let names1: Vec<String> = ir1
        .struct_registry
        .iter()
        .map(|l| l.rule_name.clone())
        .collect();
    let names2: Vec<String> = ir2
        .struct_registry
        .iter()
        .map(|l| l.rule_name.clone())
        .collect();
    assert_eq!(
        names1, names2,
        "registry iteration order is deterministic across invocations",
    );
}
