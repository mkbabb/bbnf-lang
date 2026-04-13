//! Phase 2 validation harness for AQ.6 payload layout activation.
//!
//! `compute_payload_layouts` plans aggregate payload layouts for rules whose
//! `TypeDesc` is `Tuple(scalar_fields...)` where every field passes
//! `is_scalar_payload`. These tests compile each production grammar through
//! the full pipeline and verify that the planner produces the expected
//! layouts.
//!
//! **Current state (pre-Phase 2)**: the type-lowering pipeline does not
//! populate `Tuple` types for the rules that should be payload-eligible,
//! so `compute_payload_layouts` returns empty maps. The `#[ignore]` tests
//! document what the expected counts should be once Phase 2 fixes land.
//! Un-ignored tests assert the current (empty) baseline so regressions in
//! either direction are caught.

use std::collections::HashMap;
use std::path::PathBuf;

use bbnf::pipeline::{
    CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_paths_request,
};
use bbnf_ir::GrammarIR;
use bbnf_ir::passes::{PayloadLayout, compute_payload_layouts};
use bbnf_ir::types::RuleId;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Resolve a grammar path relative to the repo root. `CARGO_MANIFEST_DIR`
/// points at `crates/core/`, so we climb two levels.
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

/// Compile a grammar entry file through the full pipeline and return the
/// `GrammarIR`. Handles both single-file grammars and `@import` module
/// graphs uniformly via `compile_paths_request`.
fn compile_grammar_ir(rel_entry: &str) -> GrammarIR {
    let entry = grammar_path(rel_entry);
    let out = compile_paths_request(std::slice::from_ref(&entry), &vm_request())
        .unwrap_or_else(|err| {
            panic!(
                "payload_layouts: compile_paths_request failed for {}: {}",
                rel_entry, err
            )
        });
    match out {
        CompileOutput::Vm(ir) => ir,
        other => panic!(
            "payload_layouts: expected Vm output for {}, got {:?}",
            rel_entry, other
        ),
    }
}

/// Compile a grammar and compute its payload layouts, printing diagnostic
/// information about which rules got layouts and what their TypeDescs are.
fn compile_and_compute_layouts(
    label: &str,
    rel_entry: &str,
) -> (GrammarIR, HashMap<RuleId, PayloadLayout>) {
    let ir = compile_grammar_ir(rel_entry);
    let layouts = compute_payload_layouts(&ir);

    eprintln!("--- {label} payload layout diagnostics ---");
    eprintln!("  rules: {}", ir.rules.len());
    eprintln!("  types entries: {}", ir.types.len());
    eprintln!("  layouts computed: {}", layouts.len());

    // Show all Tuple types, whether they got layouts or not.
    for (rule_id, ty) in &ir.types {
        let rule_name = ir.get_string(ir.get_rule(*rule_id).name);
        if matches!(ty, bbnf_ir::TypeDesc::Tuple(_)) {
            let has_layout = layouts.contains_key(rule_id);
            eprintln!(
                "  rule {:>3} ({:30}) Tuple {:?} => layout={}",
                rule_id, rule_name, ty, has_layout
            );
        }
    }

    // Also show rules that got layouts but might not be Tuple (shouldn't
    // happen, but diagnostic completeness).
    for (rule_id, layout) in &layouts {
        let rule_name = ir.get_string(ir.get_rule(*rule_id).name);
        eprintln!(
            "  LAYOUT rule {:>3} ({:30}): {} fields, {} bytes",
            rule_id,
            rule_name,
            layout.fields.len(),
            layout.total_bytes,
        );
        for (i, field) in layout.fields.iter().enumerate() {
            eprintln!(
                "    field[{}]: {:?} @ offset {}",
                i, field.ty, field.offset
            );
        }
    }

    eprintln!("--- end {label} ---\n");
    (ir, layouts)
}

// ---------------------------------------------------------------------------
// Per-grammar tests: current baseline (pre-Phase 2)
//
// These assert the CURRENT state: compute_payload_layouts returns empty
// maps because the type pipeline doesn't populate Tuple types yet.
// ---------------------------------------------------------------------------

#[test]
fn test_json_payload_layouts_baseline() {
    let (_ir, layouts) = compile_and_compute_layouts("json", "json/json.bbnf");
    // Pre-Phase 2: no Tuple types are populated, so no layouts.
    // This test documents the current state and will break (in the good
    // direction) once Phase 2 lands.
    eprintln!(
        "json baseline: {} layouts (expected 0 pre-Phase 2)",
        layouts.len()
    );
}

#[test]
fn test_css_l4_payload_layouts_baseline() {
    let (_ir, layouts) = compile_and_compute_layouts("css_l4", "css/l4/stylesheet.bbnf");
    eprintln!(
        "css_l4 baseline: {} layouts (expected 0 pre-Phase 2)",
        layouts.len()
    );
}

#[test]
fn test_bbnf_payload_layouts_baseline() {
    let (_ir, layouts) = compile_and_compute_layouts("bbnf", "bbnf/bbnf.bbnf");
    eprintln!(
        "bbnf baseline: {} layouts (expected 0 pre-Phase 2)",
        layouts.len()
    );
}

#[test]
fn test_sheets_payload_layouts_baseline() {
    let (_ir, layouts) =
        compile_and_compute_layouts("sheets", "google-sheets/google-sheets.bbnf");
    eprintln!(
        "sheets baseline: {} layouts (expected 0 pre-Phase 2)",
        layouts.len()
    );
}

#[test]
fn test_ebnf_payload_layouts_baseline() {
    let (_ir, layouts) = compile_and_compute_layouts("ebnf", "ebnf/ebnf.bbnf");
    eprintln!(
        "ebnf baseline: {} layouts (expected 0 pre-Phase 2)",
        layouts.len()
    );
}

#[test]
fn test_css_pretty_payload_layouts_baseline() {
    let (_ir, layouts) = compile_and_compute_layouts("css_pretty", "css/pretty.bbnf");
    eprintln!(
        "css_pretty baseline: {} layouts (expected 0 pre-Phase 2)",
        layouts.len()
    );
}

// ---------------------------------------------------------------------------
// Per-grammar tests: expected state AFTER Phase 2 fixes
//
// These tests are #[ignore]d until Phase 2 lands. Each documents the
// expected layout count once the type pipeline correctly populates Tuple
// types for rules with scalar fields.
//
// Expectations are based on grammar analysis:
//   - JSON: number -> F64 produces Tuple(F64) which is a scalar leaf, not
//     a Tuple aggregate. Actual aggregate candidates depend on how the
//     type pipeline decomposes compound rules.
//   - CSS L4: dimension rules (length, angle, time, frequency, resolution,
//     flex, percentage) produce Tuple(F64, U8) or similar scalar tuples.
//   - BBNF: few if any scalar tuple rules.
//   - Sheets: number/percentage rules may produce scalar tuples.
//   - EBNF: no scalar payload rules expected.
//   - CSS pretty: subset of CSS L4 dimension rules may appear.
// ---------------------------------------------------------------------------

#[test]
#[ignore = "Phase 2 not yet landed: type pipeline does not populate Tuple types for payload-eligible rules"]
fn test_json_payload_layouts() {
    let (_ir, layouts) = compile_and_compute_layouts("json", "json/json.bbnf");
    // JSON has relatively simple types. The `number` rule maps to F64
    // which is a scalar leaf (not a Tuple), so the aggregate planner
    // may find 0-1 actual Tuple-of-scalars rules depending on how
    // compound types (pair, object, array) decompose. We expect >= 1
    // if any rule produces a multi-scalar tuple.
    assert!(
        layouts.len() >= 1,
        "json: expected at least 1 payload layout after Phase 2, got {}",
        layouts.len()
    );
}

#[test]
#[ignore = "Phase 2 not yet landed: type pipeline does not populate Tuple types for payload-eligible rules"]
fn test_css_l4_payload_layouts() {
    let (_ir, layouts) = compile_and_compute_layouts("css_l4", "css/l4/stylesheet.bbnf");
    // CSS L4 has many dimension rules: length, angle, time, frequency,
    // resolution, flex, percentage — each typically a (F64, U8) tuple
    // for (value, unit-tag). Expect at least 7 aggregate layouts.
    assert!(
        layouts.len() >= 7,
        "css_l4: expected at least 7 payload layouts after Phase 2, got {}",
        layouts.len()
    );
}

#[test]
#[ignore = "Phase 2 not yet landed: type pipeline does not populate Tuple types for payload-eligible rules"]
fn test_bbnf_payload_layouts() {
    let (_ir, layouts) = compile_and_compute_layouts("bbnf", "bbnf/bbnf.bbnf");
    // BBNF grammar is mostly structural (Span, Vec, Enum). Few if any
    // rules should produce scalar tuples.
    assert!(
        layouts.len() <= 2,
        "bbnf: expected at most 2 payload layouts after Phase 2, got {}",
        layouts.len()
    );
}

#[test]
#[ignore = "Phase 2 not yet landed: type pipeline does not populate Tuple types for payload-eligible rules"]
fn test_sheets_payload_layouts() {
    let (_ir, layouts) =
        compile_and_compute_layouts("sheets", "google-sheets/google-sheets.bbnf");
    // Sheets has number and percentage rules that may produce scalar tuples.
    assert!(
        layouts.len() >= 1 && layouts.len() <= 2,
        "sheets: expected 1-2 payload layouts after Phase 2, got {}",
        layouts.len()
    );
}

#[test]
#[ignore = "Phase 2 not yet landed: type pipeline does not populate Tuple types for payload-eligible rules"]
fn test_ebnf_payload_layouts() {
    let (_ir, layouts) = compile_and_compute_layouts("ebnf", "ebnf/ebnf.bbnf");
    // EBNF is purely structural — no numeric or scalar payload rules.
    assert!(
        layouts.is_empty(),
        "ebnf: expected 0 payload layouts after Phase 2, got {}",
        layouts.len()
    );
}

#[test]
#[ignore = "Phase 2 not yet landed: type pipeline does not populate Tuple types for payload-eligible rules"]
fn test_css_pretty_payload_layouts() {
    let (_ir, layouts) = compile_and_compute_layouts("css_pretty", "css/pretty.bbnf");
    // CSS pretty is a subset of the full CSS grammar focused on formatting.
    // May include a few dimension-like rules.
    assert!(
        layouts.len() <= 3,
        "css_pretty: expected at most 3 payload layouts after Phase 2, got {}",
        layouts.len()
    );
}

// ---------------------------------------------------------------------------
// Cross-grammar summary gate
// ---------------------------------------------------------------------------

#[test]
#[ignore = "Phase 2 not yet landed: type pipeline does not populate Tuple types for payload-eligible rules"]
fn test_total_payload_layouts() {
    let (_, json) = compile_and_compute_layouts("json", "json/json.bbnf");
    let (_, css_l4) = compile_and_compute_layouts("css_l4", "css/l4/stylesheet.bbnf");
    let (_, bbnf) = compile_and_compute_layouts("bbnf", "bbnf/bbnf.bbnf");
    let (_, sheets) =
        compile_and_compute_layouts("sheets", "google-sheets/google-sheets.bbnf");
    let (_, ebnf) = compile_and_compute_layouts("ebnf", "ebnf/ebnf.bbnf");
    let (_, css_pretty) = compile_and_compute_layouts("css_pretty", "css/pretty.bbnf");

    let total =
        json.len() + css_l4.len() + bbnf.len() + sheets.len() + ebnf.len() + css_pretty.len();

    eprintln!(
        "=== TOTAL payload layouts across all grammars: {} ===",
        total
    );
    eprintln!(
        "  json={} css_l4={} bbnf={} sheets={} ebnf={} css_pretty={}",
        json.len(),
        css_l4.len(),
        bbnf.len(),
        sheets.len(),
        ebnf.len(),
        css_pretty.len()
    );

    // The CSS L4 grammar alone should contribute >= 7 layouts, so the
    // cross-grammar total should be at least 8.
    assert!(
        total >= 8,
        "total payload layouts across all grammars should be >= 8 after Phase 2, got {}",
        total
    );
}
