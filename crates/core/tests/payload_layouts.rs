//! Validation harness for payload layout activation (AR.2.1).
//!
//! `compute_payload_layouts` plans aggregate payload layouts for rules whose
//! `TypeDesc` is a scalar (`is_scalar_payload`) or `Tuple(scalar_fields...)`
//! where every field passes `is_scalar_payload`. These tests compile each
//! production grammar through the full pipeline and verify that the planner
//! produces the expected layouts.
//!
//! After the AR.2.1 fix, `lower_map_arrow` produces concrete `TypeDesc`
//! variants (`F64`, `U8`, `Bool`, etc.) instead of `Named("f64")` for
//! well-known scalar type names, and correctly recovers type suffixes from
//! `value_atom` span text when the bootstrap grammar's tape-rewrite has
//! folded `int_lit`/`float_lit`/`bool_lit` into `value_atom`.

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

    // Show all rule types, highlighting Tuple and scalar payload types.
    for (rule_id, ty) in &ir.types {
        let rule_name = ir.get_string(ir.get_rule(*rule_id).name);
        let has_layout = layouts.contains_key(rule_id);
        let is_interesting =
            matches!(ty, bbnf_ir::TypeDesc::Tuple(_)) || ty.is_scalar_payload();
        if is_interesting {
            eprintln!(
                "  rule {:>3} ({:30}) {:?} => layout={}",
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
    // Bare-Span rules (comma, colon, string) and scalar-Alt rules
    // (bool = "true" -> true | "false" -> false) each register a
    // single-field layout via `compute_payload_layouts`. Map-bodied
    // scalar rules (null = "null" -> 0u8, number = /regex/ -> f64)
    // remain on the `TapeSpanOnly` scalar path (`InlineScalar` /
    // `WideScalar`) — body shape is `Map`, not `Alt`, so
    // `scalar_layout_eligible` rejects them.
    assert!(
        layouts.len() >= 4,
        "json: expected at least 4 payload layouts (bare-Span + \
         scalar-Alt admissions), got {}",
        layouts.len()
    );
}

#[test]
fn test_css_l4_payload_layouts_baseline() {
    let (_ir, layouts) = compile_and_compute_layouts("css_l4", "css/l4/stylesheet.bbnf");
    // Post-AR.2.1: keyword + unit rules with u8 discriminants produce
    // scalar U8 layouts. Dimension rules (length, angle, ...) remain
    // Tuple([Span, U8]) — Span blocks aggregate promotion until Span
    // scalar admission (AR.2 Phase 2).
    assert!(
        layouts.len() >= 7,
        "css_l4: expected at least 7 payload layouts, got {}",
        layouts.len()
    );
}

#[test]
fn test_bbnf_payload_layouts_baseline() {
    let (_ir, layouts) = compile_and_compute_layouts("bbnf", "bbnf/bbnf.bbnf");
    // BBNF grammar is mostly structural — few if any scalar payload rules.
    eprintln!("bbnf: {} layouts", layouts.len());
}

#[test]
fn test_sheets_payload_layouts_baseline() {
    let (_ir, layouts) =
        compile_and_compute_layouts("sheets", "google-sheets/google-sheets.bbnf");
    eprintln!("sheets: {} layouts", layouts.len());
}

#[test]
fn test_ebnf_payload_layouts_baseline() {
    let (_ir, layouts) = compile_and_compute_layouts("ebnf", "ebnf/ebnf.bbnf");
    // EBNF is purely structural — no scalar payload rules.
    eprintln!("ebnf: {} layouts", layouts.len());
}

#[test]
fn test_css_pretty_payload_layouts_baseline() {
    let (_ir, layouts) = compile_and_compute_layouts("css_pretty", "css/pretty.bbnf");
    eprintln!("css_pretty: {} layouts", layouts.len());
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

// ---------------------------------------------------------------------------
// Per-grammar assertion tests (AR.2.1 activated)
// ---------------------------------------------------------------------------

#[test]
fn test_json_payload_layouts() {
    let (_ir, layouts) = compile_and_compute_layouts("json", "json/json.bbnf");
    // AV.0.1 close-out: scalar-Alt rules (`bool = "true" -> true |
    // "false" -> false`) join bare-Span rules (comma, colon,
    // string) in the aggregate planner. Map-bodied scalar rules
    // (`null = "null" -> 0u8`, `number = /regex/ -> f64`) stay on
    // the scalar `InlineScalar` / `WideScalar` path.
    assert!(
        layouts.len() >= 4,
        "json: expected at least 4 payload layouts (bare-Span + \
         scalar-Alt admissions), got {}",
        layouts.len()
    );
}

#[test]
fn test_css_l4_payload_layouts() {
    let (_ir, layouts) = compile_and_compute_layouts("css_l4", "css/l4/stylesheet.bbnf");
    // Keyword rules + unit rules with u8 discriminants.
    assert!(
        layouts.len() >= 7,
        "css_l4: expected at least 7 payload layouts, got {}",
        layouts.len()
    );
}

#[test]
fn test_bbnf_payload_layouts() {
    let (_ir, layouts) = compile_and_compute_layouts("bbnf", "bbnf/bbnf.bbnf");
    // BBNF grammar is mostly structural. May have some scalar payload rules
    // from directive discriminants.
    eprintln!("bbnf assertion: {} layouts", layouts.len());
}

#[test]
fn test_sheets_payload_layouts() {
    let (_ir, layouts) =
        compile_and_compute_layouts("sheets", "google-sheets/google-sheets.bbnf");
    eprintln!("sheets assertion: {} layouts", layouts.len());
}

#[test]
fn test_ebnf_payload_layouts() {
    let (_ir, layouts) = compile_and_compute_layouts("ebnf", "ebnf/ebnf.bbnf");
    // EBNF has no numeric scalar payloads, but bare-Span rules
    // (letter, digit, symbol, terminator, S) admit the AV.0.2
    // single-field `[Span @ 0, total 8]` layout. No scalar-Alt
    // admissions — EBNF's Alt-bodied rules project to Span via
    // their regex-of-literals branches, not a distinct scalar
    // type.
    assert!(
        layouts.len() >= 5,
        "ebnf: expected at least 5 bare-Span payload layouts, got {}",
        layouts.len()
    );
}

#[test]
fn test_css_pretty_payload_layouts() {
    let (_ir, layouts) = compile_and_compute_layouts("css_pretty", "css/pretty.bbnf");
    eprintln!("css_pretty assertion: {} layouts", layouts.len());
}

// ---------------------------------------------------------------------------
// Cross-grammar summary gate
// ---------------------------------------------------------------------------

#[test]
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

    // W6.D: single-scalar rules (json's null/bool/number, css_l4's
    // u8 keyword rules) bypass the aggregate planner. The layouts
    // map captures only multi-field tuples — KV pairs and dimension
    // `(f64, u8)` aggregates. CSS L4 still contributes 7 (dimension
    // + KV-pair shapes); sheets contributes 2; JSON/EBNF/BBNF/css_pretty
    // contribute 0.
    assert!(
        total >= 7,
        "total aggregate layouts across all grammars should be >= 7, \
         got {}",
        total
    );
}
