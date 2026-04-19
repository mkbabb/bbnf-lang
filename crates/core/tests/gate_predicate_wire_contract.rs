//! AX.W0a.2 — gate-predicate wire contract.
//!
//! Freezes the per-grammar outputs of the three shape-admission
//! predicates that drive emission gating:
//!
//! - [`has_w4_classified`] — gates visitor-path emission against
//!   Pratt/Unordered trait-bound widening.
//! - [`has_full_shape_coverage`] — gates per-shape fn emission
//!   (substrate).
//! - [`has_shape_dispatcher_entrypoint`] — gates top-level `parse()`
//!   routing through the shape dispatcher.
//!
//! Per AX invariant 9 ("Gate predicates frozen after introducing
//! wave"), any subsequent wave that widens any of these three
//! predicates MUST amend the `EXPECTED` matrix below. The amendment
//! forces visibility: a gate change can no longer happen silently
//! because the test must be touched in the same commit.
//!
//! Grammars covered per the wave spec:
//!
//! - JSON — `grammar/json/json.bbnf`
//! - CSS L4 — `grammar/css/l4/stylesheet.bbnf` (multi-file @import)
//! - Sheets — `grammar/google-sheets/google-sheets.bbnf`
//! - BBNF — `grammar/bbnf/bbnf.bbnf`
//! - EBNF — `grammar/ebnf/ebnf.bbnf`
//! - BNF — `grammar/bnf/bnf.bbnf`
//! - BbnfBootstrap — `grammar/bbnf/bbnf.bbnf` with `structural` pipeline
//!
//! The last entry is the same grammar file as BBNF but compiled in
//! the structural-preserving pipeline variant the
//! `#[derive(Parser)] #[parser(structural)]` sites use (e.g.
//! `crates/bootstrap`). Structural mode disables destructive IR
//! optimisations that collapse Span wrappers; shape classifications
//! may differ, so BbnfBootstrap is a distinct row in the matrix.

use bbnf::backend::rust::emitter::shapes::{
    has_full_shape_coverage, has_shape_dispatcher_entrypoint,
};
use bbnf::backend::rust::emitter::shapes::dispatcher::has_w4_classified;
use bbnf::pipeline::{
    compile_paths_request, CompileOutput, CompileRequest, CompileTarget, PipelineOptions,
};
use bbnf_ir::GrammarIR;
use std::path::PathBuf;

/// Compile a grammar file into its post-pipeline IR. `structural`
/// routes through the same pipeline branch the
/// `#[parser(structural)]` derive uses.
fn compile(rel: &str, structural: bool) -> GrammarIR {
    let manifest = env!("CARGO_MANIFEST_DIR");
    let p = PathBuf::from(manifest).join(rel);
    assert!(
        p.exists(),
        "grammar file {} missing — cannot compile",
        p.display()
    );
    let options = PipelineOptions {
        structural,
        ..PipelineOptions::default()
    };
    let request = CompileRequest {
        options,
        target: CompileTarget::Vm,
    };
    let out = compile_paths_request(std::slice::from_ref(&p), &request)
        .unwrap_or_else(|e| panic!("grammar {rel} (structural={structural}) compile: {e:?}"));
    match out {
        CompileOutput::Vm(ir) => ir,
        other => panic!("expected Vm output for {rel}, got {other:?}"),
    }
}

/// Evaluate the three predicates in a single pass and return the
/// frozen triple.
struct Predicates {
    has_w4_classified: bool,
    has_full_shape_coverage: bool,
    has_shape_dispatcher_entrypoint: bool,
}

fn evaluate(ir: &GrammarIR) -> Predicates {
    Predicates {
        has_w4_classified: has_w4_classified(ir),
        has_full_shape_coverage: has_full_shape_coverage(ir),
        has_shape_dispatcher_entrypoint: has_shape_dispatcher_entrypoint(ir),
    }
}

// ─────────────────────────────────────────────────────────────────────
// Per-grammar × per-predicate frozen matrix.
//
// Each row below is one grammar. Each `assert_eq!` freezes one
// (grammar, predicate) pair. The expected values encode the state
// AFTER AX.W0a.2's predicate narrowing — same behaviour as pre-
// narrowing for all grammars (all unclassified rules are reachable
// from the entry in every non-JSON case), but matches the docstring
// intent.
//
// Invariant 9 ledger: any downstream wave that flips ANY expected
// value must (a) amend this file with a same-commit rationale, and
// (b) re-bench the full matrix per invariant 16.
// ─────────────────────────────────────────────────────────────────────

#[test]
fn json_predicate_contract() {
    let ir = compile("../../grammar/json/json.bbnf", false);
    let p = evaluate(&ir);
    // JSON's classified rules are String/Number/Keyword/Object/Array
    // — all W3-pure. No Pratt / Unordered. Visitor path admissible.
    assert_eq!(p.has_w4_classified, false, "JSON has_w4_classified");
    // Entry `value` is a transparent Alt-of-Refs; all branches
    // classified. Criterion 1.
    assert_eq!(
        p.has_full_shape_coverage, true,
        "JSON has_full_shape_coverage"
    );
    assert_eq!(
        p.has_shape_dispatcher_entrypoint, true,
        "JSON has_shape_dispatcher_entrypoint"
    );
}

#[test]
fn css_l4_predicate_contract() {
    let ir = compile("../../grammar/css/l4/stylesheet.bbnf", false);
    let p = evaluate(&ir);
    // CSS L4 carries Pratt (mathExpr, mathProduct, complexSelector)
    // + Unordered (compoundSelector). Visitor path gated off.
    assert_eq!(p.has_w4_classified, true, "CSS L4 has_w4_classified");
    // `stylesheet` classified as Array via list-rule detector.
    assert_eq!(
        p.has_full_shape_coverage, true,
        "CSS L4 has_full_shape_coverage"
    );
    // 34 entry-reachable unclassified Refs (bgDecl → value,
    // compoundSelector → attrSelector, etc.). Predicate correctly
    // rejects to prevent `__value` fallback emission that would
    // infinite-loop at parse time.
    assert_eq!(
        p.has_shape_dispatcher_entrypoint, false,
        "CSS L4 has_shape_dispatcher_entrypoint"
    );
}

#[test]
fn sheets_predicate_contract() {
    let ir = compile(
        "../../grammar/google-sheets/google-sheets.bbnf",
        false,
    );
    let p = evaluate(&ir);
    // Sheets carries Pratt (operator tower) + Flat rules. Visitor
    // path gated off.
    assert_eq!(p.has_w4_classified, true, "Sheets has_w4_classified");
    // `formula` classified as Flat.
    assert_eq!(
        p.has_full_shape_coverage, true,
        "Sheets has_full_shape_coverage"
    );
    // Entry-reachable unclassified: `exp_expr → unary_expr`.
    assert_eq!(
        p.has_shape_dispatcher_entrypoint, false,
        "Sheets has_shape_dispatcher_entrypoint"
    );
}

#[test]
fn bbnf_predicate_contract() {
    let ir = compile("../../grammar/bbnf/bbnf.bbnf", false);
    let p = evaluate(&ir);
    // BBNF self-grammar's value-expression operator tower (term,
    // value_or, value_and, value_cmp, value_add, value_mul) admits
    // Pratt classification — operator-chain rung + associativity
    // config yields `ShapeTag::Pratt` on multiple rules. Visitor path
    // gates off.
    assert_eq!(p.has_w4_classified, true, "BBNF has_w4_classified");
    // Entry `grammar` classified as Array.
    assert_eq!(
        p.has_full_shape_coverage, true,
        "BBNF has_full_shape_coverage"
    );
    // Entry-reachable unclassified: host_directive → type_name,
    // import_directive → {import_items, import_path},
    // rhs → alternation.
    assert_eq!(
        p.has_shape_dispatcher_entrypoint, false,
        "BBNF has_shape_dispatcher_entrypoint"
    );
}

#[test]
fn ebnf_predicate_contract() {
    let ir = compile("../../grammar/ebnf/ebnf.bbnf", false);
    let p = evaluate(&ir);
    assert_eq!(p.has_w4_classified, false, "EBNF has_w4_classified");
    assert_eq!(
        p.has_full_shape_coverage, true,
        "EBNF has_full_shape_coverage"
    );
    // Entry-reachable unclassified: identifier → letter,
    // rule → alternation.
    assert_eq!(
        p.has_shape_dispatcher_entrypoint, false,
        "EBNF has_shape_dispatcher_entrypoint"
    );
}

#[test]
fn bnf_predicate_contract() {
    let ir = compile("../../grammar/bnf/bnf.bbnf", false);
    let p = evaluate(&ir);
    assert_eq!(p.has_w4_classified, false, "BNF has_w4_classified");
    assert_eq!(
        p.has_full_shape_coverage, true,
        "BNF has_full_shape_coverage"
    );
    // Entry-reachable unclassified: rule → alternation,
    // rule → nonterminal.
    assert_eq!(
        p.has_shape_dispatcher_entrypoint, false,
        "BNF has_shape_dispatcher_entrypoint"
    );
}

#[test]
fn bbnf_bootstrap_predicate_contract() {
    // Structural pipeline variant — mirrors
    // `#[derive(Parser)] #[parser(structural)] struct BbnfBootstrap`
    // in `crates/bootstrap/src/lib.rs`. Structural mode disables
    // destructive optimisations (Span-collapse, sub-variant
    // unification), which may reshape classifications relative to
    // the non-structural BBNF row.
    let ir = compile("../../grammar/bbnf/bbnf.bbnf", true);
    let p = evaluate(&ir);
    // Same value-expression operator tower as non-structural BBNF.
    // Structural mode preserves more identity but does not change
    // shape classification for these rules.
    assert_eq!(
        p.has_w4_classified, true,
        "BbnfBootstrap has_w4_classified"
    );
    assert_eq!(
        p.has_full_shape_coverage, true,
        "BbnfBootstrap has_full_shape_coverage"
    );
    assert_eq!(
        p.has_shape_dispatcher_entrypoint, false,
        "BbnfBootstrap has_shape_dispatcher_entrypoint"
    );
}
