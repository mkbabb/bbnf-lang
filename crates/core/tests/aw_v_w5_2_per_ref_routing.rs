//! AW-V.W5.2 — per-Ref value-position routing wire contract.
//!
//! Verifies that the W5.2 refactor produces direct per-Ref calls in
//! the emitted shape functions, rather than routing every Ref through
//! the `__value` dispatcher.
//!
//! # Wire contract
//!
//! The W5.2 refactor changes the dispatcher from a central `__value`
//! body (byte-dispatch over Alt branches) to per-Ref direct calls. At
//! every value-position Ref inside a classified rule, the emitter
//! resolves the target's shape at codegen time and emits a direct
//! call to `parse_<shape>_<grammar>_<rule>`.
//!
//! This closes the architectural gap identified at AW-V.W4 close
//! (2026-04-18, PROGRESS.md §W4-activation): non-Alt-rooted grammars
//! cannot route through `__value` because the root shape fn IS the
//! target, producing cycles. Per-Ref routing breaks the cycle by
//! resolving each Ref target at emission time.

use bbnf::backend::rust::emitter::shapes::{
    dispatcher::{emit_ref_call_tape, emit_ref_call_visitor},
    has_full_shape_coverage, has_shape_dispatch, has_shape_dispatcher_entrypoint,
};
use bbnf::pipeline::{
    compile_paths_request, CompileOutput, CompileRequest, CompileTarget, PipelineOptions,
};
use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag;
use bbnf_ir::GrammarIR;
use std::path::PathBuf;

/// Compile a grammar at the given path into a VM-ready IR.
fn compile_grammar(path: &str) -> GrammarIR {
    let manifest = env!("CARGO_MANIFEST_DIR");
    let p = PathBuf::from(manifest).join(path);
    let request = CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Vm,
    };
    let out = compile_paths_request(std::slice::from_ref(&p), &request)
        .unwrap_or_else(|e| panic!("grammar {path} must compile: {e:?}"));
    match out {
        CompileOutput::Vm(ir) => ir,
        other => panic!("expected Vm output, got {other:?}"),
    }
}

// ─── Helper availability + return shape ─────────────────────────────

/// `emit_ref_call_tape` returns Some(TokenStream) for a classified
/// target rule. The emitted stream is non-empty.
#[test]
fn emit_ref_call_tape_returns_some_for_classified_target() {
    let ir = compile_grammar("../../grammar/json/json.bbnf");
    // Find the string rule — JSON has a String-classified rule called "string".
    let string_rule = ir
        .rules
        .iter()
        .find(|r| {
            !r.meta.is_transparent
                && matches!(
                    ir.shape_assignments.get(r.id),
                    ShapeTag::String
                )
        })
        .expect("JSON must have a String-classified rule");
    let call = emit_ref_call_tape("JsonGrammar", string_rule.id, &ir);
    assert!(
        call.is_some(),
        "emit_ref_call_tape must return Some for a classified target"
    );
    let text = call.unwrap().to_string();
    assert!(
        text.contains("parse_string_JsonGrammar"),
        "emitted call must invoke parse_string_JsonGrammar_<rule>: {text}"
    );
}

/// `emit_ref_call_tape` returns None for an unclassified target rule.
#[test]
fn emit_ref_call_tape_returns_none_for_unclassified_target() {
    // AX.W0a.2.b widened detector coverage — Sheets no longer has
    // any unclassified entry-reachable rules. Use a synthesised
    // RuleId above the rule count to exercise the None return path
    // (a lookup that falls off `ir.rules`).
    let ir = compile_grammar("../../grammar/google-sheets/google-sheets.bbnf");
    let nonexistent: bbnf_ir::RuleId = ir.rules.len() as u32 + 999;
    let call = emit_ref_call_tape("Sheets", nonexistent, &ir);
    assert!(
        call.is_none(),
        "emit_ref_call_tape must return None when the target RuleId is \
         out of range"
    );
}

/// Visitor-path emitter mirrors the tape-path contract: Some for
/// classified, None for unclassified.
#[test]
fn emit_ref_call_visitor_mirrors_tape_path_contract() {
    let ir = compile_grammar("../../grammar/json/json.bbnf");
    let string_rule = ir
        .rules
        .iter()
        .find(|r| matches!(ir.shape_assignments.get(r.id), ShapeTag::String))
        .expect("JSON must have a String-classified rule");
    let call = emit_ref_call_visitor("JsonGrammar", string_rule.id, &ir);
    assert!(call.is_some(), "visitor-path must return Some for classified");
    let text = call.unwrap().to_string();
    assert!(
        text.contains("parse_string_visitor_JsonGrammar"),
        "visitor-path call must invoke parse_string_visitor_<grammar>_<rule>: {text}"
    );
}

// ─── Admission gate behavior ────────────────────────────────────────

/// JSON admits via the shape dispatcher entrypoint. All value-Ref
/// targets in JSON are classified (object / array / string / number /
/// bool / null); the entry rule `value` is Alt-of-Refs to these.
#[test]
fn json_admits_via_shape_dispatcher_entrypoint() {
    let ir = compile_grammar("../../grammar/json/json.bbnf");
    assert!(
        has_shape_dispatch(&ir),
        "JSON must have classified rules"
    );
    assert!(
        has_full_shape_coverage(&ir),
        "JSON's entry is Alt-of-Refs to classified branches"
    );
    assert!(
        has_shape_dispatcher_entrypoint(&ir),
        "JSON admits entrypoint routing — every value-Ref target is classified"
    );
}

/// The admission gate correctly propagates: if ANY classified rule's
/// body contains a Ref to an unclassified rule, admission fails.
///
/// This is the W5.2 correctness invariant: per-Ref routing emits
/// direct calls to target shape fns; calling a non-existent shape fn
/// would be a compilation error. The admission gate prevents that.
#[test]
fn admission_accepts_grammars_with_classified_value_ref_targets() {
    // Post AX.W0a.2.b — detector widening (AltDispatch + Flat +
    // Scalar extensions + fixed-point classification) closes the
    // entry-reachable unclassified Ref gap for Sheets / CSS L4 /
    // BBNF / EBNF / BNF / BbnfBootstrap. The admission gate now
    // accepts every grammar with classified entry-reachable Refs.
    // Per AX invariant 9, any subsequent wave that flips this back
    // without cause must amend `gate_predicate_wire_contract.rs`.
    let ir = compile_grammar("../../grammar/google-sheets/google-sheets.bbnf");
    assert!(
        has_shape_dispatch(&ir),
        "Sheets has classified rules — has_shape_dispatch must be true"
    );
    assert!(
        has_full_shape_coverage(&ir),
        "Sheets's entry (formula) is classified (Flat) — has_full_shape_coverage admits"
    );
    assert!(
        has_shape_dispatcher_entrypoint(&ir),
        "Post AX.W0a.2.b: Sheets's entry-reachable Ref graph is closed \
         over classified targets (let_args → Flat, let_binding → Flat, \
         etc.). The admission gate accepts."
    );
}

// ─── End-to-end: JSON parses through shape dispatcher ────────────────

use bbnf_derive::Parser;

#[derive(Parser)]
#[parser(path = "../../grammar/json/json.bbnf")]
struct JsonWire;

/// JSON's shape dispatcher routing is functional — a minimal document
/// parses end-to-end via `parse()` calling into the per-shape fns
/// (not the walker).
#[test]
fn json_parses_minimal_document_via_shape_dispatcher() {
    // An empty object.
    let parsed = JsonWire::parse("{}").expect("empty object must parse");
    assert_eq!(parsed.input(), "{}");

    // A small mixed document.
    let parsed = JsonWire::parse(r#"{"a":1,"b":[true,null]}"#)
        .expect("mixed document must parse");
    assert_eq!(parsed.input(), r#"{"a":1,"b":[true,null]}"#);
}
