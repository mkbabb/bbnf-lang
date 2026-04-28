//! AX.W0a.2.g — Keyword Ref-branch emission wire contract.
//!
//! The W0a.2.g Keyword-emitter extension admits Ref-led Alt branches in
//! addition to literal-led ones. BBNF's `directive` rule is the canonical
//! example:
//!
//! ```text
//! directive = import_directive | recover_directive | pretty_directive
//!           | ws_directive | token_directive | debug_directive
//!           | host_directive ;
//! ```
//!
//! Every branch is a [`IrNode::Ref`] to a sibling rule whose body starts
//! with an `"@<name>"` literal. The pre-W0a.2.g Keyword emitter filtered
//! `branches.iter()` to `IrNode::Literal`-only bodies, producing an empty
//! match in `parse_keyword_BbnfGrammar_directive` which unconditionally
//! returned `Err(Syntax)`.
//!
//! This test freezes the post-W0a.2.g contract:
//!
//! 1. `directive` classifies as [`ShapeTag::Keyword`] on the non-
//!    structural BBNF pipeline.
//! 2. `emit_parse_keyword` on `directive` produces a token stream that
//!    references each admitted directive target's shape fn. Concretely
//!    the emission delegates via `emit_ref_call_tape` to
//!    `parse_<shape>_<grammar>_<import_directive | recover_directive |
//!    ...>` for each Ref branch.
//! 3. The emitted fn's signature carries the `state: &mut ScanState`
//!    parameter threaded through for Ref-branch delegation.
//!
//! Hard gate 1 of W0a.2.g: this test must pass.

use bbnf::backend::rust::emitter::shapes::keyword::emit_parse_keyword;
use bbnf_ir::registry::EmitStrategy;
use bbnf::pipeline::{
    compile_paths_request, CompileOutput, CompileRequest, CompileTarget, PipelineOptions,
};
use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag;
use bbnf_ir::GrammarIR;
use std::path::PathBuf;

/// Compile the BBNF grammar (non-structural pipeline).
fn compile_bbnf() -> GrammarIR {
    let manifest = env!("CARGO_MANIFEST_DIR");
    let p = PathBuf::from(manifest).join("../../grammar/bbnf/bbnf.bbnf");
    let request = CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Vm,
    };
    let out = compile_paths_request(std::slice::from_ref(&p), &request)
        .expect("BBNF grammar must compile");
    match out {
        CompileOutput::Vm(ir) => ir,
        other => panic!("expected Vm output, got {other:?}"),
    }
}

/// BBNF's `directive` rule classifies as Keyword (post-W3.1 detector,
/// via `leading_literal_rec` following Ref targets).
#[test]
fn bbnf_directive_classifies_as_keyword() {
    let ir = compile_bbnf();
    let directive = ir
        .rules
        .iter()
        .find(|r| ir.get_string(r.name) == "directive")
        .expect("BBNF grammar must contain `directive` rule");
    let tag = ir.shape_assignments.get(directive.id);
    assert_eq!(
        tag,
        ShapeTag::Keyword,
        "BBNF `directive` must classify as Keyword (post-W0a.2.b widening); got {:?}",
        tag
    );
}

/// `emit_parse_keyword` on BBNF's `directive` produces a Ref-delegating
/// stream, not an empty match. The emitted stream must reference each
/// admitted sibling directive's shape fn name.
#[test]
fn bbnf_directive_keyword_emitter_delegates_to_ref_targets() {
    let ir = compile_bbnf();
    let directive = ir
        .rules
        .iter()
        .find(|r| ir.get_string(r.name) == "directive")
        .expect("BBNF grammar must contain `directive` rule");
    // BBNF resolves to TapeDirect — the existing wire contract scrutinises
    // the tape-path emission unchanged.
    let strategy = EmitStrategy::TapeDirect;
    let tokens = emit_parse_keyword("BbnfGrammar", directive, &ir, &strategy);
    let emitted = tokens.to_string();

    // The fn signature must carry the threaded `state: &mut
    // __shape_support_BbnfGrammar::ScanState` parameter.
    assert!(
        emitted.contains("first_byte") && emitted.contains("state"),
        "emitted keyword fn must thread first_byte + state; got:\n{}",
        emitted,
    );

    // Each of the seven directive branches must appear as a downstream
    // target shape-fn reference. `emit_ref_call_tape` embeds the target
    // fn ident (e.g. `parse_<shape>_BbnfGrammar_import_directive`).
    let expected_targets = [
        "import_directive",
        "recover_directive",
        "pretty_directive",
        "ws_directive",
        "token_directive",
        "debug_directive",
        "host_directive",
    ];
    for target in expected_targets.iter() {
        let needle = format!("BbnfGrammar_{}", target);
        assert!(
            emitted.contains(&needle),
            "emitted stream must reference target `{}`; got:\n{}",
            target,
            emitted,
        );
    }

    // Byte-match prefix bytes (`@` = 0x40 = 64) must appear — the first
    // byte of every admitted directive is `@`.
    assert!(
        emitted.contains("64u8"),
        "emitted stream must byte-match `@` (64u8) for directive first-byte dispatch; got:\n{}",
        emitted,
    );
}

/// The emitted fn identifier matches the per-grammar shape-fn naming
/// convention — `parse_keyword_<grammar_suffix>_<rule>`.
#[test]
fn bbnf_directive_keyword_emitter_produces_named_fn() {
    let ir = compile_bbnf();
    let directive = ir
        .rules
        .iter()
        .find(|r| ir.get_string(r.name) == "directive")
        .expect("BBNF grammar must contain `directive` rule");
    let strategy = EmitStrategy::TapeDirect;
    let tokens = emit_parse_keyword("BbnfGrammar", directive, &ir, &strategy);
    let emitted = tokens.to_string();
    assert!(
        emitted.contains("parse_keyword_BbnfGrammar_directive"),
        "emitted fn must be named parse_keyword_BbnfGrammar_directive; got:\n{}",
        emitted,
    );
}
