//! AZ-I.W2-act.B1 — leaf test for the `EmitStrategy::for_grammar`
//! resolver.
//!
//! Exercises the resolver's load-bearing dispatch axes:
//!
//! 1. JSON's parser ident (`"JsonParser"`) + populated registry
//!    routes to `EmitStrategy::StructDirect` with the stable
//!    builder/document path strings the emitter splices into
//!    `parse()`'s body — the W2-act.B1 arm flip.
//! 2. JSON's plan-nominal alias (`"JsonGrammar"`) + populated
//!    registry routes the same way (forward-compat with hand-
//!    written test fixtures per the W2-EMITTER-REWIRE plan §1).
//! 3. BBNF (`"BbnfBootstrap"`) + populated registry routes to
//!    `EmitStrategy::TapeDirect` — only JSON gets struct-direct
//!    in W2-act.B1; the resolver explicitly filters by grammar
//!    identity, not just by registry-emptiness.
//! 4. BBNF + empty registry routes to `EmitStrategy::TapeDirect`
//!    (registry-emptiness can never escalate a grammar to
//!    struct-direct).
//! 5. JSON + empty registry routes to `EmitStrategy::TapeDirect`
//!    (the activation guard prevents emitting a body that calls
//!    `begin_compound` against a missing layout — the substrate-
//!    with-consumer rule from instructions/SPEC §Activation-gate).
//!
//! Per `feedback_no-orthogonal-codepaths` the resolver is the single
//! decision surface; per `feedback_pluggable-components` adding a
//! new struct-direct grammar (Sheets W2-act.B2, CSS L4 W2-act.B3)
//! extends the resolver with a new arm and lands a sibling test
//! here.

// AZ-I.W2-act.A — `EmitStrategy` lives in `bbnf_ir::registry::strategy`
// per `audit/AUDIT-6-ARCHITECTURE.md` §4 + §8.1. The Rust emitter
// re-exports the IR-level enum so existing
// `bbnf::backend::rust::emitter::EmitStrategy` paths continue to
// resolve, but the canonical home is the IR registry.
use bbnf_ir::registry::EmitStrategy;
use bbnf_ir::registry::{LayoutKind, StructLayout, StructRegistry};
use bbnf_ir::TypeDesc;

/// Construct a `StructRegistry` with a single synthetic layout —
/// just enough to test the populated-registry branch of
/// `EmitStrategy::for_grammar`. The layout's `rule_id` /
/// `rule_name` do not interact with the resolver; only `len()` /
/// `is_empty()` do.
fn populated_registry() -> StructRegistry {
    let mut r = StructRegistry::new();
    r.insert(StructLayout {
        rule_id: 0,
        rule_name: "fixture".to_string(),
        kind: LayoutKind::Struct,
        rule_type: TypeDesc::Tuple(Vec::new()),
        fields: Vec::new(),
    });
    r
}

/// Construct an empty `StructRegistry`. Mirrors the pre-W1 state
/// or any grammar whose `project_types` saw no Named rules worth
/// recording.
fn empty_registry() -> StructRegistry {
    StructRegistry::new()
}

#[test]
fn json_parser_with_populated_registry_routes_struct_direct() {
    // AZ-I.W2-act.B1 — the resolver's positive JSON arm. Once the
    // struct registry carries at least one layout, `JsonParser`
    // resolves onto the struct-direct path with the canonical
    // `JsonStructBuilder` / `JsonDocument` substrate bindings. The
    // post-flip orchestrator regen consumes these paths to emit a
    // `parse()` body that returns `Result<JsonDocument<'_>, ParseErr>`.
    let registry = populated_registry();
    let strategy = EmitStrategy::for_grammar("JsonParser", &registry);
    match strategy {
        EmitStrategy::StructDirect { rust, ts, wasm } => {
            assert_eq!(
                rust.builder_path, "crate::runtime::json::JsonStructBuilder",
                "JsonParser StructDirect must wire the canonical builder path",
            );
            assert_eq!(
                rust.document_path, "crate::runtime::json::JsonDocument",
                "JsonParser StructDirect must wire the canonical document path",
            );
            assert!(
                ts.is_none(),
                "TS binding stays None until BA host-bindings populate it",
            );
            assert!(
                wasm.is_none(),
                "WASM binding stays None until BA host-bindings populate it",
            );
        }
        other => panic!(
            "AZ-I.W2-act.B1: JsonParser + populated registry must route StructDirect; got {:?}",
            other,
        ),
    }
}

#[test]
fn json_grammar_alias_routes_struct_direct() {
    // Forward-compat alias resolves identically to `JsonParser`;
    // hand-written test fixtures naming the grammar `JsonGrammar`
    // reach the same struct-direct binding pair.
    let registry = populated_registry();
    let strategy = EmitStrategy::for_grammar("JsonGrammar", &registry);
    match strategy {
        EmitStrategy::StructDirect { rust, .. } => {
            assert_eq!(
                rust.builder_path, "crate::runtime::json::JsonStructBuilder",
                "JsonGrammar alias must wire the same builder path as JsonParser",
            );
            assert_eq!(
                rust.document_path, "crate::runtime::json::JsonDocument",
                "JsonGrammar alias must wire the same document path as JsonParser",
            );
        }
        other => panic!(
            "AZ-I.W2-act.B1: JsonGrammar (alias) + populated registry must route StructDirect; got {:?}",
            other,
        ),
    }
}

#[test]
fn bbnf_with_populated_registry_routes_struct_direct() {
    // AZ-II.cutover.A — BBNF's project_types pass populates a
    // non-trivial registry; the cutover.A resolver-arm extension
    // routes `BbnfBootstrap` / `BbnfParser` to StructDirect when the
    // registry is populated. cutover.B regens the parser onto the
    // struct-direct path; cutover.C deletes the tape crate.
    //
    // Pre-AZ-II.cutover.A this test asserted TapeDirect routing per
    // AZ-I §Invariant 4 ("BBNF continues on tape" — frozen for AZ-I).
    // AZ-II.cutover lifts that freeze.
    let registry = populated_registry();
    let strategy = EmitStrategy::for_grammar("BbnfBootstrap", &registry);
    match strategy {
        EmitStrategy::StructDirect { rust, .. } => {
            assert_eq!(
                rust.builder_path,
                "crate::runtime::bbnf::BbnfStructBuilder",
                "BbnfBootstrap StructDirect arm binds the BBNF runtime builder",
            );
            assert_eq!(
                rust.document_path,
                "crate::runtime::bbnf::BbnfDocument",
                "BbnfBootstrap StructDirect arm binds the BBNF runtime document",
            );
        }
        EmitStrategy::TapeDirect => panic!(
            "BbnfBootstrap with populated registry must route StructDirect post-cutover.A; got TapeDirect"
        ),
    }
}

#[test]
fn bbnf_with_empty_registry_routes_tape_direct() {
    // The activation guard in EmitStrategy::for_grammar downgrades a
    // nominally-struct-direct grammar to TapeDirect when the registry
    // is empty. BBNF + empty registry therefore stays on TapeDirect
    // — the closure-substrate gate is the same one JSON / CSS L4 /
    // Sheets respect.
    let registry = empty_registry();
    let strategy = EmitStrategy::for_grammar("BbnfBootstrap", &registry);
    assert_eq!(
        strategy,
        EmitStrategy::TapeDirect,
        "BbnfBootstrap + empty registry routes TapeDirect (default fallthrough)",
    );
}

#[test]
fn json_parser_with_empty_registry_routes_tape_direct() {
    // The activation guard in EmitStrategy::for_grammar downgrades
    // a nominally-struct-direct grammar to TapeDirect when the
    // registry is empty. Per instructions/SPEC §Activation-gate
    // the substrate (registry) and the consumer (struct-direct
    // body emission) must land in lockstep; an empty registry
    // signals project_types saw no Named rules worth recording,
    // and emitting a body that calls `begin_compound` against a
    // missing layout would be a runtime crash dressed as a
    // codegen success.
    let registry = empty_registry();
    let strategy = EmitStrategy::for_grammar("JsonParser", &registry);
    assert_eq!(
        strategy,
        EmitStrategy::TapeDirect,
        "JsonParser + empty registry must downgrade to TapeDirect (activation guard)",
    );
}

#[test]
fn css_l4_parser_with_populated_registry_routes_struct_direct() {
    // AZ-I.W2-act.B3 — the CssL4Parser arm flips StructDirect when
    // the registry is populated. The SubstrateBinding payload carries
    // the canonical crate::runtime::css_l4 paths the emitter splices
    // into the generated parse fn body.
    let registry = populated_registry();
    let strategy = EmitStrategy::for_grammar("CssL4Parser", &registry);
    assert!(
        matches!(strategy, EmitStrategy::StructDirect { .. }),
        "CssL4Parser + populated registry must route StructDirect (W2-act.B3 activation)"
    );
    if let EmitStrategy::StructDirect { rust, .. } = strategy {
        assert_eq!(rust.builder_path, "crate::runtime::css_l4::CssStructBuilder");
        assert_eq!(rust.document_path, "crate::runtime::css_l4::CssDocument");
    }
}

#[test]
fn css_l4_parser_with_empty_registry_routes_tape_direct() {
    // The struct-direct path requires a populated registry — an empty
    // registry on a nominally struct-direct grammar resolves to
    // TapeDirect rather than emitting a body that calls
    // begin_compound against a missing layout.
    let registry = bbnf_ir::registry::StructRegistry::default();
    let strategy = EmitStrategy::for_grammar("CssL4Parser", &registry);
    assert_eq!(
        strategy,
        EmitStrategy::TapeDirect,
        "CssL4Parser + empty registry must downgrade to TapeDirect (activation guard)",
    );
}

#[test]
fn google_sheets_parser_with_populated_registry_routes_struct_direct() {
    // AZ-I.W2-act.B2 — Sheets struct-direct activation. The resolver
    // returns `EmitStrategy::StructDirect` carrying the
    // `SheetsStructBuilder` / `SheetsDocument` paths the emitter
    // splices into the generated `parse()` body.
    let registry = populated_registry();
    let strategy = EmitStrategy::for_grammar("GoogleSheetsParser", &registry);
    let EmitStrategy::StructDirect { rust, ts, wasm } = strategy else {
        panic!(
            "GoogleSheetsParser + populated registry must route StructDirect; got {:?}",
            strategy
        );
    };
    assert_eq!(
        rust.builder_path,
        "crate::runtime::google_sheets::SheetsStructBuilder",
        "Sheets rust builder path must address SheetsStructBuilder",
    );
    assert_eq!(
        rust.document_path,
        "crate::runtime::google_sheets::SheetsDocument",
        "Sheets rust document path must address SheetsDocument",
    );
    assert!(ts.is_none(), "Sheets ts binding reserved for BA");
    assert!(wasm.is_none(), "Sheets wasm binding reserved for BA");
}

#[test]
fn google_sheets_grammar_alias_routes_struct_direct() {
    // The `GoogleSheetsGrammar` alias resolves identically to
    // `GoogleSheetsParser`, mirroring the JSON pattern (JsonParser /
    // JsonGrammar). Forward-compat with hand-authored test fixtures.
    let registry = populated_registry();
    let strategy = EmitStrategy::for_grammar("GoogleSheetsGrammar", &registry);
    assert!(
        matches!(strategy, EmitStrategy::StructDirect { .. }),
        "GoogleSheetsGrammar (alias) + populated registry routes StructDirect; got {:?}",
        strategy,
    );
}

#[test]
fn google_sheets_parser_with_empty_registry_routes_tape_direct() {
    // The activation guard downgrades a nominally-struct-direct
    // grammar to TapeDirect when the registry is empty.
    let registry = empty_registry();
    let strategy = EmitStrategy::for_grammar("GoogleSheetsParser", &registry);
    assert_eq!(
        strategy,
        EmitStrategy::TapeDirect,
        "GoogleSheetsParser + empty registry must downgrade to TapeDirect (activation guard)",
    );
}

#[test]
fn unknown_grammar_with_populated_registry_routes_tape_direct() {
    // Default fallthrough: any grammar the resolver does not explicitly
    // admit routes TapeDirect regardless of registry shape. This pins
    // the negative default so an accidental wildcard match-all in a
    // future refactor fails here.
    //
    // AZ-II.cutover.M Phase 3c — CsvParser, EbnfParser, MathParser,
    // BnfParser, CssPrettyParser are now all admitted onto StructDirect
    // alongside JSON / Sheets / CSS L4 / BBNF. The negative-default test
    // therefore probes a synthetic grammar ident the resolver does not
    // know about; the catch-all `_ => TapeDirect` arm must still hold.
    let registry = populated_registry();
    let strategy = EmitStrategy::for_grammar("UnknownFutureGrammar", &registry);
    assert_eq!(
        strategy,
        EmitStrategy::TapeDirect,
        "UnknownFutureGrammar must route TapeDirect (catch-all default)",
    );
}

#[test]
fn pipeline_resolver_matches_emitter_resolver() {
    // The pipeline-level adapter `pipeline::compile::resolve_emit_strategy`
    // delegates to `EmitStrategy::for_grammar`; the two paths must
    // produce identical strategies for any (ident, registry) pair.
    // This test pins the equivalence so a future drift between the
    // pipeline-side wrapper and the backend-side resolver fails
    // visibly here.
    let registry = populated_registry();
    let mut ir = bbnf_ir::GrammarIR::default();
    ir.struct_registry = registry;

    let pipeline_strategy = bbnf::pipeline::compile::resolve_emit_strategy("JsonParser", &ir);
    let emitter_strategy =
        EmitStrategy::for_grammar("JsonParser", &ir.struct_registry);
    assert_eq!(
        pipeline_strategy, emitter_strategy,
        "pipeline-side resolver must match emitter-side resolver byte-for-byte",
    );
}
