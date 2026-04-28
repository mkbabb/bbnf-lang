//! AZ-I.W2.RA — leaf test for the `EmitStrategy::for_grammar`
//! resolver.
//!
//! Exercises the resolver's three load-bearing dispatch axes:
//!
//! 1. JSON's parser ident (`"JsonParser"`) + populated registry
//!    routes to `EmitStrategy::StructDirect` with the stable
//!    builder/document path strings the emitter splices into
//!    `parse()`'s body.
//! 2. JSON's plan-nominal alias (`"JsonGrammar"`) + populated
//!    registry routes the same way (forward-compat with hand-
//!    written test fixtures per the W2-EMITTER-REWIRE plan §1).
//! 3. BBNF (`"BbnfBootstrap"`) + populated registry routes to
//!    `EmitStrategy::TapeDirect` — only JSON gets struct-direct
//!    in W2; the resolver explicitly filters by grammar identity,
//!    not just by registry-emptiness.
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
//! new struct-direct grammar (Sheets W2.B, CSS L4 W3) extends the
//! resolver with a new arm and lands a sibling test here.

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
fn json_parser_with_populated_registry_routes_tape_direct_post_w2_close() {
    // AZ-I.W2 close ceremony: substrate landed but activation
    // reverted to TapeDirect per W2.md §Reversal. JsonParser +
    // populated registry resolves to TapeDirect until the W2-act
    // follow-on wave migrates `parsed.view()` / `parsed.to_value()`
    // callers, recodes the parity harnesses, and runs the cargo
    // bench gate. The substrate (StructBuilder + JsonDocument +
    // 9 per-shape struct-direct paths) exists; only the resolver
    // arm activating it is gated.
    let registry = populated_registry();
    let strategy = EmitStrategy::for_grammar("JsonParser", &registry);
    assert!(
        matches!(strategy, EmitStrategy::TapeDirect),
        "Post-W2-close: JsonParser + populated registry routes TapeDirect (activation gated on W2-act); got {:?}",
        strategy,
    );
}

#[test]
fn json_grammar_alias_routes_tape_direct_post_w2_close() {
    // Forward-compat alias resolves identically to JsonParser; the
    // post-W2-close-ceremony resolver collapses both onto TapeDirect
    // until W2-act activates struct-direct.
    let registry = populated_registry();
    let strategy = EmitStrategy::for_grammar("JsonGrammar", &registry);
    assert!(
        matches!(strategy, EmitStrategy::TapeDirect),
        "Post-W2-close: JsonGrammar (alias) + populated registry routes TapeDirect; got {:?}",
        strategy,
    );
}

#[test]
fn bbnf_with_populated_registry_routes_tape_direct() {
    // BBNF's project_types pass populates a non-trivial registry,
    // but BBNF is exempt from the W2 struct-direct activation
    // (AZ-I.md §Invariant 4: "BBNF continues on tape" — the
    // bootstrap parser reads / writes tape records throughout
    // AZ-I; AZ-II cuts BBNF over). This test pins the resolver's
    // identity-based filter: grammar name, not registry shape,
    // gates struct-direct admission.
    let registry = populated_registry();
    let strategy = EmitStrategy::for_grammar("BbnfBootstrap", &registry);
    assert_eq!(
        strategy,
        EmitStrategy::TapeDirect,
        "BbnfBootstrap must route TapeDirect even with populated registry",
    );
}

#[test]
fn bbnf_with_empty_registry_routes_tape_direct() {
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
fn unknown_grammar_with_populated_registry_routes_tape_direct() {
    // Default fallthrough: any grammar the resolver does not
    // explicitly admit (CssL4Parser, GoogleSheetsParser pre-W2.B,
    // CsvParser, EbnfParser, MathParser, BnfParser) routes
    // TapeDirect regardless of registry shape. W2.B / W3
    // extend the resolver by adding positive arms for the new
    // grammars; this test pins the negative default so an
    // accidental wildcard match-all in a future refactor fails
    // here.
    let registry = populated_registry();
    let strategy = EmitStrategy::for_grammar("CssL4Parser", &registry);
    assert_eq!(
        strategy,
        EmitStrategy::TapeDirect,
        "CssL4Parser must route TapeDirect in W2 (W3 activates struct-direct)",
    );
    let strategy = EmitStrategy::for_grammar("GoogleSheetsParser", &registry);
    assert_eq!(
        strategy,
        EmitStrategy::TapeDirect,
        "GoogleSheetsParser must route TapeDirect in W2.RA (W2.B activates struct-direct)",
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
