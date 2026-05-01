//! AZ-IV.W0.3 — synthetic grammar strategy binding test (scaffold).
//!
//! Per Fermat hardening pass §F5
//! (`docs/tranches/AZ-IV/audit/HARDENING-2026-05-01-fermat.md`) and
//! AZ-IV §Hard Gates 2, "a synthetic grammar rename/addition test
//! fails if a new literal parser-name arm is required" in
//! [`bbnf_ir::registry::EmitStrategy::for_grammar`]
//! (`crates/ir/src/registry/strategy.rs:143-262`).
//!
//! This file is the scaffold W0.3 lands so W1.5 / W1.8 (the full
//! manifest-driven binding lift) can fill in additional rows and
//! cross-grammar dispatch as the runtime template eats the per-grammar
//! `from_rule_name` tables.
//!
//! # Totality contract — what passes here
//!
//! `EmitStrategy::for_grammar_with_manifest` consults a manifest-row
//! slice before falling through to the in-source literal arm-list.
//! When the synthetic grammar `__SyntheticParser__` registers exactly
//! one row in the slice, the resolver returns its declared substrate
//! binding without ever consulting the source-side arm-list.
//!
//! That is the W0.3 totality proof: a brand-new grammar registers and
//! resolves without a single source-arm edit.
//!
//! # Why a scaffold
//!
//! W0.3 may not change lowering / emitter surfaces beyond the regen
//! template (per `docs/tranches/AZ-IV/waves/W0.md` §File Bounds and
//! the W0.3 dispatch). The full lift — replacing the source-side arm
//! list at `crates/ir/src/registry/strategy.rs:143-262` with a
//! manifest-only resolver, and threading the manifest through the
//! emitter call sites — is W1.5 / W1.8's job. W0.3 ships the
//! substrate (the new `for_grammar_with_manifest` resolver and the
//! `ManifestStrategyEntry` row shape) plus this synthetic test.

use bbnf_ir::TypeDesc;
use bbnf_ir::registry::{
    EmitStrategy, LayoutKind, ManifestStrategyEntry, StructLayout, StructRegistry,
};

/// Construct a populated `StructRegistry` for the synthetic grammar.
/// One layout is enough — `EmitStrategy::for_grammar_with_manifest`
/// only checks `is_empty()` to decide whether StructDirect generation
/// is permitted; the layout's contents are not inspected by the
/// resolver itself.
fn populated_synth_registry() -> StructRegistry {
    let mut r = StructRegistry::new();
    r.insert(StructLayout {
        rule_id: 0,
        rule_name: "synthetic_root".to_string(),
        kind: LayoutKind::Struct,
        rule_type: TypeDesc::Tuple(Vec::new()),
        fields: Vec::new(),
    });
    r
}

#[test]
fn synthetic_grammar_resolves_through_manifest_row_without_source_arm() {
    // The synthetic grammar's parser-struct idents — neither name
    // appears in `EmitStrategy::for_grammar`'s literal arm-list, so
    // a fall-through to the source-side resolver would `panic!` with
    // "unknown production grammar".
    static SYNTH_IDENTS: &[&str] = &["__SyntheticParser__", "__SyntheticGrammar__"];

    let manifest_table: [ManifestStrategyEntry; 1] = [ManifestStrategyEntry {
        idents: SYNTH_IDENTS,
        rust_builder_path: "crate::runtime::__synthetic__::SyntheticStructBuilder",
        rust_document_path: "crate::runtime::__synthetic__::SyntheticDocument",
    }];

    let registry = populated_synth_registry();

    // Both ident aliases resolve to the same binding via the manifest
    // row. No literal arm in `strategy.rs` is consulted; if the
    // resolver were to fall through to `for_grammar`, it would panic
    // because neither ident is in the in-source allow-list.
    for ident in SYNTH_IDENTS {
        let strategy = EmitStrategy::for_grammar_with_manifest(ident, &registry, &manifest_table);
        let EmitStrategy::StructDirect { rust, ts, wasm } = strategy;
        assert_eq!(
            rust.builder_path, "crate::runtime::__synthetic__::SyntheticStructBuilder",
            "synthetic grammar `{ident}` must take its builder path from the manifest row, \
             not from any source-side arm",
        );
        assert_eq!(
            rust.document_path, "crate::runtime::__synthetic__::SyntheticDocument",
            "synthetic grammar `{ident}` must take its document path from the manifest row",
        );
        assert!(
            ts.is_none(),
            "W0.3 manifest row stops at the rust binding; W1.5 / W1.8 fills in ts / wasm slots",
        );
        assert!(
            wasm.is_none(),
            "W0.3 manifest row stops at the rust binding; W1.5 / W1.8 fills in ts / wasm slots",
        );
    }
}

#[test]
#[should_panic(expected = "unknown production grammar")]
fn manifest_resolver_panics_on_unknown_grammar_with_empty_manifest() {
    // AZ-IV.W1.8 — the W0.3 fallthrough-to-source-arms test is
    // retired. Pre-W1.8 the resolver fell through to a 9-arm
    // literal allow-list; W1.8 collapses that arm-list into a
    // canonical [`bbnf_ir::registry::PRODUCTION_MANIFEST_TABLE`]
    // and `for_grammar` itself routes through this same resolver.
    // When a caller passes an empty fixture manifest table AND the
    // grammar ident does not match any row, the resolver panics
    // with the offending ident — matching `for_grammar`'s
    // fail-closed contract per AZ-IV §Hard Gates 18.
    let registry = populated_synth_registry();
    let empty_manifest: [ManifestStrategyEntry; 0] = [];
    let _ = EmitStrategy::for_grammar_with_manifest("JsonParser", &registry, &empty_manifest);
}

#[test]
fn for_grammar_routes_json_through_production_manifest_table() {
    // AZ-IV.W1.8 — `EmitStrategy::for_grammar` is a thin alias for
    // `for_grammar_with_manifest(..., PRODUCTION_MANIFEST_TABLE)`.
    // The test confirms that production grammars resolve through
    // the canonical manifest table at the public API surface,
    // proving the 9-arm allow-list is gone.
    let registry = populated_synth_registry();
    let strategy = EmitStrategy::for_grammar("JsonParser", &registry);
    let EmitStrategy::StructDirect { rust, .. } = strategy;
    assert_eq!(
        rust.builder_path, "crate::runtime::json::JsonStructBuilder",
        "JsonParser must resolve through PRODUCTION_MANIFEST_TABLE at the \
         `for_grammar` API surface — proving the 9-arm allow-list is gone",
    );
}

#[test]
fn manifest_row_takes_priority_over_source_arm_for_same_ident() {
    // A manifest row keyed on an ident the source arm-list also
    // covers must win — W1.5 / W1.8's migration relies on this:
    // each grammar's manifest row supersedes its source arm without
    // requiring the source arm to be deleted in lock-step.
    static OVERRIDE_IDENTS: &[&str] = &["JsonParser"];
    let manifest_table: [ManifestStrategyEntry; 1] = [ManifestStrategyEntry {
        idents: OVERRIDE_IDENTS,
        rust_builder_path: "crate::runtime::__overridden__::OverrideBuilder",
        rust_document_path: "crate::runtime::__overridden__::OverrideDocument",
    }];

    let registry = populated_synth_registry();
    let strategy =
        EmitStrategy::for_grammar_with_manifest("JsonParser", &registry, &manifest_table);
    let EmitStrategy::StructDirect { rust, .. } = strategy;
    assert_eq!(
        rust.builder_path, "crate::runtime::__overridden__::OverrideBuilder",
        "manifest row must win over the source-side arm-list when both cover `JsonParser`",
    );
}

#[test]
#[should_panic(expected = "empty StructRegistry")]
fn manifest_resolver_panics_on_empty_registry() {
    // Parity with `for_grammar`: an empty registry is a hard
    // generation error, not a tape fallback. The manifest path must
    // honour the same invariant or it becomes a sneaky escape hatch.
    static SYNTH_IDENTS: &[&str] = &["__SyntheticParser__"];
    let manifest_table: [ManifestStrategyEntry; 1] = [ManifestStrategyEntry {
        idents: SYNTH_IDENTS,
        rust_builder_path: "crate::runtime::__synthetic__::SyntheticStructBuilder",
        rust_document_path: "crate::runtime::__synthetic__::SyntheticDocument",
    }];
    let empty_registry = StructRegistry::new();
    let _ = EmitStrategy::for_grammar_with_manifest(
        "__SyntheticParser__",
        &empty_registry,
        &manifest_table,
    );
}
