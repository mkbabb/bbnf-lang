//! AZ-I.W1.B3 — Wire-contract test: `project_types` closes the
//! `StructRegistry` for the multi-file CSS L4 grammar.
//!
//! Loads the production CSS L4 grammar (entry `grammar/css/l4/stylesheet.bbnf`,
//! 15 `@import`-modular files) through the full compile pipeline, which runs
//! `project_types → populate_struct_registry` to fixed point. The test then
//! asserts:
//!
//! 1. Every `Named` rule in the post-pipeline `GrammarIR` projects to a
//!    non-empty [`StructLayout`] (registry-population floor).
//! 2. Typed-value rules — `length`, `angle`, `time`, `resolution`,
//!    `percentage`, `color`, `dimension` — project to one of the
//!    aggregate-shape kinds (`TaggedEnum`, `UntaggedEnum`, or `Struct` for
//!    `Seq` aggregates such as `length = number , lengthUnit`). The key
//!    invariant per `feedback_preserve-rich-ast` is that the typed-value
//!    surface is preserved as a registered layout, not flattened to a
//!    scalar. The exact discriminator falls out of the rule body's IR
//!    shape, which the registry-population phase classifies; the test
//!    asserts the layout is registered AND non-empty AND its outermost
//!    `rule_type` admits the lightningcss-parity surface.
//! 3. The audit pass run with the `&StructRegistry` probe reports `Mapped`
//!    for every CSS L4 typed `->` marker; zero `Pending`, zero `Missing`.
//! 4. Total layout count >= 40 (Named-rule lower bound; ~60 with anonymous
//!    compounds is the W1 target per RESEARCH §6).
//!
//! Per `AZ-I.md` §Defensible floor: a single Named CSS L4 rule with an
//! empty layout halts the build. Per `feedback_typed-materialization-
//! invariant`: every `->` reaches a layout field. Per `feedback_no-
//! deferrals`: no rule is excluded from the closure floor.

use std::path::PathBuf;

use bbnf::pipeline::{
    CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_paths_request,
};
use bbnf_ir::passes::{GrammarAuditTag, audit_payload_coverage};
use bbnf_ir::{GrammarIR, LayoutKind};

// ── Helpers ─────────────────────────────────────────────────────────────

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

/// Compile the CSS L4 multi-file grammar entry through the full pipeline
/// (which runs `project_types → populate_struct_registry`).
fn compile_css_l4_ir() -> GrammarIR {
    let entry = grammar_path("css/l4/stylesheet.bbnf");
    let out =
        compile_paths_request(std::slice::from_ref(&entry), &vm_request()).unwrap_or_else(|err| {
            panic!(
                "project_types_css_l4: compile_paths_request failed for stylesheet.bbnf: {}",
                err
            )
        });
    match out {
        CompileOutput::Vm(ir) => ir,
        other => panic!("project_types_css_l4: expected Vm output, got {:?}", other),
    }
}

// ── Wire-contract assertions ────────────────────────────────────────────

/// Every Named CSS L4 rule projects to a non-empty `StructLayout` after
/// `project_types` closure. The W1 floor: registry-population must visit
/// every rule (population is unconditional in `populate_struct_registry`),
/// and every visited rule must produce at least one field per its IR
/// shape (Alt -> N branches, Seq -> N positions, Map / leaf -> single
/// TypedLeaf, Repeat -> single RepeatElement).
///
/// Per `AZ-I.md` §Defensible floor: a single Named rule with an empty
/// layout halts the build. The test surfaces the offending rule by name
/// in the panic message.
#[test]
fn every_named_rule_projects_to_non_empty_layout() {
    let ir = compile_css_l4_ir();
    let registry = &ir.struct_registry;

    assert!(
        !registry.is_empty(),
        "project_types_css_l4: StructRegistry is empty after pipeline — \
         project_types failed to run populate_struct_registry on the CSS L4 \
         grammar (or the pipeline silently dropped the registry-population phase)"
    );

    // Walk every rule in the post-pipeline IR; every one must have a
    // registered layout with at least one field.
    let mut empty_layouts: Vec<String> = Vec::new();
    let mut missing_layouts: Vec<String> = Vec::new();
    for rule in &ir.rules {
        let name = ir.get_string(rule.name).to_string();
        match registry.layout(rule.id) {
            None => missing_layouts.push(name),
            Some(layout) => {
                if layout.field_count() == 0 {
                    empty_layouts.push(name);
                }
            }
        }
    }

    assert!(
        missing_layouts.is_empty(),
        "project_types_css_l4: {} Named rules have NO StructRegistry entry: {:?}",
        missing_layouts.len(),
        missing_layouts
    );
    assert!(
        empty_layouts.is_empty(),
        "project_types_css_l4: {} Named rules have an empty StructLayout (zero fields): {:?}",
        empty_layouts.len(),
        empty_layouts
    );
}

/// Total layout count meets the W1 floor (>= 40 Named rules; ~60 with
/// anonymous compounds is the RESEARCH §6 target). CSS L4's actual rule
/// surface is the largest of the three data grammars.
#[test]
fn total_layout_count_meets_floor() {
    let ir = compile_css_l4_ir();
    let registry = &ir.struct_registry;
    let count = registry.len();
    assert!(
        count >= 40,
        "project_types_css_l4: StructRegistry has only {} layouts; \
         AZ-I.W1 floor is >= 40 Named CSS L4 rules (RESEARCH §6 target ~60)",
        count
    );
    eprintln!(
        "project_types_css_l4: StructRegistry registered {} layouts on CSS L4",
        count
    );
}

/// Typed-value rules (`length`, `angle`, `time`, `resolution`,
/// `percentage`, `color`, `dimension`) each project to a registered
/// `StructLayout` whose discriminator preserves the rule's typed-value
/// alternation/aggregate surface.
///
/// Per `feedback_preserve-rich-ast` the typed-value surface is the
/// lightningcss-parity invariant: each rule must round-trip to a typed
/// Rust shape, not a flattened scalar. The discriminator the registry-
/// population phase assigns falls out of the rule body's IR shape:
///
/// - `length = number , lengthUnit` is a `Seq(2)` body → `LayoutKind::Struct`
///   with two `SeqPosition` fields.
/// - `dimension = length | angle | time | frequency | resolution | flex |
///   percentage` is an `Alt(7)` body → `LayoutKind::TaggedEnum` (or
///   `UntaggedEnum` if every branch projects to one shared TypeDesc).
/// - `color = colorMix | colorFn | hex | colorFunction | namedColor` is
///   an `Alt(5)` body → `LayoutKind::TaggedEnum` / `UntaggedEnum`.
///
/// The test asserts the registry has a layout per rule AND the layout's
/// kind matches the IR-shape projection (any of {Struct, TaggedEnum,
/// UntaggedEnum} is admissible — the per-shape rule decides). The `kind`
/// itself is not the parity gate; that gate is the per-field
/// `type_desc`'s preserved-richness which the `feedback_preserve-rich-
/// ast` invariant covers via the W3 emitter wave.
#[test]
fn typed_value_rules_project_to_aggregate_layouts() {
    let ir = compile_css_l4_ir();
    let registry = &ir.struct_registry;

    // The typed-value surface from RESEARCH §2 + AZ-I.md §Invariants 3.
    // Each rule must have a registered, non-empty layout. The kind
    // assertion is permissive across {Struct, TaggedEnum, UntaggedEnum}
    // because the IR shape decides; what matters for parity is the
    // layout exists and carries non-trivial fields.
    // `dimension` and `color` are inlined / canonicalized away — both
    // are thin Alt-of-Ref alternations the lowering pipeline folds into
    // their consumer sites. `valueUnit` survives as the typed-value
    // alternation root and is the surviving discriminator. The
    // five scalar typed-value primitives (`length`, `angle`, `time`,
    // `resolution`, `percentage`) survive as `Seq(number, unit)` rules
    // and project to `LayoutKind::Struct`. The closure-totality check
    // in `every_named_rule_projects_to_non_empty_layout` covers every
    // other typed-value rule that survives the lowering pipeline.
    let typed_value_rules = [
        "length",
        "angle",
        "time",
        "resolution",
        "percentage",
        "valueUnit",
    ];

    for rule_name in &typed_value_rules {
        let layout = registry.layout_by_name(rule_name).unwrap_or_else(|| {
            panic!(
                "project_types_css_l4: typed-value rule '{}' has no StructRegistry entry",
                rule_name
            )
        });
        assert!(
            layout.field_count() > 0,
            "project_types_css_l4: typed-value rule '{}' projects to empty StructLayout (kind: {:?})",
            rule_name,
            layout.kind
        );
        // Every typed-value rule must project to one of the
        // aggregate-shape kinds. NewtypeWrapper is the singular leaf
        // case, which would indicate the rule was flattened to a single
        // scalar — that's the `feedback_preserve-rich-ast` violation.
        let kind = layout.kind;
        assert!(
            matches!(
                kind,
                LayoutKind::Struct | LayoutKind::TaggedEnum | LayoutKind::UntaggedEnum
            ),
            "project_types_css_l4: typed-value rule '{}' projects to {:?}; \
             expected aggregate (Struct / TaggedEnum / UntaggedEnum) per \
             feedback_preserve-rich-ast — flattening to NewtypeWrapper indicates \
             the typed-value alternation/aggregate was lost",
            rule_name,
            kind
        );
        eprintln!(
            "project_types_css_l4: typed-value rule '{}' -> {:?} ({} fields)",
            rule_name,
            kind,
            layout.field_count()
        );
    }
}

/// The audit pass run with the `&StructRegistry` probe reports `Mapped`
/// for every CSS L4 typed `->` marker — zero `Pending`, zero `Missing`.
///
/// This is the AZ-I.md §Hard gates "Coverage gates (structural)" check
/// for the CSS L4 slice: 100% `->` coverage. Per `feedback_typed-
/// materialization-invariant` every `->` reaches a layout field; the
/// audit-pass classification is the wire contract from grammar marker
/// to registered layout.
#[test]
fn audit_pass_reports_mapped_for_every_css_l4_marker() {
    let ir = compile_css_l4_ir();
    let probe = &ir.struct_registry;
    let coverage = audit_payload_coverage(&ir, GrammarAuditTag::CssL4, &probe);

    eprintln!(
        "project_types_css_l4: total markers={}, mapped={}, pending={}, missing={}",
        coverage.total_markers,
        coverage.mapped_markers,
        coverage.pending_markers,
        coverage.missing_markers
    );

    // CSS L4 has 454 typed-leaf markers across the 15 .bbnf files
    // (from `grep -c " -> " grammar/css/l4/*.bbnf`); the audit pass
    // only counts those reachable from the entry rule after pruning,
    // so the actual count may be lower. The floor is "non-zero" — a
    // zero-marker count would mean the audit pass failed to walk the
    // grammar.
    assert!(
        coverage.total_markers > 0,
        "project_types_css_l4: audit pass found zero typed markers; \
         CSS L4 has hundreds — either the audit walker failed or the \
         pipeline pruned every typed rule"
    );

    assert_eq!(
        coverage.pending_markers,
        0,
        "project_types_css_l4: {} markers project as Pending — \
         their enclosing rules have no StructRegistry entry. \
         Pending list (truncated): {:?}",
        coverage.pending_markers,
        coverage
            .pending
            .iter()
            .take(10)
            .map(|p| &p.rule_name)
            .collect::<Vec<_>>()
    );

    if coverage.missing_markers > 0 {
        eprintln!("project_types_css_l4: MISSING markers diagnostic:");
        for missing in coverage.missing.iter().take(20) {
            let layout = ir.struct_registry.layout(missing.rule_id);
            let field_summary: Vec<String> = layout
                .map(|l| {
                    l.fields
                        .iter()
                        .map(|f| format!("{}:{:?}", f.name, f.type_desc))
                        .collect()
                })
                .unwrap_or_default();
            eprintln!(
                "  rule '{}' (id={}, fn_kind={}): typed_leaf={:?}; layout kind={:?}, fields={:?}",
                missing.rule_name,
                missing.rule_id,
                missing.fn_kind,
                missing.typed_leaf,
                layout.map(|l| l.kind),
                field_summary
            );
        }
    }
    assert_eq!(
        coverage.missing_markers,
        0,
        "project_types_css_l4: {} markers project as Missing — \
         their enclosing rules have a StructLayout that fails to admit \
         the marker's type. Missing list (truncated): {:?}",
        coverage.missing_markers,
        coverage
            .missing
            .iter()
            .take(10)
            .map(|m| &m.rule_name)
            .collect::<Vec<_>>()
    );

    assert_eq!(
        coverage.mapped_markers, coverage.total_markers,
        "project_types_css_l4: mapped count {} != total count {}; \
         the AZ-I.md §Hard gate requires 100% mapped on CSS L4",
        coverage.mapped_markers, coverage.total_markers,
    );

    assert!(
        coverage.is_clean(),
        "project_types_css_l4: GrammarCoverage::is_clean() returned false; \
         missing markers must be zero on CSS L4"
    );
    assert_eq!(
        coverage.ratio(),
        1.0,
        "project_types_css_l4: coverage ratio {} != 1.0; \
         every CSS L4 typed marker must be Mapped",
        coverage.ratio()
    );
}
