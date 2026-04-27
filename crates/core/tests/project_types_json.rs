//! AZ-I.W1.B1 — JSON `project_types` registry-closure wire contract.
//!
//! Loads `grammar/json/json.bbnf` through the existing
//! `compile_paths_request(CompileTarget::Vm)` pipeline (which runs
//! `project_types` and populates `ir.struct_registry`), then asserts
//! that the registry-shape gate from AZ-I.W1 holds end-to-end:
//!
//! 1. Every `Named` JSON rule (`null`, `bool`, `number`, `string`,
//!    `array`, `pair`, `object`, `value`, plus the `comma` / `colon`
//!    helpers) has a non-empty [`StructLayout`].
//! 2. The `value` rule's layout is [`LayoutKind::TaggedEnum`] (the
//!    six-way alternation `object | array | string | number | bool |
//!    null` is heterogeneous and projects per W1.A semantics; an
//!    [`LayoutKind::UntaggedEnum`] is also acceptable should every
//!    branch type collapse to a single shared `TypeDesc` under
//!    closure).
//! 3. The `pair` rule's layout is [`LayoutKind::Struct`] with at
//!    least two fields (key + value, Skip-flattened from `string,
//!    colon >> value`).
//! 4. The `array` rule's layout has at least one field whose
//!    [`FieldSource`] is `RepeatElement`, since the body's
//!    `(value << comma ? ) *` ultimately classifies as a `Repeat`
//!    structure under the layout-builder's classify-body
//!    discipline.
//! 5. The audit pass run with the `&StructRegistry` probe reports
//!    `Mapped` for every typed-leaf marker present in `json.bbnf` —
//!    zero `Pending`, zero `Missing`.
//! 6. The total registered layout count meets RESEARCH §6's "~7
//!    layouts" expectation. We assert `len() >= 7` to allow
//!    legitimate anonymous-compound additions without making the
//!    gate brittle on future grammar refactors.
//!
//! Hard-fail-and-block per AZ-I.Q2 disposition: if `project_types`
//! fails to close on JSON post-edit, the test fails with a
//! diagnostic naming the unclosed rule. No `#[ignore]`, no
//! `#[should_panic]` — `feedback_no-deferrals` and
//! `feedback_no-workarounds`.

use std::path::PathBuf;

use bbnf::pipeline::{
    CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_paths_request,
};
use bbnf_ir::passes::{GrammarAuditTag, audit_payload_coverage};
use bbnf_ir::{GrammarIR, StructLayout};

/// Resolve the JSON grammar path from `CARGO_MANIFEST_DIR`. The crate
/// manifest lives at `crates/core/`, so `../../grammar/json/json.bbnf`
/// is the project-relative entry.
fn json_grammar_path() -> PathBuf {
    let manifest = env!("CARGO_MANIFEST_DIR");
    PathBuf::from(manifest).join("../../grammar/json/json.bbnf")
}

/// Compile JSON through the VM target. `CompileTarget::Vm` is the
/// minimal target that runs `project_types` end-to-end and returns
/// the populated `GrammarIR` (including `ir.struct_registry`); using
/// it keeps the test crate's link surface minimal vs the Rust target
/// which links the full backend driver.
fn compile_json_ir() -> GrammarIR {
    let entry = json_grammar_path();
    let request = CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Vm,
    };
    let out = compile_paths_request(std::slice::from_ref(&entry), &request)
        .expect("compile_paths_request failed for grammar/json/json.bbnf");
    match out {
        CompileOutput::Vm(ir) => ir,
        other => panic!("expected VM output for JSON, got {other:?}"),
    }
}

/// Resolve a registered layout by rule name; panics with a
/// build-stop-style diagnostic if the lookup fails. Mirrors the
/// hard-fail-and-block discipline AZ-I.W1 enforces on the registry
/// closure.
fn require_layout<'a>(ir: &'a GrammarIR, rule_name: &str) -> &'a StructLayout {
    ir.struct_registry
        .layout_by_name(rule_name)
        .unwrap_or_else(|| {
            panic!(
                "AZ-I.W1.B1: rule `{rule_name}` has no registered StructLayout. \
                 `project_types` did not close on this rule. Available rule \
                 names: {:?}",
                ir.struct_registry
                    .iter()
                    .map(|l| l.rule_name.as_str())
                    .collect::<Vec<_>>()
            )
        })
}

// ── Tests ───────────────────────────────────────────────────────────────

/// Every Named rule that survives lowering must have a non-empty
/// `StructLayout`. The bbnf pipeline inlines / prunes helper rules
/// like single-token aliases (`comma = "," ?w`, `colon = ":" ?w`)
/// which are absorbed into their use-sites and dropped from
/// `ir.rules`; the registry walks `ir.rules` post-pipeline so the
/// closure-coverage gate is "every surviving rule has a layout"
/// rather than "every grammar-declared rule has a layout".
///
/// The eight load-bearing JSON rules — the four primary shapes
/// (`value` / `array` / `object` / `pair`) plus the four typed-leaf
/// rules (`null` / `bool` / `number` / `string`) — must always
/// survive. Their layouts are the AZ-I gate per RESEARCH §6.
#[test]
fn every_named_json_rule_has_a_struct_layout() {
    let ir = compile_json_ir();

    // 1. Every rule in `ir.rules` (post-lowering, post-pruning) has
    //    a registered layout with at least one field. Missing-rule
    //    coverage is the registry-closure regression signal.
    for rule in &ir.rules {
        let rule_name = ir.get_string(rule.name).to_string();
        let layout = ir
            .struct_registry
            .layout(rule.id)
            .unwrap_or_else(|| {
                panic!(
                    "AZ-I.W1.B1: rule `{rule_name}` (id={}) has no registered \
                     StructLayout. `project_types` did not close on this \
                     rule.",
                    rule.id
                )
            });
        assert!(
            layout.field_count() > 0,
            "AZ-I.W1.B1: rule `{rule_name}` has empty StructLayout (kind: \
             {:?}). Every surviving rule must have at least one field \
             after closure.",
            layout.kind
        );
    }

    // 2. The eight load-bearing JSON rules are ALWAYS present after
    //    lowering. Their absence indicates a regression in either
    //    grammar parsing or in the inline-pass admission policy.
    let json_load_bearing = [
        "null", "bool", "number", "string", "array", "pair", "object", "value",
    ];
    for name in json_load_bearing {
        let layout = require_layout(&ir, name);
        assert!(
            layout.field_count() > 0,
            "AZ-I.W1.B1: load-bearing rule `{name}` has empty \
             StructLayout (kind: {:?}). RESEARCH §6 expects this rule's \
             layout populated.",
            layout.kind
        );
    }
}

/// `value` is the central tagged-enum rule for JSON: `object | array
/// | string | number | bool | null`. Six branches, each with a
/// distinct payload type, project as `LayoutKind::TaggedEnum` with
/// six `BranchTag` fields.
#[test]
fn value_rule_layout_is_tagged_enum_with_six_branches() {
    let ir = compile_json_ir();
    let layout = require_layout(&ir, "value");

    // Either `TaggedEnum` (heterogeneous) or `UntaggedEnum` (every
    // branch shares one `TypeDesc`) is acceptable. The grammar's six
    // distinct branches normally project as `TaggedEnum`; the
    // `UntaggedEnum` admission is for the homogeneous-collapse case
    // documented in `crates/ir/src/registry/struct.rs::LayoutKind`.
    assert!(
        layout.is_tagged_enum() || layout.is_untagged_enum(),
        "AZ-I.W1.B1: `value` must project as a tagged or untagged \
         enum (Alt → TaggedEnum/UntaggedEnum); got {:?}",
        layout.kind
    );

    // Six branches: object | array | string | number | bool | null.
    assert_eq!(
        layout.field_count(),
        6,
        "AZ-I.W1.B1: `value` has {} fields; expected six (one per Alt \
         branch — object / array / string / number / bool / null)",
        layout.field_count()
    );

    // Every field is a BranchTag.
    for (idx, field) in layout.fields.iter().enumerate() {
        assert!(
            field.is_branch_tag(),
            "AZ-I.W1.B1: `value` field {idx} is not BranchTag (source: \
             {:?})",
            field.source
        );
        assert_eq!(field.branch_index(), Some(idx as u32));
    }
}

/// `pair = string, colon >> value` — two-positional Seq once `Skip`
/// (the `>>` discard) flattens out, projecting as
/// `LayoutKind::Struct` with two fields.
#[test]
fn pair_rule_layout_is_struct_with_at_least_two_fields() {
    let ir = compile_json_ir();
    let layout = require_layout(&ir, "pair");

    assert!(
        layout.is_struct(),
        "AZ-I.W1.B1: `pair` must project as Struct (Seq → Struct); \
         got {:?}",
        layout.kind
    );
    // Field count is at least two — one for `string`, one for
    // `value`. The middle `colon` term is a `Skip(_, colon)` and
    // discards from the Seq. The exact post-Skip count depends on
    // closure shape (some closures retain a 2-tuple, others lift
    // the `Skip(string, Next(colon, value))` into a 2-position Seq);
    // both are admissible.
    assert!(
        layout.field_count() >= 2,
        "AZ-I.W1.B1: `pair` has {} fields; expected >= 2 (key + \
         value)",
        layout.field_count()
    );
}

/// `array = "[" >> ((value << comma ?) *)?w << "]"` — the outer
/// `Skip` chain unwraps to the `Repeat` over `(value << comma ?)`.
/// Layout has at least one `RepeatElement` field carrying the
/// element's `TypeDesc`.
#[test]
fn array_rule_layout_carries_repeat_element_field() {
    let ir = compile_json_ir();
    let layout = require_layout(&ir, "array");

    assert!(
        layout.field_count() >= 1,
        "AZ-I.W1.B1: `array` has zero fields; expected at least one \
         (the Repeat element)"
    );

    let has_repeat_element = layout
        .fields
        .iter()
        .any(|f| f.is_repeat_element());
    assert!(
        has_repeat_element,
        "AZ-I.W1.B1: `array` has no `RepeatElement` field; expected \
         the `(value << comma ?) *` Repeat to project at least one. \
         Field provenances: {:?}",
        layout
            .fields
            .iter()
            .map(|f| f.source)
            .collect::<Vec<_>>()
    );
}

/// `object = "{" >> ((pair << comma ?) *)?w << "}"` — same shape as
/// `array` but over `pair`. Exposes the `Repeat<pair>` structure.
#[test]
fn object_rule_layout_carries_repeat_element_field() {
    let ir = compile_json_ir();
    let layout = require_layout(&ir, "object");

    assert!(
        layout.field_count() >= 1,
        "AZ-I.W1.B1: `object` has zero fields; expected at least one \
         (the Repeat element)"
    );

    let has_repeat_element = layout
        .fields
        .iter()
        .any(|f| f.is_repeat_element());
    assert!(
        has_repeat_element,
        "AZ-I.W1.B1: `object` has no `RepeatElement` field; expected \
         the `(pair << comma ?) *` Repeat to project at least one. \
         Field provenances: {:?}",
        layout
            .fields
            .iter()
            .map(|f| f.source)
            .collect::<Vec<_>>()
    );
}

/// `null = "null" -> 0u8` — a typed-leaf rule. Projects as
/// `LayoutKind::NewtypeWrapper` with a single `TypedLeaf` field whose
/// `TypeDesc` is the typed `->` payload.
#[test]
fn null_rule_layout_is_newtype_wrapper_with_typed_leaf() {
    let ir = compile_json_ir();
    let layout = require_layout(&ir, "null");

    // NewtypeWrapper for primitive payloads, single-field Struct
    // otherwise; per `feedback_pluggable-components` we query the
    // accessor surface rather than the discriminator directly.
    assert_eq!(
        layout.field_count(),
        1,
        "AZ-I.W1.B1: `null` has {} fields; expected one (the typed \
         `-> 0u8` leaf)",
        layout.field_count()
    );
    let field = &layout.fields[0];
    assert!(
        field.is_typed_leaf(),
        "AZ-I.W1.B1: `null`'s sole field is not a TypedLeaf (source: \
         {:?})",
        field.source
    );
}

/// `bool = "true" -> true | "false" -> false` — a homogeneous Alt
/// over two typed leaves. Projects per `populate_struct_registry`'s
/// homogeneous-Alt rule into `UntaggedEnum` with two `BranchTag`
/// fields (or `TaggedEnum` if per-branch types diverge).
#[test]
fn bool_rule_layout_has_two_branch_tags() {
    let ir = compile_json_ir();
    let layout = require_layout(&ir, "bool");

    assert!(
        layout.is_tagged_enum() || layout.is_untagged_enum(),
        "AZ-I.W1.B1: `bool` must project as Alt → enum (kind: {:?})",
        layout.kind
    );
    assert_eq!(
        layout.field_count(),
        2,
        "AZ-I.W1.B1: `bool` has {} fields; expected two (true/false \
         branches)",
        layout.field_count()
    );
    for field in &layout.fields {
        assert!(
            field.is_branch_tag(),
            "AZ-I.W1.B1: `bool` branch field is not BranchTag \
             (source: {:?})",
            field.source
        );
    }
}

/// The audit pass run with the `&StructRegistry` probe must report
/// `Mapped` for every typed `->` marker in JSON post-closure. Zero
/// `Pending` (registry empty for the rule) and zero `Missing`
/// (registry has no field admitting the typed leaf) is the AZ-I gate.
#[test]
fn audit_pass_reports_one_hundred_percent_mapped_on_json() {
    let ir = compile_json_ir();
    let probe = &ir.struct_registry;
    let coverage = audit_payload_coverage(&ir, GrammarAuditTag::Json, &probe);

    assert!(
        coverage.total_markers > 0,
        "AZ-I.W1.B1: JSON has zero typed `->` markers — the grammar \
         lost its typed-leaf annotations"
    );
    assert_eq!(
        coverage.pending_markers, 0,
        "AZ-I.W1.B1: {} typed `->` markers are Pending (rule's \
         StructLayout is empty). Pending markers: {:#?}",
        coverage.pending_markers, coverage.pending
    );
    assert_eq!(
        coverage.missing_markers, 0,
        "AZ-I.W1.B1: {} typed `->` markers are Missing (rule's \
         StructLayout has no field admitting the typed leaf). \
         Missing markers: {:#?}",
        coverage.missing_markers, coverage.missing
    );
    assert_eq!(
        coverage.mapped_markers, coverage.total_markers,
        "AZ-I.W1.B1: mapped {} of {} markers; gate requires 100% \
         coverage. Pending: {:#?} Missing: {:#?}",
        coverage.mapped_markers, coverage.total_markers,
        coverage.pending, coverage.missing,
    );
    assert!(coverage.is_clean());
    assert_eq!(coverage.ratio(), 1.0);
}

/// Total registered layouts on JSON must meet RESEARCH §6's "~7
/// layouts" expectation. The lower bound is the four primary rules
/// (`value`, `array`, `object`, `pair`) plus the four typed-leaf
/// rules (`null`, `bool`, `number`, `string`) — `>= 7` makes the
/// gate robust against legitimate anonymous-compound additions
/// during closure without being brittle to grammar additions.
#[test]
fn json_struct_registry_has_at_least_seven_layouts() {
    let ir = compile_json_ir();
    let count = ir.struct_registry.len();

    // Diagnostic dump: every layout's name + kind + field count is
    // emitted to stderr so a regression that lands a wrong shape
    // surfaces in the test log without a re-run. The summary row
    // gates the assertion.
    eprintln!(
        "AZ-I.W1.B1 JSON registry summary: {count} layouts, {} rules in \
         ir.rules",
        ir.rules.len()
    );
    for layout in ir.struct_registry.iter() {
        eprintln!(
            "  {} (id={}) -> {:?} fields={} rule_type={:?}",
            layout.rule_name,
            layout.rule_id,
            layout.kind,
            layout.field_count(),
            layout.rule_type,
        );
    }

    assert!(
        count >= 7,
        "AZ-I.W1.B1: JSON `StructRegistry` has {count} layouts; RESEARCH \
         §6 expects ~7. Layouts: {:?}",
        ir.struct_registry
            .iter()
            .map(|l| (l.rule_name.as_str(), l.kind))
            .collect::<Vec<_>>()
    );
}
