//! AX.W1r.1 — CSS L4 color parity gate.
//!
//! Exercises the IR-derived [`RustNamedTypes`] resolver introduced
//! in W1r.1. The pre-W1r.1 static `BINDINGS` slice +
//! `resolve_named_type` free function are deleted; the resolver now
//! builds its shape table by walking `ir.types` at `from_ir` time.
//!
//! The end-to-end `rgb(...)` → `Color` struct projection is already
//! covered by the decoder tests in `css_l4_color_view.rs`; this
//! file focuses on the resolver contract.

use bbnf::backend::rust::view::named_types::RustNamedTypes;
use bbnf_ir::passes::{plan_layout_with_cap, NamedTypeResolver};
use bbnf_ir::{GrammarIR, IrNode, IrRule, RuleId, RuleMeta, StringId, TypeDesc};

// ─── Helpers ──────────────────────────────────────────────────────────

/// Build a minimal `GrammarIR` whose string table carries the given
/// names at their corresponding `StringId`s. Used so the resolver's
/// lookup surface can be exercised without a full grammar compile.
fn ir_with_names(names: &[&str]) -> GrammarIR {
    let mut ir = GrammarIR::default();
    for n in names {
        ir.strings.push((*n).to_string());
    }
    ir
}

/// Build a minimal `GrammarIR` whose `ir.types` declares a single
/// rule projecting `TypeDesc::Named(sid)`. The rule's body is the
/// supplied `IrNode`. Lets us drive the W1r.1 inference walker with
/// hand-crafted bodies without spinning up the whole compile
/// pipeline.
fn ir_with_named_rule(rule_name: &str, sid_name: &str, body: IrNode) -> GrammarIR {
    let mut ir = GrammarIR::default();
    let name_sid = ir.strings.len() as StringId;
    ir.strings.push(rule_name.to_string());
    let type_sid = ir.strings.len() as StringId;
    ir.strings.push(sid_name.to_string());
    let rule_id: RuleId = 0;
    ir.rules.push(IrRule {
        id: rule_id,
        name: name_sid,
        body,
        meta: RuleMeta::default(),
        source_span: None,
    });
    ir.types.push((rule_id, TypeDesc::Named(type_sid)));
    ir
}

// ─── Resolver returns None when no Named types exist ─────────────────

#[test]
fn resolver_empty_when_no_named_types() {
    // Baseline: `ir.types` carries no `Named(sid)` entries, so the
    // resolver's binding map is empty and every `resolve_named` call
    // returns `None`.
    let ir = ir_with_names(&["Widget", "Stylesheet"]);
    let resolver = RustNamedTypes::from_ir(&ir);
    assert_eq!(resolver.binding_count(), 0);
    for sid in 0..ir.strings.len() as StringId {
        assert!(
            resolver.resolve_named(sid).is_none(),
            "unknown name {:?} must not resolve",
            ir.strings[sid as usize],
        );
    }
}

// ─── Single-leaf body infers (U32, U32) arena-handle shape ───────────

#[test]
fn regex_body_infers_arena_handle_shape() {
    // JSON-like: `rule = /regex/ -> ... : String` — the body kernel
    // is a bare regex, the resolver projects `(U32 offset, U32
    // length)` as the arena-backed owned-bytes shape.
    let mut ir = GrammarIR::default();
    let regex_sid = ir.strings.len() as StringId;
    ir.strings.push(r#"[a-z]+"#.to_string());
    let body = IrNode::Regex(regex_sid);
    let ir = {
        let _ = ir;
        ir_with_named_rule("string", "String", body)
    };
    let resolver = RustNamedTypes::from_ir(&ir);
    let type_sid = ir.strings.len() as StringId - 1;
    let resolved =
        resolver.resolve_named(type_sid).expect("String must resolve");
    assert_eq!(
        resolved,
        TypeDesc::Tuple(vec![TypeDesc::U32, TypeDesc::U32]),
        "owned-bytes projection shape",
    );
}

// ─── Seq body infers per-child scalar tuple ──────────────────────────

#[test]
fn seq_body_infers_scalar_tuple_from_type_map() {
    // Hand-shaped Seq body whose children project to distinct
    // scalars via the IR's `type_map` dag-keyed lookup. The
    // resolver walks the Seq, picks up each child's type, and
    // returns a `TypeDesc::Tuple(...)` of the scalars.
    //
    // This test is the shape of the CSS L4 colorFunction body —
    // `(colorType, colorValue, colorValue, colorValue, colorValue)`
    // with the types `(U8, F64, F64, F64, F64)`. The grammar's own
    // `-> input : Color` is not activated in current grammars (the
    // colorFunction rule is eliminated pre-type projection); this
    // hand-constructed IR exercises the shape inference surface
    // directly.
    use bbnf_ir::dag::GrammarDag;

    let mut ir = GrammarIR::default();
    let string_name = "colorLike";
    let named = "ColorLike";
    let name_sid = ir.strings.len() as StringId;
    ir.strings.push(string_name.to_string());
    let type_sid = ir.strings.len() as StringId;
    ir.strings.push(named.to_string());

    // Helper rules: `u8_leaf = ... -> 0u8`, `f64_leaf = /regex/ -> f64`.
    // Structural bodies do not matter — only the rule types that
    // `ir.types` reports through `Ref` resolution.
    let u8_rule_id: RuleId = 1;
    let f64_rule_id: RuleId = 2;
    ir.rules.push(IrRule {
        id: u8_rule_id,
        name: name_sid,
        body: IrNode::Epsilon,
        meta: RuleMeta::default(),
        source_span: None,
    });
    ir.rules.push(IrRule {
        id: f64_rule_id,
        name: name_sid,
        body: IrNode::Epsilon,
        meta: RuleMeta::default(),
        source_span: None,
    });
    ir.types.push((u8_rule_id, TypeDesc::U8));
    ir.types.push((f64_rule_id, TypeDesc::F64));

    let body = IrNode::Seq(vec![
        IrNode::Ref(u8_rule_id),
        IrNode::Ref(f64_rule_id),
        IrNode::Ref(f64_rule_id),
        IrNode::Ref(f64_rule_id),
        IrNode::Ref(f64_rule_id),
    ]);
    let rule_id: RuleId = 0;
    ir.rules.insert(
        0,
        IrRule {
            id: rule_id,
            name: name_sid,
            body,
            meta: RuleMeta::default(),
            source_span: None,
        },
    );
    ir.types.push((rule_id, TypeDesc::Named(type_sid)));
    ir.types.sort_by_key(|(id, _)| *id);
    ir.dag = Some(GrammarDag::from_ir(&ir));

    let resolver = RustNamedTypes::from_ir(&ir);
    let resolved = resolver
        .resolve_named(type_sid)
        .expect("ColorLike must resolve via Seq walker");
    assert_eq!(
        resolved,
        TypeDesc::Tuple(vec![
            TypeDesc::U8,
            TypeDesc::F64,
            TypeDesc::F64,
            TypeDesc::F64,
            TypeDesc::F64,
        ]),
    );
}

// ─── Planner admits the resolved tuple at LARGE_PAYLOAD_MAX ──────────

#[test]
fn resolved_color_like_tuple_plans_at_40_bytes_under_large_cap() {
    // AX.W1r.1 + AW.0.5: the 5-field (U8, F64, F64, F64, F64) tuple
    // the resolver infers for a colour-function-shaped rule must
    // plan to 40 B at natural alignment under the 64 B
    // `LARGE_PAYLOAD_MAX` cap. This is the layout the emitter's
    // `PayloadData::LargeAggregate` routing relies on.
    let fields: Vec<TypeDesc> = vec![
        TypeDesc::U8,
        TypeDesc::F64,
        TypeDesc::F64,
        TypeDesc::F64,
        TypeDesc::F64,
    ];

    // 16 B cap rejects (40 > 16).
    assert!(
        plan_layout_with_cap(&fields, 16).is_none(),
        "5-field colour tuple must NOT fit under 16 B MAX_PAYLOAD_BYTES",
    );

    // 64 B cap admits and plans to 40 B.
    let layout = plan_layout_with_cap(&fields, 64)
        .expect("5-field colour tuple must fit under 64 B LARGE_PAYLOAD_MAX");
    assert_eq!(layout.total_bytes, 40, "total bytes");
    assert_eq!(layout.fields.len(), 5, "field count");
    assert_eq!(layout.fields[0].offset, 0, "u8 @ 0");
    assert_eq!(layout.fields[1].offset, 8, "f64 @ 8 (align-bump)");
    assert_eq!(layout.fields[2].offset, 16, "f64 @ 16");
    assert_eq!(layout.fields[3].offset, 24, "f64 @ 24");
    assert_eq!(layout.fields[4].offset, 32, "f64 @ 32");
}
