use std::collections::HashMap;

use bbnf_ir::passes::types::{TypeObligation, project_types};
use bbnf_ir::{AltBranch, FnDescriptor, GrammarIR, IrNode, IrRule, RuleId, RuleMeta, TypeDesc};

fn make_ir(rules: Vec<IrRule>) -> GrammarIR {
    GrammarIR {
        entry: 0,
        rules,
        strings: vec!["r0".into(), "r1".into(), "r2".into(), "lit".into()],
        fns: vec![],
        types: vec![],
        follow_sets: HashMap::new(),
        ws_pattern: None,
        collapse_simple_spans: false,
        debug_all: false,
        debug_labels: Vec::new(),
        type_map: None,
        pattern_annotations: std::collections::HashMap::new(),
        regex_info: std::collections::HashMap::new(),
        node_facts: HashMap::new(),
        recognizer_decisions: HashMap::new(),
        delim_scan_configs: std::collections::HashMap::new(),
        key_dispatch_configs: std::collections::HashMap::new(),
        context_facts: std::collections::HashMap::new(),
        has_family_recognizers: false,
        regex_engine_decisions: std::collections::HashMap::new(),
        dag: None,
        cost_config: bbnf_ir::CostConfig::default(),
        type_desc_interner: bbnf_ir::TypeDescInterner::new(),
        materialization: std::collections::HashMap::new(),
        string_index: std::collections::HashMap::new(),
        payload_layouts: std::collections::HashMap::new(),
        structural_alphabet: None,
        push_fingerprint: None,
        dedup_eligible_rules: Vec::new(),

        shape_assignments: bbnf_ir::passes::recognizers::shape_dispatch::ShapeAssignments::default(
        ),
        eclass_facts: std::collections::HashMap::new(),
        shape_dict_templates: Vec::new(),
        shape_dict_selection: Vec::new(),
        keyword_branches: std::collections::HashMap::new(),
        disjoint_first_tables: std::collections::HashMap::new(),
        pattern_alphabets: std::collections::HashMap::new(),
        ctns_lifts: std::collections::HashSet::new(),
        struct_registry: bbnf_ir::StructRegistry::default(),
        type_obligations: Vec::new(),
    }
}

fn rule(id: RuleId, body: IrNode) -> IrRule {
    IrRule {
        id,
        name: id,
        body,
        meta: RuleMeta::default(),
        source_span: None,
    }
}

fn alt(nodes: Vec<IrNode>) -> IrNode {
    IrNode::Alt(
        nodes
            .into_iter()
            .map(|n| AltBranch {
                node: n,
                first_set: None,
            })
            .collect(),
        None,
    )
}

fn get_type(ir: &GrammarIR, id: RuleId) -> &TypeDesc {
    ir.types
        .iter()
        .find(|(rid, _)| *rid == id)
        .map(|(_, t)| t)
        .unwrap()
}

#[test]
fn literal_is_span() {
    let mut ir = make_ir(vec![rule(0, IrNode::Literal(3))]);
    bbnf_ir::dag::ensure_dag(&mut ir);
    project_types(&mut ir);
    assert_eq!(*get_type(&ir, 0), TypeDesc::Span);
}

#[test]
fn regex_is_span() {
    let mut ir = make_ir(vec![rule(0, IrNode::Regex(3))]);
    bbnf_ir::dag::ensure_dag(&mut ir);
    project_types(&mut ir);
    assert_eq!(*get_type(&ir, 0), TypeDesc::Span);
}

#[test]
fn optional_span_collapses() {
    // Optional(Literal) -> Span (not Option<Span>)
    let mut ir = make_ir(vec![rule(
        0,
        IrNode::Repeat {
            inner: Box::new(IrNode::Literal(3)),
            lo: 0,
            hi: 1,
        },
    )]);
    bbnf_ir::dag::ensure_dag(&mut ir);
    project_types(&mut ir);
    assert_eq!(*get_type(&ir, 0), TypeDesc::Span);
}

#[test]
fn many_span_collapses() {
    let mut ir = make_ir(vec![rule(
        0,
        IrNode::Repeat {
            inner: Box::new(IrNode::Literal(3)),
            lo: 0,
            hi: u32::MAX,
        },
    )]);
    bbnf_ir::dag::ensure_dag(&mut ir);
    project_types(&mut ir);
    assert_eq!(*get_type(&ir, 0), TypeDesc::Span);
}

#[test]
fn many_non_span_is_vec() {
    // Rule 1 (leaf) first, then rule 0 references it (topological order).
    let mut ir = make_ir(vec![
        rule(1, alt(vec![IrNode::Literal(2), IrNode::Regex(3)])),
        rule(
            0,
            IrNode::Repeat {
                inner: Box::new(IrNode::Ref(1)),
                lo: 0,
                hi: u32::MAX,
            },
        ),
    ]);
    // Rule 1: Alt(Span, Span) -> Span. Ref(1) in Vec context returns Enum
    // (Vec provides heap indirection, so Box is unnecessary), so many(Enum) -> Vec(Enum).
    bbnf_ir::dag::ensure_dag(&mut ir);
    project_types(&mut ir);
    assert_eq!(*get_type(&ir, 0), TypeDesc::Vec(Box::new(TypeDesc::Enum)));
}

#[test]
fn seq_consecutive_span_compression() {
    // Seq(Lit, Lit, Ref(Span)) -> Span (all consecutive Spans merge)
    let mut ir = make_ir(vec![rule(
        0,
        IrNode::Seq(vec![
            IrNode::Literal(2),
            IrNode::Literal(3),
            IrNode::Regex(3),
        ]),
    )]);
    bbnf_ir::dag::ensure_dag(&mut ir);
    project_types(&mut ir);
    assert_eq!(*get_type(&ir, 0), TypeDesc::Span);
}

#[test]
fn seq_mixed_tuple() {
    // Rule 1 (leaf) first, then rule 0 references it.
    let mut ir = make_ir(vec![
        rule(
            1,
            alt(vec![
                IrNode::Literal(2),
                IrNode::Seq(vec![IrNode::Literal(2), IrNode::Literal(3)]),
            ]),
        ),
        rule(
            0,
            IrNode::Seq(vec![IrNode::Literal(2), IrNode::Ref(1), IrNode::Literal(3)]),
        ),
    ]);
    bbnf_ir::dag::ensure_dag(&mut ir);
    project_types(&mut ir);
    // Rule 1: Alt(Span, Span) -> Span.
    // Rule 0: Seq(Span, Ref(1)→Span, Span) -> three consecutive Spans,
    // which the Seq span-compression rule collapses to a single Span.
    // AU.2.5: `RefConstraint` now inherits the target rule's scalar
    // type (Span qualifies) so the Seq sees three Spans and compresses
    // correctly; the previous `BoxedEnum` ground was the source of the
    // `Tuple(Span, BoxedEnum, Span)` fan-out that blocked aggregate
    // composition at every Seq-of-Refs site.
    assert_eq!(*get_type(&ir, 0), TypeDesc::Span);
}

#[test]
fn pair_flattening() {
    // Seq(Literal, Repeat(Literal)) where Repeat(Span) -> Span (collapsed)
    // So Seq(Span, Span) -> Span (all-Span seq compression)
    let mut ir = make_ir(vec![rule(
        0,
        IrNode::Seq(vec![
            IrNode::Literal(2),
            IrNode::Repeat {
                inner: Box::new(IrNode::Literal(2)),
                lo: 0,
                hi: u32::MAX,
            },
        ]),
    )]);
    bbnf_ir::dag::ensure_dag(&mut ir);
    project_types(&mut ir);
    // Repeat(Span) -> Span, Seq(Span, Span) -> Span
    assert_eq!(*get_type(&ir, 0), TypeDesc::Span);
}

#[test]
fn skip_keeps_left() {
    let mut ir = make_ir(vec![rule(
        0,
        IrNode::Skip(Box::new(IrNode::Literal(2)), Box::new(IrNode::Literal(3))),
    )]);
    bbnf_ir::dag::ensure_dag(&mut ir);
    project_types(&mut ir);
    assert_eq!(*get_type(&ir, 0), TypeDesc::Span);
}

#[test]
fn next_keeps_right() {
    let mut ir = make_ir(vec![rule(
        0,
        IrNode::Next(Box::new(IrNode::Literal(2)), Box::new(IrNode::Literal(3))),
    )]);
    bbnf_ir::dag::ensure_dag(&mut ir);
    project_types(&mut ir);
    assert_eq!(*get_type(&ir, 0), TypeDesc::Span);
}

#[test]
fn alt_homogeneous_span() {
    // Alt(Literal, Optional(Literal)) where Optional(Span) -> Span
    // Alt(Span, Span) -> Span (homogeneous)
    let mut ir = make_ir(vec![rule(
        0,
        alt(vec![
            IrNode::Literal(2),
            IrNode::Repeat {
                inner: Box::new(IrNode::Literal(2)),
                lo: 0,
                hi: 1,
            },
        ]),
    )]);
    bbnf_ir::dag::ensure_dag(&mut ir);
    project_types(&mut ir);
    // Literal -> Span, Optional(Span) -> Span, Alt(Span, Span) -> Span
    assert_eq!(*get_type(&ir, 0), TypeDesc::Span);
}

#[test]
fn cyclic_ref_defaults_to_boxed_enum() {
    // Rule 0 references rule 1, rule 1 references rule 0 (cycle).
    // Rule 0 is processed first -> Ref(1) is unknown -> BoxedEnum.
    let mut ir = make_ir(vec![rule(0, IrNode::Ref(1)), rule(1, IrNode::Ref(0))]);
    bbnf_ir::dag::ensure_dag(&mut ir);
    project_types(&mut ir);
    assert_eq!(*get_type(&ir, 0), TypeDesc::BoxedEnum);
}

#[test]
fn map_enum_wrap() {
    let mut ir = GrammarIR {
        entry: 0,
        rules: vec![rule(
            0,
            IrNode::Map {
                inner: Box::new(IrNode::Literal(0)),
                fn_id: 0,
            },
        )],
        strings: vec!["Variant".into()],
        fns: vec![FnDescriptor::EnumWrap { variant: 0 }],
        types: vec![],
        follow_sets: HashMap::new(),
        ws_pattern: None,
        collapse_simple_spans: false,
        debug_all: false,
        debug_labels: Vec::new(),
        type_map: None,
        pattern_annotations: std::collections::HashMap::new(),
        regex_info: std::collections::HashMap::new(),
        node_facts: HashMap::new(),
        recognizer_decisions: HashMap::new(),
        delim_scan_configs: std::collections::HashMap::new(),
        key_dispatch_configs: std::collections::HashMap::new(),
        context_facts: std::collections::HashMap::new(),
        has_family_recognizers: false,
        regex_engine_decisions: std::collections::HashMap::new(),
        dag: None,
        cost_config: bbnf_ir::CostConfig::default(),
        type_desc_interner: bbnf_ir::TypeDescInterner::new(),
        materialization: std::collections::HashMap::new(),
        string_index: std::collections::HashMap::new(),
        payload_layouts: std::collections::HashMap::new(),
        structural_alphabet: None,
        push_fingerprint: None,
        dedup_eligible_rules: Vec::new(),

        shape_assignments: bbnf_ir::passes::recognizers::shape_dispatch::ShapeAssignments::default(
        ),
        eclass_facts: std::collections::HashMap::new(),
        shape_dict_templates: Vec::new(),
        shape_dict_selection: Vec::new(),
        keyword_branches: std::collections::HashMap::new(),
        disjoint_first_tables: std::collections::HashMap::new(),
        pattern_alphabets: std::collections::HashMap::new(),
        ctns_lifts: std::collections::HashSet::new(),
        struct_registry: bbnf_ir::StructRegistry::default(),
        type_obligations: Vec::new(),
    };
    bbnf_ir::dag::ensure_dag(&mut ir);
    project_types(&mut ir);
    assert_eq!(*get_type(&ir, 0), TypeDesc::Enum);
}

#[test]
fn map_box_wrap() {
    let mut ir = GrammarIR {
        entry: 0,
        rules: vec![rule(
            0,
            IrNode::Map {
                inner: Box::new(IrNode::Literal(0)),
                fn_id: 0,
            },
        )],
        strings: vec!["r0".into()],
        fns: vec![FnDescriptor::BoxWrap],
        types: vec![],
        follow_sets: HashMap::new(),
        ws_pattern: None,
        collapse_simple_spans: false,
        debug_all: false,
        debug_labels: Vec::new(),
        type_map: None,
        pattern_annotations: std::collections::HashMap::new(),
        regex_info: std::collections::HashMap::new(),
        node_facts: HashMap::new(),
        recognizer_decisions: HashMap::new(),
        delim_scan_configs: std::collections::HashMap::new(),
        key_dispatch_configs: std::collections::HashMap::new(),
        context_facts: std::collections::HashMap::new(),
        has_family_recognizers: false,
        regex_engine_decisions: std::collections::HashMap::new(),
        dag: None,
        cost_config: bbnf_ir::CostConfig::default(),
        type_desc_interner: bbnf_ir::TypeDescInterner::new(),
        materialization: std::collections::HashMap::new(),
        string_index: std::collections::HashMap::new(),
        payload_layouts: std::collections::HashMap::new(),
        structural_alphabet: None,
        push_fingerprint: None,
        dedup_eligible_rules: Vec::new(),

        shape_assignments: bbnf_ir::passes::recognizers::shape_dispatch::ShapeAssignments::default(
        ),
        eclass_facts: std::collections::HashMap::new(),
        shape_dict_templates: Vec::new(),
        shape_dict_selection: Vec::new(),
        keyword_branches: std::collections::HashMap::new(),
        disjoint_first_tables: std::collections::HashMap::new(),
        pattern_alphabets: std::collections::HashMap::new(),
        ctns_lifts: std::collections::HashSet::new(),
        struct_registry: bbnf_ir::StructRegistry::default(),
        type_obligations: Vec::new(),
    };
    bbnf_ir::dag::ensure_dag(&mut ir);
    project_types(&mut ir);
    assert_eq!(*get_type(&ir, 0), TypeDesc::BoxedEnum);
}

// ─── AU.2.5 scalar projection tests ────────────────────────────

/// Build a minimal GrammarIR where each rule carries an `Expr`
/// FnDescriptor return type (`u8`, `f64`, …). The rules use Map
/// wrappers so projection flows through the `FnDescriptor::Expr`
/// branch of `generate_node`.
fn make_ir_with_fns(rules: Vec<IrRule>, fns: Vec<FnDescriptor>, strings: Vec<String>) -> GrammarIR {
    GrammarIR {
        entry: 0,
        rules,
        strings,
        fns,
        types: vec![],
        follow_sets: HashMap::new(),
        ws_pattern: None,
        collapse_simple_spans: false,
        debug_all: false,
        debug_labels: Vec::new(),
        type_map: None,
        pattern_annotations: std::collections::HashMap::new(),
        regex_info: std::collections::HashMap::new(),
        node_facts: HashMap::new(),
        recognizer_decisions: HashMap::new(),
        delim_scan_configs: std::collections::HashMap::new(),
        key_dispatch_configs: std::collections::HashMap::new(),
        context_facts: std::collections::HashMap::new(),
        has_family_recognizers: false,
        regex_engine_decisions: std::collections::HashMap::new(),
        dag: None,
        cost_config: bbnf_ir::CostConfig::default(),
        type_desc_interner: bbnf_ir::TypeDescInterner::new(),
        materialization: std::collections::HashMap::new(),
        string_index: std::collections::HashMap::new(),
        payload_layouts: std::collections::HashMap::new(),
        structural_alphabet: None,
        push_fingerprint: None,
        dedup_eligible_rules: Vec::new(),

        shape_assignments: bbnf_ir::passes::recognizers::shape_dispatch::ShapeAssignments::default(
        ),
        eclass_facts: std::collections::HashMap::new(),
        shape_dict_templates: Vec::new(),
        shape_dict_selection: Vec::new(),
        keyword_branches: std::collections::HashMap::new(),
        disjoint_first_tables: std::collections::HashMap::new(),
        pattern_alphabets: std::collections::HashMap::new(),
        ctns_lifts: std::collections::HashSet::new(),
        struct_registry: bbnf_ir::StructRegistry::default(),
        type_obligations: Vec::new(),
    }
}

/// A `-> T` return-type Expr descriptor with no custom body; the
/// projection code only reads the return type.
fn expr_fn(return_type: TypeDesc) -> FnDescriptor {
    FnDescriptor::Expr {
        expr: bbnf_ir::MapExpr::Input,
        return_type: Some(return_type),
    }
}

#[test]
fn ref_projects_target_scalar_type() {
    // AU.2.5: a Ref to a rule declared `-> f64` must itself carry
    // `TypeDesc::F64`, not the historical `BoxedEnum` ground. Without
    // this, `Seq(number, unit)` projects as `Tuple([BoxedEnum, ...])`
    // and the aggregate-payload path never activates.
    //
    //  rule 0 = Ref(1)       expected F64
    //  rule 1 = Map(Lit, f0)  fn 0 = Expr { return F64 }
    let mut ir = make_ir_with_fns(
        vec![
            rule(0, IrNode::Ref(1)),
            rule(
                1,
                IrNode::Map {
                    inner: Box::new(IrNode::Literal(3)),
                    fn_id: 0,
                },
            ),
        ],
        vec![expr_fn(TypeDesc::F64)],
        vec!["r0".into(), "r1".into(), "r2".into(), "lit".into()],
    );
    bbnf_ir::dag::ensure_dag(&mut ir);
    project_types(&mut ir);
    assert_eq!(*get_type(&ir, 0), TypeDesc::F64);
    assert_eq!(*get_type(&ir, 1), TypeDesc::F64);
}

#[test]
fn seq_of_refs_composes_f64_u8_aggregate() {
    // AU.2.5: the dimension shape. `dimension = number , anyUnit`
    // where `number -> f64` and `anyUnit -> u8`. Before the fix this
    // projected to `Tuple([BoxedEnum, BoxedEnum])`; with Ref-scalar
    // projection the target rules' scalar types flow through.
    //
    //  rule 0 = Seq(Ref(1), Ref(2))
    //  rule 1 = Map(Lit, f0)  fn 0 = Expr { return F64 }
    //  rule 2 = Map(Lit, f1)  fn 1 = Expr { return U8  }
    let mut ir = make_ir_with_fns(
        vec![
            rule(0, IrNode::Seq(vec![IrNode::Ref(1), IrNode::Ref(2)])),
            rule(
                1,
                IrNode::Map {
                    inner: Box::new(IrNode::Literal(3)),
                    fn_id: 0,
                },
            ),
            rule(
                2,
                IrNode::Map {
                    inner: Box::new(IrNode::Literal(3)),
                    fn_id: 1,
                },
            ),
        ],
        vec![expr_fn(TypeDesc::F64), expr_fn(TypeDesc::U8)],
        vec!["r0".into(), "r1".into(), "r2".into(), "lit".into()],
    );
    bbnf_ir::dag::ensure_dag(&mut ir);
    project_types(&mut ir);
    assert_eq!(
        *get_type(&ir, 0),
        TypeDesc::Tuple(vec![TypeDesc::F64, TypeDesc::U8])
    );
}

// ─── AU.2.5 factor-pass type-loss tests ────────────────────────

// ─── AZ-III.W3a.3 — heterogeneous Alt obligation surfacing ─────

/// CSS-like `value = ident | length | percentage | color` shape.
///
/// Each branch has a distinct typed payload (`Span` for `ident`,
/// `F64` for `length`/`percentage`, `U32` for `color`). The
/// historical `BoxedEnum` fallback at `revise.rs:123` swallowed
/// these without diagnostic. AZ-III.W3a.3 lifts the join into a
/// `TypeDesc::HeterogeneousAltJoin` carrying the deduplicated
/// branch types AND populates `GrammarIR::type_obligations` with a
/// matching `TypeObligation::HeterogeneousAltJoin` entry.
#[test]
fn heterogeneous_css_alt_surfaces_named_obligation() {
    // rule 0 = Alt(Ref(1), Ref(2), Ref(3), Ref(4))   -> HeterogeneousAltJoin
    // rule 1 = Map(Lit, f0)  fn 0 = Expr { return Span }  (ident-shape)
    // rule 2 = Map(Lit, f1)  fn 1 = Expr { return F64  }  (length)
    // rule 3 = Map(Lit, f2)  fn 2 = Expr { return F64  }  (percentage)
    // rule 4 = Map(Lit, f3)  fn 3 = Expr { return U32  }  (color)
    let mut ir = make_ir_with_fns(
        vec![
            rule(
                0,
                alt(vec![
                    IrNode::Ref(1),
                    IrNode::Ref(2),
                    IrNode::Ref(3),
                    IrNode::Ref(4),
                ]),
            ),
            rule(
                1,
                IrNode::Map {
                    inner: Box::new(IrNode::Literal(3)),
                    fn_id: 0,
                },
            ),
            rule(
                2,
                IrNode::Map {
                    inner: Box::new(IrNode::Literal(3)),
                    fn_id: 1,
                },
            ),
            rule(
                3,
                IrNode::Map {
                    inner: Box::new(IrNode::Literal(3)),
                    fn_id: 2,
                },
            ),
            rule(
                4,
                IrNode::Map {
                    inner: Box::new(IrNode::Literal(3)),
                    fn_id: 3,
                },
            ),
        ],
        vec![
            expr_fn(TypeDesc::Span),
            expr_fn(TypeDesc::F64),
            expr_fn(TypeDesc::F64),
            expr_fn(TypeDesc::U32),
        ],
        vec![
            "value".into(),
            "ident".into(),
            "length".into(),
            "percentage".into(),
            "color".into(),
        ],
    );
    bbnf_ir::dag::ensure_dag(&mut ir);
    project_types(&mut ir);

    // The join surfaces the distinct branch types as a named
    // obligation in `ir.types`. The deduplicated set is
    // `[Span, F64, U32]` — `length` and `percentage` collapse to a
    // single `F64` entry preserving first-appearance order.
    let projected = get_type(&ir, 0);
    match projected {
        TypeDesc::HeterogeneousAltJoin(branches) => {
            assert_eq!(
                branches,
                &vec![TypeDesc::Span, TypeDesc::F64, TypeDesc::U32],
                "heterogeneous CSS alt must lift distinct branch types into the obligation",
            );
        }
        other => panic!(
            "expected `TypeDesc::HeterogeneousAltJoin`, got {:?} — silent BoxedEnum fallback resurrected?",
            other,
        ),
    }

    // The obligation also surfaces in the diagnostic stream so
    // downstream consumers (audit, codegen, debug adapters) see the
    // join site without re-walking `ir.types`.
    assert!(
        !ir.type_obligations.is_empty(),
        "heterogeneous Alt must emit at least one TypeObligation",
    );
    let surfaced = ir
        .type_obligations
        .iter()
        .find(|ob| {
            matches!(
                ob,
                TypeObligation::HeterogeneousAltJoin { rule: Some(0), .. }
            )
        })
        .expect("obligation stream must include rule 0's heterogeneous join");
    if let TypeObligation::HeterogeneousAltJoin { branches, .. } = surfaced {
        assert_eq!(
            branches,
            &vec![TypeDesc::Span, TypeDesc::F64, TypeDesc::U32],
            "obligation evidence must match the lifted TypeDesc payload",
        );
    } else {
        unreachable!("matched above");
    }
}

/// Sanity: the homogeneous Alt path still does NOT emit an
/// obligation. Only heterogeneous joins surface as obligations; an
/// `Alt(Ref(F64), Ref(F64))` collapses to `F64` cleanly and the
/// stream stays empty.
#[test]
fn homogeneous_alt_emits_no_obligation() {
    let mut ir = make_ir_with_fns(
        vec![
            rule(0, alt(vec![IrNode::Ref(1), IrNode::Ref(2)])),
            rule(
                1,
                IrNode::Map {
                    inner: Box::new(IrNode::Literal(3)),
                    fn_id: 0,
                },
            ),
            rule(
                2,
                IrNode::Map {
                    inner: Box::new(IrNode::Literal(3)),
                    fn_id: 0,
                },
            ),
        ],
        vec![expr_fn(TypeDesc::F64)],
        vec!["value".into(), "a".into(), "b".into(), "lit".into()],
    );
    bbnf_ir::dag::ensure_dag(&mut ir);
    project_types(&mut ir);
    assert_eq!(*get_type(&ir, 0), TypeDesc::F64);
    assert!(
        ir.type_obligations.is_empty(),
        "homogeneous join must not emit an obligation; got {:?}",
        ir.type_obligations,
    );
}

#[test]
fn factored_prefix_alt_projects_scalar() {
    // AU.2.5: `factor_common_prefixes` rewrites
    //     "px" -> u8 | "cm" -> u8 | "mm" -> u8 | "Q" -> u8
    // into an Alt whose first branch is
    //     Seq(Literal("p"), Alt(Map(Literal("x"), u8), ...))
    // whose shape is `Tuple([Span, U8])` — heterogeneous with the
    // remaining bare-`Map` branches whose shape is `U8`. The fixed
    // `join_types` strips the factored-prefix Span and reconciles
    // every branch's effective payload as `U8`.
    let mut ir = make_ir_with_fns(
        vec![rule(
            0,
            IrNode::Alt(
                vec![
                    AltBranch {
                        node: IrNode::Seq(vec![
                            IrNode::Literal(1),
                            IrNode::Alt(
                                vec![
                                    AltBranch {
                                        node: IrNode::Map {
                                            inner: Box::new(IrNode::Literal(2)),
                                            fn_id: 0,
                                        },
                                        first_set: None,
                                    },
                                    AltBranch {
                                        node: IrNode::Map {
                                            inner: Box::new(IrNode::Literal(3)),
                                            fn_id: 0,
                                        },
                                        first_set: None,
                                    },
                                ],
                                None,
                            ),
                        ]),
                        first_set: None,
                    },
                    AltBranch {
                        node: IrNode::Map {
                            inner: Box::new(IrNode::Literal(4)),
                            fn_id: 0,
                        },
                        first_set: None,
                    },
                ],
                None,
            ),
        )],
        vec![expr_fn(TypeDesc::U8)],
        vec!["r0".into(), "p".into(), "x".into(), "c".into(), "mm".into()],
    );
    bbnf_ir::dag::ensure_dag(&mut ir);
    project_types(&mut ir);
    assert_eq!(*get_type(&ir, 0), TypeDesc::U8);
}

// ─── AZ-III.W3a.2 obligation-surface tests ──────────────────────
//
// Per AZ-III invariant 7 (no silent fallback), every compound `Ref`
// collapse to `BoxedEnum` MUST surface a named
// `TypeObligation::UnresolvedCompoundRef` in `ir.type_obligations`.
// Cyclic rule grounds MUST surface the same obligation with `cyclic
// == true`. These assertions fail without the obligation substrate
// (the field would not exist) and pass with the W3a.2 wiring.
//
// The shapes are the canonical reproducers from the AZ-III audit
// archaeology:
//
//   * `compound_ref_obligation_surfaces` — heterogeneous EBNF-style
//     alternation: a `Ref` whose target rule resolves to `Vec<Span>`
//     (a compound shape) is wrapped in `BoxedEnum`, but the
//     obligation records the structural `Vec(Span)` so audit /
//     registry / debug consumers can see the underlying shape.
//
//   * `cyclic_ref_obligation_surfaces` — recursive grammar: rule 0
//     references rule 1 references rule 0. Both rules ground out via
//     the cycle-break path. The obligation records the cycle with
//     `cyclic == true` for each Ref into the cycle.

#[test]
fn compound_ref_obligation_surfaces() {
    // EBNF-style heterogeneous alternation. Rule 1 is a Many of an
    // Alt with two non-Span branches (Map { -> u8 }), which resolves
    // to `Vec(U8)` — a compound type. Rule 0 references rule 1.
    // Without the obligation substrate, the Ref silently falls back
    // to BoxedEnum. With the substrate, the projection still
    // produces BoxedEnum (the grammar-general layout for a tagged
    // union wrapping a compound) but emits a named obligation
    // recording the underlying `Vec(U8)` shape.
    let mut ir = make_ir_with_fns(
        vec![
            // rule 0 = Ref(1)
            rule(0, IrNode::Ref(1)),
            // rule 1 = Many(Map(Lit, U8)) → Vec(U8)
            rule(
                1,
                IrNode::Repeat {
                    inner: Box::new(IrNode::Map {
                        inner: Box::new(IrNode::Literal(3)),
                        fn_id: 0,
                    }),
                    lo: 0,
                    hi: u32::MAX,
                },
            ),
        ],
        vec![expr_fn(TypeDesc::U8)],
        vec!["r0".into(), "r1".into(), "_".into(), "lit".into()],
    );
    bbnf_ir::dag::ensure_dag(&mut ir);
    project_types(&mut ir);

    // Rule 1 must remain Vec(U8); rule 0's Ref position must
    // collapse to BoxedEnum because the target is compound.
    assert_eq!(
        *get_type(&ir, 1),
        TypeDesc::Vec(Box::new(TypeDesc::U8)),
        "rule 1 must resolve to Vec(U8)",
    );
    assert_eq!(
        *get_type(&ir, 0),
        TypeDesc::BoxedEnum,
        "rule 0's Ref must collapse to BoxedEnum (tagged-union wrapper)",
    );

    // Per W3a.2: the collapse must surface a NAMED obligation.
    let compound_obligation = ir.type_obligations.iter().find(|o| match o {
        TypeObligation::UnresolvedCompoundRef {
            target_rule,
            structural_type,
            cyclic,
            ..
        } => {
            *target_rule == 1
                && !*cyclic
                && *structural_type == TypeDesc::Vec(Box::new(TypeDesc::U8))
        }
        TypeObligation::HeterogeneousAltJoin { .. } => false,
    });
    assert!(
        compound_obligation.is_some(),
        "expected a non-cyclic UnresolvedCompoundRef obligation for rule 1 → Vec(U8); \
         got obligations: {:?}",
        ir.type_obligations
    );
}

#[test]
fn cyclic_ref_obligation_surfaces() {
    // Rule 0 references rule 1, rule 1 references rule 0. The
    // cycle never resolves in the propagator; the cycle-break
    // grounds both rule vars to BoxedEnum. Per W3a.2 each Ref into
    // a cyclic rule must surface a `cyclic: true` obligation.
    let mut ir = make_ir(vec![rule(0, IrNode::Ref(1)), rule(1, IrNode::Ref(0))]);
    bbnf_ir::dag::ensure_dag(&mut ir);
    project_types(&mut ir);

    assert_eq!(*get_type(&ir, 0), TypeDesc::BoxedEnum);
    assert_eq!(*get_type(&ir, 1), TypeDesc::BoxedEnum);

    // At least one obligation per cyclic rule, all marked `cyclic`.
    let cyclic_for_rule_0 = ir.type_obligations.iter().any(|o| match o {
        TypeObligation::UnresolvedCompoundRef {
            target_rule,
            cyclic,
            ..
        } => *target_rule == 0 && *cyclic,
        TypeObligation::HeterogeneousAltJoin { .. } => false,
    });
    let cyclic_for_rule_1 = ir.type_obligations.iter().any(|o| match o {
        TypeObligation::UnresolvedCompoundRef {
            target_rule,
            cyclic,
            ..
        } => *target_rule == 1 && *cyclic,
        TypeObligation::HeterogeneousAltJoin { .. } => false,
    });

    assert!(
        cyclic_for_rule_0,
        "expected a cyclic obligation for rule 0; got: {:?}",
        ir.type_obligations
    );
    assert!(
        cyclic_for_rule_1,
        "expected a cyclic obligation for rule 1; got: {:?}",
        ir.type_obligations
    );

    // Diagnostic rendering must clearly say "cyclic compound Ref".
    let any_diag = ir
        .type_obligations
        .iter()
        .map(|o| o.diagnostic())
        .find(|d| d.contains("cyclic compound Ref"));
    assert!(
        any_diag.is_some(),
        "expected at least one obligation diagnostic to include 'cyclic compound Ref'; \
         got: {:?}",
        ir.type_obligations
            .iter()
            .map(|o| o.diagnostic())
            .collect::<Vec<_>>(),
    );
}

#[test]
fn scalar_ref_emits_no_obligation() {
    // Sanity check: a Ref into a scalar rule (here, a `-> f64` Map)
    // must NOT emit an obligation. This guards against the
    // obligation surface accidentally firing on the well-formed
    // scalar-projection path.
    let mut ir = make_ir_with_fns(
        vec![
            rule(0, IrNode::Ref(1)),
            rule(
                1,
                IrNode::Map {
                    inner: Box::new(IrNode::Literal(3)),
                    fn_id: 0,
                },
            ),
        ],
        vec![expr_fn(TypeDesc::F64)],
        vec!["r0".into(), "r1".into(), "_".into(), "lit".into()],
    );
    bbnf_ir::dag::ensure_dag(&mut ir);
    project_types(&mut ir);

    assert_eq!(*get_type(&ir, 0), TypeDesc::F64);
    assert_eq!(*get_type(&ir, 1), TypeDesc::F64);
    assert!(
        ir.type_obligations.is_empty(),
        "scalar Ref must not emit an obligation; got: {:?}",
        ir.type_obligations
    );
}
