//! IR-structural sub-variant projection tests.
//!
//! AX.W0a.2.i.b — prove that `project_types`' sub-variant emission
//! keys on IR-structural identity (heterogeneous branch TypeDescs)
//! rather than on walker-compound / tape-shape identities. The
//! synthesised-Alt fixtures below bypass the bootstrap parser +
//! host.rs tape walk entirely: they build a `GrammarIR` directly,
//! ensure the DAG, run `project_types`, and assert that
//! `rule.meta.sub_variants` carries the expected entries.
//!
//! If Agent A's shape-authoritative host.rs + wrap emitter produces
//! an equivalent IR body at end-to-end regen time, these assertions
//! hold verbatim. If the projection instead regressed to dropping
//! sub-variants at the IR-type layer, these tests fail fast with a
//! concrete reproduction — no bootstrap cycle required.

use std::collections::HashMap;

use bbnf_ir::passes::types::project_types;
use bbnf_ir::{AltBranch, FnDescriptor, GrammarIR, IrNode, IrRule, MapExpr, RuleId, RuleMeta, TypeDesc};

fn make_ir(rules: Vec<IrRule>, fns: Vec<FnDescriptor>, strings: Vec<String>) -> GrammarIR {
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
        pattern_annotations: HashMap::new(),
        regex_info: HashMap::new(),
        node_facts: HashMap::new(),
        recognizer_decisions: HashMap::new(),
        delim_scan_configs: HashMap::new(),
        key_dispatch_configs: HashMap::new(),
        context_facts: HashMap::new(),
        has_family_recognizers: false,
        regex_engine_decisions: HashMap::new(),
        dag: None,
        cost_config: bbnf_ir::CostConfig::default(),
        type_desc_interner: bbnf_ir::TypeDescInterner::new(),
        materialization: HashMap::new(),
        string_index: HashMap::new(),
        payload_layouts: HashMap::new(),
        structural_alphabet: None,
        push_fingerprint: None,
        dedup_eligible_rules: Vec::new(),
        shape_assignments: bbnf_ir::passes::recognizers::shape_dispatch::ShapeAssignments::default(),
        eclass_facts: HashMap::new(),
        shape_dict_templates: Vec::new(),
        shape_dict_selection: Vec::new(),
        keyword_branches: HashMap::new(),
        disjoint_first_tables: HashMap::new(),
        pattern_alphabets: HashMap::new(),
        ctns_lifts: std::collections::HashSet::new(),
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

fn expr_fn(return_type: TypeDesc) -> FnDescriptor {
    FnDescriptor::Expr {
        expr: MapExpr::Input,
        return_type: Some(return_type),
    }
}

fn alt_branches(nodes: Vec<IrNode>) -> IrNode {
    IrNode::Alt(
        nodes
            .into_iter()
            .map(|node| AltBranch {
                node,
                first_set: None,
            })
            .collect(),
        None,
    )
}

#[test]
fn heterogeneous_alt_f64_u8_emits_two_sub_variants() {
    // Two `Map` branches with distinct return types (F64, U8) form a
    // heterogeneous Alt. The projection CSP assigns each branch its
    // own TypeDesc via the Map fn's return type; the Alt's `join_
    // types` lifts them to `BoxedEnum` at the rule level and the
    // sub-variant collector emits one variant per distinct non-Enum
    // branch type.
    //
    // rule 0 = Alt(Map(Lit, f0), Map(Lit, f1))
    // fn 0   = Expr -> F64
    // fn 1   = Expr -> U8
    let mut ir = make_ir(
        vec![rule(
            0,
            alt_branches(vec![
                IrNode::Map {
                    inner: Box::new(IrNode::Literal(1)),
                    fn_id: 0,
                },
                IrNode::Map {
                    inner: Box::new(IrNode::Literal(1)),
                    fn_id: 1,
                },
            ]),
        )],
        vec![expr_fn(TypeDesc::F64), expr_fn(TypeDesc::U8)],
        vec!["r0".into(), "lit".into()],
    );
    bbnf_ir::dag::ensure_dag(&mut ir);
    project_types(&mut ir);

    let rule0 = &ir.rules[0];
    assert_eq!(
        rule0.meta.sub_variants.len(),
        2,
        "heterogeneous Alt(F64, U8) must emit two sub-variants; got {:?}",
        rule0.meta.sub_variants,
    );

    let tys: Vec<&TypeDesc> = rule0.meta.sub_variants.iter().map(|sv| &sv.ty).collect();
    assert!(
        tys.iter().any(|t| **t == TypeDesc::F64),
        "missing F64 sub-variant; got {:?}",
        tys,
    );
    assert!(
        tys.iter().any(|t| **t == TypeDesc::U8),
        "missing U8 sub-variant; got {:?}",
        tys,
    );
}

#[test]
fn heterogeneous_alt_span_u32_emits_two_sub_variants() {
    // Literal branch (Span) vs Map -> U32 branch. The Literal's type
    // stays Span through the projection; the Map's return type U32
    // is heterogeneous with Span, so both sides get sub-variants.
    //
    // rule 0 = Alt(Literal, Map(Lit, f0))
    // fn 0   = Expr -> U32
    let mut ir = make_ir(
        vec![rule(
            0,
            alt_branches(vec![
                IrNode::Literal(1),
                IrNode::Map {
                    inner: Box::new(IrNode::Literal(1)),
                    fn_id: 0,
                },
            ]),
        )],
        vec![expr_fn(TypeDesc::U32)],
        vec!["r0".into(), "lit".into()],
    );
    bbnf_ir::dag::ensure_dag(&mut ir);
    project_types(&mut ir);

    let rule0 = &ir.rules[0];
    assert!(
        rule0.meta.sub_variants.len() >= 2,
        "heterogeneous Alt(Span, U32) must emit ≥ 2 sub-variants; got {:?}",
        rule0.meta.sub_variants,
    );

    let tys: Vec<&TypeDesc> = rule0.meta.sub_variants.iter().map(|sv| &sv.ty).collect();
    assert!(
        tys.iter().any(|t| **t == TypeDesc::Span),
        "missing Span sub-variant; got {:?}",
        tys,
    );
    assert!(
        tys.iter().any(|t| **t == TypeDesc::U32),
        "missing U32 sub-variant; got {:?}",
        tys,
    );
}

#[test]
fn homogeneous_alt_span_emits_no_sub_variants() {
    // Both branches are plain `Literal` → Span. Homogeneous
    // heterogeneity check in subvariants.rs:65 returns false; no
    // sub-variants emitted. Negative-control for the positive
    // heterogeneous cases above — asserts the projection does not
    // spuriously emit sub-variants for uniform branches.
    let mut ir = make_ir(
        vec![rule(
            0,
            alt_branches(vec![IrNode::Literal(1), IrNode::Literal(1)]),
        )],
        vec![],
        vec!["r0".into(), "lit".into()],
    );
    bbnf_ir::dag::ensure_dag(&mut ir);
    project_types(&mut ir);

    let rule0 = &ir.rules[0];
    assert!(
        rule0.meta.sub_variants.is_empty(),
        "homogeneous Alt(Span, Span) must emit zero sub-variants; got {:?}",
        rule0.meta.sub_variants,
    );
}

#[test]
fn nested_heterogeneous_alt_inside_seq_emits_sub_variants() {
    // A Seq whose child is a heterogeneous Alt. The
    // `collect_sub_variants_walk` recursion into Seq's children must
    // see the nested Alt and emit its sub-variants — the walk is
    // not limited to top-level Alts.
    //
    // rule 0 = Seq(Literal, Alt(Map(Lit,f0), Map(Lit,f1)))
    // fn 0   = Expr -> F64
    // fn 1   = Expr -> U8
    let mut ir = make_ir(
        vec![rule(
            0,
            IrNode::Seq(vec![
                IrNode::Literal(1),
                alt_branches(vec![
                    IrNode::Map {
                        inner: Box::new(IrNode::Literal(1)),
                        fn_id: 0,
                    },
                    IrNode::Map {
                        inner: Box::new(IrNode::Literal(1)),
                        fn_id: 1,
                    },
                ]),
            ]),
        )],
        vec![expr_fn(TypeDesc::F64), expr_fn(TypeDesc::U8)],
        vec!["r0".into(), "lit".into()],
    );
    bbnf_ir::dag::ensure_dag(&mut ir);
    project_types(&mut ir);

    let rule0 = &ir.rules[0];
    assert_eq!(
        rule0.meta.sub_variants.len(),
        2,
        "nested heterogeneous Alt must emit sub-variants via recursive walk; got {:?}",
        rule0.meta.sub_variants,
    );
}
