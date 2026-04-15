use std::collections::HashMap;

use bbnf_ir::passes::types::project_types;
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
        dag: None, cost_config: bbnf_ir::CostConfig::default(), type_desc_interner: bbnf_ir::TypeDescInterner::new(),
        materialization: std::collections::HashMap::new(),
        string_index: std::collections::HashMap::new(),
        payload_layouts: std::collections::HashMap::new(),
        struct_registry: Default::default(),
        structural_alphabet: None,
        push_fingerprint: None,
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
    // Rule 1: Alt(Span, Span) -> Span
    // Rule 0: Seq(Span, Ref(1)→BoxedEnum, Span) -> Tuple(Span, BoxedEnum, Span)
    // Ref(1) always returns BoxedEnum (matching emit_ref's Box::new wrapping).
    assert_eq!(
        *get_type(&ir, 0),
        TypeDesc::Tuple(vec![TypeDesc::Span, TypeDesc::BoxedEnum, TypeDesc::Span])
    );
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
        dag: None, cost_config: bbnf_ir::CostConfig::default(), type_desc_interner: bbnf_ir::TypeDescInterner::new(),
        materialization: std::collections::HashMap::new(),
        string_index: std::collections::HashMap::new(),
        payload_layouts: std::collections::HashMap::new(),
        struct_registry: Default::default(),
        structural_alphabet: None,
        push_fingerprint: None,
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
        dag: None, cost_config: bbnf_ir::CostConfig::default(), type_desc_interner: bbnf_ir::TypeDescInterner::new(),
        materialization: std::collections::HashMap::new(),
        string_index: std::collections::HashMap::new(),
        payload_layouts: std::collections::HashMap::new(),
        struct_registry: Default::default(),
        structural_alphabet: None,
        push_fingerprint: None,
    };
    bbnf_ir::dag::ensure_dag(&mut ir);
    project_types(&mut ir);
    assert_eq!(*get_type(&ir, 0), TypeDesc::BoxedEnum);
}
