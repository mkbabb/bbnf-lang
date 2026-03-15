use std::collections::HashMap;

use bbnf_ir::passes::types::infer_types;
use bbnf_ir::{AltBranch, FnDescriptor, GrammarIR, IrNode, IrRule, RuleId, RuleMeta, TypeDesc};

fn make_ir(rules: Vec<IrRule>) -> GrammarIR {
    GrammarIR {
        entry: 0,
        rules,
        strings: vec!["r0".into(), "r1".into(), "r2".into(), "lit".into()],
        fns: vec![],
        types: vec![],
        follow_sets: HashMap::new(),
    }
}

fn rule(id: RuleId, body: IrNode) -> IrRule {
    IrRule {
        id,
        name: id,
        body,
        meta: RuleMeta::default(),
    }
}

fn rule_no_collapse(id: RuleId, body: IrNode) -> IrRule {
    IrRule {
        id,
        name: id,
        body,
        meta: RuleMeta {
            no_collapse: true,
            ..Default::default()
        },
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
    infer_types(&mut ir);
    assert_eq!(*get_type(&ir, 0), TypeDesc::Span);
}

#[test]
fn regex_is_span() {
    let mut ir = make_ir(vec![rule(0, IrNode::Regex(3))]);
    infer_types(&mut ir);
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
    infer_types(&mut ir);
    assert_eq!(*get_type(&ir, 0), TypeDesc::Span);
}

#[test]
fn optional_span_no_collapse() {
    let mut ir = make_ir(vec![rule_no_collapse(
        0,
        IrNode::Repeat {
            inner: Box::new(IrNode::Literal(3)),
            lo: 0,
            hi: 1,
        },
    )]);
    infer_types(&mut ir);
    assert_eq!(
        *get_type(&ir, 0),
        TypeDesc::Option(Box::new(TypeDesc::Span))
    );
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
    infer_types(&mut ir);
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
    // Rule 1: Alt(Span, Span) -> Span. But Ref(1) always returns BoxedEnum
    // (matching emit_ref's Box::new wrapping), so many(BoxedEnum) -> Vec(BoxedEnum).
    infer_types(&mut ir);
    assert_eq!(*get_type(&ir, 0), TypeDesc::Vec(Box::new(TypeDesc::BoxedEnum)));
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
    infer_types(&mut ir);
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
            IrNode::Seq(vec![
                IrNode::Literal(2),
                IrNode::Ref(1),
                IrNode::Literal(3),
            ]),
        ),
    ]);
    infer_types(&mut ir);
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
    // Seq(Ref(1), Repeat(Ref(1))) where Ref(1) = Span -> Span (collapsed)
    // But with no_collapse, Seq(Span, Vec<Span>) -> Vec<Span>
    let mut ir = make_ir(vec![rule_no_collapse(
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
    infer_types(&mut ir);
    // no_collapse: Repeat(Span) -> Vec<Span>, Seq(Span, Vec<Span>) -> Vec<Span> (flattened)
    assert_eq!(*get_type(&ir, 0), TypeDesc::Vec(Box::new(TypeDesc::Span)));
}

#[test]
fn skip_keeps_left() {
    let mut ir = make_ir(vec![rule(
        0,
        IrNode::Skip(
            Box::new(IrNode::Literal(2)),
            Box::new(IrNode::Literal(3)),
        ),
    )]);
    infer_types(&mut ir);
    assert_eq!(*get_type(&ir, 0), TypeDesc::Span);
}

#[test]
fn next_keeps_right() {
    let mut ir = make_ir(vec![rule(
        0,
        IrNode::Next(
            Box::new(IrNode::Literal(2)),
            Box::new(IrNode::Literal(3)),
        ),
    )]);
    infer_types(&mut ir);
    assert_eq!(*get_type(&ir, 0), TypeDesc::Span);
}

#[test]
fn alt_heterogeneous_is_boxed_enum() {
    // Alt(Span, Tuple(Span, Span)) -> BoxedEnum
    let mut ir = make_ir(vec![rule_no_collapse(
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
    infer_types(&mut ir);
    // no_collapse: Literal -> Span, Optional(Literal) -> Option<Span>
    // Alt(Span, Option<Span>) -> BoxedEnum (heterogeneous)
    assert_eq!(*get_type(&ir, 0), TypeDesc::BoxedEnum);
}

#[test]
fn cyclic_ref_defaults_to_boxed_enum() {
    // Rule 0 references rule 1, rule 1 references rule 0 (cycle).
    // Rule 0 is processed first -> Ref(1) is unknown -> BoxedEnum.
    let mut ir = make_ir(vec![rule(0, IrNode::Ref(1)), rule(1, IrNode::Ref(0))]);
    infer_types(&mut ir);
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
    };
    infer_types(&mut ir);
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
    };
    infer_types(&mut ir);
    assert_eq!(*get_type(&ir, 0), TypeDesc::BoxedEnum);
}
