use std::collections::HashMap;

use bbnf_ir::passes::refine_span_eligibility;
use bbnf_ir::{FnDescriptor, GrammarIR, IrNode, IrRule, RuleId, RuleMeta};

fn make_rule(id: RuleId, body: IrNode) -> IrRule {
    IrRule {
        id,
        name: id,
        body,
        meta: RuleMeta::default(),
        source_span: None,
    }
}

#[test]
fn literal_is_span_eligible() {
    let mut ir = GrammarIR {
        rules: vec![make_rule(0, IrNode::Literal(1))],
        entry: 0,
        strings: vec!["rule".into(), "x".into()],
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
    };

    refine_span_eligibility(&mut ir);
    assert!(ir.rules[0].meta.span_eligible);
}

#[test]
fn map_not_span_eligible() {
    let mut ir = GrammarIR {
        rules: vec![make_rule(
            0,
            IrNode::Map {
                inner: Box::new(IrNode::Literal(1)),
                fn_id: 0,
            },
        )],
        entry: 0,
        strings: vec!["rule".into(), "x".into()],
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
    };

    refine_span_eligibility(&mut ir);
    assert!(!ir.rules[0].meta.span_eligible);
}

#[test]
fn transitive_span_eligibility() {
    // rule a = b ; rule b = "x" ;
    // b is span-eligible, so a should also be span-eligible.
    let mut ir = GrammarIR {
        rules: vec![
            make_rule(0, IrNode::Ref(1)),
            make_rule(1, IrNode::Literal(2)),
        ],
        entry: 0,
        strings: vec!["a".into(), "b".into(), "x".into()],
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
    };

    refine_span_eligibility(&mut ir);
    assert!(ir.rules[0].meta.span_eligible);
    assert!(ir.rules[1].meta.span_eligible);
}

#[test]
fn cyclic_not_span_eligible() {
    let mut ir = GrammarIR {
        rules: vec![IrRule {
            id: 0,
            name: 0,
            body: IrNode::Ref(0),
            meta: RuleMeta {
                is_cyclic: true,
                ..Default::default()
            },
            source_span: None,
        }],
        entry: 0,
        strings: vec!["expr".into()],
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
    };

    refine_span_eligibility(&mut ir);
    assert!(!ir.rules[0].meta.span_eligible);
}

#[test]
fn seq_of_literals_span_eligible() {
    let mut ir = GrammarIR {
        rules: vec![make_rule(
            0,
            IrNode::Seq(vec![IrNode::Literal(1), IrNode::Literal(2)]),
        )],
        entry: 0,
        strings: vec!["rule".into(), "a".into(), "b".into()],
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
    };

    refine_span_eligibility(&mut ir);
    assert!(ir.rules[0].meta.span_eligible);
}
