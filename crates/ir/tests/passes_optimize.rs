use std::collections::HashMap;

use bbnf_ir::passes::{eliminate_epsilon, merge_literals};
use bbnf_ir::{AltBranch, GrammarIR, IrNode, IrRule, RuleMeta};

fn make_ir(body: IrNode, strings: Vec<String>) -> GrammarIR {
    GrammarIR {
        entry: 0,
        rules: vec![IrRule {
            id: 0,
            name: 0,
            body,
            meta: RuleMeta::default(),
            source_span: None,
        }],
        strings,
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
        dag: None, cost_config: bbnf_ir::CostConfig::default(),
    }
}

#[test]
fn epsilon_removed_from_seq() {
    let mut ir = make_ir(
        IrNode::Seq(vec![
            IrNode::Literal(1),
            IrNode::Epsilon,
            IrNode::Literal(2),
        ]),
        vec!["start".into(), "a".into(), "b".into()],
    );
    eliminate_epsilon(&mut ir);
    match &ir.rules[0].body {
        IrNode::Seq(children) => {
            assert_eq!(children.len(), 2);
            assert!(matches!(children[0], IrNode::Literal(1)));
            assert!(matches!(children[1], IrNode::Literal(2)));
        }
        other => panic!("expected Seq, got {:?}", other),
    }
}

#[test]
fn singleton_seq_unwrapped() {
    let mut ir = make_ir(
        IrNode::Seq(vec![IrNode::Epsilon, IrNode::Literal(1)]),
        vec!["start".into(), "a".into()],
    );
    eliminate_epsilon(&mut ir);
    assert!(matches!(ir.rules[0].body, IrNode::Literal(1)));
}

#[test]
fn all_epsilon_seq_becomes_epsilon() {
    let mut ir = make_ir(
        IrNode::Seq(vec![IrNode::Epsilon, IrNode::Epsilon]),
        vec!["start".into()],
    );
    eliminate_epsilon(&mut ir);
    assert!(matches!(ir.rules[0].body, IrNode::Epsilon));
}

#[test]
fn singleton_alt_unwrapped() {
    let mut ir = make_ir(
        IrNode::Alt(
            vec![AltBranch {
                node: IrNode::Literal(1),
                first_set: None,
            }],
            None,
        ),
        vec!["start".into(), "a".into()],
    );
    eliminate_epsilon(&mut ir);
    assert!(matches!(ir.rules[0].body, IrNode::Literal(1)));
}

#[test]
fn adjacent_literals_merged() {
    let mut ir = make_ir(
        IrNode::Seq(vec![
            IrNode::Literal(1),
            IrNode::Literal(2),
            IrNode::Literal(3),
        ]),
        vec!["start".into(), "a".into(), "b".into(), "c".into()],
    );
    merge_literals(&mut ir);
    // Should become a single literal "abc".
    match &ir.rules[0].body {
        IrNode::Literal(sid) => {
            assert_eq!(ir.strings[*sid as usize], "abc");
        }
        other => panic!("expected merged Literal, got {:?}", other),
    }
}

#[test]
fn non_adjacent_literals_not_merged() {
    let mut ir = make_ir(
        IrNode::Seq(vec![
            IrNode::Literal(1),
            IrNode::Regex(2),
            IrNode::Literal(3),
        ]),
        vec!["start".into(), "a".into(), "\\d+".into(), "b".into()],
    );
    merge_literals(&mut ir);
    match &ir.rules[0].body {
        IrNode::Seq(children) => assert_eq!(children.len(), 3),
        other => panic!("expected Seq of 3, got {:?}", other),
    }
}

#[test]
fn partial_merge() {
    // Seq([Lit("a"), Lit("b"), Regex, Lit("c"), Lit("d")])
    // -> Seq([Lit("ab"), Regex, Lit("cd")])
    let mut ir = make_ir(
        IrNode::Seq(vec![
            IrNode::Literal(1),
            IrNode::Literal(2),
            IrNode::Regex(3),
            IrNode::Literal(4),
            IrNode::Literal(5),
        ]),
        vec![
            "start".into(),
            "a".into(),
            "b".into(),
            "\\d+".into(),
            "c".into(),
            "d".into(),
        ],
    );
    merge_literals(&mut ir);
    match &ir.rules[0].body {
        IrNode::Seq(children) => {
            assert_eq!(children.len(), 3);
            if let IrNode::Literal(sid) = &children[0] {
                assert_eq!(ir.strings[*sid as usize], "ab");
            }
            assert!(matches!(children[1], IrNode::Regex(3)));
            if let IrNode::Literal(sid) = &children[2] {
                assert_eq!(ir.strings[*sid as usize], "cd");
            }
        }
        other => panic!("expected Seq of 3, got {:?}", other),
    }
}
