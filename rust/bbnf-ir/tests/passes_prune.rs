use std::collections::HashMap;

use bbnf_ir::passes::prune_unreachable;
use bbnf_ir::{AltBranch, GrammarIR, IrNode, IrRule, RuleId, RuleMeta};

fn make_rule(id: RuleId, name_id: u32, body: IrNode) -> IrRule {
    IrRule {
        id,
        name: name_id,
        body,
        meta: RuleMeta::default(),
        source_span: None,
    }
}

#[test]
fn prune_removes_unreachable() {
    let mut ir = GrammarIR {
        rules: vec![
            make_rule(0, 0, IrNode::Ref(1)),     // start -> a
            make_rule(1, 1, IrNode::Literal(2)), // a -> "x"
            make_rule(2, 3, IrNode::Literal(4)), // dead -> "y"
        ],
        entry: 0,
        strings: vec![
            "start".into(),
            "a".into(),
            "x".into(),
            "dead".into(),
            "y".into(),
        ],
        fns: vec![],
        types: vec![],
        follow_sets: HashMap::new(),
        ws_pattern: None,
        b1_span_collapse: false,
        debug_all: false,
        debug_labels: Vec::new(),
        infer_map: None,
    };

    prune_unreachable(&mut ir);

    assert_eq!(ir.rules.len(), 2);
    // IDs should be compacted to 0, 1.
    assert_eq!(ir.rules[0].id, 0);
    assert_eq!(ir.rules[1].id, 1);
    // The Ref in rule 0 should point to new id of rule "a".
    assert!(matches!(ir.rules[0].body, IrNode::Ref(1)));
}

#[test]
fn prune_keeps_all_when_all_reachable() {
    let mut ir = GrammarIR {
        rules: vec![
            make_rule(0, 0, IrNode::Ref(1)),
            make_rule(1, 1, IrNode::Literal(2)),
        ],
        entry: 0,
        strings: vec!["start".into(), "a".into(), "x".into()],
        fns: vec![],
        types: vec![],
        follow_sets: HashMap::new(),
        ws_pattern: None,
        b1_span_collapse: false,
        debug_all: false,
        debug_labels: Vec::new(),
        infer_map: None,
    };

    prune_unreachable(&mut ir);
    assert_eq!(ir.rules.len(), 2);
}

#[test]
fn prune_follows_alt_branches() {
    let mut ir = GrammarIR {
        rules: vec![
            make_rule(
                0,
                0,
                IrNode::Alt(
                    vec![
                        AltBranch {
                            node: IrNode::Ref(1),
                            first_set: None,
                        },
                        AltBranch {
                            node: IrNode::Ref(2),
                            first_set: None,
                        },
                    ],
                    None,
                ),
            ),
            make_rule(1, 1, IrNode::Literal(3)),
            make_rule(2, 2, IrNode::Literal(4)),
            make_rule(3, 5, IrNode::Literal(6)), // dead
        ],
        entry: 0,
        strings: vec![
            "start".into(),
            "a".into(),
            "b".into(),
            "x".into(),
            "y".into(),
            "dead".into(),
            "z".into(),
        ],
        fns: vec![],
        types: vec![],
        follow_sets: HashMap::new(),
        ws_pattern: None,
        b1_span_collapse: false,
        debug_all: false,
        debug_labels: Vec::new(),
        infer_map: None,
    };

    prune_unreachable(&mut ir);
    assert_eq!(ir.rules.len(), 3); // start, a, b -- dead removed
}
