use std::collections::HashMap;

use bbnf_ir::passes::compute_follow_sets;
use bbnf_ir::{CharSet128, GrammarIR, IrNode, IrRule, RuleMeta};

#[test]
fn follow_set_basic_seq() {
    // Grammar: start = a , "x" ;  a = "y" ;
    // FOLLOW(a) should contain {x}
    let mut first_a = CharSet128::new();
    first_a.add(b'y');

    let ir = GrammarIR {
        rules: vec![
            IrRule {
                id: 0,
                name: 0,
                body: IrNode::Seq(vec![IrNode::Ref(1), IrNode::Literal(2)]),
                meta: RuleMeta {
                    first_set: {
                        let mut cs = CharSet128::new();
                        cs.add(b'y');
                        cs
                    },
                    ..Default::default()
                },
                source_span: None,
            },
            IrRule {
                id: 1,
                name: 1,
                body: IrNode::Literal(3),
                meta: RuleMeta {
                    first_set: first_a,
                    ..Default::default()
                },
                source_span: None,
            },
        ],
        entry: 0,
        strings: vec!["start".into(), "a".into(), "x".into(), "y".into()],
        fns: vec![],
        types: vec![],
        follow_sets: HashMap::new(),
        ws_pattern: None,
        collapse_simple_spans: false,
        debug_all: false,
        debug_labels: Vec::new(),
        type_map: None,
        pattern_annotations: std::collections::HashMap::new(),
    };

    let follow = compute_follow_sets(&ir);
    assert!(follow[&1].has(b'x'));
}

#[test]
fn follow_set_propagates_through_nullable() {
    // Grammar: start = a , b? , "z" ;  a = "x" ;  b = "y" ;
    // b is nullable (Optional -> Repeat{0,1}), so FOLLOW(a) should contain {y, z}
    let ir = GrammarIR {
        rules: vec![
            IrRule {
                id: 0,
                name: 0,
                body: IrNode::Seq(vec![
                    IrNode::Ref(1),
                    IrNode::Repeat {
                        inner: Box::new(IrNode::Ref(2)),
                        lo: 0,
                        hi: 1,
                    },
                    IrNode::Literal(3),
                ]),
                meta: RuleMeta::default(),
                source_span: None,
            },
            IrRule {
                id: 1,
                name: 1,
                body: IrNode::Literal(4),
                meta: RuleMeta {
                    first_set: {
                        let mut cs = CharSet128::new();
                        cs.add(b'x');
                        cs
                    },
                    ..Default::default()
                },
                source_span: None,
            },
            IrRule {
                id: 2,
                name: 2,
                body: IrNode::Literal(5),
                meta: RuleMeta {
                    first_set: {
                        let mut cs = CharSet128::new();
                        cs.add(b'y');
                        cs
                    },
                    ..Default::default()
                },
                source_span: None,
            },
        ],
        entry: 0,
        strings: vec![
            "start".into(),
            "a".into(),
            "b".into(),
            "z".into(),
            "x".into(),
            "y".into(),
        ],
        fns: vec![],
        types: vec![],
        follow_sets: HashMap::new(),
        ws_pattern: None,
        collapse_simple_spans: false,
        debug_all: false,
        debug_labels: Vec::new(),
        type_map: None,
        pattern_annotations: std::collections::HashMap::new(),
    };

    let follow = compute_follow_sets(&ir);
    // FOLLOW(a) should contain 'y' (from FIRST(b?)) and 'z' (from literal after nullable b?).
    assert!(follow[&1].has(b'y'));
    assert!(follow[&1].has(b'z'));
}
