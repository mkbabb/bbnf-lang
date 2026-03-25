use std::collections::HashMap;

use bbnf_ir::{AltBranch, CharSet128, GrammarIR, IrNode, IrRule, MemoStrategy, RuleMeta};
use bbnf_ir::passes::refine_memo_strategies;

#[test]
fn cyclic_entry_point_gets_full_memo() {
    // Single cyclic rule -- it is its own entry point.
    let mut ir = GrammarIR {
        rules: vec![IrRule {
            id: 0,
            name: 0,
            body: IrNode::Alt(
                vec![
                    AltBranch {
                        node: IrNode::Seq(vec![IrNode::Ref(0), IrNode::Literal(2)]),
                        first_set: None,
                    },
                    AltBranch {
                        node: IrNode::Literal(3),
                        first_set: None,
                    },
                ],
                None,
            ),
            meta: RuleMeta {
                is_cyclic: true,
                scc_id: Some(0),
                ..Default::default()
            },
            source_span: None,
        }],
        entry: 0,
        strings: vec!["expr".into(), "term".into(), "+".into(), "x".into()],
        fns: vec![],
        types: vec![],
        follow_sets: HashMap::new(),
        ws_pattern: None,
        b1_span_collapse: false,
        debug_all: false,
        debug_labels: Vec::new(),
    };

    refine_memo_strategies(&mut ir);
    assert_eq!(ir.rules[0].meta.memo, MemoStrategy::Full);
}

#[test]
fn scc_non_entry_gets_none() {
    // Two mutually-recursive rules in the same SCC.
    // Rule 0 is referenced from outside (entry), rule 1 is only referenced from rule 0.
    let mut ir = GrammarIR {
        rules: vec![
            IrRule {
                id: 0,
                name: 0,
                body: IrNode::Ref(1),
                meta: RuleMeta {
                    is_cyclic: true,
                    scc_id: Some(0),
                    ..Default::default()
                },
                source_span: None,
            },
            IrRule {
                id: 1,
                name: 1,
                body: IrNode::Alt(
                    vec![
                        AltBranch {
                            node: IrNode::Ref(0),
                            first_set: None,
                        },
                        AltBranch {
                            node: IrNode::Literal(2),
                            first_set: None,
                        },
                    ],
                    None,
                ),
                meta: RuleMeta {
                    is_cyclic: true,
                    scc_id: Some(0),
                    ..Default::default()
                },
                source_span: None,
            },
            // External rule that references rule 0.
            IrRule {
                id: 2,
                name: 2,
                body: IrNode::Ref(0),
                meta: RuleMeta::default(),
                source_span: None,
            },
        ],
        entry: 2,
        strings: vec!["a".into(), "b".into(), "x".into()],
        fns: vec![],
        types: vec![],
        follow_sets: HashMap::new(),
        ws_pattern: None,
        b1_span_collapse: false,
        debug_all: false,
        debug_labels: Vec::new(),
    };

    refine_memo_strategies(&mut ir);
    // Rule 0 is the SCC entry point (referenced from rule 2, outside the SCC).
    assert_eq!(ir.rules[0].meta.memo, MemoStrategy::Full);
    // Rule 1 is NOT an entry point -- only referenced from within the SCC.
    assert_eq!(ir.rules[1].meta.memo, MemoStrategy::None);
}

#[test]
fn highly_referenced_gets_selective() {
    let mut ir = GrammarIR {
        rules: vec![
            IrRule {
                id: 0,
                name: 0,
                body: IrNode::Seq(vec![
                    IrNode::Ref(1),
                    IrNode::Ref(1),
                    IrNode::Ref(1),
                    IrNode::Ref(1),
                ]),
                meta: RuleMeta::default(),
                source_span: None,
            },
            IrRule {
                id: 1,
                name: 1,
                body: IrNode::Literal(2),
                meta: RuleMeta::default(),
                source_span: None,
            },
        ],
        entry: 0,
        strings: vec!["start".into(), "common".into(), "x".into()],
        fns: vec![],
        types: vec![],
        follow_sets: HashMap::new(),
        ws_pattern: None,
        b1_span_collapse: false,
        debug_all: false,
        debug_labels: Vec::new(),
    };

    refine_memo_strategies(&mut ir);
    assert_eq!(ir.rules[1].meta.memo, MemoStrategy::Selective);
}

#[test]
fn low_ref_count_gets_none() {
    let mut ir = GrammarIR {
        rules: vec![
            IrRule {
                id: 0,
                name: 0,
                body: IrNode::Ref(1),
                meta: RuleMeta::default(),
                source_span: None,
            },
            IrRule {
                id: 1,
                name: 1,
                body: IrNode::Literal(2),
                meta: RuleMeta::default(),
                source_span: None,
            },
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
    };

    refine_memo_strategies(&mut ir);
    assert_eq!(ir.rules[1].meta.memo, MemoStrategy::None);
}

#[test]
fn large_follow_set_lowers_memo_threshold() {
    // Rule 1 referenced 3 times (= default threshold).
    // Without FOLLOW boost, 3 refs does not exceed threshold -> None.
    // With large FOLLOW set (>= 8 chars), threshold drops to 2 -> Selective.
    let mut follow = CharSet128::new();
    // Add 10 chars to FOLLOW set (exceeds FOLLOW_BOOST_THRESHOLD of 8).
    for c in b'a'..=b'j' {
        follow.add(c);
    }
    let mut follow_sets = HashMap::new();
    follow_sets.insert(1u32, follow);

    let mut ir = GrammarIR {
        rules: vec![
            IrRule {
                id: 0,
                name: 0,
                body: IrNode::Seq(vec![
                    IrNode::Ref(1),
                    IrNode::Ref(1),
                    IrNode::Ref(1),
                ]),
                meta: RuleMeta::default(),
                source_span: None,
            },
            IrRule {
                id: 1,
                name: 1,
                body: IrNode::Literal(2),
                meta: RuleMeta::default(),
                source_span: None,
            },
        ],
        entry: 0,
        strings: vec!["start".into(), "common".into(), "x".into()],
        fns: vec![],
        types: vec![],
        follow_sets,
        ws_pattern: None,
        b1_span_collapse: false,
        debug_all: false,
        debug_labels: Vec::new(),
    };

    refine_memo_strategies(&mut ir);
    // 3 refs > lowered threshold of 2 -> Selective.
    assert_eq!(ir.rules[1].meta.memo, MemoStrategy::Selective);
}

#[test]
fn small_follow_set_raises_memo_threshold() {
    // Rule 1 referenced 4 times (> default threshold of 3).
    // Without FOLLOW, 4 > 3 -> Selective.
    // With small FOLLOW set (< 4 chars), threshold rises to 4 -> None (4 not > 4).
    let mut follow = CharSet128::new();
    follow.add(b';'); // Only 1 char in FOLLOW.
    let mut follow_sets = HashMap::new();
    follow_sets.insert(1u32, follow);

    let mut ir = GrammarIR {
        rules: vec![
            IrRule {
                id: 0,
                name: 0,
                body: IrNode::Seq(vec![
                    IrNode::Ref(1),
                    IrNode::Ref(1),
                    IrNode::Ref(1),
                    IrNode::Ref(1),
                ]),
                meta: RuleMeta::default(),
                source_span: None,
            },
            IrRule {
                id: 1,
                name: 1,
                body: IrNode::Literal(2),
                meta: RuleMeta::default(),
                source_span: None,
            },
        ],
        entry: 0,
        strings: vec!["start".into(), "common".into(), "x".into()],
        fns: vec![],
        types: vec![],
        follow_sets,
        ws_pattern: None,
        b1_span_collapse: false,
        debug_all: false,
        debug_labels: Vec::new(),
    };

    refine_memo_strategies(&mut ir);
    // 4 refs not > raised threshold of 4 -> None.
    assert_eq!(ir.rules[1].meta.memo, MemoStrategy::None);
}
