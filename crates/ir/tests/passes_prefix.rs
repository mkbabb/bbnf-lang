use std::collections::HashMap;

use bbnf_ir::passes::factor_common_prefixes;
use bbnf_ir::{AltBranch, FnDescriptor, GrammarIR, IrNode, IrRule, RuleMeta};

fn make_ir(body: IrNode) -> GrammarIR {
    GrammarIR {
        entry: 0,
        rules: vec![IrRule {
            id: 0,
            name: 0,
            body,
            meta: RuleMeta::default(),
            source_span: None,
        }],
        strings: vec!["start".into(), "a".into(), "b".into(), "c".into()],
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
        regex_engine_decisions: std::collections::HashMap::new(),
        dag: None, cost_config: bbnf_ir::CostConfig::default(),
    }
}

fn make_ir_with_strings(body: IrNode, strings: Vec<String>) -> GrammarIR {
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
        regex_engine_decisions: std::collections::HashMap::new(),
        dag: None, cost_config: bbnf_ir::CostConfig::default(),
    }
}

fn alt(branches: Vec<IrNode>) -> IrNode {
    IrNode::Alt(
        branches
            .into_iter()
            .map(|n| AltBranch {
                node: n,
                first_set: None,
            })
            .collect(),
        None,
    )
}

#[test]
fn common_prefix_factored() {
    // Alt([Seq(Lit(1), Lit(2)), Seq(Lit(1), Lit(3))])
    // -> Seq(Lit(1), Alt([Lit(2), Lit(3)]))
    let mut ir = make_ir(alt(vec![
        IrNode::Seq(vec![IrNode::Literal(1), IrNode::Literal(2)]),
        IrNode::Seq(vec![IrNode::Literal(1), IrNode::Literal(3)]),
    ]));

    factor_common_prefixes(&mut ir);

    match &ir.rules[0].body {
        IrNode::Seq(children) => {
            assert_eq!(children.len(), 2);
            assert_eq!(children[0], IrNode::Literal(1));
            match &children[1] {
                IrNode::Alt(branches, _) => {
                    assert_eq!(branches.len(), 2);
                    assert_eq!(branches[0].node, IrNode::Literal(2));
                    assert_eq!(branches[1].node, IrNode::Literal(3));
                }
                other => panic!("expected Alt, got {:?}", other),
            }
        }
        other => panic!("expected Seq, got {:?}", other),
    }
}

#[test]
fn no_common_prefix_unchanged() {
    let mut ir = make_ir(alt(vec![
        IrNode::Seq(vec![IrNode::Literal(1), IrNode::Literal(2)]),
        IrNode::Seq(vec![IrNode::Literal(3), IrNode::Literal(2)]),
    ]));

    factor_common_prefixes(&mut ir);

    match &ir.rules[0].body {
        IrNode::Alt(branches, _) => {
            assert_eq!(branches.len(), 2);
        }
        other => panic!("expected Alt with 2 branches, got {:?}", other),
    }
}

#[test]
fn three_branches_two_shared() {
    // Alt([Seq(A, B), Seq(A, C), Seq(D, E)])
    // -> Alt([Seq(A, Alt([B, C])), Seq(D, E)])
    let mut ir = make_ir(alt(vec![
        IrNode::Seq(vec![IrNode::Literal(1), IrNode::Literal(2)]),
        IrNode::Seq(vec![IrNode::Literal(1), IrNode::Literal(3)]),
        IrNode::Seq(vec![IrNode::Regex(2), IrNode::Literal(3)]),
    ]));

    factor_common_prefixes(&mut ir);

    match &ir.rules[0].body {
        IrNode::Alt(branches, _) => {
            assert_eq!(branches.len(), 2);
            // First branch is factored.
            match &branches[0].node {
                IrNode::Seq(children) => {
                    assert_eq!(children[0], IrNode::Literal(1));
                }
                other => panic!("expected Seq, got {:?}", other),
            }
        }
        other => panic!("expected Alt, got {:?}", other),
    }
}

#[test]
fn depth_two_prefix_factored() {
    // Alt([Seq(A, B, C), Seq(A, B, D)]) should factor to Seq(A, B, Alt([C, D]))
    // via multi-round fixed-point: pass 1 factors A, pass 2 factors B.
    let mut ir = make_ir(alt(vec![
        IrNode::Seq(vec![
            IrNode::Literal(1),
            IrNode::Literal(2),
            IrNode::Literal(3),
        ]),
        IrNode::Seq(vec![
            IrNode::Literal(1),
            IrNode::Literal(2),
            IrNode::Regex(3),
        ]),
    ]));

    factor_common_prefixes(&mut ir);

    // Result: Seq(Lit(1), Seq(Lit(2), Alt([Lit(3), Regex(3)])))
    // Pass 1 factors out Lit(1), leaving Alt([Seq(Lit(2),Lit(3)), Seq(Lit(2),Regex(3))])
    // Re-factor recurses and factors out Lit(2), producing Seq(Lit(2), Alt([Lit(3), Regex(3)]))
    match &ir.rules[0].body {
        IrNode::Seq(outer) => {
            assert_eq!(
                outer.len(),
                2,
                "expected Seq(A, Seq(B, Alt(...))), got {:?}",
                outer
            );
            assert_eq!(outer[0], IrNode::Literal(1));
            match &outer[1] {
                IrNode::Seq(inner) => {
                    assert_eq!(inner.len(), 2);
                    assert_eq!(inner[0], IrNode::Literal(2));
                    match &inner[1] {
                        IrNode::Alt(branches, _) => {
                            assert_eq!(branches.len(), 2);
                            assert_eq!(branches[0].node, IrNode::Literal(3));
                            assert_eq!(branches[1].node, IrNode::Regex(3));
                        }
                        other => panic!("expected inner Alt, got {:?}", other),
                    }
                }
                other => panic!("expected inner Seq, got {:?}", other),
            }
        }
        other => panic!(
            "expected outer Seq after depth-2 factoring, got {:?}",
            other
        ),
    }
}

#[test]
fn single_node_branches_unchanged() {
    // Alt([Lit(1), Lit(1)]) — identical single-node branches are NOT factored
    // because all remainders would be Epsilon (non-productive factoring).
    let mut ir = make_ir(alt(vec![IrNode::Literal(1), IrNode::Literal(1)]));

    factor_common_prefixes(&mut ir);

    match &ir.rules[0].body {
        IrNode::Alt(branches, _) => {
            assert_eq!(branches.len(), 2);
            assert_eq!(branches[0].node, IrNode::Literal(1));
            assert_eq!(branches[1].node, IrNode::Literal(1));
        }
        other => panic!("expected unchanged Alt, got {:?}", other),
    }
}

// ─── Byte-level literal prefix splitting tests ─────────────────────────────

#[test]
fn literal_byte_split_basic() {
    // Alt([Literal("rem"), Literal("rlh")])
    // → Seq(Literal("r"), Alt([Literal("em"), Literal("lh")]))
    let mut ir = make_ir_with_strings(
        alt(vec![IrNode::Literal(1), IrNode::Literal(2)]),
        vec!["start".into(), "rem".into(), "rlh".into()],
    );

    factor_common_prefixes(&mut ir);

    match &ir.rules[0].body {
        IrNode::Seq(children) => {
            assert_eq!(children.len(), 2);
            // First child: Literal("r")
            match &children[0] {
                IrNode::Literal(sid) => {
                    assert_eq!(ir.strings[*sid as usize], "r");
                }
                other => panic!("expected Literal(\"r\"), got {:?}", other),
            }
            // Second child: Alt([Literal("em"), Literal("lh")])
            match &children[1] {
                IrNode::Alt(branches, _) => {
                    assert_eq!(branches.len(), 2);
                    match &branches[0].node {
                        IrNode::Literal(sid) => assert_eq!(ir.strings[*sid as usize], "em"),
                        other => panic!("expected Literal(\"em\"), got {:?}", other),
                    }
                    match &branches[1].node {
                        IrNode::Literal(sid) => assert_eq!(ir.strings[*sid as usize], "lh"),
                        other => panic!("expected Literal(\"lh\"), got {:?}", other),
                    }
                }
                other => panic!("expected Alt, got {:?}", other),
            }
        }
        other => panic!("expected Seq after byte split, got {:?}", other),
    }
}

#[test]
fn literal_byte_split_multiple_groups() {
    // Alt([Literal("rem"), Literal("rlh"), Literal("em"), Literal("ex")])
    // → Alt([
    //     Seq(Literal("r"), Alt([Literal("em"), Literal("lh")])),  // "r" group
    //     Seq(Literal("e"), Alt([Literal("m"), Literal("x")])),    // "e" group
    // ])
    let mut ir = make_ir_with_strings(
        alt(vec![
            IrNode::Literal(1),
            IrNode::Literal(2),
            IrNode::Literal(3),
            IrNode::Literal(4),
        ]),
        vec![
            "start".into(),
            "rem".into(),
            "rlh".into(),
            "em".into(),
            "ex".into(),
        ],
    );

    factor_common_prefixes(&mut ir);

    match &ir.rules[0].body {
        IrNode::Alt(branches, _) => {
            assert_eq!(branches.len(), 2, "expected 2 groups, got {:?}", branches);

            // First group: Seq(Literal("r"), Alt([Literal("em"), Literal("lh")]))
            match &branches[0].node {
                IrNode::Seq(children) => {
                    match &children[0] {
                        IrNode::Literal(sid) => assert_eq!(ir.strings[*sid as usize], "r"),
                        other => panic!("expected Literal(\"r\"), got {:?}", other),
                    }
                    match &children[1] {
                        IrNode::Alt(inner, _) => {
                            assert_eq!(inner.len(), 2);
                        }
                        other => panic!("expected Alt for r-group, got {:?}", other),
                    }
                }
                other => panic!("expected Seq for r-group, got {:?}", other),
            }

            // Second group: Seq(Literal("e"), Alt([Literal("m"), Literal("x")]))
            match &branches[1].node {
                IrNode::Seq(children) => {
                    match &children[0] {
                        IrNode::Literal(sid) => assert_eq!(ir.strings[*sid as usize], "e"),
                        other => panic!("expected Literal(\"e\"), got {:?}", other),
                    }
                    match &children[1] {
                        IrNode::Alt(inner, _) => {
                            assert_eq!(inner.len(), 2);
                        }
                        other => panic!("expected Alt for e-group, got {:?}", other),
                    }
                }
                other => panic!("expected Seq for e-group, got {:?}", other),
            }
        }
        other => panic!("expected Alt after byte split, got {:?}", other),
    }
}

#[test]
fn literal_byte_split_with_map_wrapper() {
    // Alt([Map(Literal("rem"), fn0), Map(Literal("rlh"), fn1)])
    // → Seq(Literal("r"), Alt([Map(Literal("em"), fn0), Map(Literal("lh"), fn1)]))
    let mut ir = make_ir_with_strings(
        alt(vec![
            IrNode::Map {
                inner: Box::new(IrNode::Literal(1)),
                fn_id: 0,
            },
            IrNode::Map {
                inner: Box::new(IrNode::Literal(2)),
                fn_id: 1,
            },
        ]),
        vec!["start".into(), "rem".into(), "rlh".into()],
    );
    ir.fns.push(FnDescriptor::Expr {
        expr: bbnf_ir::MapExpr::IntLit(0),
        return_type: None,
    });
    ir.fns.push(FnDescriptor::Expr {
        expr: bbnf_ir::MapExpr::IntLit(0),
        return_type: None,
    });

    factor_common_prefixes(&mut ir);

    match &ir.rules[0].body {
        IrNode::Seq(children) => {
            assert_eq!(children.len(), 2);
            // Prefix: bare Literal("r")
            match &children[0] {
                IrNode::Literal(sid) => assert_eq!(ir.strings[*sid as usize], "r"),
                other => panic!("expected Literal(\"r\"), got {:?}", other),
            }
            // Continuation: Alt with Map-wrapped remainders
            match &children[1] {
                IrNode::Alt(branches, _) => {
                    assert_eq!(branches.len(), 2);
                    // Each branch should be Map(Literal("em"/"lh"), fn_id)
                    for (i, expected_rem) in [("em", 0u32), ("lh", 1u32)].iter().enumerate() {
                        match &branches[i].node {
                            IrNode::Map { inner, fn_id } => {
                                assert_eq!(*fn_id, expected_rem.1);
                                match inner.as_ref() {
                                    IrNode::Literal(sid) => {
                                        assert_eq!(ir.strings[*sid as usize], expected_rem.0);
                                    }
                                    other => panic!("expected Literal inside Map, got {:?}", other),
                                }
                            }
                            other => panic!("expected Map, got {:?}", other),
                        }
                    }
                }
                other => panic!("expected Alt, got {:?}", other),
            }
        }
        other => panic!("expected Seq after byte split, got {:?}", other),
    }
}

#[test]
fn literal_byte_split_single_byte_not_split() {
    // Single-byte literals can't be split further — they should be left as-is.
    // Alt([Literal("a"), Literal("b")]) — different first bytes, no grouping.
    let mut ir = make_ir_with_strings(
        alt(vec![IrNode::Literal(1), IrNode::Literal(2)]),
        vec!["start".into(), "a".into(), "b".into()],
    );

    factor_common_prefixes(&mut ir);

    match &ir.rules[0].body {
        IrNode::Alt(branches, _) => {
            assert_eq!(
                branches.len(),
                2,
                "single-byte literals should not be split"
            );
        }
        other => panic!("expected unchanged Alt, got {:?}", other),
    }
}

#[test]
fn literal_byte_split_mixed_with_non_literal() {
    // Non-literal branches act as barriers.
    // Alt([Literal("rem"), Regex(X), Literal("rlh")]) — the Regex separates,
    // so "rem" and "rlh" are NOT grouped.
    let mut ir = make_ir_with_strings(
        alt(vec![
            IrNode::Literal(1),
            IrNode::Regex(3),
            IrNode::Literal(2),
        ]),
        vec!["start".into(), "rem".into(), "rlh".into(), "x+".into()],
    );

    factor_common_prefixes(&mut ir);

    match &ir.rules[0].body {
        IrNode::Alt(branches, _) => {
            // All 3 branches should remain (no consecutive same-first-byte literals).
            assert_eq!(
                branches.len(),
                3,
                "non-literal barrier should prevent grouping"
            );
        }
        other => panic!("expected Alt with 3 branches, got {:?}", other),
    }
}

#[test]
fn literal_byte_split_idempotent() {
    // Running the pass twice should produce the same result.
    let mut ir = make_ir_with_strings(
        alt(vec![IrNode::Literal(1), IrNode::Literal(2)]),
        vec!["start".into(), "rem".into(), "rlh".into()],
    );

    factor_common_prefixes(&mut ir);
    let body_after_first = ir.rules[0].body.clone();
    let strings_after_first = ir.strings.clone();

    factor_common_prefixes(&mut ir);
    assert_eq!(
        ir.rules[0].body, body_after_first,
        "pass should be idempotent"
    );
    assert_eq!(
        ir.strings.len(),
        strings_after_first.len(),
        "no new strings on second pass"
    );
}

#[test]
fn literal_byte_split_recursive_trie() {
    // Alt([Literal("abc"), Literal("abd"), Literal("aef")])
    // After first split: Seq(Literal("a"), Alt([Literal("bc"), Literal("bd"), Literal("ef")]))
    // After recursive re-factor: the inner Alt gets further split:
    //   Alt([Literal("bc"), Literal("bd"), Literal("ef")])
    //   → Alt([Seq(Literal("b"), Alt([Literal("c"), Literal("d")])), Literal("ef")])
    let mut ir = make_ir_with_strings(
        alt(vec![
            IrNode::Literal(1),
            IrNode::Literal(2),
            IrNode::Literal(3),
        ]),
        vec!["start".into(), "abc".into(), "abd".into(), "aef".into()],
    );

    factor_common_prefixes(&mut ir);

    // Outermost: Seq(Literal("a"), ...)
    match &ir.rules[0].body {
        IrNode::Seq(outer) => {
            assert_eq!(outer.len(), 2);
            match &outer[0] {
                IrNode::Literal(sid) => assert_eq!(ir.strings[*sid as usize], "a"),
                other => panic!("expected Literal(\"a\"), got {:?}", other),
            }
            // Inner: Alt([Seq(Literal("b"), Alt([...])), Literal("ef")])
            match &outer[1] {
                IrNode::Alt(inner_branches, _) => {
                    assert_eq!(inner_branches.len(), 2, "inner alt: {:?}", inner_branches);
                    // First inner branch: Seq(Literal("b"), Alt([Literal("c"), Literal("d")]))
                    match &inner_branches[0].node {
                        IrNode::Seq(bs) => {
                            assert_eq!(bs.len(), 2);
                            match &bs[0] {
                                IrNode::Literal(sid) => {
                                    assert_eq!(ir.strings[*sid as usize], "b");
                                }
                                other => panic!("expected Literal(\"b\"), got {:?}", other),
                            }
                            match &bs[1] {
                                IrNode::Alt(cd, _) => {
                                    assert_eq!(cd.len(), 2);
                                    match &cd[0].node {
                                        IrNode::Literal(sid) => {
                                            assert_eq!(ir.strings[*sid as usize], "c");
                                        }
                                        other => {
                                            panic!("expected Literal(\"c\"), got {:?}", other)
                                        }
                                    }
                                    match &cd[1].node {
                                        IrNode::Literal(sid) => {
                                            assert_eq!(ir.strings[*sid as usize], "d");
                                        }
                                        other => {
                                            panic!("expected Literal(\"d\"), got {:?}", other)
                                        }
                                    }
                                }
                                other => panic!("expected Alt([c,d]), got {:?}", other),
                            }
                        }
                        other => panic!("expected Seq for b-group, got {:?}", other),
                    }
                    // Second inner branch: Literal("ef")
                    match &inner_branches[1].node {
                        IrNode::Literal(sid) => {
                            assert_eq!(ir.strings[*sid as usize], "ef");
                        }
                        other => panic!("expected Literal(\"ef\"), got {:?}", other),
                    }
                }
                other => panic!("expected inner Alt, got {:?}", other),
            }
        }
        other => panic!("expected outer Seq, got {:?}", other),
    }
}
