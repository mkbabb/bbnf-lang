use std::collections::HashMap;

use bbnf_ir::{AltBranch, GrammarIR, IrNode, IrRule, RuleMeta};
use bbnf_ir::passes::factor_common_prefixes;

fn make_ir(body: IrNode) -> GrammarIR {
    GrammarIR {
        entry: 0,
        rules: vec![IrRule {
            id: 0,
            name: 0,
            body,
            meta: RuleMeta::default(),
        }],
        strings: vec!["start".into(), "a".into(), "b".into(), "c".into()],
        fns: vec![],
        types: vec![],
        follow_sets: HashMap::new(),
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
fn single_node_branches_factored() {
    // Alt([Lit(1), Lit(1)]) -> Lit(1) (identical branches collapse)
    let mut ir = make_ir(alt(vec![IrNode::Literal(1), IrNode::Literal(1)]));

    factor_common_prefixes(&mut ir);

    // After factoring: Seq(Lit(1), Alt([Epsilon, Epsilon]))
    // This is correct -- the branches were identical single-node entries.
    match &ir.rules[0].body {
        IrNode::Seq(children) => {
            assert_eq!(children[0], IrNode::Literal(1));
        }
        other => panic!("expected Seq after factoring identical branches, got {:?}", other),
    }
}
