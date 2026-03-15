//! Pass: Common prefix factoring for alternations.
//!
//! Rewrites `Alt([Seq(A, B), Seq(A, C)])` → `Seq(A, Alt([B, C]))` when branches
//! share a common prefix. Reduces backtracking by hoisting shared work.

use crate::{AltBranch, GrammarIR, IrNode};

/// Factor common prefixes out of alternation branches.
///
/// Walks the entire IR tree and rewrites any `Alt` node whose branches
/// share a common leading node. The factored prefix becomes a `Seq`
/// wrapping the remaining alternation.
pub fn factor_common_prefixes(ir: &mut GrammarIR) {
    for rule in &mut ir.rules {
        rule.body = factor(std::mem::replace(&mut rule.body, IrNode::Epsilon));
    }
}

fn factor(node: IrNode) -> IrNode {
    match node {
        IrNode::Alt(branches, dispatch) => {
            // Recurse into children first.
            let branches: Vec<AltBranch> = branches
                .into_iter()
                .map(|mut b| {
                    b.node = factor(b.node);
                    b
                })
                .collect();

            // Group branches by their leading node.
            let factored = factor_branches(branches);

            if factored.len() == 1 {
                factored.into_iter().next().unwrap().node
            } else {
                IrNode::Alt(factored, dispatch)
            }
        }
        IrNode::Seq(children) => {
            IrNode::Seq(children.into_iter().map(factor).collect())
        }
        IrNode::Repeat { inner, lo, hi } => IrNode::Repeat {
            inner: Box::new(factor(*inner)),
            lo,
            hi,
        },
        IrNode::Skip(a, b) => {
            IrNode::Skip(Box::new(factor(*a)), Box::new(factor(*b)))
        }
        IrNode::Next(a, b) => {
            IrNode::Next(Box::new(factor(*a)), Box::new(factor(*b)))
        }
        IrNode::Minus(a, b) => {
            IrNode::Minus(Box::new(factor(*a)), Box::new(factor(*b)))
        }
        IrNode::Negate(inner) => IrNode::Negate(Box::new(factor(*inner))),
        IrNode::OptionalWhitespace(inner) => {
            IrNode::OptionalWhitespace(Box::new(factor(*inner)))
        }
        IrNode::Map { inner, fn_id } => IrNode::Map {
            inner: Box::new(factor(*inner)),
            fn_id,
        },
        other => other,
    }
}

/// Extract the leading node from a branch (first element of Seq, or the node itself).
fn leading_node(node: &IrNode) -> &IrNode {
    match node {
        IrNode::Seq(children) if !children.is_empty() => &children[0],
        other => other,
    }
}

/// Strip the leading node from a branch, returning the remainder.
fn strip_leading(node: IrNode) -> IrNode {
    match node {
        IrNode::Seq(mut children) if children.len() > 1 => {
            children.remove(0);
            if children.len() == 1 {
                children.into_iter().next().unwrap()
            } else {
                IrNode::Seq(children)
            }
        }
        // Single-node or non-Seq: nothing left after stripping.
        _ => IrNode::Epsilon,
    }
}

/// Group branches by common leading node and merge groups of size > 1.
fn factor_branches(branches: Vec<AltBranch>) -> Vec<AltBranch> {
    if branches.len() < 2 {
        return branches;
    }

    // Collect runs of branches with the same leading node.
    // We use sequential grouping (not arbitrary grouping) to preserve
    // alternation order semantics.
    let mut result: Vec<AltBranch> = Vec::new();
    let mut i = 0;

    while i < branches.len() {
        let leader = leading_node(&branches[i].node).clone();

        // Find how many consecutive branches share this leader.
        let mut j = i + 1;
        while j < branches.len() && leading_node(&branches[j].node) == &leader {
            j += 1;
        }

        if j - i == 1 {
            // No common prefix — keep as-is.
            result.push(branches[i].clone());
        } else {
            // Factor out the common prefix.
            let remainder_branches: Vec<AltBranch> = branches[i..j]
                .iter()
                .map(|b| AltBranch {
                    node: strip_leading(b.node.clone()),
                    first_set: None,
                })
                .collect();

            let remainder_alt = if remainder_branches.len() == 1 {
                remainder_branches.into_iter().next().unwrap().node
            } else {
                IrNode::Alt(remainder_branches, None)
            };

            let factored_node = IrNode::Seq(vec![leader.clone(), remainder_alt]);

            result.push(AltBranch {
                node: factored_node,
                first_set: None,
            });
        }

        i = j;
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;
    use crate::{IrRule, RuleMeta};

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
        // → Seq(Lit(1), Alt([Lit(2), Lit(3)]))
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
        // → Alt([Seq(A, Alt([B, C])), Seq(D, E)])
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
        // Alt([Lit(1), Lit(1)]) → Lit(1) (identical branches collapse)
        let mut ir = make_ir(alt(vec![IrNode::Literal(1), IrNode::Literal(1)]));

        factor_common_prefixes(&mut ir);

        // After factoring: Seq(Lit(1), Alt([Epsilon, Epsilon]))
        // This is correct — the branches were identical single-node entries.
        match &ir.rules[0].body {
            IrNode::Seq(children) => {
                assert_eq!(children[0], IrNode::Literal(1));
            }
            other => panic!("expected Seq after factoring identical branches, got {:?}", other),
        }
    }
}
