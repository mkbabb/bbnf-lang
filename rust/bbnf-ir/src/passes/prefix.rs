//! Pass: Common prefix factoring for alternations.
//!
//! Rewrites `Alt([Seq(A, B), Seq(A, C)])` → `Seq(A, Alt([B, C]))` when branches
//! share a common prefix. Reduces backtracking by hoisting shared work.

use rayon::prelude::*;

use crate::{AltBranch, GrammarIR, IrNode};

/// Factor common prefixes out of alternation branches.
///
/// Walks the entire IR tree and rewrites any `Alt` node whose branches
/// share a common leading node. The factored prefix becomes a `Seq`
/// wrapping the remaining alternation.
pub fn factor_common_prefixes(ir: &mut GrammarIR) {
    if ir.rules.len() >= 16 {
        ir.rules.par_iter_mut().for_each(|rule| {
            rule.body = factor(std::mem::replace(&mut rule.body, IrNode::Epsilon));
        });
    } else {
        for rule in &mut ir.rules {
            rule.body = factor(std::mem::replace(&mut rule.body, IrNode::Epsilon));
        }
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

            // Group branches by their leading node, then re-factor remainder
            // alternations to catch depth-2+ prefixes. E.g.:
            //   Alt([Seq(A,B,C), Seq(A,B,D)]) → Seq(A, Alt([Seq(B,C), Seq(B,D)]))
            // First pass factors out A, second pass (via recursive factor call on
            // the remainder) factors out B.
            let factored = factor_branches(branches);
            // Re-factor each produced branch to catch nested prefixes.
            let factored: Vec<AltBranch> = factored
                .into_iter()
                .map(|mut b| {
                    b.node = factor(b.node);
                    b
                })
                .collect();

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

            // If all remainders are Epsilon, the branches were identical single
            // nodes — factoring just wraps in Seq(leader, Alt([Eps,...])) which
            // is non-productive. Keep the original branches as-is.
            if remainder_branches
                .iter()
                .all(|b| b.node == IrNode::Epsilon)
            {
                for b in &branches[i..j] {
                    result.push(b.clone());
                }
                i = j;
                continue;
            }

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
