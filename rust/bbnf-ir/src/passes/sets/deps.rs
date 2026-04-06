//! Rule dependency graph computation.
//!
//! Walks each rule body collecting `IrNode::Ref` edges to build an adjacency list.
//! This is transient data consumed by `compute_scc` — not stored in `GrammarIR`.

use crate::{GrammarIR, IrNode, RuleId};

/// Compute the rule dependency graph.
///
/// Returns an adjacency list indexed by rule position in `ir.rules`.
/// `deps[i]` contains the RuleIds that rule `i` directly references.
pub fn compute_rule_deps(ir: &GrammarIR) -> Vec<Vec<RuleId>> {
    let n = ir.rules.len();
    let mut deps = vec![Vec::new(); n];
    for rule in &ir.rules {
        collect_refs(&rule.body, &mut deps[rule.id as usize]);
    }
    deps
}

fn collect_refs(node: &IrNode, out: &mut Vec<RuleId>) {
    match node {
        IrNode::Ref(id) => {
            if !out.contains(id) {
                out.push(*id);
            }
        }

        IrNode::Seq(children) => {
            for child in children {
                collect_refs(child, out);
            }
        }
        IrNode::Alt(branches, _) => {
            for branch in branches {
                collect_refs(&branch.node, out);
            }
        }

        IrNode::Repeat { inner, .. }
        | IrNode::Map { inner, .. }
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Negate(inner) => collect_refs(inner, out),

        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            collect_refs(a, out);
            collect_refs(b, out);
        }

        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            collect_refs(token, out);
            for arm in arms {
                collect_refs(&arm.continuation, out);
            }
            collect_refs(fallback, out);
        }

        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => {}
    }
}
