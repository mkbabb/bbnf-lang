//! Pass: Acyclic rule inlining.
//!
//! Replaces `Ref(id)` with the rule body when the target rule is small,
//! acyclic, and not the entry point. Reduces call overhead in the interpreter
//! and exposes further optimization opportunities (literal merging, dispatch).

use crate::{GrammarIR, IrNode, RuleId};

/// Maximum node count for a rule to be considered inlinable.
/// Increased from 3 to 4 to enable more inlining opportunities,
/// exposing further literal merging and dispatch optimizations.
const INLINE_THRESHOLD: usize = 4;

/// Inline small acyclic rules at their call sites.
///
/// A rule is inlinable when:
/// 1. It is not cyclic
/// 2. It is not the grammar entry point
/// 3. Its body has at most `INLINE_THRESHOLD` nodes
///
/// After inlining, the original rule remains (it may be referenced externally).
/// Run `prune_unreachable` afterward to clean up dead rules.
pub fn inline_acyclic(ir: &mut GrammarIR) {
    // Identify inlinable rules.
    let inlinable: Vec<(RuleId, IrNode)> = ir
        .rules
        .iter()
        .filter(|r| {
            r.id != ir.entry
                && !r.meta.is_cyclic
                && r.meta.scc_id.is_none()
                && node_count(&r.body) <= INLINE_THRESHOLD
        })
        .map(|r| (r.id, r.body.clone()))
        .collect();

    if inlinable.is_empty() {
        return;
    }

    // Build a lookup: rule_id → body (for inlinable rules only).
    let max_id = inlinable.iter().map(|(id, _)| *id).max().unwrap_or(0) as usize;
    let mut bodies: Vec<Option<IrNode>> = vec![None; max_id + 1];
    for (id, body) in &inlinable {
        bodies[*id as usize] = Some(body.clone());
    }

    // Rewrite all rule bodies.
    for rule in &mut ir.rules {
        rule.body = inline_refs(std::mem::replace(&mut rule.body, IrNode::Epsilon), &bodies);
    }
}

/// Count the number of nodes in an IR tree (for threshold check).
fn node_count(node: &IrNode) -> usize {
    match node {
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => 1,
        IrNode::Seq(children) => 1 + children.iter().map(node_count).sum::<usize>(),
        IrNode::Alt(branches, _) => 1 + branches.iter().map(|b| node_count(&b.node)).sum::<usize>(),
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => 1 + node_count(inner),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            1 + node_count(a) + node_count(b)
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            1 + node_count(token)
                + arms
                    .iter()
                    .map(|a| node_count(&a.continuation))
                    .sum::<usize>()
                + node_count(fallback)
        }
    }
}

/// Recursively replace `Ref(id)` with the inlined body where applicable.
fn inline_refs(node: IrNode, bodies: &[Option<IrNode>]) -> IrNode {
    match node {
        IrNode::Ref(id) => {
            if let Some(Some(body)) = bodies.get(id as usize) {
                body.clone()
            } else {
                IrNode::Ref(id)
            }
        }
        IrNode::Seq(children) => IrNode::Seq(
            children
                .into_iter()
                .map(|c| inline_refs(c, bodies))
                .collect(),
        ),
        IrNode::Alt(branches, dispatch) => {
            let branches = branches
                .into_iter()
                .map(|mut b| {
                    // Don't inline bare Refs in Alt branches — codegen needs rule
                    // identity for proper enum variant wrapping. Refs inside Map
                    // wrappers or deeper sub-expressions are fine to inline.
                    if !matches!(&b.node, IrNode::Ref(_)) {
                        b.node = inline_refs(b.node, bodies);
                    }
                    b
                })
                .collect();
            IrNode::Alt(branches, dispatch)
        }
        IrNode::Repeat { inner, lo, hi } => IrNode::Repeat {
            inner: Box::new(inline_refs(*inner, bodies)),
            lo,
            hi,
        },
        IrNode::Skip(a, b) => IrNode::Skip(
            Box::new(inline_refs(*a, bodies)),
            Box::new(inline_refs(*b, bodies)),
        ),
        IrNode::Next(a, b) => IrNode::Next(
            Box::new(inline_refs(*a, bodies)),
            Box::new(inline_refs(*b, bodies)),
        ),
        IrNode::Minus(a, b) => IrNode::Minus(
            Box::new(inline_refs(*a, bodies)),
            Box::new(inline_refs(*b, bodies)),
        ),
        IrNode::Negate(inner) => IrNode::Negate(Box::new(inline_refs(*inner, bodies))),
        IrNode::OptionalWhitespace(inner) => {
            IrNode::OptionalWhitespace(Box::new(inline_refs(*inner, bodies)))
        }
        IrNode::Map { inner, fn_id } => IrNode::Map {
            inner: Box::new(inline_refs(*inner, bodies)),
            fn_id,
        },
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => IrNode::TokenDispatch {
            token: Box::new(inline_refs(*token, bodies)),
            arms: arms
                .into_iter()
                .map(|mut a| {
                    a.continuation = inline_refs(a.continuation, bodies);
                    a
                })
                .collect(),
            fallback: Box::new(inline_refs(*fallback, bodies)),
        },
        other => other,
    }
}
