//! sk-v27 LW3.ORC2-WS-C (RULINGS 2026-11-14f PERF1D-B2 (c)): where the
//! grammar's text reads whitespace.
//!
//! Root reads whitespace only where the grammar writes it (the rule is
//! written in the sk-v27 LEDGER, row "LW3.ORC2-WS-C: the rule the cure
//! implements"): `x ?w` reads `ws* x ws*` at exactly that place, and no
//! rule, sequence, repetition or entry reads any other. The shape
//! emitters lower each `?w` they meet; these helpers answer, for the
//! structures a shape recognises, whether a `?w` stands at a given
//! boundary.

use bbnf_ir::{GrammarIR, IrNode};

/// Whether `node`, seen through `->` mappings, is `x ?w`: the grammar
/// reads `?w` whitespace immediately before and after it.
pub(crate) fn root_reads_ow(node: &IrNode) -> bool {
    match node {
        IrNode::Map { inner, .. } => root_reads_ow(inner),
        IrNode::OptionalWhitespace(_) => true,
        _ => false,
    }
}

/// Whether every match of `node` begins by reading `?w` whitespace: the
/// grammar reads whitespace at the boundary before `node`. Looks through
/// `->`, the first item of a sequence, `>>` / `<<`, a repetition of at
/// least one, an alternation all of whose branches lead with `?w`, and
/// (a few hops of) rule references.
pub(crate) fn leads_ow(node: &IrNode, ir: &GrammarIR) -> bool {
    edge_ow(node, ir, Edge::Lead, 4)
}

/// Whether every match of `node` ends by reading `?w` whitespace: the
/// grammar reads whitespace at the boundary after `node`. The mirror of
/// [`leads_ow`].
pub(crate) fn trails_ow(node: &IrNode, ir: &GrammarIR) -> bool {
    edge_ow(node, ir, Edge::Trail, 4)
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Edge {
    Lead,
    Trail,
}

fn edge_ow(node: &IrNode, ir: &GrammarIR, edge: Edge, budget: u32) -> bool {
    let next = |n: &IrNode| edge_ow(n, ir, edge, budget);
    match node {
        IrNode::OptionalWhitespace(_) => true,
        IrNode::Map { inner, .. } => next(inner),
        IrNode::Seq(children) => {
            let mut kept = children.iter().filter(|c| !matches!(c, IrNode::Epsilon));
            let end = if edge == Edge::Lead { kept.next() } else { kept.next_back() };
            end.is_some_and(next)
        }
        IrNode::Next(lhs, rhs) | IrNode::Skip(lhs, rhs) => {
            let (near, far) = if edge == Edge::Lead { (lhs, rhs) } else { (rhs, lhs) };
            if matches!(near.as_ref(), IrNode::Epsilon) { next(far) } else { next(near) }
        }
        IrNode::Alt(branches, _) => {
            !branches.is_empty() && branches.iter().all(|b| next(&b.node))
        }
        IrNode::Repeat { inner, lo, .. } if *lo > 0 => next(inner),
        IrNode::Ref(rid) if budget > 0 => ir
            .rules
            .iter()
            .find(|r| r.id == *rid)
            .is_some_and(|r| edge_ow(&r.body, ir, edge, budget - 1)),
        _ => false,
    }
}
