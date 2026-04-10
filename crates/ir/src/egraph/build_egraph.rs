//! Insert an `IrNode` tree into an e-graph, returning the root e-class `Id`.

use egraph::{Analysis, EGraph, Id};

use super::node::{ENodeTdArm, GrammarENode};
use crate::{GrammarIR, IrNode};

/// Insert every rule's body into the e-graph. Returns the vector of root
/// e-class Ids, one per rule in rule order.
pub fn insert_ir<A: Analysis<GrammarENode>>(
    egraph: &mut EGraph<GrammarENode, A>,
    ir: &GrammarIR,
) -> Vec<Id> {
    ir.rules
        .iter()
        .map(|rule| insert_node(egraph, &rule.body))
        .collect()
}

/// Recursively insert an `IrNode` and return its e-class Id.
pub fn insert_node<A: Analysis<GrammarENode>>(
    egraph: &mut EGraph<GrammarENode, A>,
    node: &IrNode,
) -> Id {
    let enode = match node {
        IrNode::Literal(sid) => GrammarENode::Literal(*sid),
        IrNode::Regex(sid) => GrammarENode::Regex(*sid),
        IrNode::Epsilon => GrammarENode::Epsilon,
        IrNode::Ref(rule_id) => GrammarENode::Ref(*rule_id),
        IrNode::Seq(children) => {
            let ids: Box<[Id]> =
                children.iter().map(|c| insert_node(egraph, c)).collect();
            GrammarENode::Seq(ids)
        }
        IrNode::Alt(branches, dispatch) => {
            let ids: Box<[Id]> = branches
                .iter()
                .map(|b| insert_node(egraph, &b.node))
                .collect();
            GrammarENode::Alt(ids, dispatch.clone())
        }
        IrNode::Repeat { inner, lo, hi } => {
            let inner_id = insert_node(egraph, inner);
            GrammarENode::Repeat {
                inner: inner_id,
                lo: *lo,
                hi: *hi,
            }
        }
        IrNode::Skip(a, b) => {
            let a_id = insert_node(egraph, a);
            let b_id = insert_node(egraph, b);
            GrammarENode::Skip([a_id, b_id])
        }
        IrNode::Next(a, b) => {
            let a_id = insert_node(egraph, a);
            let b_id = insert_node(egraph, b);
            GrammarENode::Next([a_id, b_id])
        }
        IrNode::Minus(a, b) => {
            let a_id = insert_node(egraph, a);
            let b_id = insert_node(egraph, b);
            GrammarENode::Minus([a_id, b_id])
        }
        IrNode::Negate(inner) => {
            let inner_id = insert_node(egraph, inner);
            GrammarENode::Negate(inner_id)
        }
        IrNode::Map { inner, fn_id } => {
            let inner_id = insert_node(egraph, inner);
            GrammarENode::Map {
                inner: inner_id,
                fn_id: *fn_id,
            }
        }
        IrNode::OptionalWhitespace(inner) => {
            let inner_id = insert_node(egraph, inner);
            GrammarENode::OptionalWhitespace(inner_id)
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            let token_id = insert_node(egraph, token);
            let fallback_id = insert_node(egraph, fallback);
            let e_arms: Box<[ENodeTdArm]> = arms
                .iter()
                .map(|arm| {
                    let cont = insert_node(egraph, &arm.continuation);
                    ENodeTdArm {
                        patterns: arm.patterns.clone(),
                        guard_byte: arm.guard_byte,
                        continuation: cont,
                        map_fn: arm.map_fn,
                    }
                })
                .collect();
            GrammarENode::TokenDispatch {
                token: token_id,
                arms: e_arms,
                fallback: fallback_id,
            }
        }
    };
    egraph.add(enode)
}
