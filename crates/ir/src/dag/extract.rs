//! Extract an `IrNode` tree from a `GrammarDag` — the inverse of `build`.
//!
//! Walks the DAG from a root `NodeId` and reconstructs a tree. Since the
//! DAG may share sub-expressions across multiple parents, extraction
//! produces a cloned subtree per parent (no structural sharing at the
//! `IrNode` level).

use super::GrammarDag;
use super::node::{DagNode, NodeId};
use crate::{AltBranch, IrNode, TokenDispatchArm};

impl GrammarDag {
    /// Extract a full `IrNode` tree rooted at `id`.
    pub fn extract(&self, id: NodeId) -> IrNode {
        let node = self.node(id);
        match node {
            DagNode::Literal(sid) => IrNode::Literal(*sid),
            DagNode::Regex(sid) => IrNode::Regex(*sid),
            DagNode::Epsilon => IrNode::Epsilon,
            DagNode::Ref(rule_id) => IrNode::Ref(*rule_id),
            DagNode::Seq(children) => {
                let extracted = children.iter().map(|&id| self.extract(id)).collect();
                IrNode::Seq(extracted)
            }
            DagNode::Alt(children, dispatch) => {
                let branches = children
                    .iter()
                    .map(|&id| AltBranch {
                        node: self.extract(id),
                        first_set: None,
                    })
                    .collect();
                IrNode::Alt(branches, dispatch.clone())
            }
            DagNode::Repeat { inner, lo, hi } => IrNode::Repeat {
                inner: Box::new(self.extract(*inner)),
                lo: *lo,
                hi: *hi,
            },
            DagNode::Skip(a, b) => {
                IrNode::Skip(Box::new(self.extract(*a)), Box::new(self.extract(*b)))
            }
            DagNode::Next(a, b) => {
                IrNode::Next(Box::new(self.extract(*a)), Box::new(self.extract(*b)))
            }
            DagNode::Minus(a, b) => {
                IrNode::Minus(Box::new(self.extract(*a)), Box::new(self.extract(*b)))
            }
            DagNode::Negate(inner) => IrNode::Negate(Box::new(self.extract(*inner))),
            DagNode::Map { inner, fn_id } => IrNode::Map {
                inner: Box::new(self.extract(*inner)),
                fn_id: *fn_id,
            },
            DagNode::OptionalWhitespace(inner) => {
                IrNode::OptionalWhitespace(Box::new(self.extract(*inner)))
            }
            DagNode::TokenDispatch {
                token,
                arms,
                fallback,
            } => {
                let ir_arms = arms
                    .iter()
                    .map(|arm| TokenDispatchArm {
                        patterns: arm.patterns.clone(),
                        guard_byte: arm.guard_byte,
                        continuation: self.extract(arm.continuation),
                        map_fn: arm.map_fn,
                    })
                    .collect();
                IrNode::TokenDispatch {
                    token: Box::new(self.extract(*token)),
                    arms: ir_arms,
                    fallback: Box::new(self.extract(*fallback)),
                }
            }
        }
    }
}
