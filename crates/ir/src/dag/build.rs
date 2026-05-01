//! Build a `GrammarDag` from a `GrammarIR` — bottom-up interning of every
//! sub-expression.

use super::GrammarDag;
use super::node::{DagNode, DagTokenDispatchArm, NodeId};
use crate::{GrammarIR, IrNode, IrRule};

/// Build a `GrammarDag` by interning every sub-expression in every rule.
pub fn build(ir: &GrammarIR) -> GrammarDag {
    let mut builder = GrammarDagBuilder::new();
    for rule in &ir.rules {
        let root = builder.intern_ir(&rule.body);
        builder.dag.set_rule_root(rule.id, root);
    }
    builder.finish()
}

/// Incremental builder for `GrammarDag`. Owned by `build::build` — exposed
/// so consumers can construct a DAG outside the IR pipeline if needed.
pub struct GrammarDagBuilder {
    dag: GrammarDag,
}

impl GrammarDagBuilder {
    /// Create an empty builder.
    pub fn new() -> Self {
        Self {
            dag: GrammarDag::empty(),
        }
    }

    /// Intern an `IrNode` (recursive) and return its `NodeId`.
    ///
    /// Also records the reverse mapping `*const IrNode → NodeId` so
    /// downstream consumers can look up the stable id for any visited
    /// tree position via [`GrammarDag::node_for`].
    pub fn intern_ir(&mut self, node: &IrNode) -> NodeId {
        let dag_node = self.project_ir(node);
        let id = self.dag.intern(dag_node);
        self.dag.record_ir_occurrence(node, id);
        id
    }

    /// Register a rule's root node (after interning its body).
    pub fn register_rule(&mut self, rule: &IrRule) -> NodeId {
        let root = self.intern_ir(&rule.body);
        self.dag.set_rule_root(rule.id, root);
        root
    }

    /// Consume the builder and return the finished DAG.
    pub fn finish(self) -> GrammarDag {
        self.dag
    }

    /// Project a single `IrNode` into its `DagNode` form, recursively
    /// interning children.
    fn project_ir(&mut self, node: &IrNode) -> DagNode {
        match node {
            IrNode::Literal(sid) => DagNode::Literal(*sid),
            IrNode::Regex(sid) => DagNode::Regex(*sid),
            IrNode::Epsilon => DagNode::Epsilon,
            IrNode::Ref(rule_id) => DagNode::Ref(*rule_id),
            IrNode::Seq(children) => {
                let ids = children.iter().map(|c| self.intern_ir(c)).collect();
                DagNode::Seq(ids)
            }
            IrNode::Alt(branches, dispatch) => {
                let ids = branches.iter().map(|b| self.intern_ir(&b.node)).collect();
                DagNode::Alt(ids, dispatch.clone())
            }
            IrNode::Repeat { inner, lo, hi } => {
                let inner_id = self.intern_ir(inner);
                DagNode::Repeat {
                    inner: inner_id,
                    lo: *lo,
                    hi: *hi,
                }
            }
            IrNode::Skip(a, b) => {
                let a_id = self.intern_ir(a);
                let b_id = self.intern_ir(b);
                DagNode::Skip(a_id, b_id)
            }
            IrNode::Next(a, b) => {
                let a_id = self.intern_ir(a);
                let b_id = self.intern_ir(b);
                DagNode::Next(a_id, b_id)
            }
            IrNode::Minus(a, b) => {
                let a_id = self.intern_ir(a);
                let b_id = self.intern_ir(b);
                DagNode::Minus(a_id, b_id)
            }
            IrNode::Negate(inner) => {
                let inner_id = self.intern_ir(inner);
                DagNode::Negate(inner_id)
            }
            IrNode::Map { inner, fn_id } => {
                let inner_id = self.intern_ir(inner);
                DagNode::Map {
                    inner: inner_id,
                    fn_id: *fn_id,
                }
            }
            IrNode::OptionalWhitespace(inner) => {
                let inner_id = self.intern_ir(inner);
                DagNode::OptionalWhitespace(inner_id)
            }
            IrNode::TokenDispatch {
                token,
                arms,
                fallback,
            } => {
                let token_id = self.intern_ir(token);
                let fallback_id = self.intern_ir(fallback);
                let dag_arms = arms
                    .iter()
                    .map(|arm| {
                        let cont_id = self.intern_ir(&arm.continuation);
                        DagTokenDispatchArm {
                            patterns: arm.patterns.clone(),
                            guard_byte: arm.guard_byte,
                            continuation: cont_id,
                            map_fn: arm.map_fn,
                        }
                    })
                    .collect();
                DagNode::TokenDispatch {
                    token: token_id,
                    arms: dag_arms,
                    fallback: fallback_id,
                }
            }
        }
    }
}

impl Default for GrammarDagBuilder {
    fn default() -> Self {
        Self::new()
    }
}
