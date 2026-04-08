//! Canonical Grammar DAG with hash-consed identity.
//!
//! The DAG sits alongside the existing `IrNode` tree. Each distinct
//! sub-expression is interned to a stable `NodeId(u32)`, so downstream
//! passes and tools can key maps on `NodeId` instead of raw pointers.
//!
//! This module is foundational for Phase 4 (grammar e-graph) and Phase 5
//! (`NodeFacts` stability). Consumers migrate incrementally — the existing
//! pointer-keyed maps continue to work unmodified during the transition.
//!
//! ```text
//!    IrNode tree            GrammarDag
//!    ───────────            ──────────
//!    Seq([A, B])            n0: Seq [n3, n4]
//!    Alt([A, C])            n1: Alt [n3, n5]
//!    │ shared:              n2: Literal("+")
//!    │   A = Literal("+")   n3: Literal("+")
//!    │                      ...
//!    ├──> intern ──>         (A reaches n3 once, shared)
//! ```

mod build;
mod extract;
mod intern;
mod node;

pub use build::GrammarDagBuilder;
pub use node::{DagNode, NodeId};

use std::collections::HashMap;

use crate::{GrammarIR, IrNode, RuleId};

/// The canonical grammar DAG — an arena of hash-consed nodes with per-rule
/// entry points.
#[derive(Clone, Debug)]
pub struct GrammarDag {
    /// Arena of distinct nodes. Indexed by `NodeId(u32) -> DagNode`.
    nodes: Vec<DagNode>,
    /// Hash-cons table: node structural hash → candidate `NodeId`s (collision list).
    /// Walked during insertion to find an existing equivalent node before
    /// allocating a new one.
    intern_map: HashMap<u64, Vec<NodeId>>,
    /// Per-rule entry points. `rule_roots[rule_id]` is the `NodeId` of the
    /// rule's body in the DAG.
    rule_roots: HashMap<RuleId, NodeId>,
    /// Reverse map: `*const IrNode` → `NodeId` for the source tree this DAG
    /// was built from. Populated during `from_ir` for every visited
    /// `IrNode`. Valid as long as the source IR tree is alive and
    /// unmodified. Downstream passes query via [`GrammarDag::node_for`]
    /// to obtain a stable `NodeId` for any sub-expression without
    /// re-walking the tree.
    ///
    /// Note: hash-consing means multiple distinct tree positions can
    /// collapse to the same `NodeId` (sharing). The reverse map records
    /// the `NodeId` for each *tree occurrence*, so repeated calls with
    /// structurally-equal but pointer-distinct nodes return the same id.
    ir_node_to_id: HashMap<usize, NodeId>,
}

impl GrammarDag {
    /// Build a DAG from a `GrammarIR`, interning every sub-expression.
    pub fn from_ir(ir: &GrammarIR) -> Self {
        build::build(ir)
    }

    /// Total number of distinct nodes in the DAG.
    pub fn node_count(&self) -> usize {
        self.nodes.len()
    }

    /// Borrow a node by its `NodeId`.
    pub fn node(&self, id: NodeId) -> &DagNode {
        &self.nodes[id.as_usize()]
    }

    /// Iterate over all nodes with their IDs.
    pub fn iter(&self) -> impl Iterator<Item = (NodeId, &DagNode)> {
        self.nodes
            .iter()
            .enumerate()
            .map(|(i, n)| (NodeId::from_usize(i), n))
    }

    /// The `NodeId` for a rule's body (the rule's entry point in the DAG).
    pub fn rule_root(&self, rule_id: RuleId) -> Option<NodeId> {
        self.rule_roots.get(&rule_id).copied()
    }

    /// Iterate over (rule_id, root_id) pairs.
    pub fn rule_roots(&self) -> impl Iterator<Item = (RuleId, NodeId)> + '_ {
        self.rule_roots.iter().map(|(&r, &n)| (r, n))
    }

    /// Look up the `NodeId` for an `IrNode` tree occurrence.
    ///
    /// Returns `Some(id)` if this exact tree position was visited during
    /// `from_ir` (identified by pointer address). Returns `None` for
    /// newly-constructed `IrNode`s that weren't present when the DAG
    /// was built.
    ///
    /// The caller must guarantee the source IR tree is still alive —
    /// the lookup uses the `IrNode`'s address, which is invalidated if
    /// the tree is mutated or freed. Any pipeline step that rewrites
    /// `ir.rules[*].body` must rebuild the DAG afterwards.
    #[inline]
    pub fn node_for(&self, node: &IrNode) -> Option<NodeId> {
        self.ir_node_to_id
            .get(&(node as *const IrNode as usize))
            .copied()
    }

    /// Structural sharing ratio: `nodes_in_tree / nodes_in_dag`. Higher
    /// values indicate the DAG deduplicated more sub-expressions.
    pub fn sharing_ratio(&self, ir: &GrammarIR) -> f64 {
        let tree_nodes = count_tree_nodes(ir);
        if self.nodes.is_empty() {
            1.0
        } else {
            tree_nodes as f64 / self.nodes.len() as f64
        }
    }
}

fn count_tree_nodes(ir: &GrammarIR) -> usize {
    let mut count = 0;
    for rule in &ir.rules {
        count += count_ir_node(&rule.body);
    }
    count
}

fn count_ir_node(node: &crate::IrNode) -> usize {
    use crate::IrNode;
    1 + match node {
        IrNode::Literal(_)
        | IrNode::Regex(_)
        | IrNode::Epsilon
        | IrNode::Ref(_) => 0,
        IrNode::Seq(children) => children.iter().map(count_ir_node).sum::<usize>(),
        IrNode::Alt(branches, _) => branches.iter().map(|b| count_ir_node(&b.node)).sum::<usize>(),
        IrNode::Repeat { inner, .. } => count_ir_node(inner),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            count_ir_node(a) + count_ir_node(b)
        }
        IrNode::Negate(inner) | IrNode::OptionalWhitespace(inner) => count_ir_node(inner),
        IrNode::Map { inner, .. } => count_ir_node(inner),
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            count_ir_node(token)
                + count_ir_node(fallback)
                + arms.iter().map(|a| count_ir_node(&a.continuation)).sum::<usize>()
        }
    }
}
