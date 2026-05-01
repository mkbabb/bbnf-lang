//! Hash-consing: allocate a `NodeId` for a `DagNode`, reusing an existing
//! ID if a structurally-equal node is already in the arena.

use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

use super::GrammarDag;
use super::node::{DagNode, NodeId};

impl GrammarDag {
    /// Intern `node`, returning the `NodeId` of its (possibly existing) entry.
    pub(super) fn intern(&mut self, node: DagNode) -> NodeId {
        let hash = hash_node(&node);
        if let Some(candidates) = self.intern_map.get(&hash) {
            for &id in candidates {
                if self.nodes[id.as_usize()] == node {
                    return id;
                }
            }
        }
        let id = NodeId::from_usize(self.nodes.len());
        self.nodes.push(node);
        self.intern_map.entry(hash).or_default().push(id);
        id
    }

    /// Construct an empty DAG.
    pub(super) fn empty() -> Self {
        Self {
            nodes: Vec::new(),
            intern_map: Default::default(),
            rule_roots: Default::default(),
            ir_node_to_id: Default::default(),
        }
    }

    /// Set the root `NodeId` for a rule.
    pub(super) fn set_rule_root(&mut self, rule_id: crate::RuleId, node_id: NodeId) {
        self.rule_roots.insert(rule_id, node_id);
    }

    /// Record that a specific `IrNode` tree position maps to a `NodeId`.
    /// Called by the builder for every visited tree occurrence.
    pub(super) fn record_ir_occurrence(&mut self, node: &crate::IrNode, id: NodeId) {
        self.ir_node_to_id
            .insert(node as *const crate::IrNode as usize, id);
    }
}

fn hash_node(node: &DagNode) -> u64 {
    let mut hasher = DefaultHasher::new();
    node.hash(&mut hasher);
    hasher.finish()
}
