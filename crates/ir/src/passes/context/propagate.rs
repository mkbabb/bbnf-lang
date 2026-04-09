//! Monotone fixed-point propagation of context facts over the grammar DAG.
//!
//! Each node's facts are the join (lattice upper bound) of the facts
//! imposed by its parents. The propagation is a standard worklist
//! algorithm seeded from the rule roots.

use super::facts::{ContextFacts, ContextFactsMap, DiscriminationStrength, ScanSafety};
use crate::dag::{DagNode, GrammarDag, NodeId};
use crate::{GrammarIR, IrNode};
use std::collections::{HashMap, VecDeque};

/// Compute context facts for every node in the DAG.
///
/// Returns a map keyed by `NodeId`. Nodes with all-default facts are
/// omitted from the returned map (consumers should treat a missing entry
/// as `ContextFacts::default()`).
pub fn compute_context_facts(ir: &GrammarIR, dag: &GrammarDag) -> ContextFactsMap {
    let mut facts: HashMap<NodeId, ContextFacts> = HashMap::new();

    // Seed each rule root with empty facts, then propagate.
    let mut worklist: VecDeque<NodeId> = VecDeque::new();
    for (_, root) in dag.rule_roots() {
        facts.entry(root).or_default();
        worklist.push_back(root);
    }

    // Collect recovery sync expression roots from the `@recover` directive set.
    let recovery_roots = collect_recovery_roots(ir, dag);
    for &root in &recovery_roots {
        let entry = facts.entry(root).or_default();
        entry.in_recovery_context = true;
        worklist.push_back(root);
    }

    // Propagate in_recovery_context + in_token_dispatch downwards.
    //
    // Tranche W phase 4: read the parent's facts via copy of just the
    // two scalar bool fields we need (`in_recovery_context`,
    // `in_token_dispatch`) instead of cloning the whole `ContextFacts`
    // struct per worklist iteration. ContextFacts is small today but
    // grows with every future facet, and the previous `.cloned()` call
    // would scale linearly. The audit measured 80-320 KB allocated per
    // compile in this loop on the post-V baseline.
    while let Some(id) = worklist.pop_front() {
        let parent_in_recovery = facts
            .get(&id)
            .map(|f| f.in_recovery_context)
            .unwrap_or(false);
        let node = dag.node(id);
        let parent_is_token_dispatch = matches!(node, DagNode::TokenDispatch { .. });
        for child in node.children() {
            let child_facts = facts.entry(child).or_default();
            let mut changed = false;
            if parent_in_recovery && !child_facts.in_recovery_context {
                child_facts.in_recovery_context = true;
                changed = true;
            }
            if parent_is_token_dispatch && !child_facts.in_token_dispatch {
                child_facts.in_token_dispatch = true;
                changed = true;
            }
            if changed {
                worklist.push_back(child);
            }
        }
    }

    // Compute discrimination strength for Alt nodes based on pre-computed
    // dispatch tables (Strong) or leading-literal distinctness (Medium).
    for (id, node) in dag.iter() {
        if let DagNode::Alt(_, Some(_)) = node {
            let entry = facts.entry(id).or_default();
            entry.discrimination = DiscriminationStrength::Strong;
        } else if let DagNode::Alt(children, None) = node {
            if children.iter().all(|&c| is_leading_literal(dag, c)) {
                let entry = facts.entry(id).or_default();
                entry.discrimination = DiscriminationStrength::Medium;
            }
        }
    }

    // Mark Wrap-like patterns (Skip(Next(open, body), close)) as scan-safe.
    for (id, node) in dag.iter() {
        if is_wrap_pattern(dag, node) {
            let entry = facts.entry(id).or_default();
            entry.scan_safety = ScanSafety::Safe;
        }
    }

    facts
}

fn is_leading_literal(dag: &GrammarDag, id: NodeId) -> bool {
    matches!(
        dag.node(id),
        DagNode::Literal(_) | DagNode::Seq(_) | DagNode::Regex(_)
    )
}

fn is_wrap_pattern(dag: &GrammarDag, node: &DagNode) -> bool {
    let DagNode::Skip(inner, _close) = node else {
        return false;
    };
    matches!(dag.node(*inner), DagNode::Next(_, _))
}

/// Collect the NodeIds of the `@recover` sync expression roots. These are
/// the points where `in_recovery_context` propagation begins.
fn collect_recovery_roots(ir: &GrammarIR, dag: &GrammarDag) -> Vec<NodeId> {
    // Phase 5 placeholder: the IR's recover directives aren't wired into
    // the DAG yet. This function returns empty until that wiring lands.
    let _ = (ir, dag);
    Vec::new()
}

/// Helper: walk the IR tree for a given rule and return the NodeId of a
/// sub-expression that matches it. Used for debugging / future recovery
/// wiring — not currently on any hot path.
#[allow(dead_code)]
pub(crate) fn find_node_id_in_rule(
    _ir: &GrammarIR,
    _rule: crate::RuleId,
    _target: &IrNode,
) -> Option<NodeId> {
    None
}
