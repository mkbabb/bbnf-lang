use std::collections::HashSet;

use csp_solver::{
    Csp,
    constraint::{ImplicationConstraint, VarId},
};

use super::super::domain::{AltMode, RegexEngine, StrategyDomain, StrategyValue};
use super::super::site::ByNodeVars;
use crate::IrNode;
use crate::passes::inspect::visit_children_alt;

/// Add cross-variable constraints encoding parent-child compatibility.
///
/// Currently a single rule:
/// - `Alt(node) = ByteDispatch` ⇒ each child `Engine(child_node)` must
///   be one of the one-pass-compatible variants
///   (`OnePass`, `FamilyHelper`, the memchr family, `NibbleLut`).
///
/// Encoded via [`ImplicationConstraint`] from csp-solver. The
/// constraint is no-op when the antecedent doesn't bind to the trigger
/// value, so it costs nothing for non-token-dispatch nodes.
///
/// Returns the count of constraints added so the caller can short-
/// circuit the finalize+solve when no cross-variable wiring exists.
pub(in crate::passes::csp_strategy) fn install(
    body: &IrNode,
    dag: &crate::dag::GrammarDag,
    by_node: &ByNodeVars,
    csp: &mut Csp<StrategyDomain>,
) -> usize {
    let one_pass_engines: Vec<StrategyValue> = vec![
        StrategyValue::Engine(RegexEngine::OnePass),
        StrategyValue::Engine(RegexEngine::FamilyHelper),
        StrategyValue::Engine(RegexEngine::Memchr1),
        StrategyValue::Engine(RegexEngine::Memchr2),
        StrategyValue::Engine(RegexEngine::Memchr3),
        StrategyValue::Engine(RegexEngine::NibbleLut),
    ];
    let mut count = 0usize;
    let mut installed = HashSet::new();
    walk_token_dispatch(
        body,
        dag,
        by_node,
        csp,
        &one_pass_engines,
        &mut installed,
        &mut count,
    );
    count
}

fn walk_token_dispatch(
    node: &IrNode,
    dag: &crate::dag::GrammarDag,
    by_node: &ByNodeVars,
    csp: &mut Csp<StrategyDomain>,
    one_pass_engines: &[StrategyValue],
    installed: &mut HashSet<(VarId, VarId)>,
    count: &mut usize,
) {
    if let IrNode::Alt(branches, _) = node {
        if let Some(parent_id) = dag.node_for(node) {
            if let Some((Some(alt_var), _, _)) = by_node.get(&parent_id) {
                // For each child Regex within the Alt branches, emit
                // "Alt = ByteDispatch → child Engine ∈ one_pass". The
                // implication models the constraint that a dispatch-
                // style Alt requires one-pass-eligible child regex
                // engines; Y.3 folded TokenDispatch into ByteDispatch
                // so this is now the single trigger value.
                let mut child_engine_vars = Vec::new();
                for branch in branches {
                    collect_engine_vars_in(&branch.node, dag, by_node, &mut child_engine_vars);
                }
                for child_var in child_engine_vars {
                    if !installed.insert((*alt_var, child_var)) {
                        continue;
                    }
                    csp.add_constraint(ImplicationConstraint::new(
                        *alt_var,
                        child_var,
                        StrategyValue::Alt(AltMode::ByteDispatch),
                        one_pass_engines.to_vec(),
                    ));
                    *count += 1;
                }
            }
        }
    }
    visit_children_alt(node, |child| {
        walk_token_dispatch(child, dag, by_node, csp, one_pass_engines, installed, count)
    });
}

fn collect_engine_vars_in(
    node: &IrNode,
    dag: &crate::dag::GrammarDag,
    by_node: &ByNodeVars,
    out: &mut Vec<VarId>,
) {
    if let Some(nid) = dag.node_for(node) {
        if let Some((_, _, Some(engine_var))) = by_node.get(&nid) {
            out.push(*engine_var);
        }
    }
    visit_children_alt(node, |child| {
        collect_engine_vars_in(child, dag, by_node, out)
    });
}
