//! Phase 1 of the dispatch pass: walk every `Alt` node, push each into a
//! CSP variable, propagate, and return a `NodeId → bool` eligibility table.
//!
//! The eligibility table lets the tree-walk annotator skip the full
//! `try_build_dispatch` call for Alts the CSP has already classified as
//! non-dispatchable, except for Alts with nullable branches whose FOLLOW
//! context could rescue dispatch.

use std::collections::HashMap;

use csp_solver::Csp;
use csp_solver::constraint::VarId;

use crate::dag::{GrammarDag, NodeId};
use crate::{CharSet128, GrammarIR, IrNode};

use super::constraint::DisjointConstraint;
use super::domain::{DispatchDecision, DispatchDomain};
use super::first_set::node_first_set;

/// Pre-computed dispatch eligibility for all Alt nodes, keyed on the
/// stable `NodeId` from `ir.dag`.
pub(super) type DispatchEligibility = HashMap<NodeId, bool>;

/// Collect all Alt nodes from the IR tree and determine dispatch eligibility
/// via CSP propagation.
///
/// For each Alt node with `>=2` and `<=127` branches, checks pairwise
/// FIRST set disjointness (without FOLLOW context). The result maps Alt
/// `NodeId` → bool. The tree walk uses this to skip redundant disjointness
/// checks for non-dispatchable Alts.
pub(super) fn precompute_dispatch_eligibility(
    ir: &GrammarIR,
    dag: &GrammarDag,
    rule_metas: &[(CharSet128, bool)],
    strings: &[String],
) -> DispatchEligibility {
    let mut alts: Vec<(NodeId, Vec<Option<CharSet128>>)> = Vec::new();

    // Phase 1: Collect every Alt node in the IR.
    for rule in &ir.rules {
        collect_alts(&rule.body, dag, rule_metas, strings, &mut alts);
        if let Some(ref recover) = rule.meta.directives.recover {
            collect_alts(recover, dag, rule_metas, strings, &mut alts);
        }
    }

    if alts.is_empty() {
        return HashMap::new();
    }

    // Phase 2: Build CSP — one variable per Alt, one constraint per Alt.
    let mut csp: Csp<DispatchDomain> = Csp::new();
    let mut var_ids: Vec<VarId> = Vec::with_capacity(alts.len());
    let mut alt_nids: Vec<NodeId> = Vec::with_capacity(alts.len());

    for (nid, branch_firsts) in &alts {
        let var = csp.add_variable(DispatchDomain::unknown());
        var_ids.push(var);
        alt_nids.push(*nid);

        let dispatchable = is_pairwise_disjoint(branch_firsts);
        csp.add_constraint(DisjointConstraint::new(var, dispatchable));
    }

    // Phase 3: Propagate.
    let _ = csp.propagate();

    // Phase 4: Extract results.
    let mut result = HashMap::with_capacity(alts.len());
    for (i, nid) in alt_nids.iter().enumerate() {
        let decision = &csp.variables[var_ids[i] as usize].domain.decision;
        let eligible = matches!(decision, DispatchDecision::Dispatchable);
        result.insert(*nid, eligible);
    }

    result
}

/// Check if a set of branch FIRST sets are pairwise disjoint.
fn is_pairwise_disjoint(branch_firsts: &[Option<CharSet128>]) -> bool {
    let sets: Vec<&CharSet128> = match branch_firsts
        .iter()
        .map(|f| f.as_ref())
        .collect::<Option<Vec<_>>>()
    {
        Some(v) => v,
        None => return false,
    };

    for i in 0..sets.len() {
        for j in (i + 1)..sets.len() {
            if !sets[i].is_disjoint(sets[j]) {
                return false;
            }
        }
    }
    true
}

/// Recursively collect Alt nodes and their branch FIRST sets.
fn collect_alts(
    node: &IrNode,
    dag: &GrammarDag,
    rule_metas: &[(CharSet128, bool)],
    strings: &[String],
    out: &mut Vec<(NodeId, Vec<Option<CharSet128>>)>,
) {
    match node {
        IrNode::Alt(branches, dispatch) => {
            for branch in branches {
                collect_alts(&branch.node, dag, rule_metas, strings, out);
            }

            if dispatch.is_none() && branches.len() >= 2 && branches.len() <= 127 {
                let nid = dag
                    .node_for(node)
                    .expect("every Alt visited by collect_alts must be in the DAG");
                let firsts: Vec<Option<CharSet128>> = branches
                    .iter()
                    .map(|b| {
                        b.first_set
                            .clone()
                            .or_else(|| node_first_set(&b.node, rule_metas, strings))
                    })
                    .collect();
                out.push((nid, firsts));
            }
        }
        IrNode::Seq(children) => {
            for child in children {
                collect_alts(child, dag, rule_metas, strings, out);
            }
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => {
            collect_alts(inner, dag, rule_metas, strings, out);
        }
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            collect_alts(a, dag, rule_metas, strings, out);
            collect_alts(b, dag, rule_metas, strings, out);
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            collect_alts(token, dag, rule_metas, strings, out);
            for arm in arms {
                collect_alts(&arm.continuation, dag, rule_metas, strings, out);
            }
            collect_alts(fallback, dag, rule_metas, strings, out);
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
    }
}
