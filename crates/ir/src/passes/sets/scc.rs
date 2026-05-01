//! Strongly-connected component analysis via Tarjan's algorithm.
//!
//! Computes cycle membership for all rules. Populates `RuleMeta.is_cyclic`,
//! `RuleMeta.scc_id`, and `RuleMeta.memo` (cyclic rules get `MemoStrategy::Full`).

use crate::{GrammarIR, MemoStrategy, RuleId};

use super::deps::compute_rule_deps;

/// Compute SCCs and populate cycle metadata on all rules.
pub fn compute_scc(ir: &mut GrammarIR) {
    let deps = compute_rule_deps(ir);
    let sccs = tarjan(&deps);

    for (scc_idx, scc) in sccs.iter().enumerate() {
        let is_cyclic = scc.len() > 1 || deps[scc[0] as usize].contains(&scc[0]);
        for &rule_id in scc {
            let meta = &mut ir.rules[rule_id as usize].meta;
            meta.is_cyclic = is_cyclic;
            meta.scc_id = if is_cyclic {
                Some(scc_idx as u32)
            } else {
                None
            };
            meta.memo = if is_cyclic {
                MemoStrategy::Full
            } else {
                MemoStrategy::None
            };
        }
    }
}

/// Tarjan's SCC algorithm on a RuleId adjacency list.
///
/// Returns SCCs in reverse-topological order (leaf SCCs first).
fn tarjan(deps: &[Vec<RuleId>]) -> Vec<Vec<RuleId>> {
    let n = deps.len();
    let mut index_counter: u32 = 0;
    let mut stack: Vec<RuleId> = Vec::new();
    let mut on_stack = vec![false; n];
    let mut index = vec![u32::MAX; n]; // u32::MAX = undefined
    let mut lowlink = vec![0u32; n];
    let mut sccs: Vec<Vec<RuleId>> = Vec::new();

    for v in 0..n {
        if index[v] == u32::MAX {
            strongconnect(
                v as RuleId,
                deps,
                &mut index_counter,
                &mut stack,
                &mut on_stack,
                &mut index,
                &mut lowlink,
                &mut sccs,
            );
        }
    }

    sccs
}

fn strongconnect(
    v: RuleId,
    deps: &[Vec<RuleId>],
    index_counter: &mut u32,
    stack: &mut Vec<RuleId>,
    on_stack: &mut [bool],
    index: &mut [u32],
    lowlink: &mut [u32],
    sccs: &mut Vec<Vec<RuleId>>,
) {
    let vi = v as usize;
    index[vi] = *index_counter;
    lowlink[vi] = *index_counter;
    *index_counter += 1;
    stack.push(v);
    on_stack[vi] = true;

    for &w in &deps[vi] {
        let wi = w as usize;
        if wi >= deps.len() {
            continue; // dangling reference — skip
        }
        if index[wi] == u32::MAX {
            strongconnect(
                w,
                deps,
                index_counter,
                stack,
                on_stack,
                index,
                lowlink,
                sccs,
            );
            lowlink[vi] = lowlink[vi].min(lowlink[wi]);
        } else if on_stack[wi] {
            lowlink[vi] = lowlink[vi].min(index[wi]);
        }
    }

    if lowlink[vi] == index[vi] {
        let mut scc = Vec::new();
        loop {
            let w = stack.pop().unwrap();
            on_stack[w as usize] = false;
            scc.push(w);
            if w == v {
                break;
            }
        }
        sccs.push(scc);
    }
}
