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
    let mut tarjan = Tarjan {
        deps,
        index_counter: 0,
        stack: Vec::new(),
        on_stack: vec![false; n],
        index: vec![u32::MAX; n], // u32::MAX = undefined
        lowlink: vec![0u32; n],
        sccs: Vec::new(),
    };

    for v in 0..n {
        if tarjan.index[v] == u32::MAX {
            tarjan.strongconnect(v as RuleId);
        }
    }

    tarjan.sccs
}

/// The traversal state of one [`tarjan`] run.
struct Tarjan<'a> {
    deps: &'a [Vec<RuleId>],
    index_counter: u32,
    stack: Vec<RuleId>,
    on_stack: Vec<bool>,
    index: Vec<u32>,
    lowlink: Vec<u32>,
    sccs: Vec<Vec<RuleId>>,
}

impl Tarjan<'_> {
    fn strongconnect(&mut self, v: RuleId) {
        let vi = v as usize;
        self.index[vi] = self.index_counter;
        self.lowlink[vi] = self.index_counter;
        self.index_counter += 1;
        self.stack.push(v);
        self.on_stack[vi] = true;

        let deps = self.deps;
        for &w in &deps[vi] {
            let wi = w as usize;
            if wi >= deps.len() {
                continue; // dangling reference — skip
            }
            if self.index[wi] == u32::MAX {
                self.strongconnect(w);
                self.lowlink[vi] = self.lowlink[vi].min(self.lowlink[wi]);
            } else if self.on_stack[wi] {
                self.lowlink[vi] = self.lowlink[vi].min(self.index[wi]);
            }
        }

        if self.lowlink[vi] == self.index[vi] {
            let mut scc = Vec::new();
            loop {
                let w = self.stack.pop().unwrap();
                self.on_stack[w as usize] = false;
                scc.push(w);
                if w == v {
                    break;
                }
            }
            self.sccs.push(scc);
        }
    }
}
