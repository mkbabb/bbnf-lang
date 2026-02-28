//! Tarjan's SCC algorithm and topological sorting.

use std::collections::{HashMap, HashSet};

use indexmap::IndexMap;

use crate::types::{Expression, AST};
use super::deps::Dependencies;

/// Result of Tarjan's strongly-connected-component analysis.
#[derive(Debug)]
pub struct SccResult<'a> {
    /// SCCs in reverse-topological order (leaf SCCs first).
    pub sccs: Vec<Vec<&'a Expression<'a>>>,
    /// Set of rules that participate in a cycle.
    pub cyclic_rules: HashSet<Expression<'a>>,
    /// Maps each expression to the index of its SCC in `sccs`.
    pub scc_index: HashMap<&'a Expression<'a>, usize>,
}

/// Tarjan's SCC algorithm over the dependency graph.
pub fn tarjan_scc<'a>(deps: &'a Dependencies<'a>) -> SccResult<'a> {
    struct State<'a> {
        index_counter: usize,
        stack: Vec<&'a Expression<'a>>,
        on_stack: HashSet<&'a Expression<'a>>,
        indices: HashMap<&'a Expression<'a>, usize>,
        lowlinks: HashMap<&'a Expression<'a>, usize>,
        sccs: Vec<Vec<&'a Expression<'a>>>,
        deps: &'a Dependencies<'a>,
    }

    fn strongconnect<'a>(v: &'a Expression<'a>, state: &mut State<'a>) {
        state.indices.insert(v, state.index_counter);
        state.lowlinks.insert(v, state.index_counter);
        state.index_counter += 1;
        state.stack.push(v);
        state.on_stack.insert(v);

        if let Some(successors) = state.deps.get(v) {
            for w in successors {
                let w_key = match state.deps.get_key_value(w) {
                    Some((k, _)) => k,
                    None => continue,
                };

                if !state.indices.contains_key(w_key) {
                    strongconnect(w_key, state);
                    let low_w = state.lowlinks[w_key];
                    let low_v = state.lowlinks[&v];
                    if low_w < low_v {
                        state.lowlinks.insert(v, low_w);
                    }
                } else if state.on_stack.contains(w_key) {
                    let idx_w = state.indices[w_key];
                    let low_v = state.lowlinks[&v];
                    if idx_w < low_v {
                        state.lowlinks.insert(v, idx_w);
                    }
                }
            }
        }

        if state.lowlinks[&v] == state.indices[&v] {
            let mut scc = Vec::new();
            loop {
                let w = state.stack.pop().unwrap();
                state.on_stack.remove(w);
                scc.push(w);
                if std::ptr::eq(w, v) {
                    break;
                }
            }
            state.sccs.push(scc);
        }
    }

    let mut state = State {
        index_counter: 0,
        stack: Vec::new(),
        on_stack: HashSet::new(),
        indices: HashMap::new(),
        lowlinks: HashMap::new(),
        sccs: Vec::new(),
        deps,
    };

    for v in deps.keys() {
        if !state.indices.contains_key(v) {
            strongconnect(v, &mut state);
        }
    }

    let mut scc_index = HashMap::new();
    let mut cyclic_rules = HashSet::new();

    for (i, scc) in state.sccs.iter().enumerate() {
        for &expr in scc {
            scc_index.insert(expr, i);
        }

        if scc.len() > 1 {
            for &expr in scc {
                cyclic_rules.insert(expr.clone());
            }
        } else {
            let expr = scc[0];
            if let Some(successors) = deps.get(expr) {
                if successors.contains(expr) {
                    cyclic_rules.insert(expr.clone());
                }
            }
        }
    }

    SccResult {
        sccs: state.sccs,
        cyclic_rules,
        scc_index,
    }
}

/// Reorder the AST in topological order using the SCC condensation DAG.
pub fn topological_sort_scc<'a>(
    ast: &AST<'a>,
    scc_result: &SccResult<'a>,
    deps: &Dependencies<'a>,
) -> AST<'a> {
    let num_sccs = scc_result.sccs.len();
    if num_sccs == 0 {
        return ast.clone();
    }

    let mut in_degree = vec![0u32; num_sccs];
    let mut scc_dependents: Vec<HashSet<usize>> = vec![HashSet::new(); num_sccs];

    for (node, successors) in deps {
        if let Some(&src_scc) = scc_result.scc_index.get(node) {
            for succ in successors {
                if let Some(&dst_scc) = scc_result.scc_index.get(succ) {
                    if src_scc != dst_scc && scc_dependents[dst_scc].insert(src_scc) {
                        in_degree[src_scc] += 1;
                    }
                }
            }
        }
    }

    let mut queue: std::collections::VecDeque<usize> = (0..num_sccs)
        .filter(|&i| in_degree[i] == 0)
        .collect();

    let mut topo_order: Vec<usize> = Vec::with_capacity(num_sccs);
    while let Some(scc_idx) = queue.pop_front() {
        topo_order.push(scc_idx);
        for &dependent in &scc_dependents[scc_idx] {
            in_degree[dependent] -= 1;
            if in_degree[dependent] == 0 {
                queue.push_back(dependent);
            }
        }
    }

    let depth_score: HashMap<&Expression<'a>, usize> = deps
        .iter()
        .map(|(expr, sub_deps)| {
            let score: usize = sub_deps
                .iter()
                .map(|d| deps.get(d).map_or(0, |dd| dd.len()))
                .sum();
            (expr, score)
        })
        .collect();

    let mut new_ast = IndexMap::with_capacity(ast.len());
    for &scc_idx in &topo_order {
        let mut scc_entries: Vec<_> = ast
            .iter()
            .filter(|(key, _)| {
                scc_result.scc_index.get(*key) == Some(&scc_idx)
                    && !new_ast.contains_key(*key)
            })
            .collect();
        scc_entries.sort_by_key(|(key, _)| depth_score.get(*key).copied().unwrap_or(0));

        for (key, val) in scc_entries {
            new_ast.insert(key.clone(), val.clone());
        }
    }

    for (lhs, rhs) in ast {
        if !new_ast.contains_key(lhs) {
            new_ast.insert(lhs.clone(), rhs.clone());
        }
    }

    new_ast
}

/// Compute acyclic dependencies using SCC data.
pub fn calculate_acyclic_deps_scc<'a>(
    deps: &Dependencies<'a>,
    _scc_result: &SccResult<'a>,
) -> Dependencies<'a> {
    fn is_acyclic_dfs<'a>(
        expr: &'a Expression<'a>,
        deps: &'a Dependencies<'a>,
        visited: &mut HashSet<&'a Expression<'a>>,
    ) -> bool {
        if visited.contains(expr) {
            return false;
        }
        visited.insert(expr);
        if let Some(sub_deps) = deps.get(expr) {
            for sub in sub_deps {
                let sub_canonical = match deps.get_key_value(sub) {
                    Some((k, _)) => k,
                    None => continue,
                };
                if !is_acyclic_dfs(sub_canonical, deps, visited) {
                    return false;
                }
            }
        }
        true
    }

    deps.iter()
        .filter(|(name, _)| {
            let mut visited = HashSet::new();
            is_acyclic_dfs(name, deps, &mut visited)
        })
        .map(|(name, sub_deps)| (name.clone(), sub_deps.clone()))
        .collect()
}

/// Non-acyclic deps: everything not in the acyclic set.
pub fn calculate_non_acyclic_deps_scc<'a>(
    deps: &Dependencies<'a>,
    acyclic_deps: &Dependencies<'a>,
) -> Dependencies<'a> {
    deps.iter()
        .filter(|(lhs, _)| !acyclic_deps.contains_key(*lhs))
        .map(|(lhs, deps)| (lhs.clone(), deps.clone()))
        .collect()
}
