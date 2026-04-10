//! GAC All-Different propagator (Régin 1994).
//!
//! Achieves generalized arc consistency on all-different constraints by:
//! 1. Building a bipartite value graph (variables -> domain values)
//! 2. Finding a maximum matching via Hopcroft-Karp
//! 3. Building a directed residual graph
//! 4. Finding SCCs via iterative Tarjan's algorithm
//! 5. Pruning values not in any maximum matching and not in the same SCC

use crate::constraint::{Revision, VarId};
use crate::domain::Domain;
use crate::variable::Variable;

const NONE: u32 = u32::MAX;

// ---------------------------------------------------------------------------
// Hopcroft-Karp maximum bipartite matching
// ---------------------------------------------------------------------------

/// Hopcroft-Karp maximum bipartite matching on integer-indexed bipartite graph.
///
/// u-nodes: `0..n_u`, v-nodes: `0..n_v`.
/// `adj[u]` lists the v-node neighbors of u-node `u`.
///
/// Returns `(match_u, match_v)` where `match_u[u]` is the matched v-node
/// (or `NONE`) and `match_v[v]` is the matched u-node (or `NONE`).
fn hopcroft_karp(
    n_u: usize,
    n_v: usize,
    adj: &[Vec<u32>],
) -> (Vec<u32>, Vec<u32>) {
    let mut match_u = vec![NONE; n_u];
    let mut match_v = vec![NONE; n_v];
    let mut dist = vec![0u32; n_u];

    let inf = (n_u + n_v + 1) as u32;

    loop {
        // BFS phase: build level graph from free u-nodes.
        let mut queue: Vec<u32> = Vec::new();
        for u in 0..n_u {
            if match_u[u] == NONE {
                dist[u] = 0;
                queue.push(u as u32);
            } else {
                dist[u] = inf;
            }
        }

        let mut found = false;
        let mut head = 0;
        while head < queue.len() {
            let u = queue[head] as usize;
            head += 1;
            for &v in &adj[u] {
                let mu = match_v[v as usize];
                if mu == NONE {
                    found = true;
                } else if dist[mu as usize] == inf {
                    dist[mu as usize] = dist[u] + 1;
                    queue.push(mu);
                }
            }
        }

        if !found {
            break;
        }

        // DFS phase: find augmenting paths using level constraints.
        for u in 0..n_u {
            if match_u[u] == NONE {
                dfs_augment(u, adj, &mut match_u, &mut match_v, &mut dist, inf);
            }
        }
    }

    (match_u, match_v)
}

/// DFS along the level graph to find an augmenting path from `u`.
fn dfs_augment(
    u: usize,
    adj: &[Vec<u32>],
    match_u: &mut [u32],
    match_v: &mut [u32],
    dist: &mut [u32],
    inf: u32,
) -> bool {
    for &v in &adj[u] {
        let mu = match_v[v as usize];
        if mu == NONE || (dist[mu as usize] == dist[u] + 1
            && dfs_augment(mu as usize, adj, match_u, match_v, dist, inf))
        {
            match_u[u] = v;
            match_v[v as usize] = u as u32;
            return true;
        }
    }
    dist[u] = inf;
    false
}

// ---------------------------------------------------------------------------
// Iterative Tarjan SCC
// ---------------------------------------------------------------------------

/// Iterative Tarjan's SCC algorithm.
///
/// Returns `scc_id[node]` — the SCC index each node belongs to.
fn tarjan_scc_iterative(n_nodes: usize, adj: &[Vec<u32>]) -> Vec<u32> {
    let mut index = vec![NONE; n_nodes];
    let mut lowlink = vec![0u32; n_nodes];
    let mut on_stack = vec![false; n_nodes];
    let mut scc_id = vec![NONE; n_nodes];

    let mut stack: Vec<u32> = Vec::new();
    // Call stack: (node, neighbor_index)
    let mut call_stack: Vec<(u32, u32)> = Vec::new();
    let mut counter: u32 = 0;
    let mut scc_counter: u32 = 0;

    for start in 0..n_nodes {
        if index[start] != NONE {
            continue;
        }

        let start = start as u32;
        index[start as usize] = counter;
        lowlink[start as usize] = counter;
        counter += 1;
        stack.push(start);
        on_stack[start as usize] = true;
        call_stack.push((start, 0));

        while let Some(&mut (v, ref mut ni)) = call_stack.last_mut() {
            let v_usize = v as usize;
            let neighbors = &adj[v_usize];

            if (*ni as usize) < neighbors.len() {
                let w = neighbors[*ni as usize];
                *ni += 1;
                let w_usize = w as usize;

                if index[w_usize] == NONE {
                    // Tree edge: push w.
                    index[w_usize] = counter;
                    lowlink[w_usize] = counter;
                    counter += 1;
                    stack.push(w);
                    on_stack[w_usize] = true;
                    call_stack.push((w, 0));
                } else if on_stack[w_usize] {
                    // Back edge: update lowlink.
                    if index[w_usize] < lowlink[v_usize] {
                        lowlink[v_usize] = index[w_usize];
                    }
                }
            } else {
                // Done with v's neighbors.
                if lowlink[v_usize] == index[v_usize] {
                    // v is root of an SCC — pop until we reach v.
                    loop {
                        let w = stack.pop().unwrap();
                        on_stack[w as usize] = false;
                        scc_id[w as usize] = scc_counter;
                        if w == v {
                            break;
                        }
                    }
                    scc_counter += 1;
                }

                let v_lowlink = lowlink[v_usize];
                call_stack.pop();

                // Propagate lowlink to parent.
                if let Some(&(parent, _)) = call_stack.last() {
                    let p = parent as usize;
                    if v_lowlink < lowlink[p] {
                        lowlink[p] = v_lowlink;
                    }
                }
            }
        }
    }

    scc_id
}

// ---------------------------------------------------------------------------
// GAC All-Different propagation
// ---------------------------------------------------------------------------

/// Run GAC All-Different propagation on a group of variables.
///
/// Implements the Régin 1994 algorithm:
/// 1. Build variable-value bipartite graph from unassigned variables' domains
/// 2. Find maximum matching via Hopcroft-Karp
/// 3. Build residual graph (matched edges: val->var, unmatched: var->val)
/// 4. Mark nodes reachable from free val-nodes (Régin's free-vertex rule)
/// 5. Find SCCs via iterative Tarjan
/// 6. Prune (var, val) pairs where var and val are in different SCCs,
///    the edge is not in the matching, and the val-node is not reachable
///    from a free vertex
///
/// Only requires `D::Value: PartialEq` (implied by `Domain`). Values are mapped
/// to contiguous indices via deduplication, avoiding any `Ord` or `ValueIndex` bound.
pub fn propagate_gac_alldiff<D: Domain>(
    scope: &[VarId],
    variables: &mut [Variable<D>],
    depth: usize,
) -> Revision {
    // Collect unassigned (non-singleton) variable indices within the scope.
    let unassigned: Vec<usize> = scope
        .iter()
        .enumerate()
        .filter(|&(_, &v)| {
            let dom = &variables[v as usize].domain;
            !dom.is_singleton() && !dom.is_empty()
        })
        .map(|(i, _)| i)
        .collect();

    let n_vars = unassigned.len();

    // Binary case or fewer: singleton removal (FC) handles it.
    if n_vars <= 2 {
        return Revision::Unchanged;
    }

    // Collect assigned (singleton) values to exclude from available domains.
    let assigned_vals: Vec<D::Value> = scope
        .iter()
        .filter_map(|&v| variables[v as usize].domain.singleton_value())
        .collect();

    // Gather available values per unassigned variable, excluding assigned ones.
    // Simultaneously collect all unique values into a flat list for index mapping.
    let mut all_vals: Vec<D::Value> = Vec::new();
    let mut var_avail_raw: Vec<Vec<D::Value>> = Vec::with_capacity(n_vars);

    for &ui in &unassigned {
        let var_id = scope[ui] as usize;
        let dom_vals = variables[var_id].domain.values();
        let mut avail: Vec<D::Value> = Vec::new();
        for v in dom_vals {
            if !assigned_vals.contains(&v) {
                // Deduplicate: only add to all_vals if not already present.
                if !all_vals.contains(&v) {
                    all_vals.push(v.clone());
                }
                avail.push(v);
            }
        }
        var_avail_raw.push(avail);
    }

    if all_vals.is_empty() {
        return Revision::Unsatisfiable;
    }

    let n_vals = all_vals.len();

    // Build value -> contiguous index mapping via linear scan.
    // For Sudoku-sized domains (9 values), this is faster than a HashMap.
    let val_to_idx = |v: &D::Value| -> u32 {
        all_vals.iter().position(|x| x == v).unwrap() as u32
    };

    // Build bipartite adjacency: u-node (var idx) -> list of v-node (val idx).
    let adj: Vec<Vec<u32>> = var_avail_raw
        .iter()
        .map(|avail| avail.iter().map(|v| val_to_idx(v)).collect())
        .collect();

    // Maximum matching.
    let (match_u, match_v) = hopcroft_karp(n_vars, n_vals, &adj);

    // Check that the matching covers all unassigned variables.
    for u in 0..n_vars {
        if match_u[u] == NONE {
            return Revision::Unsatisfiable;
        }
    }

    // Build directed residual graph for SCC analysis.
    // Nodes: 0..n_vars-1 are var-nodes, n_vars..n_vars+n_vals-1 are val-nodes.
    let total_nodes = n_vars + n_vals;
    let mut res_adj: Vec<Vec<u32>> = vec![Vec::new(); total_nodes];

    for u in 0..n_vars {
        let matched_vi = match_u[u];
        for &vi in &adj[u] {
            let val_node = (n_vars as u32) + vi;
            if vi == matched_vi {
                // Matched edge: val-node -> var-node (reversed).
                res_adj[val_node as usize].push(u as u32);
            } else {
                // Unmatched edge: var-node -> val-node (forward).
                res_adj[u].push(val_node);
            }
        }
    }

    // Mark all nodes reachable from free (unmatched) val-nodes via directed
    // paths in the residual graph. Per Régin 1994, an edge (var, val) that is
    // NOT in the current matching can still be in SOME maximum matching if it
    // lies on an alternating path from a free vertex. These edges must be kept.
    let mut reachable = vec![false; total_nodes];
    {
        let mut bfs_queue: Vec<u32> = Vec::new();
        for vi in 0..n_vals {
            if match_v[vi] == NONE {
                let node = (n_vars + vi) as u32;
                reachable[node as usize] = true;
                bfs_queue.push(node);
            }
        }
        let mut head = 0;
        while head < bfs_queue.len() {
            let node = bfs_queue[head];
            head += 1;
            for &next in &res_adj[node as usize] {
                if !reachable[next as usize] {
                    reachable[next as usize] = true;
                    bfs_queue.push(next);
                }
            }
        }
    }

    // Find SCCs.
    let scc_id = tarjan_scc_iterative(total_nodes, &res_adj);

    // Prune: remove (var, val) if the edge is NOT in the matching AND
    // var and val are in different SCCs AND the val-node is NOT reachable
    // from any free vertex.
    let mut changed = false;
    for u in 0..n_vars {
        let var_id = scope[unassigned[u]] as usize;
        let matched_vi = match_u[u];

        for &vi in &adj[u] {
            if vi == matched_vi {
                continue; // Always keep matched edges.
            }
            let val_node = n_vars + vi as usize;
            // Keep if same SCC or reachable from a free vertex.
            if scc_id[u] == scc_id[val_node] || reachable[val_node] {
                continue;
            }
            // Prune this (var, val) pair.
            let val = &all_vals[vi as usize];
            if variables[var_id].prune(val, depth) {
                changed = true;
            }
        }

        if variables[var_id].domain.is_empty() {
            return Revision::Unsatisfiable;
        }
    }

    if changed { Revision::Changed } else { Revision::Unchanged }
}

