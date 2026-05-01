//! GAC All-Different-Except propagator (sentinel-aware Régin 1994).
//!
//! Parallel to [`gac_alldiff`](super::gac_alldiff) but tolerates a designated
//! *sentinel* value that any number of variables may share. Non-sentinel
//! assignments must still be pairwise distinct.
//!
//! The algorithm modifies the standard Régin bipartite-matching approach in one
//! critical way: the sentinel value is excluded from the value side of the
//! bipartite graph entirely, and variables whose domain has been narrowed to
//! *only* the sentinel are excluded from the variable side. Variables that
//! contain both sentinel and non-sentinel values participate using only their
//! non-sentinel values — the sentinel serves as an implicit "escape valve" that
//! can always be taken if no non-sentinel matching edge is available.
//!
//! # Complexity
//!
//! Hopcroft-Karp maximum bipartite matching runs in O(E√V) and Tarjan's SCC
//! decomposition in O(V + E), giving an overall O(n√n) propagation step for n
//! variables — versus the O(n) singleton pruning used for small scopes.  The
//! scope ≥ 4 threshold amortizes the higher constant factor: below 4 unassigned
//! non-sentinel-only variables, singleton removal captures all pruning
//! opportunities that GAC would find.
//!
//! # Example: GAC provides stronger pruning than singleton removal
//!
//! ```
//! use csp_solver::constraint::{Revision, VarId};
//! use csp_solver::domain::BitsetDomain;
//! use csp_solver::solver::gac_alldiff_except::propagate_gac_alldiff_except;
//! use csp_solver::variable::Variable;
//!
//! // 5 variables, domain {0, 1, 2, 3, 4}, sentinel = 0.
//! // Var 0 is assigned to 1 (non-sentinel), var 1 is assigned to 2.
//! // Singleton pruning removes 1 and 2 from peers but cannot detect
//! // that vars 2-4 now form a Hall set over {3, 4} with the sentinel
//! // as escape. GAC detects that value 3 and 4 must be consumed by
//! // exactly two of vars 2-4, so the third MUST take the sentinel.
//! let scope: Vec<VarId> = (0..5).collect();
//! let mut vars: Vec<Variable<BitsetDomain>> = vec![
//!     Variable::new(BitsetDomain::new([1])),       // assigned 1
//!     Variable::new(BitsetDomain::new([2])),       // assigned 2
//!     Variable::new(BitsetDomain::new([0, 3, 4])), // sentinel + {3, 4}
//!     Variable::new(BitsetDomain::new([0, 3, 4])), // sentinel + {3, 4}
//!     Variable::new(BitsetDomain::new([0, 3, 4])), // sentinel + {3, 4}
//! ];
//! let rev = propagate_gac_alldiff_except(&scope, &0u32, &mut vars, 0);
//! // GAC cannot prune further here (each non-sentinel value 3 and 4
//! // appears in an SCC with variables that can also take sentinel),
//! // but it would detect UNSAT if a fourth var needed {3, 4} without
//! // sentinel access.
//! ```

use crate::constraint::traits::{Revision, VarId};
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
// GAC All-Different-Except propagation
// ---------------------------------------------------------------------------

/// Run GAC All-Different-Except propagation on a group of variables.
///
/// Implements Régin's 1994 algorithm adapted for a sentinel value:
/// 1. Exclude the sentinel from the value side of the bipartite graph
/// 2. Exclude variables whose domain is `{sentinel}` only
/// 3. Include variables with sentinel + other values using only non-sentinel values
/// 4. Find maximum matching via Hopcroft-Karp
/// 5. Build residual graph and find SCCs via iterative Tarjan
/// 6. Prune non-sentinel values not in any maximum matching and not in the same SCC
///
/// Variables whose domain contains the sentinel are never forced to take a
/// non-sentinel value — the sentinel is their escape valve. Only variables
/// whose domain contains exclusively non-sentinel values *must* be matched.
///
/// Only requires `D::Value: PartialEq` (implied by `Domain`). Values are mapped
/// to contiguous indices via deduplication, avoiding any `Ord` or `ValueIndex` bound.
pub fn propagate_gac_alldiff_except<D: Domain>(
    scope: &[VarId],
    sentinel: &D::Value,
    variables: &mut [Variable<D>],
    depth: usize,
) -> Revision
where
    D::Value: PartialEq + Clone + std::fmt::Debug,
{
    // Collect non-sentinel singleton values (assigned variables). These values
    // are committed and must be excluded from unassigned variables' available
    // value sets. Sentinel singletons are ignored — they don't constrain peers.
    let assigned_non_sentinel: Vec<D::Value> = scope
        .iter()
        .filter_map(|&v| {
            let dom = &variables[v as usize].domain;
            if dom.is_empty() {
                return None;
            }
            dom.singleton_value()
        })
        .filter(|val| *val != *sentinel)
        .collect();

    // Collect unassigned variables that have at least one non-sentinel value.
    // "Unassigned" means domain size > 1 and domain is not empty.
    // Sentinel-only domains ({sentinel}) are skipped — committed to sentinel.
    // Track whether each participant has sentinel in its domain (escape valve).
    let mut participants: Vec<usize> = Vec::new(); // indices into scope
    let mut has_sentinel: Vec<bool> = Vec::new();  // per participant

    for (i, &v) in scope.iter().enumerate() {
        let dom = &variables[v as usize].domain;
        if dom.is_empty() {
            return Revision::Unsatisfiable;
        }
        if dom.is_singleton() {
            // Assigned — already handled via assigned_non_sentinel exclusion.
            continue;
        }
        let dom_has_sentinel = dom.contains(sentinel);
        let non_sentinel_count = if dom_has_sentinel { dom.size() - 1 } else { dom.size() };
        if non_sentinel_count == 0 {
            // Domain is {sentinel} only — skip.
            continue;
        }
        participants.push(i);
        has_sentinel.push(dom_has_sentinel);
    }

    // If no unassigned variables have non-sentinel values, nothing to prune.
    if participants.is_empty() {
        return Revision::Unchanged;
    }

    let n_vars = participants.len();

    // Gather non-sentinel available values per participant, excluding values
    // already assigned to singletons. Simultaneously build deduplicated value list.
    let mut all_vals: Vec<D::Value> = Vec::new();
    let mut var_avail_raw: Vec<Vec<D::Value>> = Vec::with_capacity(n_vars);

    for &pi in &participants {
        let var_id = scope[pi] as usize;
        let mut avail: Vec<D::Value> = Vec::new();
        for v in variables[var_id].domain.iter() {
            // Skip sentinel — it's the escape valve, not part of the matching.
            if v == *sentinel {
                continue;
            }
            // Skip values already committed by assigned singletons.
            if assigned_non_sentinel.contains(&v) {
                continue;
            }
            if !all_vals.contains(&v) {
                all_vals.push(v.clone());
            }
            avail.push(v);
        }
        var_avail_raw.push(avail);
    }

    // If all non-sentinel values have been consumed by singletons, check
    // whether remaining participants all have sentinel escape.
    if all_vals.is_empty() {
        for (idx, avail) in var_avail_raw.iter().enumerate() {
            if avail.is_empty() && !has_sentinel[idx] {
                return Revision::Unsatisfiable;
            }
        }
        // All remaining participants can take sentinel — prune their
        // non-sentinel values (which are all assigned-singleton values).
        let mut changed = false;
        for (idx, _) in var_avail_raw.iter().enumerate() {
            let var_id = scope[participants[idx]] as usize;
            for val in &assigned_non_sentinel {
                if variables[var_id].prune(val, depth) {
                    changed = true;
                }
            }
            if variables[var_id].domain.is_empty() {
                return Revision::Unsatisfiable;
            }
        }
        return if changed { Revision::Changed } else { Revision::Unchanged };
    }

    let n_vals = all_vals.len();

    // Build value -> contiguous index mapping via linear scan.
    let val_to_idx = |v: &D::Value| -> u32 {
        all_vals.iter().position(|x| x == v).unwrap() as u32
    };

    // Build bipartite adjacency: u-node (var idx) -> list of v-node (val idx).
    let adj: Vec<Vec<u32>> = var_avail_raw
        .iter()
        .map(|avail| avail.iter().map(&val_to_idx).collect())
        .collect();

    // Maximum matching.
    let (match_u, match_v) = hopcroft_karp(n_vars, n_vals, &adj);

    // Check coverage: variables WITHOUT sentinel must be matched.
    // Variables WITH sentinel can survive unmatched (they take sentinel).
    for (idx, &mu) in match_u.iter().enumerate().take(n_vars) {
        if mu == NONE && !has_sentinel[idx] {
            // This variable has no sentinel escape and couldn't be matched.
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
    //
    // Additionally, for the sentinel-aware variant, unmatched var-nodes (those
    // that have sentinel as escape) are also free — they inject additional
    // reachability into the residual graph.
    let mut reachable = vec![false; total_nodes];
    {
        let mut bfs_queue: Vec<u32> = Vec::new();
        // Free val-nodes (unmatched values).
        for (vi, &mv) in match_v.iter().enumerate().take(n_vals) {
            if mv == NONE {
                let node = (n_vars + vi) as u32;
                reachable[node as usize] = true;
                bfs_queue.push(node);
            }
        }
        // Free var-nodes: unmatched variables that have sentinel escape.
        for (idx, &mu) in match_u.iter().enumerate().take(n_vars) {
            if mu == NONE && has_sentinel[idx] {
                let node = idx as u32;
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

    // Phase 1: prune assigned-singleton non-sentinel values from all
    // unassigned participants. These values were excluded from the matching
    // graph and must be removed from domains to maintain consistency.
    let mut changed = false;
    for u in 0..n_vars {
        let var_id = scope[participants[u]] as usize;
        for val in &assigned_non_sentinel {
            if variables[var_id].prune(val, depth) {
                changed = true;
            }
        }
        if variables[var_id].domain.is_empty() {
            return Revision::Unsatisfiable;
        }
    }

    // Phase 2: Régin SCC-based pruning. Remove (var, val) if the edge is
    // NOT in the matching AND var and val are in different SCCs AND the
    // val-node is NOT reachable from any free vertex.
    for u in 0..n_vars {
        let var_id = scope[participants[u]] as usize;
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
