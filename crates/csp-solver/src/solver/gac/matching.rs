//! Integer-indexed graph primitives shared by the GAC core: warm-startable
//! Hopcroft-Karp bipartite matching and iterative Tarjan SCC. Value-agnostic —
//! every buffer is caller-owned scratch so the core can pool them across calls.

/// Sentinel for "no match" / "unvisited". `u32::MAX` never collides with a real
/// node index at the problem sizes GAC runs on.
pub(super) const NONE: u32 = u32::MAX;

/// Grow `adj` to at least `n` inner vectors and clear the first `n`, retaining
/// their heap capacity so repeated calls avoid per-inner reallocation.
pub(super) fn reset_adj(adj: &mut Vec<Vec<u32>>, n: usize) {
    while adj.len() < n {
        adj.push(Vec::new());
    }
    for a in adj.iter_mut().take(n) {
        a.clear();
    }
}

/// Hopcroft-Karp on an integer bipartite graph, extending whatever partial
/// matching `match_u`/`match_v` already encode (warm start). `dist`/`queue` are
/// caller scratch. On return the matching is maximum for `adj`.
pub(super) fn hopcroft_karp(
    n_u: usize,
    n_v: usize,
    adj: &[Vec<u32>],
    match_u: &mut [u32],
    match_v: &mut [u32],
    dist: &mut [u32],
    queue: &mut Vec<u32>,
) {
    let inf = (n_u + n_v + 1) as u32;

    loop {
        queue.clear();
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

        for u in 0..n_u {
            if match_u[u] == NONE {
                dfs_augment(u, adj, match_u, match_v, dist, inf);
            }
        }
    }
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
        if mu == NONE
            || (dist[mu as usize] == dist[u] + 1
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

/// Iterative Tarjan SCC over `adj`, writing `scc_id[node]`. All arrays are
/// caller-owned scratch sized to `n_nodes`; `stack`/`call_stack` are cleared.
/// `lowlink`/`on_stack` need only be sized to `n_nodes` (contents overwritten
/// before read).
#[allow(clippy::too_many_arguments)]
pub(super) fn tarjan_scc(
    n_nodes: usize,
    adj: &[Vec<u32>],
    index: &mut [u32],
    lowlink: &mut [u32],
    on_stack: &mut [bool],
    scc_id: &mut [u32],
    stack: &mut Vec<u32>,
    call_stack: &mut Vec<(u32, u32)>,
) {
    index[..n_nodes].fill(NONE);
    scc_id[..n_nodes].fill(NONE);
    stack.clear();
    call_stack.clear();
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
                    index[w_usize] = counter;
                    lowlink[w_usize] = counter;
                    counter += 1;
                    stack.push(w);
                    on_stack[w_usize] = true;
                    call_stack.push((w, 0));
                } else if on_stack[w_usize] && index[w_usize] < lowlink[v_usize] {
                    lowlink[v_usize] = index[w_usize];
                }
            } else {
                if lowlink[v_usize] == index[v_usize] {
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
                if let Some(&(parent, _)) = call_stack.last() {
                    let p = parent as usize;
                    if v_lowlink < lowlink[p] {
                        lowlink[p] = v_lowlink;
                    }
                }
            }
        }
    }
}
