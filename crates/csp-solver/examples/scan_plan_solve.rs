//! PROTO-2 probe: PBQP vs generic B&B on the near-tree scan-plan graph.
//!
//! Model (both solvers, identical feasible set):
//!   node   = scan site, domain {Exact=0, Prefilter=1}
//!   unary  = runtime cost (Exact=10, Prefilter=1) + per-site epsilon tie-break
//!   edge   = HARD coherence "sites sharing a dispatch table emit the same tier"
//!            (representable in BOTH: PBQP ∞ off-diagonal, B&B binary constraint)
//!   budget = Σ static_size_cost(tier) ≤ B   (Exact=100 LOC, Prefilter=300 LOC)
//!
//! Route A: PBQP R0/R1/R2 reductions + Lagrangian dual of the budget.
//! Route B: the crate's real B&B (solve_optimized / search.rs) with the budget
//!          as BoundedSumConstraint and coherence as a hard binary constraint.
//! Reference: brute force over all feasible tier tuples (optimality oracle).
//!
//! Run: cargo run -p csp-solver --example scan_plan_solve --release

use std::time::Instant;

use csp_solver::constraint::traits::{Constraint, VarId};
use csp_solver::constraint::BoundedSumConstraint;
use csp_solver::domain::CostFiniteDomain;
use csp_solver::{Csp, OptimizationMode, Pruning, SolveConfig};
use csp_solver::ordering::Ordering;

const EXACT: i32 = 0;
const PREFILTER: i32 = 1;
const RUNTIME: [f64; 2] = [10.0, 1.0]; // Exact slower, Prefilter faster (objective)
const LOC: [f64; 2] = [100.0, 300.0]; // Exact cheap code, Prefilter heavy code (budget)
const INF: f64 = 1.0e9;

/// A scan-plan instance: N sites + coherence edges + a size budget.
struct Instance {
    name: &'static str,
    n: usize,
    /// hard coherence edges (a,b): the two sites must emit the same tier.
    edges: Vec<(usize, usize)>,
    budget: f64,
}

/// Per-site unary cost with a deterministic, TIER-DEPENDENT epsilon tie-break
/// so the global optimum is UNIQUE (an identical-assignment claim is meaningless
/// under ties). The epsilon rides only on Exact so different Exact/Prefilter
/// partitions have distinct totals; it is < 0.02 total, far below the 9.0
/// runtime deltas, so it breaks ties without altering the coupling structure.
fn unary(site: usize, tier: usize) -> f64 {
    RUNTIME[tier] + if tier == EXACT as usize { (site as f64) * 1e-3 } else { 0.0 }
}

// ── Route A: PBQP ───────────────────────────────────────────────────────────

#[derive(Clone)]
struct Pbqp {
    n: usize,
    k: usize,
    unary: Vec<Vec<f64>>,
    /// edge[(i,j)] with i<j: k×k matrix indexed [val_i][val_j].
    edges: std::collections::BTreeMap<(usize, usize), Vec<Vec<f64>>>,
}

#[derive(Default)]
struct PbqpStats {
    r0: usize,
    r1: usize,
    r2: usize,
    rn: usize, // heuristic fallbacks (loses exactness)
    max_stack_depth: usize,
}

enum Removed {
    R0 { m: usize, u: Vec<f64> },
    R1 { m: usize, u: Vec<f64>, nbr: usize, mat: Vec<Vec<f64>> },
    R2 { m: usize, u: Vec<f64>, p: usize, q: usize, mp: Vec<Vec<f64>>, mq: Vec<Vec<f64>> },
    Rn { m: usize, val: usize },
}

impl Pbqp {
    fn new(n: usize, k: usize) -> Self {
        Pbqp { n, k, unary: vec![vec![0.0; k]; n], edges: Default::default() }
    }

    /// oriented matrix [val_a][val_b] for the edge between a and b.
    fn edge_oriented(&self, a: usize, b: usize) -> Option<Vec<Vec<f64>>> {
        if a < b {
            self.edges.get(&(a, b)).cloned()
        } else {
            self.edges.get(&(b, a)).map(|m| {
                // transpose
                let mut t = vec![vec![0.0; self.k]; self.k];
                for i in 0..self.k {
                    for j in 0..self.k {
                        t[i][j] = m[j][i];
                    }
                }
                t
            })
        }
    }

    fn neighbors(&self, m: usize, alive: &[bool]) -> Vec<usize> {
        let mut ns = Vec::new();
        for &(a, b) in self.edges.keys() {
            if a == m && alive[b] {
                ns.push(b);
            } else if b == m && alive[a] {
                ns.push(a);
            }
        }
        ns
    }

    fn add_edge(&mut self, a: usize, b: usize, mat: Vec<Vec<f64>>) {
        let (lo, hi, m) = if a < b {
            (a, b, mat)
        } else {
            // transpose so storage key stays (lo,hi) = [val_lo][val_hi]
            let mut t = vec![vec![0.0; self.k]; self.k];
            for i in 0..self.k {
                for j in 0..self.k {
                    t[i][j] = mat[j][i];
                }
            }
            (b, a, t)
        };
        self.edges
            .entry((lo, hi))
            .and_modify(|e| {
                for i in 0..self.k {
                    for j in 0..self.k {
                        e[i][j] += m[i][j];
                    }
                }
            })
            .or_insert(m);
    }

    fn remove_edges_of(&mut self, m: usize) {
        let keys: Vec<_> = self.edges.keys().cloned().filter(|&(a, b)| a == m || b == m).collect();
        for k in keys {
            self.edges.remove(&k);
        }
    }

    /// Solve via R0/R1/R2 reductions, RN heuristic only if forced.
    /// Returns (assignment, cost, stats).
    fn solve(mut self) -> (Vec<usize>, f64, PbqpStats) {
        let mut alive = vec![true; self.n];
        let mut stack: Vec<Removed> = Vec::new();
        let mut stats = PbqpStats::default();
        let mut remaining = self.n;

        while remaining > 0 {
            // find a node by lowest applicable reduction rule
            let mut did = false;
            // R0
            for m in 0..self.n {
                if alive[m] && self.neighbors(m, &alive).is_empty() {
                    let u = self.unary[m].clone();
                    stack.push(Removed::R0 { m, u });
                    alive[m] = false;
                    remaining -= 1;
                    stats.r0 += 1;
                    did = true;
                    break;
                }
            }
            if did {
                stats.max_stack_depth = stats.max_stack_depth.max(stack.len());
                continue;
            }
            // R1
            for m in 0..self.n {
                if !alive[m] {
                    continue;
                }
                let ns = self.neighbors(m, &alive);
                if ns.len() == 1 {
                    let nbr = ns[0];
                    let mat = self.edge_oriented(m, nbr).unwrap(); // [val_m][val_nbr]
                    let u = self.unary[m].clone();
                    // fold: unary[nbr][b] += min_a (u[a] + mat[a][b])
                    for b in 0..self.k {
                        let mut best = f64::INFINITY;
                        for a in 0..self.k {
                            best = best.min(u[a] + mat[a][b]);
                        }
                        self.unary[nbr][b] += best;
                    }
                    self.remove_edges_of(m);
                    stack.push(Removed::R1 { m, u, nbr, mat });
                    alive[m] = false;
                    remaining -= 1;
                    stats.r1 += 1;
                    did = true;
                    break;
                }
            }
            if did {
                stats.max_stack_depth = stats.max_stack_depth.max(stack.len());
                continue;
            }
            // R2
            for m in 0..self.n {
                if !alive[m] {
                    continue;
                }
                let ns = self.neighbors(m, &alive);
                if ns.len() == 2 {
                    let (p, q) = (ns[0], ns[1]);
                    let mp = self.edge_oriented(m, p).unwrap(); // [val_m][val_p]
                    let mq = self.edge_oriented(m, q).unwrap(); // [val_m][val_q]
                    let u = self.unary[m].clone();
                    // new edge N[bp][bq] = min_a (u[a] + mp[a][bp] + mq[a][bq])
                    let mut nmat = vec![vec![0.0; self.k]; self.k];
                    for bp in 0..self.k {
                        for bq in 0..self.k {
                            let mut best = f64::INFINITY;
                            for a in 0..self.k {
                                best = best.min(u[a] + mp[a][bp] + mq[a][bq]);
                            }
                            nmat[bp][bq] = best;
                        }
                    }
                    self.remove_edges_of(m);
                    self.add_edge(p, q, nmat); // p,q oriented; add_edge handles order
                    stack.push(Removed::R2 { m, u, p, q, mp, mq });
                    alive[m] = false;
                    remaining -= 1;
                    stats.r2 += 1;
                    did = true;
                    break;
                }
            }
            if did {
                stats.max_stack_depth = stats.max_stack_depth.max(stack.len());
                continue;
            }
            // RN heuristic fallback: every remaining node has degree >= 3.
            // Pick lowest-index alive node, greedily fix its locally-best value.
            let m = (0..self.n).find(|&i| alive[i]).unwrap();
            let ns = self.neighbors(m, &alive);
            let mut best_val = 0;
            let mut best_cost = f64::INFINITY;
            for a in 0..self.k {
                let mut c = self.unary[m][a];
                for &nb in &ns {
                    let mat = self.edge_oriented(m, nb).unwrap();
                    let mut lo = f64::INFINITY;
                    for b in 0..self.k {
                        lo = lo.min(mat[a][b]);
                    }
                    c += lo;
                }
                if c < best_cost {
                    best_cost = c;
                    best_val = a;
                }
            }
            for &nb in &ns {
                let mat = self.edge_oriented(m, nb).unwrap();
                for b in 0..self.k {
                    self.unary[nb][b] += mat[best_val][b];
                }
            }
            self.remove_edges_of(m);
            stack.push(Removed::Rn { m, val: best_val });
            alive[m] = false;
            remaining -= 1;
            stats.rn += 1;
            stats.max_stack_depth = stats.max_stack_depth.max(stack.len());
        }

        // back-substitution
        let mut sol = vec![0usize; self.n];
        while let Some(r) = stack.pop() {
            match r {
                Removed::R0 { m, u } => {
                    sol[m] = argmin(&u);
                }
                Removed::R1 { m, u, nbr, mat } => {
                    let bn = sol[nbr];
                    let mut best = 0;
                    let mut bc = f64::INFINITY;
                    for a in 0..self.k {
                        let c = u[a] + mat[a][bn];
                        if c < bc {
                            bc = c;
                            best = a;
                        }
                    }
                    sol[m] = best;
                }
                Removed::R2 { m, u, p, q, mp, mq } => {
                    let (bp, bq) = (sol[p], sol[q]);
                    let mut best = 0;
                    let mut bc = f64::INFINITY;
                    for a in 0..self.k {
                        let c = u[a] + mp[a][bp] + mq[a][bq];
                        if c < bc {
                            bc = c;
                            best = a;
                        }
                    }
                    sol[m] = best;
                }
                Removed::Rn { m, val } => {
                    sol[m] = val;
                }
            }
        }
        let cost = self.raw_cost(&sol);
        (sol, cost, stats)
    }

    fn raw_cost(&self, sol: &[usize]) -> f64 {
        // NB: uses the ORIGINAL unary/edges; call on a fresh clone.
        let mut c = 0.0;
        for i in 0..self.n {
            c += self.unary[i][sol[i]];
        }
        for (&(a, b), m) in &self.edges {
            c += m[sol[a]][sol[b]];
        }
        c
    }
}

fn argmin(v: &[f64]) -> usize {
    let mut best = 0;
    let mut bc = f64::INFINITY;
    for (i, &x) in v.iter().enumerate() {
        if x < bc {
            bc = x;
            best = i;
        }
    }
    best
}

/// Build a fresh PBQP for a Lagrangian multiplier lambda (budget relaxed into unary).
fn build_pbqp(inst: &Instance, lambda: f64) -> Pbqp {
    let mut p = Pbqp::new(inst.n, 2);
    for i in 0..inst.n {
        for t in 0..2 {
            p.unary[i][t] = unary(i, t) + lambda * LOC[t];
        }
    }
    for &(a, b) in &inst.edges {
        // hard coherence: 0 on diagonal, INF off-diagonal
        let mut mat = vec![vec![INF; 2]; 2];
        mat[0][0] = 0.0;
        mat[1][1] = 0.0;
        let (lo, hi) = if a < b { (a, b) } else { (b, a) };
        p.edges.insert((lo, hi), mat);
    }
    p
}

fn loc_of(inst: &Instance, sol: &[usize]) -> f64 {
    (0..inst.n).map(|i| LOC[sol[i]]).sum()
}

fn true_obj(inst: &Instance, sol: &[usize]) -> f64 {
    // runtime + coherence (coherence is 0 when feasible/all-satisfied)
    let mut c = 0.0;
    for i in 0..inst.n {
        c += unary(i, sol[i]);
    }
    for &(a, b) in &inst.edges {
        if sol[a] != sol[b] {
            c += INF;
        }
    }
    c
}

/// PBQP + Lagrangian dual: sweep lambda, collect budget-feasible assignments,
/// return the min true-objective one. Returns (assignment, obj, stats, sweeps).
fn solve_pbqp_lagrangian(inst: &Instance) -> (Vec<usize>, f64, PbqpStats, usize) {
    let mut best: Option<(Vec<usize>, f64)> = None;
    let mut last_stats = PbqpStats::default();
    let mut sweeps = 0;
    // lambda breakpoints: a coherence-component flips Prefilter->Exact when
    // lambda*(LOC[pre]-LOC[exact]) > runtime[exact]-runtime[pre] ≈ 9/200 ≈ 0.045.
    // The per-component breakpoints are eps-separated by ~5e-6, so the grid step
    // must be << that to trace EVERY relaxation breakpoint — otherwise a coarse
    // grid jumps the whole transition band and confounds grid-coarseness with the
    // Lagrangian integrality gap. 250k steps over [0,0.125] => step 5e-7.
    let steps = 250_000;
    for s in 0..=steps {
        let lambda = (s as f64) * 5e-7; // 0 .. 0.125
        let (sol, _relaxed, st) = build_pbqp(inst, lambda).solve();
        last_stats = st;
        sweeps += 1;
        if loc_of(inst, &sol) <= inst.budget + 1e-6 {
            let obj = true_obj(inst, &sol);
            if best.as_ref().map_or(true, |(_, bc)| obj < *bc) {
                best = Some((sol, obj));
            }
        }
    }
    let (sol, obj) = best.expect("no budget-feasible PBQP assignment found");
    (sol, obj, last_stats, sweeps)
}

// ── Route B: the crate's real B&B ────────────────────────────────────────────

/// Hard coherence constraint for B&B: two sites must emit the same tier.
#[derive(Debug)]
struct SameTier {
    scope: Vec<VarId>,
}
impl Constraint<CostFiniteDomain> for SameTier {
    fn scope(&self) -> &[VarId] {
        &self.scope
    }
    fn check(&self, assignment: &[Option<i32>]) -> bool {
        match (assignment[self.scope[0] as usize], assignment[self.scope[1] as usize]) {
            (Some(x), Some(y)) => x == y,
            _ => true, // partial: not yet violated
        }
    }
    // revise: use the trait's binary default (drives AC off check()).
}

fn bb_config() -> SolveConfig {
    SolveConfig {
        pruning: Pruning::Ac3,
        ordering: Ordering::FailFirst,
        max_solutions: 1,
        optimization_mode: OptimizationMode::MinimizeCost,
        node_budget: Some(1_000_000),
        cancel: None,
    }
}

fn solve_bb(inst: &Instance) -> (Option<Vec<usize>>, u64, u64) {
    let mut csp = Csp::new();
    let vars: Vec<_> = (0..inst.n)
        .map(|i| {
            csp.add_variable(CostFiniteDomain::new(
                vec![EXACT, PREFILTER],
                vec![unary(i, 0), unary(i, 1)],
            ))
        })
        .collect();
    // budget
    let loc_table = vec![(EXACT, LOC[0]), (PREFILTER, LOC[1])];
    csp.add_constraint(BoundedSumConstraint::new(
        vars.clone(),
        vec![loc_table; inst.n],
        inst.budget,
    ));
    // coherence
    for &(a, b) in &inst.edges {
        csp.add_constraint(SameTier { scope: vec![vars[a], vars[b]] });
    }
    csp.finalize();
    let sols = csp.solve_optimized(&bb_config());
    let st = csp.stats();
    let out = sols.first().map(|s| (0..inst.n).map(|i| s[i] as usize).collect());
    (out, st.nodes_explored, st.backtracks)
}

// ── Reference: brute force ────────────────────────────────────────────────────

fn brute_force(inst: &Instance) -> Option<(Vec<usize>, f64)> {
    let mut best: Option<(Vec<usize>, f64)> = None;
    for mask in 0u32..(1u32 << inst.n) {
        let sol: Vec<usize> = (0..inst.n).map(|i| ((mask >> i) & 1) as usize).collect();
        if loc_of(inst, &sol) > inst.budget + 1e-6 {
            continue;
        }
        if inst.edges.iter().any(|&(a, b)| sol[a] != sol[b]) {
            continue;
        }
        let obj: f64 = (0..inst.n).map(|i| unary(i, sol[i])).sum();
        if best.as_ref().map_or(true, |(_, bc)| obj < *bc) {
            best = Some((sol, obj));
        }
    }
    best
}

// ── Driver ───────────────────────────────────────────────────────────────────

fn run(inst: &Instance) {
    println!("\n=== instance: {} | n={} | edges={} | budget={} ===", inst.name, inst.n, inst.edges.len(), inst.budget);

    // reference
    let reference = brute_force(inst);

    // Route A: PBQP
    let t = Instant::now();
    let (a_sol, a_obj, a_stats, sweeps) = solve_pbqp_lagrangian(inst);
    let a_us = t.elapsed().as_secs_f64() * 1e6;

    // Route B: B&B
    let t = Instant::now();
    let (b_sol, b_nodes, b_backtracks) = solve_bb(inst);
    let b_us = t.elapsed().as_secs_f64() * 1e6;

    let ref_sol = reference.as_ref().map(|(s, _)| s.clone());
    println!("  reference (brute): {:?}", ref_sol);
    println!("  PBQP  assignment : {:?}  obj={:.3}  solve={:.1}us  (lambda sweeps={})", a_sol, a_obj, a_us, sweeps);
    println!("        reductions : R0={} R1={} R2={} RN={} (max_stack_depth={})", a_stats.r0, a_stats.r1, a_stats.r2, a_stats.rn, a_stats.max_stack_depth);
    println!("  B&B   assignment : {:?}  solve={:.1}us  nodes_explored={} backtracks={}", b_sol, b_us, b_nodes, b_backtracks);

    let agree_ab = Some(&a_sol) == b_sol.as_ref();
    let agree_ref = ref_sol.as_ref() == Some(&a_sol) && ref_sol == b_sol;
    println!("  IDENTICAL(PBQP==B&B)={}  MATCHES_REF={}  RN_fallbacks={}", agree_ab, agree_ref, a_stats.rn);
}

fn main() {
    // JSON: 3 real self-loop sites (string-scan, escape-scan, number-scan).
    // Coherence: string 0 — escape 1 share the char-class table (edge 0-1);
    //            number 2 shares the digit-class with string's 0 (edge 0-2).
    //            => node 0 has degree 2 (exercises R2).
    let json_slack = Instance { name: "json (slack budget)", n: 3, edges: vec![(0, 1), (0, 2)], budget: 900.0 };
    let json_tight = Instance { name: "json (tight budget)", n: 3, edges: vec![(0, 1), (0, 2)], budget: 500.0 };

    // CSS-L4: 12 self-loop sites. Coherence graph = a FOREST of small
    // components (near-tree): a path, an edge, a small tree with a degree-3
    // hub, another edge, and two isolated sites.
    let css_edges = vec![
        (0, 1), (1, 2),          // path 0-1-2  (node 1 deg2 -> R2)
        (3, 4),                  // edge
        (5, 6), (6, 7), (6, 8),  // tree, hub 6 deg3 (R1 peels leaves -> R0)
        (9, 10),                 // edge
        // 11 isolated -> R0
    ];
    let css_slack = Instance { name: "css-l4 (slack budget)", n: 12, edges: css_edges.clone(), budget: 3400.0 };
    let css_tight = Instance { name: "css-l4 (tight budget)", n: 12, edges: css_edges.clone(), budget: 1800.0 };

    // Boundary control: a K4 coherence clique (treewidth 3) — the shape that
    // FORCES an RN fallback. Proves the reducible/dense boundary is real and
    // that scan-plan (forest / treewidth<=2) sits on the reducible side.
    let k4 = Instance { name: "K4 clique (dense control)", n: 4, edges: vec![(0, 1), (0, 2), (0, 3), (1, 2), (1, 3), (2, 3)], budget: 900.0 };

    for inst in [&json_slack, &json_tight, &css_slack, &css_tight, &k4] {
        run(inst);
    }
}
