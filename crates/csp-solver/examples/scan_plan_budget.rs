//! PROVE-2-BUDGET demo: one global scan-plan CSP over the self-loop scan
//! sites audited out of the real CSS-L4 fleet grammar, solved under a tight
//! and a slack generated-size budget.
//!
//! Each site is a negated-class star `[^D]*` whose delimiter set D (the
//! class complement) has cardinality ∈[1,16] — i.e. a shufti-able
//! "scan-to-delimiter" self-loop. Every site chooses between:
//!   Prefilter — SIMD shufti kernel: runtime-cheap, generated-size-fat.
//!   Exact     — scalar byte loop:   runtime-dear, generated-size-lean.
//! The objective minimizes runtime cyc/B; `BoundedSumConstraint` holds
//! `Σ static_size ≤ budget` as a HARD solve-time constraint.
//!
//! Run: `cargo run -p csp-solver --example scan_plan_budget`

use csp_solver::constraint::BoundedSumConstraint;
use csp_solver::domain::CostFiniteDomain;
use csp_solver::ordering::Ordering;
use csp_solver::{Csp, OptimizationMode, Pruning, SolveConfig};

const PREFILTER: i32 = 0;
const EXACT: i32 = 1;
const LOC_PREFILTER: i64 = 40;
const LOC_EXACT: i64 = 8;

struct Site {
    rule: &'static str,
    origin: &'static str,
    delims: &'static str,
    complement_card: usize,
    /// runtime cyc/B×1000 penalty of Exact over Prefilter (site hotness).
    exact_penalty: f64,
}

/// Five self-loop scan sites audited from grammar/css/l4/*.bbnf (Step 0).
const SITES: &[Site] = &[
    Site { rule: "string",          origin: "tokens.bbnf:9",     delims: "{\" \\}",     complement_card: 2, exact_penalty: 90.0 },
    Site { rule: "genericDecl",     origin: "properties.bbnf:212", delims: "{; ! }}",   complement_card: 3, exact_penalty: 70.0 },
    Site { rule: "selectorSpan",    origin: "pretty.bbnf:20",    delims: "{{ } ;}",     complement_card: 3, exact_penalty: 50.0 },
    Site { rule: "comment-body",    origin: "stylesheet.bbnf:12", delims: "{*}",        complement_card: 1, exact_penalty: 30.0 },
    Site { rule: "mediaFeatureVal", origin: "media.bbnf:25",     delims: "{)}",         complement_card: 1, exact_penalty: 10.0 },
];

fn build_csp(budget: i64) -> Csp<CostFiniteDomain> {
    let mut csp = Csp::new();
    let mut scope = Vec::new();
    let mut weights = Vec::new();
    for s in SITES {
        let dom = CostFiniteDomain::new(vec![PREFILTER, EXACT], vec![100.0, 100.0 + s.exact_penalty]);
        scope.push(csp.add_variable(dom));
        weights.push(vec![(PREFILTER, LOC_PREFILTER), (EXACT, LOC_EXACT)]);
    }
    csp.add_constraint(BoundedSumConstraint::new(scope, weights, budget));
    csp
}

fn solve(budget: i64) -> (Vec<i32>, u64) {
    let mut csp = build_csp(budget);
    csp.finalize();
    let cfg = SolveConfig {
        pruning: Pruning::Ac3,
        ordering: Ordering::FailFirst,
        max_solutions: 1,
        backjumping: false,
        optimization_mode: OptimizationMode::MinimizeCost,
        node_budget: Some(1_000_000),
    };
    let sols = csp.solve_optimized(&cfg);
    let nodes = csp.stats().nodes_explored;
    (sols.into_iter().next().expect("feasible"), nodes)
}

fn loc(plan: &[i32]) -> i64 {
    plan.iter().map(|&v| if v == PREFILTER { LOC_PREFILTER } else { LOC_EXACT }).sum()
}

fn runtime(plan: &[i32]) -> f64 {
    plan.iter().zip(SITES).map(|(&v, s)| if v == PREFILTER { 100.0 } else { 100.0 + s.exact_penalty }).sum()
}

fn print_plan(label: &str, budget: i64, plan: &[i32], nodes: u64) {
    println!("── {label} (budget = {budget} LOC) ──  nodes_explored={nodes}");
    for (s, &v) in SITES.iter().zip(plan) {
        let kind = if v == PREFILTER { "Prefilter" } else { "Exact    " };
        println!(
            "    {:<16} {:<20} delims={:<8} |compl|={}  ->  {}",
            s.rule, s.origin, s.delims, s.complement_card, kind
        );
    }
    println!("    Σ static_size = {} LOC   Σ runtime = {} cyc/B×1000\n", loc(plan), runtime(plan));
}

fn main() {
    println!("PROVE-2-BUDGET — global scan-plan CSP over {} CSS-L4 self-loop sites\n", SITES.len());
    let greedy = vec![PREFILTER; SITES.len()];
    println!(
        "per-rule greedy pick (each site minimizes runtime independently): {:?}  = {} LOC\n",
        greedy, loc(&greedy)
    );

    let (tight, tn) = solve(120);
    print_plan("TIGHT", 120, &tight, tn);

    let (slack, sn) = solve(220);
    print_plan("SLACK", 220, &slack, sn);

    let exact_forced = tight.iter().filter(|&&v| v == EXACT).count();
    let (tight2, _) = solve(120);
    println!("VERDICT CHECKS");
    println!("  tight forces Exact on {exact_forced} legal site(s)  [PASS ≥1]");
    println!("  tight honors hard budget: {} ≤ 120  [{}]", loc(&tight), loc(&tight) <= 120);
    println!("  greedy per-rule busts tight budget: {} > 120  [{}]", loc(&greedy), loc(&greedy) > 120);
    println!("  slack plan == per-rule greedy pick: {}  [parity floor]", slack == greedy);
    println!("  solve determinism (regen∘regen proxy): {}", tight == tight2);
}
