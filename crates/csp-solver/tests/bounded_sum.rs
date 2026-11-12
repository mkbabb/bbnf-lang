//! Bounded-sum (knapsack) constraint tests.
//!
//! Two axes are exercised:
//!
//!   1. The propagator in isolation — `revise` prunes individually-infeasible
//!      values and `check` rejects over-budget full assignments.
//!
//!   2. The decision the *rigged 1-variable* decision CSP structurally cannot
//!      make: a **global** `Csp<CostFiniteDomain>` over ≥2 competing scan
//!      sites, each `{Exact, Prefilter}`, under one shared
//!      `Σ static_size_cost ≤ budget` constraint. Under a slack budget the
//!      global optimum equals the per-site greedy pick (parity floor); under a
//!      tight budget the budget *binds* and forces the expensive shape off at
//!      least one site — a cross-site coupling no per-rule solve can express.

use csp_solver::constraint::BoundedSumConstraint;
use csp_solver::domain::CostFiniteDomain;
use csp_solver::domain::traits::Domain;
use csp_solver::ordering::Ordering;
use csp_solver::{Csp, OptimizationMode, Pruning, SolveConfig};

// Shape encoding for a scan site.
const EXACT: i32 = 0; // cheap to generate, slower at runtime
const PREFILTER: i32 = 1; // expensive to generate, faster at runtime

// Objective (runtime) cost — lower is better; drives MinimizeCost.
const EXACT_RUNTIME: f64 = 10.0;
const PREFILTER_RUNTIME: f64 = 1.0;

// Generated-LOC cost — the scarce pool the budget bounds.
const EXACT_LOC: f64 = 100.0;
const PREFILTER_LOC: f64 = 300.0;

/// One scan-site variable: domain {Exact, Prefilter} priced by runtime cost.
fn site_domain() -> CostFiniteDomain {
    CostFiniteDomain::new(
        vec![EXACT, PREFILTER],
        vec![EXACT_RUNTIME, PREFILTER_RUNTIME],
    )
}

/// LOC weight table for a scan site (used by the bounded-sum budget).
fn site_loc_table() -> Vec<(i32, f64)> {
    vec![(EXACT, EXACT_LOC), (PREFILTER, PREFILTER_LOC)]
}

fn min_cost_config() -> SolveConfig {
    SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 1,
        optimization_mode: OptimizationMode::MinimizeCost,
        node_budget: Some(10_000),
        cancel: None,
    }
}

/// Solve N competing scan sites under a shared generated-LOC budget.
/// Returns the chosen shape per site (index == site).
fn solve_plan(num_sites: usize, budget: f64) -> Vec<i32> {
    let mut csp = Csp::new();
    let vars: Vec<_> = (0..num_sites).map(|_| csp.add_variable(site_domain())).collect();
    csp.add_constraint(BoundedSumConstraint::new(
        vars.clone(),
        vec![site_loc_table(); num_sites],
        budget,
    ));
    csp.finalize();
    let solutions = csp.solve_optimized(&min_cost_config());
    let solution = solutions.first().expect("feasible plan");
    (0..num_sites).map(|i| solution[i]).collect()
}

// ---------------------------------------------------------------------------
// 1. Propagator in isolation.
// ---------------------------------------------------------------------------

#[test]
fn bounded_sum_prunes_individually_infeasible_value() {
    // One site, budget 250. Prefilter (300 LOC) alone busts it → pruned.
    let mut csp = Csp::new();
    let v = csp.add_variable(site_domain());
    csp.add_constraint(BoundedSumConstraint::new(
        vec![v],
        vec![site_loc_table()],
        250.0,
    ));
    csp.finalize();
    csp.propagate().expect("still satisfiable");
    let live = csp.variables[v as usize].domain.values();
    assert_eq!(live, vec![EXACT], "Prefilter must be pruned under a 250-LOC budget");
}

#[test]
fn bounded_sum_check_rejects_over_budget_full_assignment() {
    let c = BoundedSumConstraint::new(
        vec![0, 1],
        vec![site_loc_table(), site_loc_table()],
        500.0,
    );
    use csp_solver::constraint::Constraint;
    // {Prefilter, Prefilter} = 600 LOC > 500 → rejected.
    let over = [Some(PREFILTER), Some(PREFILTER)];
    assert!(!Constraint::<CostFiniteDomain>::check(&c, &over));
    // {Prefilter, Exact} = 400 LOC ≤ 500 → accepted.
    let ok = [Some(PREFILTER), Some(EXACT)];
    assert!(Constraint::<CostFiniteDomain>::check(&c, &ok));
}

#[test]
fn bounded_sum_unsat_when_even_cheapest_busts_budget() {
    // Two sites, budget 150. Cheapest combo {Exact,Exact} = 200 > 150 → unsat.
    let mut csp = Csp::new();
    let a = csp.add_variable(site_domain());
    let b = csp.add_variable(site_domain());
    csp.add_constraint(BoundedSumConstraint::new(
        vec![a, b],
        vec![site_loc_table(), site_loc_table()],
        150.0,
    ));
    csp.finalize();
    // Propagation wipes out a domain because floor (200) already exceeds 150.
    assert!(csp.propagate().is_err(), "must be unsatisfiable under a 150-LOC budget");
}

// ---------------------------------------------------------------------------
// 2. The global budget decision the 1-variable CSP cannot make.
// ---------------------------------------------------------------------------

#[test]
fn slack_budget_equals_per_site_greedy_pick() {
    // Budget 1000 ≥ 2×300: every site independently affords its runtime-best
    // shape. Global optimum == per-site greedy == Prefilter everywhere.
    let plan = solve_plan(2, 1000.0);
    assert_eq!(plan, vec![PREFILTER, PREFILTER], "slack budget must reproduce the per-site pick");
}

#[test]
fn tight_budget_forces_exact_on_at_least_one_site() {
    // Budget 500 < 2×300: cannot afford Prefilter on both. The global CSP
    // must down-shift ≥1 site to the code-cheap Exact shape — a coupling no
    // per-rule (single-variable) solve can represent.
    let plan = solve_plan(2, 500.0);
    let exact_count = plan.iter().filter(|&&s| s == EXACT).count();
    let prefilter_count = plan.iter().filter(|&&s| s == PREFILTER).count();
    assert_eq!(exact_count, 1, "exactly one site forced to Exact under the tight budget");
    assert_eq!(prefilter_count, 1, "the other site keeps its runtime-best Prefilter");
    // And the plan genuinely differs from the slack/greedy plan.
    assert_ne!(plan, vec![PREFILTER, PREFILTER], "the budget must change the plan");
}

#[test]
fn budget_binds_monotonically_across_the_threshold() {
    // Sweep the budget across the 600-LOC greedy demand. Above it: 0 forced.
    // Straddling it: exactly 1 forced. Below 2×Exact: infeasible.
    let loc = |plan: &[i32]| plan.iter().map(|&s| if s == EXACT { EXACT_LOC } else { PREFILTER_LOC }).sum::<f64>();

    let slack = solve_plan(2, 700.0);
    assert!(loc(&slack) <= 700.0);
    assert_eq!(slack.iter().filter(|&&s| s == EXACT).count(), 0);

    let tight = solve_plan(2, 550.0);
    assert!(loc(&tight) <= 550.0);
    assert_eq!(tight.iter().filter(|&&s| s == EXACT).count(), 1);
}
