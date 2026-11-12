//! Tests for the `bounded_sum` knapsack propagator and the global
//! scan-plan CSP it enables (PROVE-2-BUDGET / LOCK-2 Brief 2).
//!
//! The scan-plan model: each self-loop scan site is a variable over
//! `{Prefilter=0, Exact=1}`. `Prefilter` (shufti/SIMD delimiter scan) is
//! runtime-cheap but generated-size-fat; `Exact` (scalar) is runtime-dear
//! but lean. The objective minimizes runtime cyc/B; the `BoundedSumConstraint`
//! holds `Σ static_size ≤ generated_loc_budget` as a HARD solve-time
//! constraint. Under a tight budget the solver is forced to pick `Exact` on
//! legal-but-unaffordable sites — the decision a 1-variable CSP cannot make.

use csp_solver::constraint::{BoundedSumConstraint, Constraint, Revision};
use csp_solver::domain::{CostFiniteDomain, Domain};
use csp_solver::ordering::Ordering;
use csp_solver::variable::Variable;
use csp_solver::{Csp, OptimizationMode, Pruning, SolveConfig};

const PREFILTER: i32 = 0;
const EXACT: i32 = 1;

/// Static generated-size (LOC) of each lowering. Prefilter emits a shufti
/// table + SIMD kernel (fat); Exact is a scalar byte loop (lean).
const LOC_PREFILTER: i64 = 40;
const LOC_EXACT: i64 = 8;

fn site_weights(n: usize) -> Vec<Vec<(i32, i64)>> {
    (0..n)
        .map(|_| vec![(PREFILTER, LOC_PREFILTER), (EXACT, LOC_EXACT)])
        .collect()
}

// ── Direct propagator unit tests ────────────────────────────────────────

#[test]
fn propagator_prunes_unaffordable_prefilter() {
    // Two sites, budget 45. A single Prefilter with the other site at its
    // floor costs 40 + 8 = 48 > 45, so every Prefilter must be pruned; only
    // an all-Exact plan (16 LOC) fits.
    let dom = CostFiniteDomain::new(vec![PREFILTER, EXACT], vec![100.0, 150.0]);
    let mut vars = vec![Variable::new(dom.clone()), Variable::new(dom)];
    let c = BoundedSumConstraint::new(vec![0, 1], site_weights(2), 45);

    let rev = c.revise(&mut vars, 0);
    assert_eq!(rev, Revision::Changed, "revise must prune");
    assert!(!vars[0].domain.contains(&PREFILTER), "site0 Prefilter pruned");
    assert!(!vars[1].domain.contains(&PREFILTER), "site1 Prefilter pruned");
    assert!(vars[0].domain.contains(&EXACT) && vars[1].domain.contains(&EXACT));
}

#[test]
fn propagator_reports_infeasible_when_floor_busts() {
    // Budget below the all-Exact floor (16) → unsatisfiable.
    let dom = CostFiniteDomain::new(vec![PREFILTER, EXACT], vec![100.0, 150.0]);
    let mut vars = vec![Variable::new(dom.clone()), Variable::new(dom)];
    let c = BoundedSumConstraint::new(vec![0, 1], site_weights(2), 10);
    assert_eq!(c.revise(&mut vars, 0), Revision::Unsatisfiable);
}

#[test]
fn propagator_leaves_slack_budget_untouched() {
    // Budget 200 over two sites: both Prefilter (80) fits, nothing pruned.
    let dom = CostFiniteDomain::new(vec![PREFILTER, EXACT], vec![100.0, 150.0]);
    let mut vars = vec![Variable::new(dom.clone()), Variable::new(dom)];
    let c = BoundedSumConstraint::new(vec![0, 1], site_weights(2), 200);
    assert_eq!(c.revise(&mut vars, 0), Revision::Unchanged);
    assert!(vars[0].domain.contains(&PREFILTER) && vars[1].domain.contains(&PREFILTER));
}

// ── Global scan-plan CSP (the 2C teeth) ─────────────────────────────────

/// Runtime cyc/B×1000 penalty of choosing Exact over Prefilter at each of the
/// 5 audited CSS-L4 self-loop sites, ordered hottest→coolest. Prefilter is a
/// flat 100 everywhere; Exact is 100 + penalty.
const PENALTY: [f64; 5] = [90.0, 70.0, 50.0, 30.0, 10.0];

fn build_scan_plan_csp(budget: i64) -> Csp<CostFiniteDomain> {
    let mut csp = Csp::new();
    let mut scope = Vec::new();
    for &pen in PENALTY.iter() {
        let dom = CostFiniteDomain::new(vec![PREFILTER, EXACT], vec![100.0, 100.0 + pen]);
        scope.push(csp.add_variable(dom));
    }
    csp.add_constraint(BoundedSumConstraint::new(scope, site_weights(5), budget));
    csp
}

fn solve_plan(budget: i64) -> Vec<i32> {
    let mut csp = build_scan_plan_csp(budget);
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
    assert!(!sols.is_empty(), "budget {budget} must be feasible");
    sols[0].clone()
}

/// The greedy per-rule pick: each site independently minimizes runtime →
/// always Prefilter. This is what today's per-rule selection emits; it
/// ignores the cross-site budget.
fn per_rule_greedy() -> Vec<i32> {
    vec![PREFILTER; 5]
}

fn total_loc(plan: &[i32]) -> i64 {
    plan.iter()
        .map(|&v| if v == PREFILTER { LOC_PREFILTER } else { LOC_EXACT })
        .sum()
}

#[test]
fn tight_budget_forces_exact_on_legal_sites() {
    // Floor (all-Exact) = 40 LOC; each Prefilter upgrade costs +32. Budget
    // 120 affords floor(40)+2 upgrades(64)=104, never a 3rd (136>120). So
    // ≥3 sites are forced to Exact even though Prefilter is runtime-cheaper.
    let plan = solve_plan(120);
    let exact_count = plan.iter().filter(|&&v| v == EXACT).count();
    assert!(
        exact_count >= 1,
        "tight budget must force Exact on ≥1 legal site, got plan {plan:?}"
    );
    assert!(total_loc(&plan) <= 120, "plan must honor the hard budget");

    // The optimum keeps Prefilter on the two hottest sites (indices 0,1) and
    // forces Exact on the three coolest (2,3,4).
    assert_eq!(
        plan,
        vec![PREFILTER, PREFILTER, EXACT, EXACT, EXACT],
        "B&B must sacrifice the coolest sites"
    );

    // The greedy per-rule plan is illegal under this budget → the global CSP
    // strictly dominates (produces a legal plan greedy cannot).
    assert!(
        total_loc(&per_rule_greedy()) > 120,
        "greedy per-rule busts the tight budget"
    );
}

#[test]
fn slack_budget_matches_per_rule_pick() {
    // Budget 220 ≥ all-Prefilter (200): the parity floor. The global plan
    // must be byte-identical to the greedy per-rule pick.
    let plan = solve_plan(220);
    assert_eq!(
        plan,
        per_rule_greedy(),
        "slack budget must reproduce the per-rule pick exactly"
    );
}

#[test]
fn solve_is_deterministic() {
    // regen∘regen proxy: the plan is a pure function of (sites, budget).
    for &budget in &[120_i64, 150, 220] {
        let a = solve_plan(budget);
        let b = solve_plan(budget);
        assert_eq!(a, b, "solve must be deterministic at budget {budget}");
    }
}

#[test]
fn budget_sweep_is_monotone_in_exact_count() {
    // As the budget tightens, the number of Exact sites is non-decreasing —
    // the budget is genuinely binding, not cosmetic.
    let mut prev_exact = 0usize;
    for &budget in &[220_i64, 180, 150, 120, 90, 60, 40] {
        let plan = solve_plan(budget);
        let exact = plan.iter().filter(|&&v| v == EXACT).count();
        assert!(
            exact >= prev_exact,
            "tighter budget {budget} must not reduce Exact count ({exact} < {prev_exact})"
        );
        assert!(total_loc(&plan) <= budget);
        prev_exact = exact;
    }
    assert_eq!(prev_exact, 5, "at budget 40 the floor is all-Exact");
}
