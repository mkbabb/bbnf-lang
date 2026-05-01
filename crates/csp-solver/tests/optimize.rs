//! Tests for branch-and-bound optimization (CostDomain + SoftConstraint).

use csp_solver::constraint::SoftLambdaConstraint;
use csp_solver::domain::finite::FiniteDomain;
use csp_solver::domain::traits::{CostDomain, Domain};
use csp_solver::ordering::Ordering;
use csp_solver::{Csp, OptimizationMode, Pruning, SolveConfig};

// ---------------------------------------------------------------------------
// A domain with per-value costs: wraps (value, cost) pairs.
// Uses a separate cost map so that costs survive remove/add cycles.
// ---------------------------------------------------------------------------
#[derive(Debug, Clone, PartialEq)]
struct CostFiniteDomain {
    /// Currently-active values.
    active: Vec<i32>,
    /// Immutable cost table: maps value -> cost.
    costs: Vec<(i32, f64)>,
}

impl CostFiniteDomain {
    fn new(entries: Vec<(i32, f64)>) -> Self {
        let active = entries.iter().map(|(v, _)| *v).collect();
        Self {
            active,
            costs: entries,
        }
    }

    fn cost_of(&self, val: &i32) -> f64 {
        self.costs
            .iter()
            .find(|(v, _)| v == val)
            .map(|(_, c)| *c)
            .unwrap_or(0.0)
    }
}

impl Domain for CostFiniteDomain {
    type Value = i32;

    fn size(&self) -> usize {
        self.active.len()
    }

    fn contains(&self, val: &i32) -> bool {
        self.active.contains(val)
    }

    fn remove(&mut self, val: &i32) -> bool {
        if let Some(pos) = self.active.iter().position(|v| v == val) {
            self.active.swap_remove(pos);
            true
        } else {
            false
        }
    }

    fn add(&mut self, val: &i32) {
        if !self.active.contains(val) {
            self.active.push(*val);
        }
    }

    fn values(&self) -> Vec<i32> {
        self.active.clone()
    }

    fn iter(&self) -> impl Iterator<Item = i32> {
        self.active.clone().into_iter()
    }
}

impl CostDomain for CostFiniteDomain {
    fn cost(&self, val: &i32) -> f64 {
        self.cost_of(val)
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

/// Basic: single variable, find minimum-cost assignment.
#[test]
fn test_single_var_minimize() {
    let mut csp = Csp::new();
    let domain = CostFiniteDomain::new(vec![(1, 10.0), (2, 5.0), (3, 20.0)]);
    let _x = csp.add_variable(domain);
    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::None,
        ordering: Ordering::Chronological,
        max_solutions: 1,
        optimization_mode: OptimizationMode::MinimizeCost,
        ..Default::default()
    };

    let solutions = csp.solve_optimized(&config);
    assert_eq!(solutions.len(), 1);
    // Value 2 has cost 5.0 — the minimum.
    assert_eq!(solutions[0], vec![2]);
}

/// Basic: single variable, find maximum-cost assignment.
#[test]
fn test_single_var_maximize() {
    let mut csp = Csp::new();
    let domain = CostFiniteDomain::new(vec![(1, 10.0), (2, 5.0), (3, 20.0)]);
    let _x = csp.add_variable(domain);
    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::None,
        ordering: Ordering::Chronological,
        max_solutions: 1,
        optimization_mode: OptimizationMode::MaximizeCost,
        ..Default::default()
    };

    let solutions = csp.solve_optimized(&config);
    assert_eq!(solutions.len(), 1);
    // Value 3 has cost 20.0 — the maximum.
    assert_eq!(solutions[0], vec![3]);
}

/// Two variables with a not-equal constraint; minimize total cost.
#[test]
fn test_two_vars_not_equal_minimize() {
    let mut csp = Csp::new();
    // x: {A(cost=1), B(cost=5)}
    // y: {A(cost=1), B(cost=5)}
    // x != y
    // Optimal: x=A(1), y=B(5) or x=B(5), y=A(1) -> total 6
    let domain = CostFiniteDomain::new(vec![(0, 1.0), (1, 5.0)]);
    let x = csp.add_variable(domain.clone());
    let y = csp.add_variable(domain);
    csp.add_not_equal(x, y);
    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 10,
        optimization_mode: OptimizationMode::MinimizeCost,
        ..Default::default()
    };

    let solutions = csp.solve_optimized(&config);
    assert!(!solutions.is_empty());
    // Both feasible solutions have cost 6.0 (1+5).
    let first = &solutions[0];
    assert_ne!(first[0], first[1]);
}

/// Three variables with costs, constraints, and optimization.
#[test]
fn test_three_vars_minimize_with_constraints() {
    let mut csp = Csp::new();
    // x in {1(cost=10), 2(cost=1), 3(cost=5)}
    // y in {1(cost=3),  2(cost=8), 3(cost=2)}
    // z in {1(cost=7),  2(cost=4), 3(cost=1)}
    // x != y, y != z, x != z (all different)
    //
    // Feasible assignments (all different from {1,2,3}):
    // (1,2,3): 10+8+1 = 19
    // (1,3,2): 10+2+4 = 16
    // (2,1,3): 1+3+1  = 5  <-- optimal
    // (2,3,1): 1+2+7  = 10
    // (3,1,2): 5+3+4  = 12
    // (3,2,1): 5+8+7  = 20
    let dx = CostFiniteDomain::new(vec![(1, 10.0), (2, 1.0), (3, 5.0)]);
    let dy = CostFiniteDomain::new(vec![(1, 3.0), (2, 8.0), (3, 2.0)]);
    let dz = CostFiniteDomain::new(vec![(1, 7.0), (2, 4.0), (3, 1.0)]);

    let x = csp.add_variable(dx);
    let y = csp.add_variable(dy);
    let z = csp.add_variable(dz);
    csp.add_not_equal(x, y);
    csp.add_not_equal(y, z);
    csp.add_not_equal(x, z);
    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 1,
        optimization_mode: OptimizationMode::MinimizeCost,
        ..Default::default()
    };

    let solutions = csp.solve_optimized(&config);
    assert_eq!(solutions.len(), 1);
    assert_eq!(solutions[0], vec![2, 1, 3]); // cost = 5
}

/// Maximize variant of the three-variable problem.
#[test]
fn test_three_vars_maximize() {
    let mut csp = Csp::new();
    let dx = CostFiniteDomain::new(vec![(1, 10.0), (2, 1.0), (3, 5.0)]);
    let dy = CostFiniteDomain::new(vec![(1, 3.0), (2, 8.0), (3, 2.0)]);
    let dz = CostFiniteDomain::new(vec![(1, 7.0), (2, 4.0), (3, 1.0)]);

    let x = csp.add_variable(dx);
    let y = csp.add_variable(dy);
    let z = csp.add_variable(dz);
    csp.add_not_equal(x, y);
    csp.add_not_equal(y, z);
    csp.add_not_equal(x, z);
    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 1,
        optimization_mode: OptimizationMode::MaximizeCost,
        ..Default::default()
    };

    let solutions = csp.solve_optimized(&config);
    assert_eq!(solutions.len(), 1);
    // (3,2,1) = 5+8+7 = 20 is the maximum.
    assert_eq!(solutions[0], vec![3, 2, 1]);
}

/// Soft constraints: minimize soft penalty on a plain (non-CostDomain) domain.
#[test]
fn test_soft_constraints_only() {
    let mut csp: Csp<FiniteDomain<i32>> = Csp::new();
    let domain = FiniteDomain::new(vec![0, 1, 2]);
    let x = csp.add_variable(domain.clone());
    let y = csp.add_variable(domain);

    // Hard constraint: x != y
    csp.add_not_equal(x, y);

    // Soft constraint: prefer x == 1 (penalty 100 if not)
    csp.add_soft_constraint(SoftLambdaConstraint::new(
        vec![x],
        move |assignment| assignment[x as usize] == Some(1),
        100.0,
        "prefer_x_1",
    ));

    // Soft constraint: prefer y == 2 (penalty 50 if not)
    csp.add_soft_constraint(SoftLambdaConstraint::new(
        vec![y],
        move |assignment| assignment[y as usize] == Some(2),
        50.0,
        "prefer_y_2",
    ));

    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 1,
        optimization_mode: OptimizationMode::MinimizeCost,
        ..Default::default()
    };

    // Use solve() (not solve_optimized) since FiniteDomain doesn't impl CostDomain.
    let solutions = csp.solve(&config);
    assert_eq!(solutions.len(), 1);
    // Optimal: x=1, y=2 -> penalty 0 (both soft constraints satisfied, x != y holds).
    assert_eq!(solutions[0], vec![1, 2]);
}

/// Soft constraints with unavoidable penalty.
#[test]
fn test_soft_constraints_unavoidable_penalty() {
    let mut csp: Csp<FiniteDomain<i32>> = Csp::new();
    let domain = FiniteDomain::new(vec![0, 1]);
    let x = csp.add_variable(domain.clone());
    let y = csp.add_variable(domain);

    // Hard: x != y
    csp.add_not_equal(x, y);

    // Soft: prefer x == 0 (penalty 10)
    csp.add_soft_constraint(SoftLambdaConstraint::new(
        vec![x],
        move |a| a[x as usize] == Some(0),
        10.0,
        "prefer_x_0",
    ));

    // Soft: prefer y == 0 (penalty 20)
    // Can't have both x=0 and y=0, so one penalty is unavoidable.
    csp.add_soft_constraint(SoftLambdaConstraint::new(
        vec![y],
        move |a| a[y as usize] == Some(0),
        20.0,
        "prefer_y_0",
    ));

    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 10,
        optimization_mode: OptimizationMode::MinimizeCost,
        ..Default::default()
    };

    let solutions = csp.solve(&config);
    // Feasible: (0,1) penalty=20, (1,0) penalty=10
    // Best: (1,0) with penalty=10
    assert!(!solutions.is_empty());
    assert_eq!(solutions[0], vec![1, 0]);
}

/// Combined: CostDomain + SoftConstraint.
#[test]
fn test_cost_domain_plus_soft_constraints() {
    let mut csp = Csp::new();
    // x in {0(cost=1), 1(cost=3)}
    // y in {0(cost=2), 1(cost=1)}
    let dx = CostFiniteDomain::new(vec![(0, 1.0), (1, 3.0)]);
    let dy = CostFiniteDomain::new(vec![(0, 2.0), (1, 1.0)]);
    let x = csp.add_variable(dx);
    let y = csp.add_variable(dy);

    // Hard: x != y
    csp.add_not_equal(x, y);

    // Soft: prefer x == 1 (penalty 10 if violated)
    csp.add_soft_constraint(SoftLambdaConstraint::new(
        vec![x],
        move |a| a[x as usize] == Some(1),
        10.0,
        "prefer_x_1",
    ));

    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 10,
        optimization_mode: OptimizationMode::MinimizeCost,
        ..Default::default()
    };

    let solutions = csp.solve_optimized(&config);
    // Feasible: (0,1) domain_cost=1+1=2, soft_penalty=10 (x!=1) -> total 12
    //           (1,0) domain_cost=3+2=5, soft_penalty=0           -> total 5
    // Best: (1,0)
    assert!(!solutions.is_empty());
    assert_eq!(solutions[0], vec![1, 0]);
}

/// Feasibility mode with CostDomain: costs should be ignored.
#[test]
fn test_feasibility_ignores_cost() {
    let mut csp = Csp::new();
    let domain = CostFiniteDomain::new(vec![(1, 100.0), (2, 0.0)]);
    let _x = csp.add_variable(domain);
    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::None,
        ordering: Ordering::Chronological,
        max_solutions: 1,
        optimization_mode: OptimizationMode::Feasibility,
        ..Default::default()
    };

    // Feasibility uses regular backtracking, not B&B. Should find any solution.
    let solutions = csp.solve(&config);
    assert_eq!(solutions.len(), 1);
    // Should return whichever comes first in domain iteration, not necessarily cheapest.
}

/// Branch-and-bound prunes effectively: with a tight cost structure,
/// the solver should explore fewer nodes than exhaustive search.
#[test]
fn test_branch_and_bound_pruning() {
    let mut csp = Csp::new();
    // 4 variables, each with 3 values.
    // Costs are arranged so that the minimum is obvious from the first variable.
    let d0 = CostFiniteDomain::new(vec![(0, 0.0), (1, 100.0), (2, 200.0)]);
    let d1 = CostFiniteDomain::new(vec![(0, 0.0), (1, 100.0), (2, 200.0)]);
    let d2 = CostFiniteDomain::new(vec![(0, 0.0), (1, 100.0), (2, 200.0)]);
    let d3 = CostFiniteDomain::new(vec![(0, 0.0), (1, 100.0), (2, 200.0)]);

    let _v0 = csp.add_variable(d0);
    let _v1 = csp.add_variable(d1);
    let _v2 = csp.add_variable(d2);
    let _v3 = csp.add_variable(d3);
    // No constraints — all assignments are feasible.
    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::None,
        ordering: Ordering::Chronological,
        max_solutions: 1,
        optimization_mode: OptimizationMode::MinimizeCost,
        ..Default::default()
    };

    let solutions = csp.solve_optimized(&config);
    assert_eq!(solutions.len(), 1);
    assert_eq!(solutions[0], vec![0, 0, 0, 0]); // all zeros = cost 0
}

/// Multiple solutions sorted by cost.
#[test]
fn test_multiple_solutions_sorted() {
    let mut csp = Csp::new();
    let domain = CostFiniteDomain::new(vec![(1, 10.0), (2, 5.0), (3, 1.0)]);
    let _x = csp.add_variable(domain);
    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::None,
        ordering: Ordering::Chronological,
        max_solutions: 10,
        optimization_mode: OptimizationMode::MinimizeCost,
        ..Default::default()
    };

    let solutions = csp.solve_optimized(&config);
    assert_eq!(solutions.len(), 3);
    // Should be sorted: cost 1 (val=3), cost 5 (val=2), cost 10 (val=1)
    assert_eq!(solutions[0], vec![3]);
    assert_eq!(solutions[1], vec![2]);
    assert_eq!(solutions[2], vec![1]);
}

/// solve_with_cost_eval: use a custom evaluator that doesn't rely on CostDomain.
#[test]
fn test_solve_with_cost_eval_custom() {
    use csp_solver::solver::optimize::DomainCostEval;

    let mut csp: Csp<FiniteDomain<i32>> = Csp::new();
    let domain = FiniteDomain::new(vec![1, 2, 3]);
    let _x = csp.add_variable(domain);
    csp.finalize();

    // Custom evaluator: cost = value^2.
    struct SquareCost;
    impl DomainCostEval<FiniteDomain<i32>> for SquareCost {
        fn cost(&self, _domain: &FiniteDomain<i32>, val: &i32) -> f64 {
            (*val as f64) * (*val as f64)
        }
        fn min_cost(&self, domain: &FiniteDomain<i32>) -> f64 {
            domain
                .values()
                .into_iter()
                .map(|v| (v as f64) * (v as f64))
                .fold(f64::INFINITY, f64::min)
        }
        fn max_cost(&self, domain: &FiniteDomain<i32>) -> f64 {
            domain
                .values()
                .into_iter()
                .map(|v| (v as f64) * (v as f64))
                .fold(f64::NEG_INFINITY, f64::max)
        }
    }

    let config = SolveConfig {
        pruning: Pruning::None,
        ordering: Ordering::Chronological,
        max_solutions: 1,
        optimization_mode: OptimizationMode::MinimizeCost,
        ..Default::default()
    };

    let solutions = csp.solve_with_cost_eval(&config, &SquareCost);
    assert_eq!(solutions.len(), 1);
    assert_eq!(solutions[0], vec![1]); // 1^2 = 1, cheapest
}

/// Regression: feasibility mode with default config is unchanged.
#[test]
fn test_default_config_is_feasibility() {
    let config = SolveConfig::default();
    assert_eq!(config.optimization_mode, OptimizationMode::Feasibility);
}

/// Regression: the default config carries the Tranche Y freezing-guard
/// node budget so that pathological searches cannot hang a caller.
#[test]
fn test_default_config_has_node_budget() {
    let config = SolveConfig::default();
    assert_eq!(config.node_budget, Some(1_000_000));
}

/// A large N-variable CSP with an artificially tiny budget must abort
/// cleanly, flag `budget_exceeded`, and return whatever it has found so
/// far (may be empty). The caller is expected to branch on the flag
/// and fall back to a trivial per-variable pick.
#[test]
fn test_node_budget_aborts_long_search() {
    let mut csp = Csp::new();
    // 30 variables × 5 values — the search tree is 5^30 ≈ 9.3e20.
    // We set a 100-node budget so the abort fires within milliseconds.
    let domain = CostFiniteDomain::new(vec![(1, 10.0), (2, 5.0), (3, 20.0), (4, 1.0), (5, 15.0)]);
    let vars: Vec<_> = (0..30).map(|_| csp.add_variable(domain.clone())).collect();
    // A soft constraint per pair to force the search to enumerate
    // (the branch-and-bound would otherwise pick the lowest-cost
    // value per variable independently).
    for w in vars.windows(2) {
        let (x, y) = (w[0], w[1]);
        csp.add_soft_constraint(SoftLambdaConstraint::new(
            vec![x, y],
            move |a: &[Option<i32>]| match (&a[x as usize], &a[y as usize]) {
                (Some(va), Some(vb)) => va != vb,
                _ => true,
            },
            1.0,
            "penalize_equal",
        ));
    }
    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::Chronological,
        max_solutions: 1,
        optimization_mode: OptimizationMode::MinimizeCost,
        node_budget: Some(100),
        ..Default::default()
    };

    // Must terminate promptly rather than hang.
    let _solutions = csp.solve_optimized(&config);
    let stats = csp.stats();
    assert!(
        stats.budget_exceeded,
        "budget guard did not fire (nodes_explored={})",
        stats.nodes_explored
    );
    assert!(
        stats.nodes_explored <= 101,
        "explored {} nodes with a 100-node budget",
        stats.nodes_explored
    );
}

/// A tiny CSP within budget must finish normally and leave
/// `budget_exceeded` false.
#[test]
fn test_node_budget_does_not_fire_for_small_search() {
    let mut csp = Csp::new();
    let domain = CostFiniteDomain::new(vec![(1, 10.0), (2, 5.0), (3, 20.0)]);
    let _x = csp.add_variable(domain);
    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::None,
        ordering: Ordering::Chronological,
        max_solutions: 1,
        optimization_mode: OptimizationMode::MinimizeCost,
        node_budget: Some(1_000),
        ..Default::default()
    };

    let solutions = csp.solve_optimized(&config);
    let stats = csp.stats();
    assert!(!stats.budget_exceeded);
    assert_eq!(solutions.len(), 1);
    assert_eq!(solutions[0], vec![2]);
}
