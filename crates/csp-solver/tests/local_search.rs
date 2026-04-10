//! Min-conflicts local search tests.
//! Extracted from solver/local_search.rs inline tests.

use csp_solver::adjacency::Adjacency;
use csp_solver::constraint::{ConstraintEnum, NotEqual};
use csp_solver::domain::finite::FiniteDomain;
use csp_solver::solver::local_search::min_conflicts;
use csp_solver::variable::Variable;

#[test]
fn simple_two_vars() {
    // x0 != x1, domain {1, 2}
    let mut vars = vec![
        Variable::new(FiniteDomain::new(vec![1, 2])),
        Variable::new(FiniteDomain::new(vec![1, 2])),
    ];
    let constraints: Vec<ConstraintEnum<FiniteDomain<i32>>> =
        vec![ConstraintEnum::NotEqual(NotEqual::new(0, 1))];
    let adj = Adjacency::build(2, &constraints);

    let sol = min_conflicts(&mut vars, &constraints, &adj, 1000);
    assert!(sol.is_some());
    let sol = sol.unwrap();
    assert_ne!(sol[0], sol[1]);
}

#[test]
fn trivial_empty() {
    let mut vars: Vec<Variable<FiniteDomain<i32>>> = vec![];
    let constraints: Vec<ConstraintEnum<FiniteDomain<i32>>> = vec![];
    let adj = Adjacency::build(0, &constraints);
    let sol = min_conflicts(&mut vars, &constraints, &adj, 100);
    assert_eq!(sol, Some(vec![]));
}

#[test]
fn three_coloring() {
    // 3 variables, all-different, domain {R, G, B}
    let dom = FiniteDomain::new(vec![0, 1, 2]);
    let mut vars = vec![
        Variable::new(dom.clone()),
        Variable::new(dom.clone()),
        Variable::new(dom.clone()),
    ];
    let constraints: Vec<ConstraintEnum<FiniteDomain<i32>>> = vec![
        ConstraintEnum::NotEqual(NotEqual::new(0, 1)),
        ConstraintEnum::NotEqual(NotEqual::new(1, 2)),
        ConstraintEnum::NotEqual(NotEqual::new(0, 2)),
    ];
    let adj = Adjacency::build(3, &constraints);

    let sol = min_conflicts(&mut vars, &constraints, &adj, 10_000);
    assert!(sol.is_some());
    let s = sol.unwrap();
    assert_ne!(s[0], s[1]);
    assert_ne!(s[1], s[2]);
    assert_ne!(s[0], s[2]);
}
