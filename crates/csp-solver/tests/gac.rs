//! GAC all-different tests.
//! Extracted from solver/gac_alldiff.rs inline tests.

use csp_solver::constraint::{Revision, VarId};
use csp_solver::domain::bitset::BitsetDomain;
use csp_solver::domain::Domain;
use csp_solver::solver::gac_alldiff::propagate_gac_alldiff;
use csp_solver::variable::Variable;

/// Helper: build a GAC test scenario and return the revision result + pruned domains.
fn run_gac(
    domains: &[Vec<u32>],
) -> (Revision, Vec<Vec<u32>>) {
    let scope: Vec<VarId> = (0..domains.len() as u32).collect();
    let mut variables: Vec<Variable<BitsetDomain>> = domains
        .iter()
        .map(|vals| Variable::new(BitsetDomain::new(vals.iter().copied())))
        .collect();

    let rev = propagate_gac_alldiff(&scope, &mut variables, 0);
    let result_domains: Vec<Vec<u32>> = variables
        .iter()
        .map(|v| v.domain.values())
        .collect();

    (rev, result_domains)
}

#[test]
fn test_no_pruning_needed() {
    // Each variable already has a unique value.
    let (rev, doms) = run_gac(&[vec![1], vec![2], vec![3]]);
    // All singletons -> n_vars (unassigned) = 0, returns Unchanged.
    assert_eq!(rev, Revision::Unchanged);
    assert_eq!(doms, vec![vec![1], vec![2], vec![3]]);
}

#[test]
fn test_simple_pruning() {
    // x0 = {1}, x1 = {1, 2}, x2 = {1, 2, 3}
    // After singleton removal + GAC:
    //   x0 assigned 1 -> x1 must be {2}, x2 must be {3}
    // But x0 is singleton so only x1 and x2 are "unassigned" => n_vars=2, returns Unchanged
    // (binary case handled by FC).
    let (rev, _doms) = run_gac(&[vec![1], vec![1, 2], vec![1, 2, 3]]);
    assert_eq!(rev, Revision::Unchanged);
}

#[test]
fn test_gac_three_vars() {
    // x0 = {1, 2}, x1 = {1, 2}, x2 = {1, 2, 3}
    // x0 and x1 must consume 1 and 2, so x2 must be 3.
    // GAC should prune 1 and 2 from x2.
    let (rev, doms) = run_gac(&[vec![1, 2], vec![1, 2], vec![1, 2, 3]]);
    assert_eq!(rev, Revision::Changed);
    assert_eq!(doms[0], vec![1, 2]);
    assert_eq!(doms[1], vec![1, 2]);
    assert_eq!(doms[2], vec![3]);
}

#[test]
fn test_unsatisfiable() {
    // x0 = {1, 2}, x1 = {1, 2}, x2 = {1, 2}
    // Matching can only cover 2 of 3 vars.
    let (rev, _doms) = run_gac(&[vec![1, 2], vec![1, 2], vec![1, 2]]);
    assert_eq!(rev, Revision::Unsatisfiable);
}

#[test]
fn test_hall_set_four_vars() {
    // Classic Hall set: x0={1,2}, x1={1,2}, x2={1,2,3,4}, x3={1,2,3,4}
    // {x0, x1} form a Hall set over {1,2}.
    // GAC should prune 1 and 2 from x2 and x3.
    let (rev, doms) = run_gac(&[
        vec![1, 2],
        vec![1, 2],
        vec![1, 2, 3, 4],
        vec![1, 2, 3, 4],
    ]);
    assert_eq!(rev, Revision::Changed);
    assert_eq!(doms[0], vec![1, 2]);
    assert_eq!(doms[1], vec![1, 2]);
    assert_eq!(doms[2], vec![3, 4]);
    assert_eq!(doms[3], vec![3, 4]);
}

#[test]
fn test_no_change_already_consistent() {
    // All domains disjoint — already arc consistent.
    let (rev, doms) = run_gac(&[vec![1, 2], vec![3, 4], vec![5, 6]]);
    assert_eq!(rev, Revision::Unchanged);
    assert_eq!(doms[0], vec![1, 2]);
    assert_eq!(doms[1], vec![3, 4]);
    assert_eq!(doms[2], vec![5, 6]);
}

#[test]
fn test_mixed_singleton_and_multi() {
    // x0={1}, x1={1,2,3}, x2={1,2,3}, x3={1,2,3}
    // x0 is singleton (assigned). Unassigned: x1, x2, x3 with available {2,3} after
    // removing assigned val 1.
    // 3 vars, 2 available values -> UNSAT.
    let (rev, _doms) = run_gac(&[vec![1], vec![1, 2, 3], vec![1, 2, 3], vec![1, 2, 3]]);
    assert_eq!(rev, Revision::Unsatisfiable);
}

/// Compile-time regression guard: propagate_gac_alldiff must NOT require
/// ValueIndex or any bound beyond Domain. If someone re-adds the bound,
/// this test fails to compile (FiniteDomain<String> has no ValueIndex impl).
#[test]
fn gac_compiles_without_value_index() {
    use csp_solver::domain::finite::FiniteDomain;
    let _: fn(&[VarId], &mut [Variable<FiniteDomain<String>>], usize) -> Revision =
        propagate_gac_alldiff::<FiniteDomain<String>>;
}
