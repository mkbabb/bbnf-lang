//! Contract tests for [`egraph::ruler::residue`].
//!
//! Verifies the e-graph fast-path filter correctly discharges pairs the
//! e-graph can prove equivalent (structurally or via seeded rewrites)
//! and surfaces residual pairs the oracle must check.

mod common;

use common::{Bool, f, not, t};
use egraph::ruler::ResidueFilter;

#[test]
fn structurally_identical_pair_is_not_residual() {
    let mut filter: ResidueFilter<Bool> = ResidueFilter::new();
    let lhs = t();
    let rhs = t();
    assert!(
        !filter.is_residual(&lhs, &rhs),
        "identical leaves collapse via hashcons"
    );
}

#[test]
fn distinct_pair_with_no_rewrite_is_residual() {
    let mut filter: ResidueFilter<Bool> = ResidueFilter::new();
    let lhs = t();
    let rhs = f();
    assert!(filter.is_residual(&lhs, &rhs));
}

#[test]
fn seeded_rewrite_discharges_pair() {
    let mut filter: ResidueFilter<Bool> = ResidueFilter::new();
    // Seed: not(not(true)) ≡ true.
    let nntrue = not(not(t()));
    filter.add_known_rewrite(&nntrue, &t());
    assert!(!filter.is_residual(&nntrue, &t()));
    // Independent pair still residual.
    assert!(filter.is_residual(&t(), &f()));
}

#[test]
fn seeded_rewrite_is_symmetric() {
    let mut filter: ResidueFilter<Bool> = ResidueFilter::new();
    let nt = not(t());
    filter.add_known_rewrite(&nt, &f());
    assert!(!filter.is_residual(&nt, &f()));
    assert!(!filter.is_residual(&f(), &nt));
}
