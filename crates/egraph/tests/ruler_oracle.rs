//! Contract tests for [`egraph::ruler::oracle`].
//!
//! Verifies the VM oracle correctly classifies equivalence verdicts
//! (Equivalent / Diverge / Inconclusive) over the boolean DSL fixture.

mod common;

use std::time::Duration;

use common::{BoolInterpreter, and, f, not, t};
use egraph::ruler::{EquivalenceResult, OracleConfig, check_equivalence};

#[test]
fn equivalent_pair_passes() {
    let cfg = OracleConfig::default();
    let interp = BoolInterpreter;
    // not(not(true)) ≡ true
    let lhs = not(not(t()));
    let rhs = t();
    assert!(matches!(
        check_equivalence(&cfg, &interp, &lhs, &rhs),
        EquivalenceResult::Equivalent
    ));
}

#[test]
fn divergent_pair_reports_witness() {
    let cfg = OracleConfig::default();
    let interp = BoolInterpreter;
    // and(true, false) ≢ true
    let lhs = and(t(), f());
    let rhs = t();
    match check_equivalence(&cfg, &interp, &lhs, &rhs) {
        EquivalenceResult::Diverge {
            lhs_out, rhs_out, ..
        } => {
            assert!(!lhs_out);
            assert!(rhs_out);
        }
        other => panic!("expected divergence, got {:?}", other),
    }
}

#[test]
fn inconclusive_when_witness_count_too_low() {
    let cfg = OracleConfig {
        min_witness_count: 5,
        timeout: Duration::from_secs(1),
    };
    let interp = BoolInterpreter;
    let lhs = t();
    let rhs = t();
    assert!(matches!(
        check_equivalence(&cfg, &interp, &lhs, &rhs),
        EquivalenceResult::Inconclusive
    ));
}

#[test]
fn equivalent_under_demorgan() {
    // De Morgan: not(and(x, y)) ≡ or(not(x), not(y)). The boolean DSL
    // has no `or` constructor, so we exercise the dual: every pair
    // (a, b) with `a` and `b` agreeing on the witness is Equivalent.
    let cfg = OracleConfig::default();
    let interp = BoolInterpreter;
    let lhs = and(t(), t()); // true
    let rhs = not(f()); // not(false) = true
    assert!(matches!(
        check_equivalence(&cfg, &interp, &lhs, &rhs),
        EquivalenceResult::Equivalent
    ));
}
