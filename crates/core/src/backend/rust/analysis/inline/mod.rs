//! Inline-plan analysis: per-rule [`CallMode`] selection via CSP
//! propagation.
//!
//! This module solves the inlining decision for every rule in the
//! grammar. The decision is modelled as a Constraint Satisfaction
//! Problem: each rule is one variable with the lattice domain
//! `{InlineBody, DirectCall}`; the constraints encode forced modes
//! (preserve_identity / @token / operator-chain / single-site /
//! cyclic / recover / prettify / aggregate-payload) and a single
//! cost-budget constraint that consolidates the heuristic
//! shape/cost/ref-count checks into one revise step.
//!
//! # Sub-modules
//!
//! - [`budgets`] — [`CostBudgets`] derived from
//!   [`egraph::CostWeights`] (Tranche AF.2 single-source-of-truth
//!   numeric projections).
//! - [`visit`] — IR-tree walkers (ref counting, fan-out, cost
//!   estimation, self-reference detection, single-site eligibility).
//! - [`constraints`] — CSP domain ([`InlineDomain`]) + constraint
//!   types ([`ForceCallMode`], [`CostBudgetConstraint`]) and the
//!   shape-guard predicate
//!   [`should_force_direct_call`](constraints::should_force_direct_call).
//! - [`plan`] — public entrypoint
//!   [`analyze_parse_inline_plan`](plan::analyze_parse_inline_plan)
//!   plus the [`CallMode`] enum and the [`InlinePlan`] result.

mod budgets;
mod constraints;
mod plan;
mod visit;

pub use constraints::should_force_direct_call;
pub use plan::{CallMode, InlinePlan, analyze_parse_inline_plan};
pub use visit::{count_refs_vec, estimate_expansion_cost, max_alt_branches};

#[cfg(test)]
pub(crate) use budgets::CostBudgets;
