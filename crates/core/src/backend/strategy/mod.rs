//! Strategy CSPs — target-specific decisions stored on BackendPreparation.
//!
//! These CSPs consume IR facts (NodeFacts, RegexInfo) plus backend-specific
//! inputs (target, cost model, prettify mode) and produce strategy choices.
//! Each backend runs the same solvers with its own cost model.

pub mod alt_strategy;
