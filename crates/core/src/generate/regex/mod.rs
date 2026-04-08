//! Regex compilation and inline code emission.
//!
//! Classification lives in the `bbnf-regex` crate (via `parse_that::regex::classify`).
//!
//! Layered architecture:
//! - **cost_model** — Centralized heuristic thresholds (CostModel, LengthHint, EmitOpts).
//! - **emit/** — Tiered code emission via pattern registry (fast_paths → hir → dfa → error).
//! - **patterns/** — Pattern detection and analysis utilities.

pub mod cost_model;
pub mod emit;
pub mod patterns;

// Re-exports for common use.
pub use cost_model::{CostModel, EmitOpts, LengthHint};
pub use emit::{
    RegexStrategy, emit_regex, emit_regex_direct_call, emit_regex_unsupported,
    is_fused_number_regex, is_fused_number_regex_cached, solve_regex_strategy,
};
