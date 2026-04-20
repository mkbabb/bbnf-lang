//! Regex compilation and inline code emission.
//!
//! Classification lives in the `bbnf-regex` crate (via `parse_that::regex::classify`).
//!
//! Layered architecture:
//! - **cost_model** — Centralized heuristic thresholds (CostModel, LengthHint, EmitOpts).
//! - **emit/** — Tiered code emission via pattern registry (fast_paths → hir → dfa → error).
//! - **patterns/** — Pattern detection and analysis utilities.
//! - **byte_class** (AY.W4.3) — Adapter-entry first-byte dispatcher,
//!   short-circuiting the pointer-equality cascade.
//! - **phf** (AY.W4.3) — Cross-rule shared keyword vocabulary
//!   (deduplicates per-rule PHF byte storage).
//! - **last_byte_set** (AY.W4.3) — BoundedRegex narrowing —
//!   per-pattern LAST-byte sets to skip DFA walks early.

pub mod byte_class;
pub mod cost_model;
pub mod emit;
pub mod last_byte_set;
pub mod patterns;
pub mod phf;

// Re-exports for common use.
pub use cost_model::{CostModel, EmitOpts, LengthHint};
pub use emit::{
    RegexStrategy, emit_regex, emit_regex_direct_call, emit_regex_unsupported,
    is_fused_number_regex, is_fused_number_regex_cached, solve_regex_strategy,
};
