//! Regex classification, compilation, and inline code emission.
//!
//! Layered architecture:
//! - **classify** — HIR-based structural regex classification (Numeric, HexDigits, Identifier, etc.)
//! - **cost_model** — Centralized heuristic thresholds (CostModel, LengthHint, EmitOpts).
//! - **emit/** — Tiered code emission via pattern registry (fast_paths → hir → dfa → error).
//! - **patterns/** — Pattern detection and analysis utilities.

pub mod classify;
pub mod cost_model;
pub mod emit;
pub mod patterns;

// Re-exports for common use.
pub use cost_model::{CostModel, EmitOpts, LengthHint};
pub use emit::{
    RegexTier, audit_regex_pattern, emit_regex, emit_regex_direct_call, emit_regex_unsupported,
    is_fused_number_regex,
};
