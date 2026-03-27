//! Regex-to-inline-code emission via regex-syntax HIR.
//!
//! Replaces ad-hoc pattern detection with principled HIR walking.
//! The monolithic codegen path uses this to emit direct byte operations
//! for regex patterns, eliminating SpanParser combinator overhead.

mod fallback;
mod hir_walk;

pub use fallback::emit_regex_lazy_static;
pub use hir_walk::try_emit_regex_inline;
