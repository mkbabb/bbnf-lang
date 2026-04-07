//! Pattern detection and analysis utilities.
//!
//! Separates pattern recognition (what IS this pattern?) from code emission
//! (how do I EMIT code for it?). Each sub-module provides detection and
//! analysis functions consumed by the `emit/` sub-module.

pub mod char_class;
pub mod shorthand;

// Re-exports for convenience.
pub use char_class::{CharClassAnalysis, CharClassStrategy};
pub use shorthand::{ShorthandClass, detect_from_bytes, detect_from_ranges, emit_predicate};
