//! Fluent builders for common CSP / COP shapes.
//!
//! Each sub-module wraps a frequently-recurring constraint pattern in
//! an ergonomic, language-agnostic API. The builders are pure
//! convenience: every problem they construct can be expressed
//! directly via [`crate::Csp`] + the underlying constraint and domain
//! types — they exist so that the same Rust implementation can be
//! mirrored line-for-line into the Python and WASM bindings without
//! re-deriving the wiring at every consumer.

pub mod assignment;

pub use assignment::{AssignmentBuilder, AssignmentError, AssignmentSolution, assignment};
