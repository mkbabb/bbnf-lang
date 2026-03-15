//! Prettify code generation utilities.
//!
//! Provides shared helpers (`hints`, `heuristics`, `prettify_utils`, `source_range`,
//! `to_doc`) consumed by the IR-based prettify generator (`ir_pretty.rs`).

pub(crate) mod prettify_utils;
pub use prettify_utils::*;

pub mod heuristics;
pub mod hints;
pub(crate) mod source_range;
pub(crate) mod to_doc;
