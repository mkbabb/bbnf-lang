//! Rust backend: driver-based code generation.
//!
//! The shared driver (`backend::driver`) walks IR and makes target-agnostic
//! decisions. The Rust emitter (`emitter/`) generates `proc_macro2::TokenStream`
//! for each decision.

pub mod analysis;
pub mod emitter;
pub mod emitter_types;
pub mod ir_enums;
pub mod ir_types;
pub mod view;

// Re-export shared utility so all `super::unescape_literal` references resolve.
pub use super::unescape_literal;
