//! Deterministic BBNF source-to-direct-Rust compiler.
//!
//! The production crate owns compile-time semantics only. Generated parsers
//! depend on `parse_that`, not on this crate or a runtime parser graph.

#![deny(unsafe_code)]
#![deny(unnameable_types)]

pub mod compiler;

pub use compiler::{
    CompileOptions, CompiledModule, Diagnostic, DiagnosticCode, DiagnosticSet, GeneratedModule,
    GrammarInstanceInput, RuntimeBudget, StageResult, compile_instance,
};
