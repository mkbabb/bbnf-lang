//! Deterministic BBNF source-to-direct-Rust compiler.
//!
//! The production crate owns compile-time semantics only. Generated parsers
//! depend on `parse_that`, not on this crate or a runtime parser graph.

#![deny(unsafe_code)]

pub mod compiler;

pub use compiler::{
    CompilationSession, CompileOptions, CompiledModule, Diagnostic, DiagnosticCode, DiagnosticSet,
    EditError, GeneratedModule, ModuleInput, RuntimeBudget, StageResult, compile_module,
};
