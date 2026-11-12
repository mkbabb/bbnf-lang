// B5.W1 — self-alias retired alongside the welded substrate
// boundary. Generated code uses `crate::runtime::*` paths uniformly
// inside the `bbnf` crate; downstream consumers (`bbnf-bootstrap`,
// integration tests) reach the same types via `bbnf::runtime::*`
// where `bbnf` is the cargo dependency, not the self-alias.
//
// sk-v27 LW3.COMP-1b: this file is written by hand at the spine merge. It
// exports every root module the published crate exports (the root pipeline
// stays the second oracle until LW4.COMP-1, quarry law X-1) and the spine's
// compiler (sk-v26 `b5fd0ca98`). `tests/root_api_present.rs` fails to
// compile if either half is dropped.

pub mod types;
pub use types::*;

/// Re-export parse_that types used by the grammar-generated enum.
pub use parse_that::Span;

pub mod grammar;

pub mod generate;
pub use generate::*;

pub mod backend;

pub mod graph;
pub use graph::*;

pub mod imports;

pub mod lower;
pub mod path;
pub mod pipeline;
pub mod runtime;

/// The deterministic BBNF source-to-direct-Rust compiler (the spine).
///
/// The compiler owns compile-time semantics only. The parsers it generates
/// depend on `parse_that`'s recognition kernel (`parse_that::kernel`), not on
/// this crate or a runtime parser graph. Its lint contract is the spine's
/// own crate-level one, kept on the module it governs.
#[deny(unsafe_code)]
#[deny(unnameable_types)]
pub mod compiler;

pub use compiler::{
    CompileOptions, CompiledModule, Diagnostic, DiagnosticCode, DiagnosticSet, GeneratedModule,
    GrammarInstanceInput, RuntimeBudget, StageResult, compile_instance,
};
