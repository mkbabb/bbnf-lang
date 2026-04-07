//! Full analysis + IR lowering pipeline.
//!
//! This module is a thin facade over the loader, validation, and compilation
//! submodules under `pipeline/`.

use std::fmt;

use bbnf_ir::GrammarIR;

pub mod compile;
pub mod loader;
pub mod validate;

pub use compile::{
    compile_ast, compile_ast_request, compile_grammar, compile_grammar_request,
    compile_paths_request, compute_call_strategies,
};

/// Options for the compilation pipeline.
#[derive(Clone, Debug, Default)]
pub struct PipelineOptions {
    /// Whether to apply left-recursion elimination.
    pub remove_left_recursion: bool,
    /// Override the entry rule name. If `None`, defaults to the last rule in source order.
    pub entry_rule: Option<String>,
}

/// Backend-specific compilation target.
#[derive(Clone, Debug)]
pub enum CompileTarget {
    /// Prepare the grammar for Rust codegen (monolithic recursive descent).
    Rust { requested_prettify: bool },
    /// Produce a VM-ready `GrammarIR`.
    Vm,
    /// Emit TypeScript source code (direct recursive descent).
    Ts,
    /// Emit a WASM module directly via wasm-encoder.
    Wasm,
}

impl Default for CompileTarget {
    fn default() -> Self {
        Self::Vm
    }
}

/// Shared compilation request.
#[derive(Clone, Debug, Default)]
pub struct CompileRequest {
    pub options: PipelineOptions,
    pub target: CompileTarget,
}

/// Output of a compilation request.
#[derive(Debug)]
pub enum CompileOutput {
    Rust(crate::backend::PreparedGrammar),
    Vm(GrammarIR),
    /// Generated TypeScript source code.
    Ts(String),
    /// Generated WASM module bytes.
    Wasm(Vec<u8>),
}

/// Structured pipeline/compiler failure.
#[derive(Debug, Clone)]
pub enum CompileError {
    Parse(String),
    Import(String),
    UnknownPrettyHint { rule: String, hint: String },
    UndefinedPrettyRule { rule: String },
    UnknownNonterminal { rule: String, name: String },
    InvalidProductionRule { rule: String },
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Parse(msg) => write!(f, "parse error: {msg}"),
            Self::Import(msg) => write!(f, "import error: {msg}"),
            Self::UnknownPrettyHint { rule, hint } => {
                write!(f, "unknown @pretty hint `{hint}` on rule `{rule}`")
            }
            Self::UndefinedPrettyRule { rule } => {
                write!(f, "`@pretty` targets undefined rule `{rule}`")
            }
            Self::UnknownNonterminal { rule, name } => {
                write!(f, "rule `{rule}` references unknown nonterminal `{name}`")
            }
            Self::InvalidProductionRule { rule } => {
                write!(
                    f,
                    "rule `{rule}` contains a production rule in expression position"
                )
            }
        }
    }
}

impl std::error::Error for CompileError {}
