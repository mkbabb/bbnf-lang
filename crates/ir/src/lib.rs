//! Canonical Grammar IR for the BBNF compiler pipeline.
//!
//! This crate defines the post-analysis intermediate representation consumed by all
//! backends: Rust codegen (AOT), WASM executor (bytecode VM), TS interpreter, and
//! the pretty-printing backend.
//!
//! The IR is fully owned (no lifetimes), serializable via MessagePack for WASM
//! boundary transfer, and captures all analysis results (FIRST sets, SCC info,
//! dispatch hints, span eligibility, pretty hints) so backends need not recompute them.
//!
//! `lib.rs` is a re-export hub. Type definitions live under [`types`]; the
//! crate root re-exports them so the existing `bbnf_ir::IrNode` etc. paths
//! remain stable.

pub mod dag;
pub mod egraph;
pub mod passes;
pub mod recognizer;
pub mod types;
pub mod vm;

// Re-export from bbnf-regex (canonical source of CharSet128, regex_first, classify)
pub use bbnf_regex::sets::charset::CharSet128;
pub mod regex_first {
    //! FIRST set extraction — delegates to bbnf_regex.
    pub use bbnf_regex::first::regex_first_chars;
}

pub use vm::bytecode;
pub use vm::compiler;
pub use vm::debug;
pub use vm::interpreter;

// IR type re-exports (from `types/`).
pub use types::{
    AltBranch, AltDispatch, DispatchHint, FnDescriptor, FnId, GrammarIR, GrammarSpan, IrNode,
    IrRule, MapBinOp, MapExpr, MapUnaryOp, MemoStrategy, PrettyHints, RuleDirectives, RuleId,
    RuleMeta, StringId, SubVariant, TokenDispatchArm, TypeDesc, parse_sep_hint, parse_split_hint,
};
