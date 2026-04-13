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

pub mod cost_config;
pub mod dag;
pub mod egraph;
pub mod passes;
pub mod recognizer;
pub mod types;
pub mod vm;

pub use cost_config::CostConfig;

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
    AltBranch, AltDispatch, BucketProbe, DelimScanConfig, DetectedBranch, DispatchHint,
    FnDescriptor, FnId, GrammarIR, GrammarSpan, IrNode, IrRule, KeyClass, KeyDispatchConfig,
    KeyDispatchMatch, KeyEntry, KeyIndex, LengthBucket, MapBinOp, MapExpr, MapUnaryOp,
    MemoStrategy, PrettyHints, RuleDirectives, RuleId, RuleMeta, StringId, SubVariant,
    TokenDispatchArm, TypeDesc, TypeDescId, TypeDescInterner, key_class_regex_pattern,
    parse_sep_hint, parse_split_hint,
};
