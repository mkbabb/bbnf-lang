//! Canonical IR type definitions.
//!
//! This directory module consolidates the pure type definitions consumed by
//! every backend (Rust codegen, bytecode VM, TS interpreter, prettify) and by
//! every analysis pass under `crate::passes`. None of these types depend on a
//! pass implementation — only on each other and on `bbnf_regex` for the
//! `CharSet128` substrate.
//!
//! Sub-module map:
//!
//! - [`node`] — `IrNode`, `AltBranch`, `AltDispatch`, `TokenDispatchArm`,
//!   `GrammarSpan`, plus the `IrNode` walking helpers and `count_nodes`.
//! - [`map_expr`] — `MapExpr`, `MapBinOp`, `MapUnaryOp` and the constant-fold
//!   / introspection impl.
//! - [`fn_descriptor`] — `FnDescriptor`, the host-function descriptor enum.
//! - [`rule`] — `IrRule`, `RuleMeta`, `RuleDirectives`, `MemoStrategy`,
//!   `DispatchHint`, `PrettyHints`, `SubVariant`, plus the `parse_sep_hint` /
//!   `parse_split_hint` helpers.
//! - [`type_desc`] — `TypeDesc`, the backend-agnostic type descriptor.
//! - [`grammar`] — `GrammarIR`, the top-level container, its accessors, and
//!   MessagePack/JSON serialization.
//!
//! `lib.rs` re-exports everything in this directory at the crate root so the
//! existing `bbnf_ir::IrNode` etc. paths remain stable.

pub mod fn_descriptor;
pub mod grammar;
pub mod map_expr;
pub mod node;
pub mod recognizer_configs;
pub mod rule;
pub mod type_desc;
pub mod type_desc_interner;

pub use fn_descriptor::FnDescriptor;
pub use grammar::GrammarIR;
pub use map_expr::{MapBinOp, MapExpr, MapUnaryOp};
pub use node::{
    AltBranch, AltDispatch, GrammarSpan, IrNode, TokenDispatchArm, count_nodes,
};
pub use recognizer_configs::{
    BucketProbe, DelimScanConfig, DetectedBranch, KeyClass, KeyDispatchConfig,
    KeyDispatchMatch, KeyEntry, KeyIndex, LengthBucket, key_class_regex_pattern,
};
pub use rule::{
    DispatchHint, IrRule, MemoStrategy, PrettyHints, RuleDirectives, RuleMeta, SubVariant,
    parse_sep_hint, parse_split_hint,
};
pub use type_desc::TypeDesc;
pub use type_desc_interner::{TypeDescId, TypeDescInterner};

// ── Identifier aliases ──────────────────────────────────────────────────────

/// Index into `GrammarIR::rules`.
pub type RuleId = u32;

/// Index into `GrammarIR::strings` interning table.
pub type StringId = u32;

/// Index into `GrammarIR::fns` host function table.
pub type FnId = u32;
