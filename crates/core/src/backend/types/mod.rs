//! Shared types for the compilation driver and emitter trait.
//!
//! Tranche Y.1 promoted this from a single `backend/types.rs` file
//! into a directory so the `decisions` sub-module — previously
//! buried under `backend/patterns/` as the lone non-shim survivor —
//! can live alongside the other shared driver types without keeping
//! an entire legacy directory alive for one file.

pub mod decisions;

use bbnf_ir::{AltDispatch, TypeDesc};

// Re-export the authoritative upstream types from `bbnf-ir` at the
// shared driver-types surface. `DelimScanConfig` / `KeyClass` /
// `KeyDispatchConfig` / `DetectedBranch` / `KeyDispatchMatch` were
// moved upstream in Tranche X.8a. This re-export block is the sole
// module-local naming for those types — every backend sub-module
// imports them via `backend::types::*` or directly from `bbnf_ir::`.
pub use bbnf_ir::{
    DelimScanConfig, DetectedBranch, KeyClass, KeyDispatchConfig, KeyDispatchMatch,
    key_class_regex_pattern,
};

// ─── Compilation Driver Types ───────────────────────────────────────────────

/// A group of children within a `Seq` node, as classified by the driver.
pub enum SeqChildGroup<O> {
    /// A single non-Span child with its emitted output and projected type.
    Single { output: O, ty: TypeDesc },
    /// Consecutive Span-typed children compressed into one Span.
    SpanCompressed { outputs: Vec<O> },
}

/// Configuration for a `sep_by` loop, as detected by the driver.
pub struct SepByConfig {
    /// Whether to trim whitespace around elements and separators.
    pub ws: bool,
    /// Minimum element count (from `Repeat { lo, .. }`).
    pub lo: u32,
    /// If the sep_by is delimited, the closing delimiter's byte(s) for early-exit.
    pub terminator_bytes: Option<Vec<u8>>,
}

/// How a rule reference should be compiled.
///
/// Tranche Z.5: the former `InlineFusion` variant was deleted as a
/// ghost — it was defined here and consumed in two sites
/// (`driver/mod.rs` and `driver/reference.rs`, both treating it as
/// a synonym of `InlineBody`) but no producer ever constructed it.
/// `@token` rules become `InlineBody`; the actual fusion happens
/// upstream in `fuse_token_dispatch` (the IR pass that inlines the
/// body at every dispatch site). Y.13's consumer-invariant test was
/// extended to `CallStrategy` so the variant cannot drift back in.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CallStrategy {
    DirectCall,
    InlineBody,
}

/// Result type classification for a `Seq` node.
pub enum SeqResultStrategy {
    AllSpan,
    Mixed {
        result_type: TypeDesc,
        flatten: Option<FlattenStrategy>,
    },
}

/// How to flatten a `(T, Vec<T>)` pair into `Vec<T>`.
#[derive(Debug)]
pub enum FlattenStrategy {
    HeadThenVec,
    VecThenTail,
}

/// Which dispatch strategy the driver selected for an `Alt` node.
pub enum AltStrategy<'a> {
    AllLiteral,
    Dispatch { table: &'a AltDispatch },
    KeyDispatch,
    Checkpoint,
}

/// Information about a single Alt branch.
pub struct AltBranchInfo {
    pub ty: TypeDesc,
    pub coercion_variant: Option<String>,
    /// AM.3 per-branch tape surgery: `true` when the branch's codegen
    /// pushes child tape records (compound), `false` for leaf branches.
    /// The Rust alt emitter uses this to emit `push_leaf` vs
    /// `mark_children` + `push_compound` per arm.
    pub pushes_children: bool,
}

/// How allocation should be handled for a value.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ValuePlacement {
    Inline,
    Alloc,
}

// ─── Token Dispatch Types ──────────────────────────────────────────────────

/// A single compiled arm for token dispatch.
pub struct TokenDispatchArmCompiled<O> {
    /// Byte patterns that select this arm (resolved from `GrammarIR::strings`).
    pub patterns: Vec<Vec<u8>>,
    /// Optional guard byte that must follow the token.
    pub guard_byte: Option<u8>,
    /// Already-compiled continuation code for this arm.
    pub continuation: O,
}

// ─── Key Dispatch Types ────────────────────────────────────────────────────

/// A single compiled branch for key dispatch.
pub struct KeyDispatchBranch<O> {
    /// Byte patterns for the key(s) that select this branch.
    pub key_bytes: Vec<Vec<u8>>,
    /// Already-compiled full branch body.
    pub body: O,
    /// Branch metadata.
    pub info: AltBranchInfo,
}

// ─── Delimiter Scan Types ──────────────────────────────────────────────────
// `DelimScanConfig` moved to `bbnf_ir::types::recognizer_configs` in
// Tranche X.8a. Re-exported at the top of this module.
