//! Shared types for the compilation driver and emitter trait.

use bbnf_ir::{AltDispatch, TypeDesc};

// Tranche X.8a: `DelimScanConfig` and key-dispatch types (`KeyClass`,
// `KeyDispatchConfig`, `DetectedBranch`, `KeyDispatchMatch`) moved
// upstream into `bbnf-ir`. Re-exported here for backward compat until
// every caller migrates to the `bbnf_ir::` path.
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
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CallStrategy {
    DirectCall,
    InlineBody,
    InlineFusion,
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
