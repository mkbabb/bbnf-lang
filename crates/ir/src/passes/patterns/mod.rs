//! Structural facts pass — per-node shape recognition.
//!
//! Recognizes optimization-relevant structural patterns on IR nodes and stores
//! them as `NodeFacts` in `GrammarIR::node_facts`, keyed by pointer identity.
//! The backend driver reads these facts instead of re-detecting patterns.
//!
//! ## Invariant
//!
//! NodeFacts are valid if and only if:
//! 1. Recomputed after all IR rewrites (no stale pointers).
//! 2. No consumer mutates or rebuilds IrNode trees afterward.
//! 3. Never serialized (pointer keys are process-local).
//! 4. Tests cover optimized and non-optimized equivalent shapes.

mod recognize;

pub use recognize::recognize_patterns;

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::{RuleId, StringId};

// ── Legacy types (kept for backward compat during migration) ─────────────

/// Pattern annotation for an alternation node.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum AltPattern {
    /// Standard checkpoint-based fallback.
    CheckpointFallback,
    /// All branches have disjoint FIRST sets -> byte dispatch table.
    DispatchTable,
    /// Branches keyed by leading literal (identifier or quoted string).
    KeyDispatch {
        key_class: KeyClass,
        separator: Option<StringId>,
    },
}

/// Pattern annotation for a sequence node.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum SeqPattern {
    /// Normal sequence.
    Normal,
    /// Binary operator chain: `head (op rhs)*`.
    OperatorChain,
    /// All children are Span-typed leaves.
    AllSpanCollapse,
}

/// Key classification for key-dispatch patterns.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum KeyClass {
    Identifier,
    QuotedString { quote_char: u8 },
}

/// Per-rule pattern annotations (legacy, will be replaced by NodeFacts).
#[derive(Serialize, Deserialize, Clone, Debug, Default)]
pub struct PatternAnnotations {
    pub alt_pattern: Option<AltPattern>,
    pub seq_pattern: Option<SeqPattern>,
    pub is_operator_chain: bool,
}

/// Map from RuleId -> pattern annotations (legacy).
pub type PatternMap = HashMap<RuleId, PatternAnnotations>;

// ── New: per-node structural facts ───────────────────────────────────────

/// Classifies the IR node kind for consumer contracts.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NodeKind {
    Alt,
    Seq,
    Wrap,
    Repeat,
    Skip,
    Leaf,
}

/// Structural fact for a single IR node, keyed by pointer identity.
///
/// Only nodes with recognized patterns get a NodeFacts entry.
#[derive(Clone, Debug)]
pub struct NodeFacts {
    /// Explicit node kind — consumer contract documentation.
    pub node_kind: NodeKind,
    /// Binary operator chain: `Seq([head, Repeat(Seq([op, rhs]), 0, MAX)])`.
    pub operator_chain: bool,
    /// Sep-by pattern: `Skip(element, Repeat(separator, 0, 1))`.
    pub sep_by: bool,
    /// All children are simple span leaves (Literal, Regex, Epsilon).
    pub all_span_collapse: bool,
}

/// Map from node pointer -> structural facts.
pub type NodeFactsMap = HashMap<usize, NodeFacts>;
