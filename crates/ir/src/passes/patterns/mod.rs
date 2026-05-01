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

/// Structural fact for a single IR node, keyed by stable `NodeId`.
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
    /// Tranche V: the recognizer record for this node, if one applies.
    /// One field, not a sidecar — every grammar-tier recognizer fact
    /// lives here. Populated by `passes::recognizers::mine_recognizers`.
    pub recognizer: Option<Recognizer>,
}

/// Map from `NodeId` -> structural facts.
pub type NodeFactsMap = HashMap<crate::dag::NodeId, NodeFacts>;

// ── Tranche V: Recognizer record ─────────────────────────────────────────
//
// One struct, one field on NodeFacts. All grammar-tier recognizer
// information lives in `Recognizer`. The regex-tier classification is
// pointed to via `RecognizerShape::Regex { sid }` — there is no
// duplication; the regex fact in `bbnf_regex::RegexInfo` is referenced.

/// Group identifier for cross-rule recognizer sharing. Sites with the
/// same `RecognizerSignature` hash collapse into one peer group; each
/// group is assigned a fresh `GroupId`.
pub type GroupId = u32;

/// One-pass grade for the recognizer's body.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum OnePassGrade {
    /// Recognizer is one-pass: no backtracking, no lookahead.
    OnePass,
    /// Recognizer requires bounded restart but not full backtracking.
    RestartSafe,
    /// Recognizer requires general backtracking.
    NotOnePass,
}

/// Output shape of the emitted helper for kernel hashing.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum OutputShape {
    /// Helper returns `Option<Span>`.
    SpanOnly,
    /// Helper returns `Option<(Span, f64)>` (numeric conversion).
    SpanPlusF64,
    /// Helper returns `Option<()>` (presence-only check).
    None,
}

/// Side of a delimited recognizer.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Side {
    Open,
    Close,
}

/// Architectural role this node plays inside its enclosing recognizer.
#[derive(Clone, Debug)]
pub enum RecognizerRole {
    /// Standalone recognizer with no enclosing role.
    Standalone,
    /// Body of an enclosing recognizer (e.g., the inner of a Wrap).
    Body { parent: crate::dag::NodeId },
    /// Leading recognizer of an enclosing structure (e.g., first Alt branch).
    Leading { within: crate::dag::NodeId },
    /// Trailing recognizer of an enclosing structure.
    Trailing { within: crate::dag::NodeId },
    /// Open or close delimiter of an enclosing Wrap.
    Delimiter {
        side: Side,
        owner: crate::dag::NodeId,
    },
    /// Dispatch key for an enclosing Alt.
    DispatchKey { owner: crate::dag::NodeId },
}

/// Shape of the recognizer — what kind of kernel emits it.
///
/// Grammar-domain shapes (DelimiterBalanced, SeparatorList,
/// TokenLedBranches, KeywordPrefix) compose regex-tier classifications
/// via `Regex { sid }` references. The `sid` indexes
/// `GrammarIR::regex_info` to read the underlying `RegexInfo`.
#[derive(Clone, Debug)]
pub enum RecognizerShape {
    /// Wrap with single-byte literal delimiters and a balanced inner.
    DelimiterBalanced {
        open: u8,
        close: u8,
        inner: Box<Recognizer>,
    },
    /// `element (sep element)*` (or trailing-separator variant).
    SeparatorList {
        element: Box<Recognizer>,
        separator: u8,
        trailing: bool,
    },
    /// Alt branches with disjoint FIRST sets, key-led dispatch.
    TokenLedBranches {
        key: Box<Recognizer>,
        branch_count: u16,
    },
    /// Fixed leading literal followed by a disjoint tail.
    KeywordPrefix {
        bytes: smallvec::SmallVec<[u8; 8]>,
        disjoint_tail: bool,
    },
    /// Pure regex — points at the per-pattern `RegexInfo` via StringId.
    /// The regex-domain classification (`RegexClass`) is read from
    /// `ir.regex_info[sid].classification`.
    Regex { sid: StringId },
    /// Punctuation+whitespace region cluster, e.g. JSON object/array
    /// structural bytes (`,`, `:`, `{`, `}`, `[`, `]`) with surrounding
    /// whitespace. The byte set is the cluster of punctuation bytes the
    /// scanner consumes in one pass.
    ///
    /// The three sibling family shapes that landed in Tranche X.10
    /// (`FunctionHead`, `HashPrefix`, `UnitTail`) were deleted in
    /// Tranche Y.4 — they produced zero matches on every production
    /// grammar (CSS L4, JSON, BBNF, Sheets, EBNF), and in the
    /// FunctionHead case the kernel's short-circuit emission
    /// semantics were structurally incompatible with CSS functions
    /// that have bodies. `PunctWsRegion` is the only family in the
    /// X.10/X.11b slate that earned its keep.
    PunctWsRegion { puncts: smallvec::SmallVec<[u8; 8]> },
}

/// Canonical hash key for kernel dedup/hoisting.
///
/// Two recognizers that hash equal collapse to the SAME emitted helper
/// function. The hash is computed deterministically from byte/class/
/// shape data only — never from `NodeId` or `StringId` — so dedup is
/// stable across compile sessions.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct RecognizerSignature {
    /// Canonical 64-bit hash over the recognizer shape.
    pub shape_hash: u64,
    pub output_shape: OutputShape,
    pub advance_on_failure: bool,
    pub grade: OnePassGrade,
}

/// The recognizer record carried by `NodeFacts.recognizer`.
///
/// Tranche Y.2: the `peer_group: Option<GroupId>` field was deleted
/// along with the `AltMode::SharedHelper` / `WrapMode::SharedHelper`
/// CSP variants and the `prefix_shared_group::mine` pass that
/// populated it. The substrate was ghost infrastructure — the
/// hoisting decisions it enabled never had a backend emission path,
/// and the mining cost paid ~5–8 KB of work per compile for zero
/// consumers. `RecognizerSignature.shape_hash` is retained because
/// downstream canonicalization still uses it for signature dedup
/// within the consumer-invariant tests.
#[derive(Clone, Debug)]
pub struct Recognizer {
    pub role: RecognizerRole,
    pub shape: RecognizerShape,
    pub signature: RecognizerSignature,
}
