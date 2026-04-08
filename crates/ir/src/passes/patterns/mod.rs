//! Structural pattern recognition pass.
//!
//! Recognizes optimization-relevant patterns on IR nodes and annotates
//! them in `GrammarIR::pattern_annotations`. The codegen driver reads
//! these annotations instead of re-detecting patterns at emit time.

mod recognize;

pub use recognize::recognize_patterns;

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::{RuleId, StringId};

/// Pattern annotation for an alternation node.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum AltPattern {
    /// Standard checkpoint-based fallback.
    CheckpointFallback,
    /// All branches have disjoint FIRST sets → byte dispatch table.
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
    /// Normal sequence — no special structure.
    Normal,
    /// Binary operator chain: `head (op rhs)*`.
    OperatorChain,
    /// All children are Span-typed leaves → collapse to single Span.
    AllSpanCollapse,
}

/// Key classification for key-dispatch patterns.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum KeyClass {
    Identifier,
    QuotedString { quote_char: u8 },
}

/// Per-rule pattern annotations computed by the recognition pass.
#[derive(Serialize, Deserialize, Clone, Debug, Default)]
pub struct PatternAnnotations {
    /// Pattern for the rule's body Alt node (if applicable).
    pub alt_pattern: Option<AltPattern>,
    /// Pattern for the rule's body Seq node (if applicable).
    pub seq_pattern: Option<SeqPattern>,
    /// Whether this rule is an operator chain at the top level.
    pub is_operator_chain: bool,
}

/// Map from RuleId → pattern annotations.
pub type PatternMap = HashMap<RuleId, PatternAnnotations>;
