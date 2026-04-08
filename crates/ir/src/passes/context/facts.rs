//! Context facts — structural roles propagated over the DAG.

use crate::dag::NodeId;
use std::collections::HashMap;

/// How strongly a node discriminates among alternatives.
///
/// Used by Alt strategy selection: nodes with `Strong` discrimination
/// can drive dispatch tables; `Weak` nodes need checkpointing.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum DiscriminationStrength {
    /// Leading literal or regex with disjoint FIRST sets from siblings.
    Strong,
    /// Leading with unique-first-byte but overlapping continuations.
    Medium,
    /// No clear discriminator — sequential fallback required.
    #[default]
    Weak,
}

/// Whether a node is safe to scan through for delim-scan / sep-by patterns.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum ScanSafety {
    /// Node is bounded by a known close-byte and can be flat-scanned.
    Safe,
    /// Scanning might overshoot — must recurse.
    #[default]
    Unsafe,
}

/// Per-node propagated context facts.
///
/// Keyed by `NodeId` from the canonical grammar DAG.
#[derive(Clone, Debug, Default)]
pub struct ContextFacts {
    pub discrimination: DiscriminationStrength,
    pub scan_safety: ScanSafety,
    /// Whether this node is inside a `@recover` sync expression.
    pub in_recovery_context: bool,
    /// Whether this node is the direct child of a TokenDispatch arm.
    pub in_token_dispatch: bool,
}

/// Per-grammar map from `NodeId` to `ContextFacts`.
pub type ContextFactsMap = HashMap<NodeId, ContextFacts>;
