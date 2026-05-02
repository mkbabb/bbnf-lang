//! Inline-substitution trace — the rule-rewriting sidecar consumed by
//! the AZ-IV.W2.2 `path_check` IR pass.
//!
//! The structural normalizer loop (`pipeline::compile`'s
//! `structural_normalizer_loop` span) runs `inline_acyclic` +
//! `fuse_single_use` to fixed point. Each pass walks every rule body
//! and replaces `Ref(id)` nodes with the target rule's body when the
//! target is small / single-use / non-cyclic. After the loop converges
//! (and `prune_unreachable` removes the now-dead source rules), the
//! `StructRegistry` populated by `project_types` no longer contains a
//! layout entry for the substituted rule's name — the user-written
//! source name has been **absorbed** by some other rule's layout.
//!
//! The W2 invariant 8 ("Path resolution uses source rule names") demands
//! that paths the user wrote against source-rule names continue to
//! resolve after the pipeline. The mechanism is this sidecar: every
//! `Ref(source) → body` substitution is recorded as an
//! [`InlineSubstitution`] event tagging which absorber rule's body now
//! contains the substituted body. Downstream the `path_check` pass
//! folds the events into a `source_rule -> absorber_rule` resolver map.
//!
//! ## Determinism
//!
//! The trace is a `Vec<InlineSubstitution>` and lists events in the
//! order the recording wrappers see them — outer rule iteration order
//! ascending by `RuleId`, then per-pass call order. The same grammar
//! IR + same pass sequence produces a byte-identical trace, which the
//! W2.2 golden test exercises. The trace is **additive**: re-running
//! the recording wrappers on the same IR appends new events without
//! mutating prior ones; consumers should clear the trace between
//! independent compile runs.
//!
//! ## Design discipline
//!
//! Per the W2 triumvirate-dispatch rule, the inline trace must not
//! change the semantics of `inline_acyclic` / `fuse_single_use`.
//! Recording is therefore implemented as opt-in `with_trace` wrappers
//! around the existing passes — the bare functions stay byte-identical
//! to their pre-W2.2 behavior; only the wrapper variants populate the
//! trace. The structural normalizer loop calls the wrapper variants so
//! every production compile has the trace available; tests that drive
//! a single pass in isolation can still call the bare function with no
//! trace overhead.

use serde::{Deserialize, Serialize};

use crate::types::RuleId;

/// Which inline pass produced an [`InlineSubstitution`] event.
#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum InlinePass {
    /// `inline_acyclic` — small acyclic non-entry rules below
    /// `INLINE_THRESHOLD` get inlined at every call site.
    InlineAcyclic,
    /// `fuse_single_use` — non-cyclic non-entry rules with reference
    /// count exactly 1 get inlined at their sole call site.
    FuseSingleUse,
}

/// One `Ref(source) → body` substitution event.
///
/// Records the source rule whose body was inlined into an absorber
/// rule's body during the structural normalizer loop. The substitution
/// is the substrate the W2.2 `path_check` pass uses to re-resolve a
/// `path!` literal that names the source rule after `prune_unreachable`
/// removes its top-level layout entry.
///
/// `source_rule_name` is the user-written rule identifier (interned in
/// `GrammarIR::strings` at the substitution moment). `absorber_rule_name`
/// names the rule whose body grew to contain the substituted body.
/// Both names are stored owned so the trace survives renames /
/// re-interning across passes.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash)]
pub struct InlineSubstitution {
    /// `RuleId` of the rule whose body was inlined into the absorber.
    /// Stable across the life of one `GrammarIR` instance.
    pub source_rule_id: RuleId,
    /// User-written name of the source rule (snapshot at substitution
    /// time; survives `prune_unreachable`).
    pub source_rule_name: String,
    /// `RuleId` of the rule whose body now carries the inlined source.
    /// May itself be inlined later in the same loop iteration; the
    /// trace captures every event, so chained substitutions show up as
    /// multiple records.
    pub absorber_rule_id: RuleId,
    /// User-written name of the absorber rule.
    pub absorber_rule_name: String,
    /// Which structural-normalizer pass produced this event.
    pub pass: InlinePass,
}

impl InlineSubstitution {
    /// Construct a substitution event with the canonical field set.
    pub fn new(
        source_rule_id: RuleId,
        source_rule_name: impl Into<String>,
        absorber_rule_id: RuleId,
        absorber_rule_name: impl Into<String>,
        pass: InlinePass,
    ) -> Self {
        Self {
            source_rule_id,
            source_rule_name: source_rule_name.into(),
            absorber_rule_id,
            absorber_rule_name: absorber_rule_name.into(),
            pass,
        }
    }
}

/// Append-only sidecar of [`InlineSubstitution`] events.
///
/// Population: the `with_trace` wrappers around `inline_acyclic` /
/// `fuse_single_use` push events as they observe per-rule substitutions.
/// Iteration: events appear in stable insertion order
/// (`RuleId`-ascending, then per-pass call order); the W2.2 golden test
/// exercises this stability.
///
/// Consumers (W2.2 `path_check`, W2.4 `path!` macro) treat the trace
/// as read-only; the IR pass owns the population side, and downstream
/// only reads.
#[derive(Serialize, Deserialize, Clone, Debug, Default, PartialEq, Eq)]
pub struct InlineTrace {
    /// Substitution events in the order the recording wrappers saw
    /// them.
    pub events: Vec<InlineSubstitution>,
}

impl InlineTrace {
    /// Construct an empty trace.
    pub fn new() -> Self {
        Self::default()
    }

    /// Append a substitution event.
    pub fn record(&mut self, event: InlineSubstitution) {
        self.events.push(event);
    }

    /// Number of recorded events.
    pub fn len(&self) -> usize {
        self.events.len()
    }

    /// True when no substitution events have been recorded.
    pub fn is_empty(&self) -> bool {
        self.events.is_empty()
    }

    /// Iterate substitution events in insertion order.
    pub fn iter(&self) -> impl Iterator<Item = &InlineSubstitution> {
        self.events.iter()
    }

    /// Drop every event. Use between independent compile runs to keep
    /// the trace from accumulating across grammars.
    pub fn clear(&mut self) {
        self.events.clear();
    }

    /// Return every absorber rule for `source_rule_name` in insertion
    /// order. A source rule may end up substituted into more than one
    /// absorber when it was acyclic-inlined (which copies into every
    /// caller); single-use fuse always produces exactly one absorber.
    pub fn absorbers_for(&self, source_rule_name: &str) -> Vec<&InlineSubstitution> {
        self.events
            .iter()
            .filter(|e| e.source_rule_name == source_rule_name)
            .collect()
    }

    /// First absorber rule id for `source_rule_name`, if any.
    /// Convenience for the `path_check` resolver map: a source rule
    /// that was substituted at all has at least one absorber, and the
    /// resolver picks the first one (deterministic by insertion order).
    pub fn first_absorber_id(&self, source_rule_name: &str) -> Option<RuleId> {
        self.events
            .iter()
            .find(|e| e.source_rule_name == source_rule_name)
            .map(|e| e.absorber_rule_id)
    }
}
