//! Ref strategy selection — whether to emit a direct function call, inline
//! the body, or fuse the body at the call site.

use bbnf_ir::RuleId;

use crate::backend::CallStrategy;

/// Resolved strategy for a Ref node. Mirrors `CallStrategy` with an
/// explicit per-callsite resolution — the full decision lives on the
/// rule's metadata, but consumers may want to override it at specific
/// call sites (e.g., fusing @token rules at dispatch call sites).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RefStrategy {
    /// Direct function call to the rule's body.
    DirectCall,
    /// Inline the rule's body at the call site (small acyclic rules).
    InlineBody,
    /// Inline-fuse: body inlined, but the containing enum variant is
    /// preserved (used for @token rules).
    InlineFusion,
}

impl From<CallStrategy> for RefStrategy {
    fn from(cs: CallStrategy) -> Self {
        match cs {
            CallStrategy::DirectCall => RefStrategy::DirectCall,
            CallStrategy::InlineBody => RefStrategy::InlineBody,
            CallStrategy::InlineFusion => RefStrategy::InlineFusion,
        }
    }
}

/// Look up a Ref's strategy from the pre-computed call strategies.
pub fn resolve_ref_strategy(
    rule_id: RuleId,
    call_strategies: &[CallStrategy],
) -> RefStrategy {
    call_strategies
        .get(rule_id as usize)
        .copied()
        .map(RefStrategy::from)
        .unwrap_or(RefStrategy::DirectCall)
}
