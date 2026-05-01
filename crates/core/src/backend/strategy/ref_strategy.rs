//! Ref strategy selection — whether to emit a direct function call or
//! inline the body at the call site.
//!
//! Tranche Z.5: the former `InlineFusion` variant on both `RefStrategy`
//! and `CallStrategy` was a ghost — defined and pattern-matched in two
//! consumer sites that treated it as a synonym of `InlineBody`, but no
//! producer ever constructed it. The actual `@token` fusion happens
//! upstream in `fuse_token_dispatch` (the IR pass that inlines the body
//! at every dispatch site); the per-Ref decision then collapses to
//! `DirectCall` vs `InlineBody`. Y.13's consumer-invariant test was
//! extended to `CallStrategy` so the variant cannot drift back in.

use bbnf_ir::RuleId;

use crate::backend::CallStrategy;

/// Resolved strategy for a Ref node. Mirrors `CallStrategy` with an
/// explicit per-callsite resolution — the full decision lives on the
/// rule's metadata, but consumers may want to override it at specific
/// call sites.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RefStrategy {
    /// Direct function call to the rule's body.
    DirectCall,
    /// Inline the rule's body at the call site (small acyclic rules).
    InlineBody,
}

impl From<CallStrategy> for RefStrategy {
    fn from(cs: CallStrategy) -> Self {
        match cs {
            CallStrategy::DirectCall => RefStrategy::DirectCall,
            CallStrategy::InlineBody => RefStrategy::InlineBody,
        }
    }
}

/// Look up a Ref's strategy from the pre-computed call strategies.
pub fn resolve_ref_strategy(rule_id: RuleId, call_strategies: &[CallStrategy]) -> RefStrategy {
    call_strategies
        .get(rule_id as usize)
        .copied()
        .map(RefStrategy::from)
        .unwrap_or(RefStrategy::DirectCall)
}
