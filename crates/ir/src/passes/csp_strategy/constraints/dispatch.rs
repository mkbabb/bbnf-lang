//! `DispatchAuthority` — pin Alt decision variables to the dispatch
//! choice implied by upstream key-dispatch / token-dispatch facts.
//!
//! # Rationale
//!
//! The Alt dispatch decision (`AltMode::Checkpoint | ByteDispatch |
//! KeyDispatch`) is consumed by:
//!
//! - `crates/core/src/backend/strategy/alt_strategy.rs:140-186`
//!   reads `ir.recognizer_decisions[id].alt_mode` to map the CSP
//!   answer to `AltStrategy` for the emitter.
//! - The recognizer dispatch surface in
//!   `crates/ir/src/passes/recognizers/shape_dispatch/` consumes
//!   the same `recognizer_decisions` map to choose between
//!   structural shape-dispatch templates.
//!
//! Without this installer, when the upstream pass populated
//! `ir.key_dispatch_configs[id]` for an Alt, the CSP cost
//! model still had `Checkpoint` and `ByteDispatch` in the domain
//! and could pick either. The consumer at
//! `alt_strategy.rs:161-166` then *re-overrode* the CSP's answer
//! by checking `ir.key_dispatch_configs.contains_key(&id)`. That
//! sidecar override is exactly the "consumer reads from sidecar
//! comments or per-call recomputation" anti-pattern the AZ-III
//! W3b plan calls out.
//!
//! Installing the pin moves the authority into the CSP fact
//! surface: `ir.recognizer_decisions[id].alt_mode = KeyDispatch`
//! whenever `ir.key_dispatch_configs.contains_key(&id)`. The
//! consumer can then drop the sidecar override.
//!
//! # Producer
//!
//! - `ir.key_dispatch_configs` populated by the recognizer
//!   `key_dispatch` miner (`crate::passes::recognizers`).
//! - `IrNode::Alt(_, dispatch)` carries an inline
//!   `AltDispatch` payload populated by `fuse_token_dispatch`,
//!   surfaced through the per-node `RecognizerShape` shape
//!   facts.
//!
//! # Consumer
//!
//! `crates/core/src/backend/strategy/alt_strategy.rs::decide_alt_strategy`
//! reads `ir.recognizer_decisions[id].alt_mode`. Once the
//! dispatch pin lands, the consumer's sidecar lookup at
//! `alt_strategy.rs:161-166` and `:176-183` can be deleted in
//! favor of a single CSP read.

use csp_solver::Csp;

use super::ConstraintCtx;
use crate::GrammarIR;
use crate::passes::csp_strategy::{AltMode, StrategyDomain, StrategyValue};

/// Install the `DispatchAuthority` constraint. Returns the number
/// of pin constraints installed.
pub fn install(ctx: &ConstraintCtx<'_>, csp: &mut Csp<StrategyDomain>, ir: &GrammarIR) -> usize {
    let mut count = 0usize;

    for (&node_id, &var) in ctx.alt_vars.iter() {
        let target = pin_for_dispatch(ir, node_id);
        if let Some(mode) = target {
            // Single canonical pin primitive shared with the
            // csp-solver crate (`Csp::add_equals`). W3b.4 alignment
            // — the IR consumer talks to the solver through one
            // surface, not two.
            csp.add_equals(var, StrategyValue::Alt(mode));
            count += 1;
        }
    }

    count
}

/// Resolve the dispatch pin for an Alt node. Priority:
///
/// 1. `key_dispatch_configs[id]` ⇒ KeyDispatch (structural
///    detector has higher coverage than recognizer-shape facts).
/// 2. `keyword_branches[id]` ⇒ KeyDispatch (keyword statistics
///    miner authoritative for keyword-led alts).
fn pin_for_dispatch(ir: &GrammarIR, node_id: crate::dag::NodeId) -> Option<AltMode> {
    if ir.key_dispatch_configs.contains_key(&node_id) {
        return Some(AltMode::KeyDispatch);
    }
    if ir.keyword_branches.contains_key(&node_id) {
        return Some(AltMode::KeyDispatch);
    }
    None
}
