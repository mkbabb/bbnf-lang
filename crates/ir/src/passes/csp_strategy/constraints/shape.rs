//! `ShapeTemplateAuthority` — pin Alt/Wrap decision variables to the
//! strategy implied by an admitted shape-dictionary template.
//!
//! # Rationale
//!
//! `ir.shape_dict_templates` is populated by
//! [`crate::passes::recognizers::shape_dict::ShapeDictMiner`]: each
//! eligible compound subtree (Alt / Wrap with a fixed leaf-hole
//! pattern) gets a `(NodeId, ShapeTemplate)` entry. Without a
//! constraint installer, the CSP saw the template pool as inert
//! sidecar data — the per-site Alt / Wrap variables were free to pick
//! any cost-min mode, and the consumer at codegen time fell back to a
//! priority cascade that recomputed the choice from the same shape
//! facts.
//!
//! This installer closes the loop: when a NodeId carries a recognizer
//! shape that authoritatively implies a single strategy
//! (`DelimiterBalanced` → `WrapMode::BalancedScan`,
//! `SeparatorList` → `WrapMode::SepBy`,
//! `TokenLedBranches` → `AltMode::ByteDispatch`,
//! `KeywordPrefix` → `AltMode::KeyDispatch`), it pins the decision
//! variable to that strategy via a hard equality constraint. The CSP
//! solution then carries the structural choice as a first-class fact;
//! the consumer reads `ir.recognizer_decisions` directly and the
//! sidecar override path is no longer needed.
//!
//! # Producer
//!
//! [`crate::passes::recognizers::ShapeDictMiner`] populates
//! `ir.shape_dict_templates`; the per-NodeId
//! `ir.node_facts[id].recognizer.shape` carries the categorical
//! shape used by this installer. Both run before
//! `solve_grammar_components`.
//!
//! # Consumer
//!
//! - Alt pin: `crates/core/src/backend/strategy/alt_strategy.rs`
//!   reads `ir.recognizer_decisions[id].alt_mode`.
//! - Wrap pin: `crates/core/src/backend/driver/wrap.rs` reads
//!   `ir.recognizer_decisions[id].wrap_mode`.
//!
//! Disconnecting the installer (returning early without adding
//! constraints) leaves the CSP free to pick a lower-cost but
//! structurally-wrong mode for shape-eligible nodes; the disconnect
//! test in `tests/lattices/csp_authority.rs` asserts the difference.

use csp_solver::Csp;

use super::ConstraintCtx;
use crate::GrammarIR;
use crate::passes::csp_strategy::{AltMode, StrategyDomain, StrategyValue, WrapMode};
use crate::passes::patterns::RecognizerShape;

/// Install the `ShapeTemplateAuthority` constraint. Walks the
/// component's Alt and Wrap variables and pins each one whose
/// upstream recognizer shape implies a single strategy.
///
/// Returns the number of pin constraints installed.
pub fn install(ctx: &ConstraintCtx<'_>, csp: &mut Csp<StrategyDomain>, ir: &GrammarIR) -> usize {
    let mut count = 0usize;

    // ── Alt pins ────────────────────────────────────────────────
    //
    // For every Alt node in the component whose recognizer shape
    // is `TokenLedBranches` (→ ByteDispatch) or `KeywordPrefix`
    // (→ KeyDispatch), pin the Alt variable. The shape facts come
    // from `ir.node_facts[id].recognizer.shape`, populated by
    // `mine_recognizers` upstream.
    for (&node_id, &var) in ctx.alt_vars.iter() {
        // Only pin Alt vars owned by rules in this component. The
        // alt_vars map is component-wide because `solve_component`
        // walks every member rule's body.
        let pinned = pin_for_alt(ir, node_id);
        if let Some(target) = pinned {
            // `Csp::add_equals` is the shared "pin a variable to a
            // value" surface that the strategy domain inherits from
            // the csp-solver crate. Delegating here keeps the
            // installer aligned with the solver's authoritative pin
            // primitive (W3b.4 isomorphic API).
            csp.add_equals(var, StrategyValue::Alt(target));
            count += 1;
        }
    }

    // ── Wrap pins ───────────────────────────────────────────────
    //
    // `delim_scan_configs[id]` is the runtime authority for
    // BalancedScan eligibility — populated by `delim_scan::collect`
    // when the wrap node has a forward-memchr-to-close shape. When
    // present, the wrap variable is pinned to BalancedScan; the
    // CSP cost model would otherwise prefer Generic on cost weight
    // alone.
    //
    // Recognizer shape `SeparatorList` → SepBy follows the same
    // structure — separator-list facts come from the recognizer
    // miner.
    for (&node_id, &var) in ctx.wrap_vars.iter() {
        let pinned = pin_for_wrap(ir, node_id);
        if let Some(target) = pinned {
            csp.add_equals(var, StrategyValue::Wrap(target));
            count += 1;
        }
    }

    count
}

fn pin_for_alt(ir: &GrammarIR, node_id: crate::dag::NodeId) -> Option<AltMode> {
    let fact = ir
        .node_facts
        .get(&node_id)
        .and_then(|f| f.recognizer.as_ref())?;
    match fact.shape {
        RecognizerShape::KeywordPrefix { .. } => Some(AltMode::KeyDispatch),
        RecognizerShape::TokenLedBranches { .. } => Some(AltMode::ByteDispatch),
        _ => None,
    }
}

fn pin_for_wrap(ir: &GrammarIR, node_id: crate::dag::NodeId) -> Option<WrapMode> {
    // Authoritative: a populated delim-scan config means the
    // recognizer pass admitted this wrap as forward-memchr-eligible.
    if ir.delim_scan_configs.contains_key(&node_id) {
        return Some(WrapMode::BalancedScan);
    }
    let fact = ir
        .node_facts
        .get(&node_id)
        .and_then(|f| f.recognizer.as_ref())?;
    match fact.shape {
        RecognizerShape::DelimiterBalanced { .. } => Some(WrapMode::BalancedScan),
        RecognizerShape::SeparatorList { .. } => Some(WrapMode::SepBy),
        _ => None,
    }
}
