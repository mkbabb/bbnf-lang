//! `LayoutAuthority` — pin Wrap decision variables to the layout
//! choice implied by upstream payload-layout facts.
//!
//! # Rationale
//!
//! The Wrap layout decision (`WrapMode::Generic | BalancedScan |
//! SepBy`) is consumed by the production payload/wrap path:
//!
//! - `crates/core/src/backend/driver/wrap.rs:104-127` reads
//!   `ir.recognizer_decisions[id].wrap_mode` to decide whether to
//!   emit a delimiter-scan body or fall through to the generic
//!   wrap emitter.
//! - Payload-layout selection (Tranche AB.5 →
//!   `ir.payload_layouts`) depends on the wrap decision because
//!   `BalancedScan` and `SepBy` carry distinct payload shapes
//!   (single-span vs separator-keyed pair list).
//!
//! Without an installer, the CSP's wrap variable picks its
//! cost-min mode independently of the payload-layout pass. When
//! the upstream pass already determined the wrap is a separator
//! list (`ir.node_facts[id].recognizer.shape ==
//! RecognizerShape::SeparatorList`), we want the CSP to encode
//! that as a hard pin so the downstream payload pass and driver
//! see one consistent decision.
//!
//! # Producer
//!
//! `crate::passes::recognizers` (specifically
//! `delim_scan::collect` and the recognizer-shape miner)
//! populates `ir.delim_scan_configs` and the per-node
//! `recognizer.shape` field.
//!
//! # Consumer
//!
//! `crates/core/src/backend/driver/wrap.rs` —
//! `compile_wrap` reads `ir.recognizer_decisions[id].wrap_mode`
//! through `dstate.recognizer_decision`. Once the layout
//! installer pins the value, the consumer no longer needs the
//! sidecar `ir.delim_scan_configs` membership check to decide
//! between BalancedScan and Generic — the CSP fact carries the
//! authority.
//!
//! # Distinction from `shape::install`
//!
//! `shape::install` pins both Alt and Wrap nodes from the broad
//! `RecognizerShape` enum. `layout::install` is the layout-axis
//! complement that also enforces the separator-vs-balanced
//! distinction by inspecting the runtime `delim_scan_configs`
//! (an authoritative payload-layout fact). The two installers
//! are symmetric and idempotent: pinning the same variable to
//! the same value twice is a no-op in the CSP.

use csp_solver::Csp;
use csp_solver::constraint::LambdaConstraint;

use super::ConstraintCtx;
use crate::GrammarIR;
use crate::passes::csp_strategy::{StrategyDomain, StrategyValue, WrapMode};
use crate::passes::patterns::RecognizerShape;

/// Install the `LayoutAuthority` constraint. Returns the number
/// of pin constraints installed.
pub fn install(ctx: &ConstraintCtx<'_>, csp: &mut Csp<StrategyDomain>, ir: &GrammarIR) -> usize {
    let mut count = 0usize;

    for (&node_id, &var) in ctx.wrap_vars.iter() {
        let target = pin_for_layout(ir, node_id);
        if let Some(mode) = target {
            csp.add_constraint(make_pin(var, mode));
            count += 1;
        }
    }

    count
}

/// Resolve the layout pin for a Wrap node. Combines authoritative
/// facts in priority order:
///
/// 1. `delim_scan_configs[id]` membership ⇒ BalancedScan.
/// 2. `node_facts[id].recognizer.shape == SeparatorList` ⇒ SepBy.
/// 3. `node_facts[id].recognizer.shape == DelimiterBalanced`
///    ⇒ BalancedScan.
fn pin_for_layout(ir: &GrammarIR, node_id: crate::dag::NodeId) -> Option<WrapMode> {
    if ir.delim_scan_configs.contains_key(&node_id) {
        return Some(WrapMode::BalancedScan);
    }
    let fact = ir
        .node_facts
        .get(&node_id)
        .and_then(|f| f.recognizer.as_ref())?;
    match fact.shape {
        RecognizerShape::SeparatorList { .. } => Some(WrapMode::SepBy),
        RecognizerShape::DelimiterBalanced { .. } => Some(WrapMode::BalancedScan),
        _ => None,
    }
}

fn make_pin(
    var: csp_solver::constraint::VarId,
    mode: WrapMode,
) -> LambdaConstraint<StrategyDomain> {
    let value = StrategyValue::Wrap(mode);
    LambdaConstraint::new(
        vec![var],
        move |assignment| match &assignment[var as usize] {
            Some(v) => *v == value,
            None => true,
        },
        format!("layout_pin({var})"),
    )
}
