//! Mine per-node `ContextFacts` during the unified recognizer walk.
//!
//! Replaces the standalone `compute_context_facts` pass with a
//! `RecognizerMiner` impl that produces identical output inside the
//! shared Z.0 tree traversal. Three fact dimensions are handled here:
//!
//! * **Discrimination strength** — set on `Alt` nodes. An Alt with a
//!   pre-computed dispatch table is `Strong`; an Alt with every branch
//!   leading with a literal / regex / seq head is `Medium`.
//! * **Scan safety** — set on `Skip(Next(open, body), close)` wrap
//!   patterns. These may be flat-scanned through without recursion.
//! * **In-token-dispatch flag** — propagated one level down from every
//!   `TokenDispatch` node to its direct children (token, arm
//!   continuations, fallback).
//!
//! The `in_recovery_context` field is intentionally not populated —
//! the legacy pass's `collect_recovery_roots` helper has always
//! returned empty because `@recover` directives are not wired into
//! the DAG yet. If that wiring lands, extend this miner rather than
//! resurrecting a second walk.
//!
//! Ordering constraint: `ContextFactsMiner` must run **before**
//! `TokenLedBranchesMiner` in the miner list because the latter reads
//! `discrimination == Strong` from the context facts at the same Alt
//! node, during the same `inspect` pass.

use crate::IrNode;
use crate::dag::NodeId;
use crate::passes::context::{DiscriminationStrength, ScanSafety};

use super::{MineOutputs, RecognizerMineCtx, RecognizerMiner};

pub struct ContextFactsMiner;

impl RecognizerMiner for ContextFactsMiner {
    fn inspect(
        &self,
        node: &IrNode,
        node_id: NodeId,
        ctx: &RecognizerMineCtx,
        outputs: &mut MineOutputs,
    ) {
        match node {
            IrNode::Alt(branches, dispatch) => {
                // Strong > Medium > Weak. Strong wins if a dispatch
                // table was already pre-computed upstream; otherwise
                // Medium applies if every branch leads with a literal,
                // regex, or seq head (which the seq body may still
                // refine further, but at this node the branch *shape*
                // is sufficient to distinguish).
                if dispatch.is_some() {
                    set_discrimination(
                        outputs,
                        node_id,
                        DiscriminationStrength::Strong,
                    );
                } else if branches.iter().all(|b| is_leading_literal(&b.node)) {
                    set_discrimination(
                        outputs,
                        node_id,
                        DiscriminationStrength::Medium,
                    );
                }
            }

            IrNode::Skip(inner, _close) => {
                // Wrap pattern: `Skip(Next(open, body), close)`.
                if matches!(inner.as_ref(), IrNode::Next(_, _)) {
                    set_scan_safety(outputs, node_id, ScanSafety::Safe);
                }
            }

            IrNode::TokenDispatch {
                token,
                arms,
                fallback,
            } => {
                // Direct-child stamp. The legacy pass read
                // `parent_is_token_dispatch` at worklist-pop time and
                // marked the children; the walker's pre-order descent
                // reaches each child AFTER this inspect runs, so
                // stamping the children's `NodeId` entries here arrives
                // in time for downstream miners that read the flag at
                // the child node (none do today, but the contract is
                // preserved).
                if let Some(id) = ctx.dag.node_for(token) {
                    set_in_token_dispatch(outputs, id);
                }
                for arm in arms {
                    if let Some(id) = ctx.dag.node_for(&arm.continuation) {
                        set_in_token_dispatch(outputs, id);
                    }
                }
                if let Some(id) = ctx.dag.node_for(fallback) {
                    set_in_token_dispatch(outputs, id);
                }
            }

            _ => {}
        }
    }
}

fn set_discrimination(
    outputs: &mut MineOutputs,
    node_id: NodeId,
    strength: DiscriminationStrength,
) {
    outputs
        .context_facts
        .entry(node_id)
        .or_default()
        .discrimination = strength;
}

fn set_scan_safety(outputs: &mut MineOutputs, node_id: NodeId, safety: ScanSafety) {
    outputs
        .context_facts
        .entry(node_id)
        .or_default()
        .scan_safety = safety;
}

fn set_in_token_dispatch(outputs: &mut MineOutputs, node_id: NodeId) {
    outputs
        .context_facts
        .entry(node_id)
        .or_default()
        .in_token_dispatch = true;
}

/// Whether a node qualifies as a "leading literal" for the Medium
/// discrimination heuristic. Mirrors the legacy `is_leading_literal`
/// helper: literal, regex, or any seq head counts — the seq may refine
/// further downstream but at the Alt level the branches are distinct
/// enough to dispatch by first byte.
fn is_leading_literal(node: &IrNode) -> bool {
    matches!(
        node,
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Seq(_)
    )
}
