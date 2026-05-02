//! Alt strategy selection — thin lookup against `ir.recognizer_decisions`.
//!
//! Tranche W phase 3c: the previous priority cascade in `decide_alt_strategy`
//! has been replaced with a read from `ir.recognizer_decisions` (populated
//! upstream by `passes::csp_strategy::solve_strategy_decisions`). The
//! per-Alt decision is now produced by a real `csp_solver::Csp` running
//! `OptimizationMode::MinimizeCost`; this file's job is to map the
//! resulting `AltMode` into the backend's `AltStrategy` enum and apply
//! the structural `AllLiteral` fast path that the CSP doesn't model.
//!
//! The `AllLiteral` fast path stays here because it depends on the
//! backend `ValuePlacement::Inline` context that isn't visible to the IR
//! pipeline. The driver gets the structural eligibility from this pass
//! and makes the final emission call.
//!
//! AZ-IV.W4.2 — CSP authority is post-selection: this file no longer
//! re-overrides the CSP's `AltMode` via `ir.key_dispatch_configs`
//! lookups. The `dispatch::install` constraint upstream pins
//! `AltMode = KeyDispatch` whenever the structural detector populated
//! a key-dispatch sidecar (or the keyword-stats miner committed a
//! `keyword_branches` entry). Sidecars carry payload only after the
//! solve; strategy is owned by `recognizer_decisions`.

use std::collections::HashMap;

use bbnf_ir::dag::{GrammarDag, NodeId};
use bbnf_ir::passes::csp_strategy::AltMode;
use bbnf_ir::{AltBranch, AltDispatch, FnDescriptor, GrammarIR, IrNode};

/// Resolved strategy for an Alt node.
#[derive(Clone, Debug)]
pub enum AltStrategy {
    /// All branches are Literal or Map(Literal, constant) with Inline alloc.
    AllLiteral,
    /// Pre-computed dispatch table (disjoint FIRST sets).
    DispatchTable,
    /// Key-based dispatch (leading literal keys + common separator).
    KeyDispatch,
    /// Sequential checkpoint fallback (general case).
    Checkpoint,
}

/// Solve Alt strategies for all Alt nodes in the grammar.
///
/// Walks the IR tree, finds all Alt nodes, and assigns each a
/// strategy. Results are keyed by stable `NodeId` from `ir.dag`;
/// entries for which no DAG id exists (structurally unreachable
/// nodes) are dropped.
pub fn solve_alt_strategies(ir: &GrammarIR) -> HashMap<NodeId, AltStrategy> {
    let mut strategies = HashMap::new();
    if let Some(dag) = ir.dag.as_ref() {
        for rule in &ir.rules {
            collect_alt_strategies(&rule.body, &mut strategies, ir, dag);
        }
    }
    strategies
}

fn collect_alt_strategies(
    node: &IrNode,
    strategies: &mut HashMap<NodeId, AltStrategy>,
    ir: &GrammarIR,
    dag: &GrammarDag,
) {
    match node {
        IrNode::Alt(branches, dispatch) => {
            let id = dag.node_for(node);
            if let Some(id) = id {
                let strategy = decide_alt_strategy(Some(id), branches, dispatch.as_ref(), ir);
                strategies.insert(id, strategy);
            }
            for branch in branches {
                collect_alt_strategies(&branch.node, strategies, ir, dag);
            }
        }

        IrNode::Seq(children) => {
            for child in children {
                collect_alt_strategies(child, strategies, ir, dag);
            }
        }

        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => {
            collect_alt_strategies(inner, strategies, ir, dag);
        }

        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            collect_alt_strategies(a, strategies, ir, dag);
            collect_alt_strategies(b, strategies, ir, dag);
        }

        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            collect_alt_strategies(token, strategies, ir, dag);
            for arm in arms {
                collect_alt_strategies(&arm.continuation, strategies, ir, dag);
            }
            collect_alt_strategies(fallback, strategies, ir, dag);
        }

        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
    }
}

/// Decide strategy for a single Alt node.
///
/// AZ-IV.W4.2 — pure lookup against `ir.recognizer_decisions`. The
/// CSP's `dispatch::install` constraint pins `AltMode = KeyDispatch`
/// whenever a `key_dispatch_configs` / `keyword_branches` sidecar
/// exists, so the consumer reads the CSP fact directly without
/// re-overriding via sidecar `contains_key` lookups.
///
/// 1. `AllLiteral` is checked first because it's a backend emission
///    optimization not modeled by the strategy CSP (depends on the
///    runtime `ValuePlacement::Inline` context).
/// 2. Otherwise, read the per-NodeId `AltMode` from
///    `ir.recognizer_decisions` and map it to `AltStrategy`:
///    - `ByteDispatch` ⇒ `DispatchTable` when the inline `AltDispatch`
///      payload exists, else `Checkpoint` (the precomputed table is
///      the load-bearing emission data).
///    - `KeyDispatch` ⇒ `KeyDispatch`.
///    - `Checkpoint` ⇒ `Checkpoint`.
/// 3. When no CSP decision exists (structural-only compile path
///    where `mine_recognizers` is skipped), the `AltDispatch`
///    precomputed table wins over the default `Checkpoint` fallback.
fn decide_alt_strategy(
    node_id: Option<NodeId>,
    branches: &[AltBranch],
    dispatch: Option<&AltDispatch>,
    ir: &GrammarIR,
) -> AltStrategy {
    // Priority 1: All-literal fast path (structural, not CSP).
    // Alloc context isn't available here (it's a compile-time parameter).
    // We mark it as AllLiteral if structurally eligible; the driver checks
    // alloc.
    if branches.iter().all(|b| is_literal_like(&b.node, ir)) {
        return AltStrategy::AllLiteral;
    }

    // Priority 2: read the strategy CSP decision (post-selection
    // authority — CSP owns the dispatch choice).
    let csp_alt_mode = node_id
        .and_then(|id| ir.recognizer_decisions.get(&id))
        .and_then(|d| d.alt_mode.as_ref());

    if let Some(mode) = csp_alt_mode {
        return match mode {
            // Tranche Y.3: `TokenDispatch` was folded into
            // `ByteDispatch`. The CSP and the backend converge on a
            // single strong-discrimination variant.
            // `fuse_token_dispatch` converts the strongest cases
            // into `IrNode::TokenDispatch` upstream, so this arm
            // only fires on the remaining ByteDispatch residue —
            // which always has either a dispatch table populated
            // or falls back to the generic Checkpoint emission path.
            AltMode::ByteDispatch => {
                if dispatch.is_some() {
                    AltStrategy::DispatchTable
                } else {
                    AltStrategy::Checkpoint
                }
            }
            AltMode::KeyDispatch => AltStrategy::KeyDispatch,
            AltMode::Checkpoint => AltStrategy::Checkpoint,
        };
    }

    // Priority 3: structural-only compile path (no CSP decision —
    // `mine_recognizers` was skipped). The `AltDispatch` precomputed
    // table wins over the default `Checkpoint` fallback.
    if dispatch.is_some() {
        return AltStrategy::DispatchTable;
    }
    AltStrategy::Checkpoint
}

/// Check if a branch node is literal-like (Literal or Map(Literal, constant)).
fn is_literal_like(node: &IrNode, ir: &GrammarIR) -> bool {
    match node {
        IrNode::Literal(_) => true,
        IrNode::Map { inner, fn_id } => {
            matches!(inner.as_ref(), IrNode::Literal(_))
                && matches!(
                    &ir.fns[*fn_id as usize],
                    FnDescriptor::Expr { expr, .. } if expr.is_constant()
                )
        }
        _ => false,
    }
}
