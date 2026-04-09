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
/// Tranche W phase 3c: thin lookup against `ir.recognizer_decisions`
/// + the structural `AllLiteral` fast path.
///
/// 1. `AllLiteral` is checked first because it's a backend emission
///    optimization not modeled by the strategy CSP.
/// 2. Otherwise, read the per-NodeId `AltMode` from
///    `ir.recognizer_decisions` and map it to `AltStrategy`. The CSP's
///    `ByteDispatch` becomes `DispatchTable`; `KeyDispatch` becomes
///    `KeyDispatch`; everything else (`Checkpoint`, `TokenDispatch`,
///    `SharedHelper(_)`) becomes `Checkpoint` until those backend
///    emission paths are wired (Phase 3d).
/// 3. When the CSP didn't produce a decision (e.g. nodes without a
///    DAG entry, or grammars compiled without recognizer mining), fall
///    back to the structural detection so the migration is incremental
///    rather than all-or-nothing.
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

    // Priority 2: read the strategy CSP decision.
    let csp_alt_mode = node_id
        .and_then(|id| ir.recognizer_decisions.get(&id))
        .and_then(|d| d.alt_mode.as_ref());

    if let Some(mode) = csp_alt_mode {
        match mode {
            AltMode::ByteDispatch => return AltStrategy::DispatchTable,
            AltMode::KeyDispatch => return AltStrategy::KeyDispatch,
            // The remaining variants (Checkpoint, TokenDispatch,
            // SharedHelper) all fall through to the universal
            // Checkpoint emission path until the kernel registry
            // (Phase 3d) wires the specialized helpers.
            AltMode::Checkpoint
            | AltMode::TokenDispatch
            | AltMode::SharedHelper(_) => return AltStrategy::Checkpoint,
        }
    }

    // Priority 3: structural fallback when the CSP didn't produce a
    // decision. This branch fires for nodes that bypass the recognizer
    // pipeline entirely (e.g. structural-only compiles where
    // `mine_recognizers` and `solve_strategy_decisions` are skipped).
    if dispatch.is_some() {
        return AltStrategy::DispatchTable;
    }
    if super::super::patterns::key_dispatch::try_detect(branches, ir).is_some() {
        return AltStrategy::KeyDispatch;
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
