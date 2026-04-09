//! Alt strategy selection — decides optimal strategy for each Alt node.
//!
//! Replaces the rigid priority cascade in `compile_alt()` with a declarative
//! strategy solver. Each Alt node gets a resolved `AltStrategy` stored on
//! `BackendPreparation`, read by the driver at compile time.

use std::collections::HashMap;

use bbnf_ir::dag::{GrammarDag, NodeId};
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
            if let Some(id) = dag.node_for(node) {
                let strategy = decide_alt_strategy(branches, dispatch.as_ref(), ir);
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
/// Current implementation mirrors the existing priority cascade.
/// Future: pluggable cost model via CSP constraints.
fn decide_alt_strategy(
    branches: &[AltBranch],
    dispatch: Option<&AltDispatch>,
    ir: &GrammarIR,
) -> AltStrategy {
    // Priority 1: All-literal fast path.
    // Note: alloc context isn't available here (it's a compile-time parameter).
    // We mark it as AllLiteral if structurally eligible; the driver checks alloc.
    if branches.iter().all(|b| is_literal_like(&b.node, ir)) {
        return AltStrategy::AllLiteral;
    }

    // Priority 2: Dispatch table (pre-computed by IR pass).
    if dispatch.is_some() {
        return AltStrategy::DispatchTable;
    }

    // Priority 3: Key dispatch.
    if super::super::patterns::key_dispatch::try_detect(branches, ir).is_some() {
        return AltStrategy::KeyDispatch;
    }

    // Priority 4: Checkpoint fallback.
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
