//! Seq strategy selection — mirror of AltStrategy for Seq nodes.
//!
//! Seq nodes have several specialized shapes: all-Span collapse, operator
//! chains, and general mixed sequences. Pre-solving these lets compile_seq
//! dispatch on the solved shape instead of re-detecting each pattern.
//!
//! Phase 6 introduces the enum + skeleton solver. Phase 7 wires it into
//! the driver alongside the existing AltStrategy integration.

use bbnf_ir::IrNode;

/// Resolved strategy for a Seq node.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SeqStrategy {
    /// All children are Span-typed leaves — collapse to a single Span.
    AllSpan,
    /// Binary operator chain: `head (op rhs)*`.
    OperatorChain,
    /// General mixed sequence — per-child compilation + grouping.
    Mixed,
}

/// Classify a Seq node's children into a `SeqStrategy`.
///
/// Mirrors the inline detection in `compile_seq` — consumers can pre-solve
/// via `solve_seq_strategies` to skip detection on the hot path.
pub fn classify_seq(children: &[IrNode]) -> SeqStrategy {
    if is_operator_chain(children) {
        return SeqStrategy::OperatorChain;
    }
    if children
        .iter()
        .all(|c| matches!(c, IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon))
    {
        return SeqStrategy::AllSpan;
    }
    SeqStrategy::Mixed
}

/// Match the operator-chain shape: `Seq([head, Repeat(Seq([op, rhs]), 0, MAX)])`.
fn is_operator_chain(children: &[IrNode]) -> bool {
    if children.len() != 2 {
        return false;
    }
    is_operator_chain_tail(&children[1])
}

fn is_operator_chain_tail(tail: &IrNode) -> bool {
    if let IrNode::Repeat { inner, lo: 0, hi } = tail {
        if *hi == u32::MAX {
            if let IrNode::Seq(inner_children) = inner.as_ref() {
                return inner_children.len() == 2;
            }
        }
    }
    if let IrNode::OptionalWhitespace(inner) = tail {
        return is_operator_chain_tail(inner);
    }
    false
}
