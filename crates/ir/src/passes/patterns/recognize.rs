//! Recursive tree walk: inspect IR node structure and assign per-node facts.

use std::collections::HashMap;

use crate::dag::{GrammarDag, NodeId};
use crate::{AltBranch, GrammarIR, IrNode};

use super::{AltPattern, NodeFacts, NodeKind, PatternAnnotations, SeqPattern};

/// Recognize structural patterns on all rules.
///
/// Populates the legacy `ir.pattern_annotations` (per-rule) and
/// `ir.node_facts` (per-node, keyed by stable `NodeId`). Requires
/// `ir.dag` to be populated — facts are written only for nodes the
/// DAG knows about. If the DAG is absent (e.g. a raw IR built by a
/// unit test), `node_facts` is cleared.
pub fn recognize_patterns(ir: &mut GrammarIR) {
    let mut legacy_annotations = HashMap::new();
    let mut node_facts = HashMap::new();

    // Legacy per-rule annotations don't need the DAG.
    for rule in &ir.rules {
        let mut ann = PatternAnnotations::default();
        recognize_body(&rule.body, &mut ann, ir);
        if ann.alt_pattern.is_some() || ann.seq_pattern.is_some() || ann.is_operator_chain {
            legacy_annotations.insert(rule.id, ann);
        }
    }

    // Per-node facts require the durable DAG to map `&IrNode` → `NodeId`.
    if let Some(dag) = ir.dag.as_ref() {
        for rule in &ir.rules {
            recognize_tree(&rule.body, &mut node_facts, ir, dag);
        }
    }

    ir.pattern_annotations = legacy_annotations;
    ir.node_facts = node_facts;
}

// ── Recursive tree walk (per-node facts, NodeId-keyed) ──────────────────

fn recognize_tree(
    node: &IrNode,
    facts: &mut HashMap<NodeId, NodeFacts>,
    ir: &GrammarIR,
    dag: &GrammarDag,
) {
    let node_id = dag.node_for(node);

    match node {
        IrNode::Seq(children) => {
            let is_op_chain = check_operator_chain(children);
            let all_span = ir.collapse_simple_spans && children.iter().all(is_span_leaf);

            if let Some(id) = node_id {
                if is_op_chain || all_span {
                    facts.insert(
                        id,
                        NodeFacts {
                            node_kind: NodeKind::Seq,
                            operator_chain: is_op_chain,
                            sep_by: false,
                            all_span_collapse: all_span,
                        },
                    );
                }
            }

            for child in children {
                recognize_tree(child, facts, ir, dag);
            }
        }

        IrNode::Alt(branches, _dispatch) => {
            for branch in branches {
                recognize_tree(&branch.node, facts, ir, dag);
            }
        }

        IrNode::Skip(element, opt_sep) => {
            let is_sep_by = check_sep_by(opt_sep);

            if let Some(id) = node_id {
                if is_sep_by {
                    facts.insert(
                        id,
                        NodeFacts {
                            node_kind: NodeKind::Skip,
                            operator_chain: false,
                            sep_by: true,
                            all_span_collapse: false,
                        },
                    );
                }
            }

            recognize_tree(element, facts, ir, dag);
            recognize_tree(opt_sep, facts, ir, dag);
        }

        IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            recognize_tree(a, facts, ir, dag);
            recognize_tree(b, facts, ir, dag);
        }

        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => {
            recognize_tree(inner, facts, ir, dag);
        }

        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            recognize_tree(token, facts, ir, dag);
            for arm in arms {
                recognize_tree(&arm.continuation, facts, ir, dag);
            }
            recognize_tree(fallback, facts, ir, dag);
        }

        // Leaves — no recursion needed.
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
    }
}

/// Check operator chain: `Seq([head, Repeat(Seq([op, rhs]), 0, MAX)])`.
fn check_operator_chain(children: &[IrNode]) -> bool {
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
    // Also check through OptionalWhitespace wrapper.
    if let IrNode::OptionalWhitespace(inner) = tail {
        return is_operator_chain_tail(inner);
    }
    false
}

/// Check sep_by: `Repeat(separator, 0, 1)`.
fn check_sep_by(opt_sep: &IrNode) -> bool {
    matches!(
        opt_sep,
        IrNode::Repeat {
            lo: 0,
            hi: 1,
            ..
        }
    )
}

/// Check if a node is a simple span leaf.
fn is_span_leaf(node: &IrNode) -> bool {
    matches!(
        node,
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon
    )
}

// ── Legacy recognition (kept during migration) ──────────────────────────

fn recognize_body(node: &IrNode, ann: &mut PatternAnnotations, ir: &GrammarIR) {
    match node {
        IrNode::Seq(children) => recognize_seq_legacy(children, ann, ir),
        IrNode::Alt(branches, dispatch) => {
            recognize_alt_legacy(branches, dispatch.is_some(), ann)
        }
        _ => {}
    }
}

fn recognize_seq_legacy(children: &[IrNode], ann: &mut PatternAnnotations, ir: &GrammarIR) {
    if children.len() == 2 && is_operator_chain_tail(&children[1]) {
        ann.seq_pattern = Some(SeqPattern::OperatorChain);
        ann.is_operator_chain = true;
        return;
    }
    if ir.collapse_simple_spans && children.iter().all(is_span_leaf) {
        ann.seq_pattern = Some(SeqPattern::AllSpanCollapse);
        return;
    }
    ann.seq_pattern = Some(SeqPattern::Normal);
}

fn recognize_alt_legacy(branches: &[AltBranch], has_dispatch: bool, ann: &mut PatternAnnotations) {
    if has_dispatch {
        ann.alt_pattern = Some(AltPattern::DispatchTable);
    } else if branches.len() > 1 {
        ann.alt_pattern = Some(AltPattern::CheckpointFallback);
    }
}
