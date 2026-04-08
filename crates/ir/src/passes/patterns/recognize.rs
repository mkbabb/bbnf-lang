//! Recursive tree walk: inspect IR node structure and assign per-node facts.

use std::collections::HashMap;

use crate::{AltBranch, GrammarIR, IrNode, RuleId};

use super::{AltPattern, NodeFacts, NodeKind, PatternAnnotations, SeqPattern};

/// Recognize structural patterns on all rules.
///
/// Populates both the legacy `ir.pattern_annotations` (per-rule) and the new
/// `ir.node_facts` (per-node, pointer-keyed) maps.
pub fn recognize_patterns(ir: &mut GrammarIR) {
    let mut legacy_annotations = HashMap::new();
    let mut node_facts = HashMap::new();

    for rule in &ir.rules {
        let mut ann = PatternAnnotations::default();
        recognize_body(&rule.body, &mut ann, ir);
        if ann.alt_pattern.is_some() || ann.seq_pattern.is_some() || ann.is_operator_chain {
            legacy_annotations.insert(rule.id, ann);
        }

        // Recursive tree walk for per-node facts.
        recognize_tree(&rule.body, &mut node_facts, ir);
    }

    ir.pattern_annotations = legacy_annotations;
    ir.node_facts = node_facts;
}

// ── Recursive tree walk (new: per-node facts) ───────────────────────────

fn recognize_tree(node: &IrNode, facts: &mut HashMap<usize, NodeFacts>, ir: &GrammarIR) {
    let node_id = node as *const IrNode as usize;

    match node {
        IrNode::Seq(children) => {
            let is_op_chain = check_operator_chain(children);
            let all_span = ir.collapse_simple_spans && children.iter().all(is_span_leaf);

            if is_op_chain || all_span {
                facts.insert(
                    node_id,
                    NodeFacts {
                        node_kind: NodeKind::Seq,
                        operator_chain: is_op_chain,
                        sep_by: false,
                        all_span_collapse: all_span,
                    },
                );
            }

            // Recurse into children.
            for child in children {
                recognize_tree(child, facts, ir);
            }
        }

        IrNode::Alt(branches, _dispatch) => {
            // Recurse into branch bodies.
            for branch in branches {
                recognize_tree(&branch.node, facts, ir);
            }
        }

        IrNode::Skip(element, opt_sep) => {
            // Check sep_by: Skip(element, Repeat(separator, 0, 1))
            let is_sep_by = check_sep_by(opt_sep);

            if is_sep_by {
                facts.insert(
                    node_id,
                    NodeFacts {
                        node_kind: NodeKind::Skip,
                        operator_chain: false,
                        sep_by: true,
                        all_span_collapse: false,
                    },
                );
            }

            // Also check for wrap pattern: Skip(Next(open, middle), close)
            // This is how Wrap is represented in the IR after lowering.
            recognize_tree(element, facts, ir);
            recognize_tree(opt_sep, facts, ir);
        }

        IrNode::Next(a, b) => {
            recognize_tree(a, facts, ir);
            recognize_tree(b, facts, ir);
        }

        IrNode::Minus(a, b) => {
            recognize_tree(a, facts, ir);
            recognize_tree(b, facts, ir);
        }

        IrNode::Repeat { inner, .. } => {
            recognize_tree(inner, facts, ir);
        }

        IrNode::Negate(inner) | IrNode::OptionalWhitespace(inner) => {
            recognize_tree(inner, facts, ir);
        }

        IrNode::Map { inner, .. } => {
            recognize_tree(inner, facts, ir);
        }

        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            recognize_tree(token, facts, ir);
            for arm in arms {
                recognize_tree(&arm.continuation, facts, ir);
            }
            recognize_tree(fallback, facts, ir);
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
