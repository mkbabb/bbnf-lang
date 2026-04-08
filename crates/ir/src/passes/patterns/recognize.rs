//! Pattern recognition: inspect IR node structure and assign annotations.

use crate::{AltBranch, GrammarIR, IrNode, RuleId};

use super::{AltPattern, PatternAnnotations, SeqPattern};

/// Recognize structural patterns on all rules and populate `ir.pattern_annotations`.
pub fn recognize_patterns(ir: &mut GrammarIR) {
    let mut annotations = std::collections::HashMap::new();

    for rule in &ir.rules {
        let mut ann = PatternAnnotations::default();
        recognize_body(&rule.body, &mut ann, ir);
        if ann.alt_pattern.is_some() || ann.seq_pattern.is_some() || ann.is_operator_chain {
            annotations.insert(rule.id, ann);
        }
    }

    ir.pattern_annotations = annotations;
}

fn recognize_body(node: &IrNode, ann: &mut PatternAnnotations, ir: &GrammarIR) {
    match node {
        IrNode::Seq(children) => recognize_seq(children, ann, ir),
        IrNode::Alt(branches, dispatch) => recognize_alt(branches, dispatch.is_some(), ann),
        _ => {}
    }
}

/// Detect Seq patterns: operator chain, all-span collapse.
fn recognize_seq(children: &[IrNode], ann: &mut PatternAnnotations, ir: &GrammarIR) {
    if children.len() == 2 {
        if is_operator_chain(&children[0], &children[1]) {
            ann.seq_pattern = Some(SeqPattern::OperatorChain);
            ann.is_operator_chain = true;
            return;
        }
    }

    // All-span collapse: every child is a simple leaf (Literal, Regex, Epsilon).
    if ir.collapse_simple_spans && children.iter().all(is_span_leaf) {
        ann.seq_pattern = Some(SeqPattern::AllSpanCollapse);
        return;
    }

    ann.seq_pattern = Some(SeqPattern::Normal);
}

/// Detect Alt patterns: dispatch table vs checkpoint fallback.
fn recognize_alt(branches: &[AltBranch], has_dispatch: bool, ann: &mut PatternAnnotations) {
    if has_dispatch {
        ann.alt_pattern = Some(AltPattern::DispatchTable);
    } else if branches.len() > 1 {
        ann.alt_pattern = Some(AltPattern::CheckpointFallback);
    }
}

/// Check if a Seq is an operator chain: `[head, Repeat { inner: Seq([op, rhs]), lo: 0, hi: MAX }]`.
fn is_operator_chain(head: &IrNode, tail: &IrNode) -> bool {
    if let IrNode::Repeat { inner, lo: 0, hi } = tail {
        if *hi == u32::MAX {
            if let IrNode::Seq(inner_children) = inner.as_ref() {
                return inner_children.len() == 2;
            }
        }
    }
    // Also check through OptionalWhitespace wrapper.
    if let IrNode::OptionalWhitespace(inner) = tail {
        return is_operator_chain(head, inner);
    }
    false
}

/// Check if a node is a simple span leaf.
fn is_span_leaf(node: &IrNode) -> bool {
    matches!(
        node,
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon
    )
}
