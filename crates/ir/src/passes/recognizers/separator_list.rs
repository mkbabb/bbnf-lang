//! Mine `RecognizerShape::SeparatorList` records on `Skip(element, opt_sep)`
//! patterns where the existing `sep_by` flag fires AND the separator is
//! a single-byte literal.

use crate::dag::GrammarDag;
use crate::passes::patterns::{
    NodeKind, OnePassGrade, OutputShape, Recognizer, RecognizerRole, RecognizerShape,
};
use crate::{GrammarIR, IrNode};

use super::install_recognizer;
use super::signature::compute_shape_hash;

pub(super) fn mine(ir: &mut GrammarIR) {
    let dag = match ir.dag.as_ref() {
        Some(d) => d.clone(),
        None => return,
    };
    let rules = ir.rules.clone();

    for rule in &rules {
        walk(&rule.body, ir, &dag);
    }
}

fn walk(node: &IrNode, ir: &mut GrammarIR, dag: &GrammarDag) {
    if let IrNode::Skip(element, opt_sep) = node {
        if let IrNode::Repeat { inner, lo: 0, hi: 1 } = opt_sep.as_ref() {
            if let Some(sep_byte) = single_byte_literal(inner, ir) {
                if let Some(node_id) = dag.node_for(node) {
                    // Build a thin element shape — the body's own miners
                    // refine this if they recognize the element shape.
                    let element_shape = element_shape_from(element, ir);
                    let element_sig = compute_shape_hash(
                        &element_shape,
                        OutputShape::SpanOnly,
                        false,
                        OnePassGrade::OnePass,
                        ir,
                    );
                    let element_rec = Recognizer {
                        role: RecognizerRole::Body { parent: node_id },
                        shape: element_shape,
                        signature: element_sig,
                        peer_group: None,
                    };

                    let shape = RecognizerShape::SeparatorList {
                        element: Box::new(element_rec),
                        separator: sep_byte,
                        trailing: false,
                    };
                    let signature = compute_shape_hash(
                        &shape,
                        OutputShape::SpanOnly,
                        false,
                        OnePassGrade::OnePass,
                        ir,
                    );
                    install_recognizer(
                        &mut ir.node_facts,
                        node_id,
                        NodeKind::Skip,
                        Recognizer {
                            role: RecognizerRole::Standalone,
                            shape,
                            signature,
                            peer_group: None,
                        },
                    );
                }
            }
        }
    }

    super::visit_children_alt(node, |child| walk(child, ir, dag));
}

fn element_shape_from(node: &IrNode, _ir: &GrammarIR) -> RecognizerShape {
    // Default: opaque keyword-prefix shell. Specific element shapes
    // (e.g., the regex inside a sep-by'd item) get refined when the
    // body's own miners visit those nodes — and the SeparatorList's
    // signature picks up the canonical hash via the recognizer pointer.
    if let IrNode::Regex(sid) = node {
        return RecognizerShape::Regex { sid: *sid };
    }
    RecognizerShape::KeywordPrefix {
        bytes: smallvec::smallvec![],
        disjoint_tail: false,
    }
}

fn single_byte_literal(node: &IrNode, ir: &GrammarIR) -> Option<u8> {
    if let IrNode::Literal(sid) = node {
        let s = ir.get_string(*sid);
        let bytes = s.as_bytes();
        if bytes.len() == 1 {
            return Some(bytes[0]);
        }
    }
    None
}
