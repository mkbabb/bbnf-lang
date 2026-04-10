//! Mine `RecognizerShape::SeparatorList` records on `Skip(element, opt_sep)`
//! patterns where the existing `sep_by` flag fires AND the separator is
//! a single-byte literal.

use crate::dag::{GrammarDag, NodeId};
use crate::passes::patterns::{
    OnePassGrade, OutputShape, Recognizer, RecognizerRole, RecognizerShape,
};
use crate::{GrammarIR, IrNode};

use super::signature::compute_shape_hash;

pub(super) fn collect(
    ir: &GrammarIR,
    dag: &GrammarDag,
    out: &mut Vec<(NodeId, Recognizer)>,
) {
    for rule in &ir.rules {
        walk(&rule.body, ir, dag, out);
    }
}

fn walk(
    node: &IrNode,
    ir: &GrammarIR,
    dag: &GrammarDag,
    out: &mut Vec<(NodeId, Recognizer)>,
) {
    if let IrNode::Skip(element, opt_sep) = node {
        if let IrNode::Repeat { inner, lo: 0, hi: 1 } = opt_sep.as_ref() {
            if let Some(sep_byte) = single_byte_literal(inner, ir) {
                if let Some(node_id) = dag.node_for(node) {
                    let element_shape = element_shape_from(element);
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
                    out.push((
                        node_id,
                        Recognizer {
                            role: RecognizerRole::Standalone,
                            shape,
                            signature,
                            },
                    ));
                }
            }
        }
    }

    super::visit_children_alt(node, |child| walk(child, ir, dag, out));
}

fn element_shape_from(node: &IrNode) -> RecognizerShape {
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
