//! Mine `RecognizerShape::DelimiterBalanced` records on Wrap-shaped
//! nodes (`Skip(Next(open, body), close)` or `Next(open, Skip(body, close))`)
//! where both delimiters are 1-byte literals.

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
    if let Some((open, _middle, close)) = unwrap_wrap(node) {
        if let (Some(open_byte), Some(close_byte)) =
            (single_byte_literal(open, ir), single_byte_literal(close, ir))
        {
            if open_byte != close_byte {
                if let Some(node_id) = dag.node_for(node) {
                    let inner_shape = RecognizerShape::KeywordPrefix {
                        bytes: smallvec::smallvec![open_byte, close_byte],
                        disjoint_tail: true,
                    };
                    let inner_sig = compute_shape_hash(
                        &inner_shape,
                        OutputShape::SpanOnly,
                        false,
                        OnePassGrade::OnePass,
                        ir,
                    );
                    let inner = Recognizer {
                        role: RecognizerRole::Body { parent: node_id },
                        shape: inner_shape,
                        signature: inner_sig,
                        peer_group: None,
                    };

                    let shape = RecognizerShape::DelimiterBalanced {
                        open: open_byte,
                        close: close_byte,
                        inner: Box::new(inner),
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
                            peer_group: None,
                        },
                    ));
                }
            }
        }
    }

    super::visit_children_alt(node, |child| walk(child, ir, dag, out));
}

fn unwrap_wrap(node: &IrNode) -> Option<(&IrNode, &IrNode, &IrNode)> {
    match node {
        IrNode::Skip(left, right) => {
            if let IrNode::Next(open, middle) = left.as_ref() {
                return Some((open, middle, right));
            }
            None
        }
        IrNode::Next(left, right) => {
            if let IrNode::Skip(middle, close) = right.as_ref() {
                return Some((left, middle, close));
            }
            None
        }
        _ => None,
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
