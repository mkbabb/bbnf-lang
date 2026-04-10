//! CSS function-head recognizer (Tranche X.10a).
//!
//! Detects `Seq([Literal(name), Literal("("), ...])` and
//! `Next(Literal(name), Literal("("))` shapes where `name` is one of
//! the eight canonical CSS function identifiers:
//!
//! ```text
//! rgb rgba hsl hsla calc var url attr
//! ```
//!
//! The detected shape lands as `RecognizerShape::FunctionHead { name,
//! paren_byte }`. The backend `kernels::function_head` module emits a
//! single combined memcmp + byte check, collapsing eight separate
//! inline `Literal("rgb").seq(Literal("("))` emissions in CSS L4 onto
//! one shared call site per family member.

use smallvec::SmallVec;

use crate::dag::{GrammarDag, NodeId};
use crate::passes::patterns::{
    OnePassGrade, OutputShape, Recognizer, RecognizerRole, RecognizerShape,
};
use crate::{GrammarIR, IrNode};

use super::signature::compute_shape_hash;

/// Canonical CSS function names that the family head recognizer
/// collapses. Matching is exact on byte contents.
const CSS_FUNCTION_NAMES: &[&[u8]] = &[
    b"rgb", b"rgba", b"hsl", b"hsla", b"calc", b"var", b"url", b"attr",
];

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
    if let Some((name_bytes, paren_byte)) = try_match_function_head(node, ir) {
        if let Some(node_id) = dag.node_for(node) {
            let shape = RecognizerShape::FunctionHead {
                name: SmallVec::from_slice(&name_bytes),
                paren_byte,
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
    super::visit_children_alt(node, |child| walk(child, ir, dag, out));
}

/// Try to extract `(name_bytes, paren_byte)` from a node that matches
/// a CSS function head shape.
///
/// Two structural variants are accepted:
/// 1. `Seq([Literal(name), Literal("("), ...])` — only the leading two
///    children must match; additional children represent arguments and
///    are left to the caller's recognizer pipeline.
/// 2. `Next(Literal(name), Literal("("))` — the equivalent binary form.
fn try_match_function_head(node: &IrNode, ir: &GrammarIR) -> Option<(Vec<u8>, u8)> {
    let (lit_name, lit_paren) = match node {
        IrNode::Seq(children) if children.len() >= 2 => (&children[0], &children[1]),
        IrNode::Next(lhs, rhs) => (lhs.as_ref(), rhs.as_ref()),
        _ => return None,
    };

    let name_sid = match lit_name {
        IrNode::Literal(sid) => *sid,
        _ => return None,
    };
    let paren_sid = match lit_paren {
        IrNode::Literal(sid) => *sid,
        _ => return None,
    };

    let name_bytes = ir.get_string(name_sid).as_bytes();
    let paren_bytes = ir.get_string(paren_sid).as_bytes();

    if paren_bytes.len() != 1 || paren_bytes[0] != b'(' {
        return None;
    }
    if !CSS_FUNCTION_NAMES.contains(&name_bytes) {
        return None;
    }

    Some((name_bytes.to_vec(), b'('))
}
