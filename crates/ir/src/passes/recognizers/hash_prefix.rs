//! CSS hash-prefix hex-tail recognizer (Tranche X.10b).
//!
//! Detects `Seq([Literal("#"), Regex(hex_class)])` and
//! `Next(Literal("#"), Regex(hex_class))` shapes where the regex node
//! classifies as a hex-digit `CharClassQuantified` with `+` or `*`
//! quantifier. Captures CSS color literals (`#abc`, `#abcdef`,
//! `#abcdefab`).
//!
//! The detected shape lands as `RecognizerShape::HashPrefix { tail_class }`.
//! The backend `kernels::hash_prefix` module emits a single memcmp `#`
//! + `scan_hex_mut` combined scanner.

use bbnf_regex::classify::{ClassRangeInfo, RegexClass};

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
    if let Some(tail_class) = try_match_hash_prefix(node, ir) {
        if let Some(node_id) = dag.node_for(node) {
            let shape = RecognizerShape::HashPrefix { tail_class };
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

/// Try to extract the tail `CharSet128` from a `#`-prefixed hex-tail
/// sequence. Returns `None` if the leading literal isn't exactly `#`
/// or the tail isn't a hex `CharClassQuantified` with `+` quantifier.
fn try_match_hash_prefix(node: &IrNode, ir: &GrammarIR) -> Option<crate::CharSet128> {
    let (lit_hash, tail) = match node {
        IrNode::Seq(children) if children.len() >= 2 => (&children[0], &children[1]),
        IrNode::Next(lhs, rhs) => (lhs.as_ref(), rhs.as_ref()),
        _ => return None,
    };

    // Leading literal must be exactly "#".
    let hash_sid = match lit_hash {
        IrNode::Literal(sid) => *sid,
        _ => return None,
    };
    if ir.get_string(hash_sid).as_bytes() != b"#" {
        return None;
    }

    // Tail must be a regex that classifies as CharClassQuantified with
    // a hex-canonical byte set.
    let regex_sid = match tail {
        IrNode::Regex(sid) => *sid,
        _ => return None,
    };
    let info = ir.regex_info.get(&regex_sid)?;
    match &info.classification {
        RegexClass::CharClassQuantified(ClassRangeInfo {
            chars,
            negated: false,
            min,
            max: _,
        }) if *min >= 1 && is_hex_class(chars) => Some(*chars),
        _ => None,
    }
}

fn is_hex_class(chars: &crate::CharSet128) -> bool {
    const HEX_BYTES: [u8; 22] = [
        b'0', b'1', b'2', b'3', b'4', b'5', b'6', b'7', b'8', b'9', b'a', b'b', b'c', b'd',
        b'e', b'f', b'A', b'B', b'C', b'D', b'E', b'F',
    ];
    if chars.len() != HEX_BYTES.len() {
        return false;
    }
    HEX_BYTES.iter().all(|&b| chars.has(b))
}
