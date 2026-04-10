//! CSS unit-tail recognizer (Tranche X.10c).
//!
//! Detects `Seq([Regex(numeric), Literal(unit)])` and
//! `Next(Regex(numeric), Literal(unit))` shapes where the numeric
//! regex classifies as `RegexClass::Numeric` or `RegexClass::JsonNumber`
//! and the literal `unit` is one of the CSS unit suffixes:
//!
//! ```text
//! px em rem %  vh vw ms s deg rad fr ch ex vmin vmax
//! ```
//!
//! The detected shape lands as `RecognizerShape::UnitTail { unit }`.
//! The backend `kernels::unit_tail` module emits a fused number scanner
//! (`css_number_scan_f64` or the span form) + unit memcmp.

use bbnf_regex::classify::RegexClass;
use smallvec::SmallVec;

use crate::dag::{GrammarDag, NodeId};
use crate::passes::patterns::{
    OnePassGrade, OutputShape, Recognizer, RecognizerRole, RecognizerShape,
};
use crate::{GrammarIR, IrNode};

use super::signature::compute_shape_hash;

/// Canonical CSS unit suffixes. Ordered by frequency in the Tailwind
/// and Bootstrap corpora so prefix probes hit the common cases first.
const CSS_UNITS: &[&[u8]] = &[
    b"px", b"em", b"rem", b"%", b"vh", b"vw", b"vmin", b"vmax", b"ms", b"s", b"deg", b"rad",
    b"fr", b"ch", b"ex",
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
    if let Some(unit_bytes) = try_match_unit_tail(node, ir) {
        if let Some(node_id) = dag.node_for(node) {
            let shape = RecognizerShape::UnitTail {
                unit: SmallVec::from_slice(&unit_bytes),
            };
            let signature = compute_shape_hash(
                &shape,
                OutputShape::SpanPlusF64,
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

/// Extract the unit bytes from a numeric+unit sequence, validating that
/// the leading child is a classified numeric regex and the trailing
/// child is a literal matching one of the canonical CSS units.
fn try_match_unit_tail(node: &IrNode, ir: &GrammarIR) -> Option<Vec<u8>> {
    let (numeric, unit_lit) = match node {
        IrNode::Seq(children) if children.len() >= 2 => (&children[0], &children[1]),
        IrNode::Next(lhs, rhs) => (lhs.as_ref(), rhs.as_ref()),
        _ => return None,
    };

    // Leading child must be a numeric regex.
    let regex_sid = match numeric {
        IrNode::Regex(sid) => *sid,
        _ => return None,
    };
    let info = ir.regex_info.get(&regex_sid)?;
    if !matches!(
        info.classification,
        RegexClass::Numeric { .. } | RegexClass::JsonNumber
    ) {
        return None;
    }

    // Trailing child must be a literal matching a CSS unit.
    let unit_sid = match unit_lit {
        IrNode::Literal(sid) => *sid,
        _ => return None,
    };
    let unit_bytes = ir.get_string(unit_sid).as_bytes();
    if !CSS_UNITS.contains(&unit_bytes) {
        return None;
    }

    Some(unit_bytes.to_vec())
}
