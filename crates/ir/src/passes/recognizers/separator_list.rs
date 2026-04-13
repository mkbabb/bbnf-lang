//! Mine `RecognizerShape::SeparatorList` records on `Skip(element, opt_sep)`
//! patterns where the separator is a single-byte literal.

use crate::IrNode;
use crate::dag::NodeId;
use crate::passes::inspect::single_byte_literal;
use crate::passes::patterns::{
    OnePassGrade, OutputShape, Recognizer, RecognizerRole, RecognizerShape,
};

use super::signature::compute_shape_hash;
use super::{MineOutputs, RecognizerMineCtx, RecognizerMiner};

pub struct SeparatorListMiner;

impl RecognizerMiner for SeparatorListMiner {
    fn inspect(
        &self,
        node: &IrNode,
        node_id: NodeId,
        ctx: &RecognizerMineCtx,
        outputs: &mut MineOutputs,
    ) {
        let IrNode::Skip(element, opt_sep) = node else {
            return;
        };
        let IrNode::Repeat {
            inner,
            lo: 0,
            hi: 1,
        } = opt_sep.as_ref()
        else {
            return;
        };
        let Some(sep_byte) = single_byte_literal(inner, ctx.ir) else {
            return;
        };

        let element_shape = element_shape_from(element);
        let element_sig = compute_shape_hash(
            &element_shape,
            OutputShape::SpanOnly,
            false,
            OnePassGrade::OnePass,
            ctx.ir,
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
            ctx.ir,
        );
        outputs.recognizers.push((
            node_id,
            Recognizer {
                role: RecognizerRole::Standalone,
                shape,
                signature,
            },
        ));
    }
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
