//! Mine `RecognizerShape::SeparatorList` records on the strict separated
//! list `element (sep element)*` (`passes::inspect::sep_list`) where the
//! separator is a single-byte literal. `( element << sep ? ) *` reads
//! literally (ruling S1-B) and is not mined.

use crate::IrNode;
use crate::dag::NodeId;
use crate::passes::inspect::sep_list;
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
        // Only the strict separated list `x (sep x)*` is a separator
        // list; `x << sep ?` under a repeat reads literally (S1-B).
        let IrNode::Seq(_) = node else {
            return;
        };
        let Some(list) = sep_list(node) else {
            return;
        };
        let Some(sep_byte) = list.separator_byte(ctx.ir) else {
            return;
        };
        let element = list.element;

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
