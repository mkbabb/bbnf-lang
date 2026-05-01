//! Mine `RecognizerShape::DelimiterBalanced` records on Wrap-shaped
//! nodes (`Skip(Next(open, body), close)` or `Next(open, Skip(body, close))`)
//! where both delimiters are 1-byte literals.

use crate::IrNode;
use crate::dag::NodeId;
use crate::passes::inspect::{single_byte_literal, unwrap_wrap};
use crate::passes::patterns::{
    OnePassGrade, OutputShape, Recognizer, RecognizerRole, RecognizerShape,
};

use super::signature::compute_shape_hash;
use super::{MineOutputs, RecognizerMineCtx, RecognizerMiner};

pub struct BalancedWrapMiner;

impl RecognizerMiner for BalancedWrapMiner {
    fn inspect(
        &self,
        node: &IrNode,
        node_id: NodeId,
        ctx: &RecognizerMineCtx,
        outputs: &mut MineOutputs,
    ) {
        let Some((open, _middle, close)) = unwrap_wrap(node) else {
            return;
        };
        let (Some(open_byte), Some(close_byte)) = (
            single_byte_literal(open, ctx.ir),
            single_byte_literal(close, ctx.ir),
        ) else {
            return;
        };
        if open_byte == close_byte {
            return;
        }

        let inner_shape = RecognizerShape::KeywordPrefix {
            bytes: smallvec::smallvec![open_byte, close_byte],
            disjoint_tail: true,
        };
        let inner_sig = compute_shape_hash(
            &inner_shape,
            OutputShape::SpanOnly,
            false,
            OnePassGrade::OnePass,
            ctx.ir,
        );
        let inner = Recognizer {
            role: RecognizerRole::Body { parent: node_id },
            shape: inner_shape,
            signature: inner_sig,
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
