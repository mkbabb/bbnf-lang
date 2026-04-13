//! Mine `RecognizerShape::Regex` records for the parameterized
//! `RegexClass::QuotedString` family. The same variant covers the
//! formerly-nominal JsonString and CssQuotedString shapes — the
//! distinguishing flags (`quote_char`, `allows_escapes`,
//! `allows_u_escapes`) are carried inside the variant.

use bbnf_regex::RegexClass;

use crate::IrNode;
use crate::dag::NodeId;
use crate::passes::patterns::{
    OnePassGrade, OutputShape, Recognizer, RecognizerRole, RecognizerShape,
};

use super::signature::compute_shape_hash;
use super::{MineOutputs, RecognizerMineCtx, RecognizerMiner};

pub struct QuotedStringMiner;

impl RecognizerMiner for QuotedStringMiner {
    fn inspect(
        &self,
        node: &IrNode,
        node_id: NodeId,
        ctx: &RecognizerMineCtx,
        outputs: &mut MineOutputs,
    ) {
        let IrNode::Regex(sid) = node else {
            return;
        };
        let Some(info) = ctx.ir.regex_info.get(sid) else {
            return;
        };
        if !matches!(info.classification, RegexClass::QuotedString { .. }) {
            return;
        }
        let shape = RecognizerShape::Regex { sid: *sid };
        let signature = compute_shape_hash(
            &shape,
            OutputShape::SpanOnly,
            false,
            if info.one_pass_eligible {
                OnePassGrade::OnePass
            } else {
                OnePassGrade::RestartSafe
            },
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
