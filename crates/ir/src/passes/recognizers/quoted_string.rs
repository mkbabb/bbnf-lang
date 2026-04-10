//! Mine `RecognizerShape::Regex` records for `RegexClass::QuotedString`,
//! `JsonString`, and `CssQuotedString` patterns at every Regex node site.

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
        if !matches!(
            info.classification,
            RegexClass::QuotedString { .. }
                | RegexClass::JsonString
                | RegexClass::CssQuotedString
        ) {
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
