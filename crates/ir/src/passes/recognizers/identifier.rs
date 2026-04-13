//! Mine `RecognizerShape::Regex` records for identifier-family regex
//! patterns. The parameterized `RegexClass::Identifier` variant
//! folds the formerly-nominal CssIdent shape (the flags
//! `allows_leading_dash` / `allows_double_dash_prefix` distinguish
//! the dialects). Adjacent shapes — `PrefixThenClass`,
//! `CharClassQuantified`, `HexDigits` — share the same recognizer
//! channel because the kernel layer treats them as a single family.

use bbnf_regex::RegexClass;

use crate::IrNode;
use crate::dag::NodeId;
use crate::passes::patterns::{
    OnePassGrade, OutputShape, Recognizer, RecognizerRole, RecognizerShape,
};

use super::signature::compute_shape_hash;
use super::{MineOutputs, RecognizerMineCtx, RecognizerMiner};

pub struct IdentifierMiner;

impl RecognizerMiner for IdentifierMiner {
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
            RegexClass::Identifier { .. }
                | RegexClass::PrefixThenClass { .. }
                | RegexClass::CharClassQuantified(_)
                | RegexClass::HexDigits
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
