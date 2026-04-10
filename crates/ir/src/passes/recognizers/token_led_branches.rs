//! Mine `RecognizerShape::TokenLedBranches` records on Alt nodes whose
//! branches each carry a leading recognizer with pairwise-disjoint
//! FIRST sets. Reads `ContextFacts::discrimination` from the upstream
//! `compute_context_facts` pass — only Alts marked `Strong` qualify.

use crate::IrNode;
use crate::dag::NodeId;
use crate::passes::context::DiscriminationStrength;
use crate::passes::patterns::{
    OnePassGrade, OutputShape, Recognizer, RecognizerRole, RecognizerShape,
};

use super::signature::compute_shape_hash;
use super::{MineOutputs, RecognizerMineCtx, RecognizerMiner};

pub struct TokenLedBranchesMiner;

impl RecognizerMiner for TokenLedBranchesMiner {
    fn inspect(
        &self,
        node: &IrNode,
        node_id: NodeId,
        ctx: &RecognizerMineCtx,
        outputs: &mut MineOutputs,
    ) {
        let IrNode::Alt(branches, _) = node else {
            return;
        };
        let Some(cf) = ctx.context_facts.get(&node_id) else {
            return;
        };
        if cf.discrimination != DiscriminationStrength::Strong {
            return;
        }

        let key_shape = RecognizerShape::KeywordPrefix {
            bytes: smallvec::smallvec![],
            disjoint_tail: true,
        };
        let key_sig = compute_shape_hash(
            &key_shape,
            OutputShape::SpanOnly,
            false,
            OnePassGrade::OnePass,
            ctx.ir,
        );
        let key_rec = Recognizer {
            role: RecognizerRole::DispatchKey { owner: node_id },
            shape: key_shape,
            signature: key_sig,
        };

        let shape = RecognizerShape::TokenLedBranches {
            key: Box::new(key_rec),
            branch_count: branches.len() as u16,
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
