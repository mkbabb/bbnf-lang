//! Mine `RecognizerShape::TokenLedBranches` records on Alt nodes whose
//! branches each carry a leading recognizer with pairwise-disjoint
//! FIRST sets. Reads `ContextFacts::discrimination` from the upstream
//! `compute_context_facts` pass — only Alts marked `Strong` qualify.

use crate::dag::{GrammarDag, NodeId};
use crate::passes::context::{ContextFactsMap, DiscriminationStrength};
use crate::passes::patterns::{
    OnePassGrade, OutputShape, Recognizer, RecognizerRole, RecognizerShape,
};
use crate::{GrammarIR, IrNode};

use super::signature::compute_shape_hash;

pub(super) fn collect(
    ir: &GrammarIR,
    dag: &GrammarDag,
    context_facts: &ContextFactsMap,
    out: &mut Vec<(NodeId, Recognizer)>,
) {
    for rule in &ir.rules {
        walk(&rule.body, ir, dag, context_facts, out);
    }
}

fn walk(
    node: &IrNode,
    ir: &GrammarIR,
    dag: &GrammarDag,
    context_facts: &ContextFactsMap,
    out: &mut Vec<(NodeId, Recognizer)>,
) {
    if let IrNode::Alt(branches, _) = node {
        if let Some(node_id) = dag.node_for(node) {
            if let Some(ctx) = context_facts.get(&node_id) {
                if ctx.discrimination == DiscriminationStrength::Strong {
                    let key_shape = RecognizerShape::KeywordPrefix {
                        bytes: smallvec::smallvec![],
                        disjoint_tail: true,
                    };
                    let key_sig = compute_shape_hash(
                        &key_shape,
                        OutputShape::SpanOnly,
                        false,
                        OnePassGrade::OnePass,
                        ir,
                    );
                    let key_rec = Recognizer {
                        role: RecognizerRole::DispatchKey { owner: node_id },
                        shape: key_shape,
                        signature: key_sig,
                        peer_group: None,
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
        }
    }

    super::visit_children_alt(node, |child| walk(child, ir, dag, context_facts, out));
}
