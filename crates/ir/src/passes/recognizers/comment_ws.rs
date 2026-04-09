//! Mine `RecognizerShape::Regex` records for
//! `RegexClass::WsBlockComment` patterns. Tags every Regex node whose
//! classification is the canonical comment-aware whitespace pattern.

use bbnf_regex::RegexClass;

use crate::passes::patterns::{
    NodeKind, OnePassGrade, OutputShape, Recognizer, RecognizerRole, RecognizerShape,
};
use crate::{GrammarIR, IrNode};

use super::install_recognizer;
use super::signature::compute_shape_hash;

pub(super) fn mine(ir: &mut GrammarIR) {
    let dag = match ir.dag.as_ref() {
        Some(d) => d.clone(),
        None => return,
    };
    let regex_info = ir.regex_info.clone();
    let rules = ir.rules.clone();

    for rule in &rules {
        walk(&rule.body, ir, &regex_info, &dag);
    }
}

fn walk(
    node: &IrNode,
    ir: &mut GrammarIR,
    regex_info: &std::collections::HashMap<crate::StringId, bbnf_regex::RegexInfo>,
    dag: &crate::dag::GrammarDag,
) {
    if let IrNode::Regex(sid) = node {
        if let Some(info) = regex_info.get(sid) {
            if matches!(info.classification, RegexClass::WsBlockComment) {
                if let Some(node_id) = dag.node_for(node) {
                    let shape = RecognizerShape::Regex { sid: *sid };
                    let signature = compute_shape_hash(
                        &shape,
                        OutputShape::SpanOnly,
                        false,
                        OnePassGrade::OnePass,
                        ir,
                    );
                    install_recognizer(
                        &mut ir.node_facts,
                        node_id,
                        NodeKind::Leaf,
                        Recognizer {
                            role: RecognizerRole::Standalone,
                            shape,
                            signature,
                            peer_group: None,
                        },
                    );
                }
            }
        }
    }

    super::visit_children_alt(node, |child| walk(child, ir, regex_info, dag));
}
