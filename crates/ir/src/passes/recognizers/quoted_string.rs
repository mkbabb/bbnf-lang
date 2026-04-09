//! Mine `RecognizerShape::Regex` records for `RegexClass::QuotedString`,
//! `JsonString`, and `CssQuotedString` patterns at every Regex node site.

use bbnf_regex::RegexClass;

use crate::dag::{GrammarDag, NodeId};
use crate::passes::patterns::{
    OnePassGrade, OutputShape, Recognizer, RecognizerRole, RecognizerShape,
};
use crate::{GrammarIR, IrNode};

use super::signature::compute_shape_hash;

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
    if let IrNode::Regex(sid) = node {
        if let Some(info) = ir.regex_info.get(sid) {
            if matches!(
                info.classification,
                RegexClass::QuotedString { .. }
                    | RegexClass::JsonString
                    | RegexClass::CssQuotedString
            ) {
                if let Some(node_id) = dag.node_for(node) {
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

    super::visit_children_alt(node, |child| walk(child, ir, dag, out));
}
