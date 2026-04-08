//! Regex analysis cache pass.
//!
//! Populates `GrammarIR::regex_info` by calling `bbnf_regex::RegexInfo::analyze()`
//! for each unique regex pattern in the grammar. Run after all regex merging and
//! simplification passes to ensure patterns are in their final form.

use std::collections::HashMap;

use crate::{GrammarIR, IrNode, StringId};

/// Compute and cache `RegexInfo` for all regex patterns in the grammar.
///
/// Walks all rules, collects unique `Regex(StringId)` nodes, and calls
/// `bbnf_regex::RegexInfo::analyze()` for each. Results are cached on
/// `ir.regex_info` keyed by `StringId`.
pub fn compute_regex_info(ir: &mut GrammarIR) {
    let mut seen = HashMap::<StringId, ()>::new();
    let mut info_map = HashMap::new();

    // Collect all unique regex StringIds.
    for rule in &ir.rules {
        collect_regex_ids(&rule.body, &mut seen);
    }

    // Analyze each unique pattern.
    for &sid in seen.keys() {
        let pattern = ir.get_string(sid);
        if let Some(info) = bbnf_regex::RegexInfo::analyze(pattern) {
            info_map.insert(sid, info);
        }
    }

    ir.regex_info = info_map;
}

fn collect_regex_ids(node: &IrNode, seen: &mut HashMap<StringId, ()>) {
    match node {
        IrNode::Regex(sid) => {
            seen.entry(*sid).or_insert(());
        }
        IrNode::Seq(children) => {
            for child in children {
                collect_regex_ids(child, seen);
            }
        }
        IrNode::Alt(branches, _) => {
            for branch in branches {
                collect_regex_ids(&branch.node, seen);
            }
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner) => {
            collect_regex_ids(inner, seen);
        }
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            collect_regex_ids(a, seen);
            collect_regex_ids(b, seen);
        }
        IrNode::Map { inner, .. } => {
            collect_regex_ids(inner, seen);
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            collect_regex_ids(token, seen);
            for arm in arms {
                collect_regex_ids(&arm.continuation, seen);
            }
            collect_regex_ids(fallback, seen);
        }
        IrNode::Literal(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
    }
}
