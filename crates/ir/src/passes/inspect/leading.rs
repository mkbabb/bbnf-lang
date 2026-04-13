//! Leading-position extraction for key-dispatch detection.
//!
//! - [`extract_leading_literals`] collects the literal strings that
//!   may appear at the head of a node, descending through trivial
//!   wrappers, [`IrNode::Ref`] indirections, and inner [`IrNode::Alt`]
//!   branches.
//! - [`extract_leading_regex_pattern`] returns the leading regex
//!   pattern string (used by key-dispatch fallback classification).
//!
//! Both walkers carry a `visited: &mut HashSet<RuleId>` cycle guard;
//! a cyclic Ref chain terminates with `None`.

use std::collections::HashSet;

use parse_that::regex::classify::{RegexClass, classify_regex};

use crate::{GrammarIR, IrNode, RuleId};

/// Extract the leading literal(s) from a branch node.
///
/// `visited` tracks rules currently in the call stack; entering an
/// already-visited rule indicates a cyclic Ref chain, which is not
/// dispatch-eligible (deterministic FIRST sets require acyclic leading
/// positions) — the walker returns `None` to bail the whole chain.
///
/// An inner [`IrNode::Alt`] contributes the union of the literals
/// from every branch; a single non-literal branch fails the whole
/// extraction.
pub fn extract_leading_literals(
    node: &IrNode,
    ir: &GrammarIR,
    visited: &mut HashSet<RuleId>,
) -> Option<Vec<String>> {
    match node {
        IrNode::Literal(sid) => Some(vec![ir.get_string(*sid).to_string()]),
        IrNode::Seq(children) if !children.is_empty() => {
            extract_leading_literals(&children[0], ir, visited)
        }
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            extract_leading_literals(inner, ir, visited)
        }
        IrNode::Ref(rule_id) => {
            if !visited.insert(*rule_id) {
                return None;
            }
            let rule = &ir.rules[*rule_id as usize];
            let result = extract_leading_literals(&rule.body, ir, visited);
            visited.remove(rule_id);
            result
        }
        IrNode::Alt(branches, _) => {
            // Inner Alt: collect literals from all branches.
            let mut all = Vec::new();
            for branch in branches {
                let lits = extract_leading_literals(&branch.node, ir, visited)?;
                all.extend(lits);
            }
            Some(all)
        }
        _ => None,
    }
}

/// Extract the leading regex pattern string from a node.
///
/// `visited` tracks rules currently in the call stack; a cyclic Ref
/// chain terminates with `None`.
///
/// For an inner [`IrNode::Alt`], scans every branch and returns the
/// first pattern that classifies as a known key class — the
/// parameterized `Identifier` / `QuotedString` variants cover both
/// the generic and CSS dialects via their flag fields. The branch
/// ordering in the grammar may put a narrow pattern (e.g. `--[\w-]+`)
/// before a general one (e.g. `-?[a-zA-Z_][\w-]*`), so every branch
/// must be considered.
pub fn extract_leading_regex_pattern<'a>(
    node: &'a IrNode,
    ir: &'a GrammarIR,
    visited: &mut HashSet<RuleId>,
) -> Option<&'a str> {
    match node {
        IrNode::Regex(sid) => Some(ir.get_string(*sid)),
        IrNode::Seq(children) if !children.is_empty() => {
            extract_leading_regex_pattern(&children[0], ir, visited)
        }
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            extract_leading_regex_pattern(inner, ir, visited)
        }
        IrNode::Ref(rule_id) => {
            if !visited.insert(*rule_id) {
                return None;
            }
            let rule = &ir.rules[*rule_id as usize];
            let result = extract_leading_regex_pattern(&rule.body, ir, visited);
            visited.remove(rule_id);
            result
        }
        IrNode::Alt(branches, _) => {
            for b in branches {
                if let Some(pat) = extract_leading_regex_pattern(&b.node, ir, visited) {
                    let cls = classify_regex(pat);
                    if matches!(
                        cls,
                        RegexClass::Identifier { .. } | RegexClass::QuotedString { .. }
                    ) {
                        return Some(pat);
                    }
                }
            }
            None
        }
        _ => None,
    }
}
