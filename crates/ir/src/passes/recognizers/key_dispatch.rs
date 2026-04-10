//! Key-dispatch detection (Tranche X.8a — upstream replacement).
//!
//! Detects `Alt` nodes where branches start with literal keys followed
//! by a common separator (e.g., `"color" ":" value | "display" ":"
//! value`). Instead of checkpoint/restore per branch, the backend can
//! scan the key token once and dispatch on the consumed bytes.
//!
//! Detection runs here (during `mine_recognizers`), populating
//! `ir.key_dispatch_configs` keyed by the Alt's stable `NodeId`. The
//! backend reads the sidecar map via `GrammarIR::key_dispatch_configs`;
//! it does not recompute.
//!
//! Previously lived at `backend/patterns/key_dispatch.rs` in the
//! `bbnf-core` crate. Moved intact in Tranche X.8a.

use std::collections::{HashMap, HashSet};

use parse_that::regex::classify::{classify_regex, RegexClass};

use crate::dag::{GrammarDag, NodeId};
use crate::{
    AltBranch, DetectedBranch, GrammarIR, IrNode, KeyClass, KeyDispatchConfig, KeyDispatchMatch,
    RuleId,
};

/// Build the per-rule key-dispatch configuration map. Walks every rule
/// body and runs [`try_detect`] on each `Alt` node; stores successful
/// detections keyed by the Alt's stable `NodeId`.
pub fn collect(ir: &GrammarIR) -> HashMap<NodeId, KeyDispatchMatch> {
    let mut out: HashMap<NodeId, KeyDispatchMatch> = HashMap::new();
    let Some(dag) = ir.dag.as_ref() else {
        return out;
    };
    for rule in &ir.rules {
        walk(&rule.body, ir, dag, &mut out);
    }
    out
}

fn walk(
    node: &IrNode,
    ir: &GrammarIR,
    dag: &GrammarDag,
    out: &mut HashMap<NodeId, KeyDispatchMatch>,
) {
    if let IrNode::Alt(branches, _) = node {
        if let Some(result) = try_detect(branches, ir) {
            if let Some(nid) = dag.node_for(node) {
                out.insert(nid, result);
            }
        }
    }

    super::visit_children_alt(node, |child| walk(child, ir, dag, out));
}

/// Try to detect a key-dispatch pattern in an alternation.
///
/// Returns `(config, detected_branches, fallback_branch_idx)`.
pub fn try_detect(branches: &[AltBranch], ir: &GrammarIR) -> Option<KeyDispatchMatch> {
    if branches.len() < 3 {
        return None;
    }

    // Check if last branch is a regex fallback.
    let mut visited = HashSet::new();
    let fallback_idx = if is_leading_regex(&branches[branches.len() - 1].node, ir, &mut visited) {
        Some(branches.len() - 1)
    } else {
        None
    };

    // Extract leading literals from all non-fallback branches.
    let mut all_literals: Vec<(usize, Vec<String>)> = Vec::new();
    for (i, branch) in branches.iter().enumerate() {
        if Some(i) == fallback_idx {
            continue;
        }
        let mut visited = HashSet::new();
        let lits = extract_leading_literals(&branch.node, ir, &mut visited)?;
        if lits.is_empty() {
            return None;
        }
        all_literals.push((i, lits));
    }

    if all_literals.len() < 2 {
        return None;
    }

    // Detect common separator.
    let separator = detect_separator(&all_literals, branches, ir);

    // Classify key type from fallback regex (if present).
    let key_class = if let Some(fb_idx) = fallback_idx {
        classify_fallback_key(&branches[fb_idx].node, ir)?
    } else {
        // Default to Identifier if no fallback regex.
        KeyClass::Identifier
    };

    // Validate all keys against key class.
    for (_, lits) in &all_literals {
        for lit in lits {
            let key = if let Some(ref sep) = separator {
                lit.strip_suffix(sep.as_str()).unwrap_or(lit)
            } else {
                lit
            };
            if !validate_key_for_class(key, &key_class) {
                return None;
            }
        }
    }

    let detected: Vec<DetectedBranch> = all_literals
        .into_iter()
        .map(|(idx, lits)| {
            let keys = lits
                .into_iter()
                .map(|lit| {
                    if let Some(ref sep) = separator {
                        lit.strip_suffix(sep.as_str())
                            .unwrap_or(&lit)
                            .to_string()
                    } else {
                        lit
                    }
                })
                .collect();
            DetectedBranch {
                key_literals: keys,
                branch_idx: idx,
            }
        })
        .collect();

    Some((
        KeyDispatchConfig {
            key_class,
            separator,
            key_scanner_regex_id: None, // Set by driver after detection.
        },
        detected,
        fallback_idx,
    ))
}

// ─── Detection Helpers ─────────────────────────────────────────────────────

/// Extract leading literal(s) from a branch node.
///
/// `visited` tracks rules currently in the call stack; entering an
/// already-visited rule indicates a cyclic Ref chain, which is not
/// dispatch-eligible (deterministic FIRST sets require acyclic leading
/// positions) — the walker returns `None` to bail the whole chain.
fn extract_leading_literals(
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

/// Check if a node has a regex in leading position.
///
/// `visited` tracks rules currently in the call stack; a cyclic Ref
/// chain terminates the walk with `false` because cyclic rules have
/// recursive FIRST sets that are ill-defined for dispatch-table
/// eligibility.
fn is_leading_regex(node: &IrNode, ir: &GrammarIR, visited: &mut HashSet<RuleId>) -> bool {
    match node {
        IrNode::Regex(_) => true,
        IrNode::Seq(children) if !children.is_empty() => {
            is_leading_regex(&children[0], ir, visited)
        }
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            is_leading_regex(inner, ir, visited)
        }
        IrNode::Ref(rule_id) => {
            if !visited.insert(*rule_id) {
                return false;
            }
            let rule = &ir.rules[*rule_id as usize];
            let result = is_leading_regex(&rule.body, ir, visited);
            visited.remove(rule_id);
            result
        }
        _ => false,
    }
}

/// Extract the leading regex pattern string from a node.
///
/// `visited` tracks rules currently in the call stack; a cyclic Ref
/// chain terminates with `None`.
fn extract_leading_regex_pattern<'a>(
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
        _ => None,
    }
}

/// Classify the fallback regex to determine key class.
fn classify_fallback_key(fallback: &IrNode, ir: &GrammarIR) -> Option<KeyClass> {
    let mut visited = HashSet::new();
    let pattern = extract_leading_regex_pattern(fallback, ir, &mut visited)?;
    match classify_regex(pattern) {
        RegexClass::Identifier | RegexClass::CssIdent => Some(KeyClass::Identifier),
        RegexClass::QuotedString {
            quote_char,
            allows_escapes: _,
        } => Some(KeyClass::QuotedString { quote_char }),
        RegexClass::CssQuotedString => Some(KeyClass::QuotedString { quote_char: b'"' }),
        _ => None,
    }
}

/// Validate a key string against the key class.
fn validate_key_for_class(key: &str, class: &KeyClass) -> bool {
    match class {
        KeyClass::Identifier => {
            let bytes = key.as_bytes();
            !bytes.is_empty()
                && (bytes[0].is_ascii_alphabetic() || bytes[0] == b'_' || bytes[0] == b'-')
        }
        KeyClass::QuotedString { .. } => !key.is_empty(),
    }
}

/// Detect a common separator across all literal-led branches.
fn detect_separator(
    all_literals: &[(usize, Vec<String>)],
    branches: &[AltBranch],
    ir: &GrammarIR,
) -> Option<String> {
    // Strategy 1: Fused suffix — all literals share a trailing non-alphanumeric byte.
    let first_lits = &all_literals[0].1;
    if let Some(first_lit) = first_lits.first() {
        if let Some(&last_byte) = first_lit.as_bytes().last() {
            if !last_byte.is_ascii_alphanumeric() && last_byte != b'_' && last_byte != b'-' {
                let suffix = String::from_utf8(vec![last_byte]).ok()?;
                let all_have_suffix = all_literals.iter().all(|(_, lits)| {
                    lits.iter()
                        .all(|l| l.as_bytes().last().copied() == Some(last_byte))
                });
                if all_have_suffix {
                    return Some(suffix);
                }
            }
        }
    }

    // Strategy 2: Shared 2nd Seq child literal across all branches.
    extract_seq_separator(branches, all_literals, ir)
}

/// Extract separator from 2nd Seq child if all branches share it.
fn extract_seq_separator(
    branches: &[AltBranch],
    all_literals: &[(usize, Vec<String>)],
    ir: &GrammarIR,
) -> Option<String> {
    let mut common_sep: Option<String> = None;
    for (idx, _) in all_literals {
        let branch = &branches[*idx];
        if let IrNode::Seq(children) = &branch.node {
            if children.len() >= 2 {
                if let IrNode::Literal(sid) = &children[1] {
                    let sep = ir.get_string(*sid).to_string();
                    if let Some(ref cs) = common_sep {
                        if *cs != sep {
                            return None;
                        }
                    } else {
                        common_sep = Some(sep);
                    }
                    continue;
                }
            }
        }
        return None;
    }
    common_sep
}
