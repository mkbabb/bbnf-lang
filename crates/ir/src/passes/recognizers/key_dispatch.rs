//! Key-dispatch detection (Tranche X.8a — upstream replacement).
//!
//! Detects `Alt` nodes where branches start with literal keys followed
//! by a common separator (e.g., `"color" ":" value | "display" ":"
//! value`). Instead of checkpoint/restore per branch, the backend can
//! scan the key token once and dispatch on the consumed bytes.
//!
//! Detection runs during `mine_recognizers` (Tranche Z.0 unified
//! walk), populating `ir.key_dispatch_configs` keyed by the Alt's
//! stable `NodeId`. The backend reads the sidecar map via
//! `GrammarIR::key_dispatch_configs`; it does not recompute.
//!
//! Previously lived at `backend/patterns/key_dispatch.rs` in the
//! `bbnf-core` crate. Moved intact in Tranche X.8a. Tranche Z.0
//! additionally collapses the per-miner tree walk into the shared
//! orchestrator `walk_unified`.

use std::collections::HashSet;

use parse_that::regex::classify::{RegexClass, classify_regex};

use crate::dag::NodeId;
use crate::passes::inspect::{
    extract_leading_literals, extract_leading_regex_pattern, resolve_to_seq,
};
use crate::{
    AltBranch, DetectedBranch, GrammarIR, IrNode, KeyClass, KeyDispatchConfig, KeyDispatchMatch,
};

use super::{MineOutputs, RecognizerMineCtx, RecognizerMiner};

pub struct KeyDispatchMiner;

impl RecognizerMiner for KeyDispatchMiner {
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
        if let Some(result) = try_detect(branches, ctx.ir) {
            outputs.key_dispatch_configs.insert(node_id, result);
        }
    }
}

/// Try to detect a key-dispatch pattern in an alternation.
///
/// Returns `(config, detected_branches, fallback_branch_indices)`.
///
/// Branches that start with literal keys become key-dispatched branches.
/// Branches that start with regex patterns (not extractable as literals)
/// become fallback branches, tried sequentially when no key matches.
/// At least one regex-led fallback must exist (typically the last branch
/// is a generic catch-all).
pub fn try_detect(branches: &[AltBranch], ir: &GrammarIR) -> Option<KeyDispatchMatch> {
    if branches.len() < 3 {
        return None;
    }

    // Partition branches into literal-led (key-dispatchable) and
    // non-literal-led (fallback). Fallback branches are tried
    // sequentially when no key matches.
    let mut all_literals: Vec<(usize, Vec<String>)> = Vec::new();
    let mut fallback_indices: Vec<usize> = Vec::new();

    for (i, branch) in branches.iter().enumerate() {
        let mut visited = HashSet::new();
        match extract_leading_literals(&branch.node, ir, &mut visited) {
            Some(lits) if !lits.is_empty() => {
                all_literals.push((i, lits));
            }
            _ => {
                // Non-literal-led branch (regex, epsilon, etc.) — treat
                // as fallback.
                fallback_indices.push(i);
            }
        }
    }

    // Need at least 2 keyed branches and at least 1 fallback.
    if all_literals.len() < 2 || fallback_indices.is_empty() {
        return None;
    }

    // Detect common separator among keyed branches.
    let separator = detect_separator(&all_literals, branches, ir);

    // Classify key type from the first regex-led fallback branch.
    let key_class = {
        let mut found_class = None;
        for &fb_idx in &fallback_indices {
            if let Some(cls) = classify_fallback_key(&branches[fb_idx].node, ir) {
                found_class = Some(cls);
                break;
            }
        }
        found_class.unwrap_or(KeyClass::Identifier)
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
                        lit.strip_suffix(sep.as_str()).unwrap_or(&lit).to_string()
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
        fallback_indices,
    ))
}

// ─── Detection Helpers ─────────────────────────────────────────────────────

/// Classify the fallback regex to determine key class.
fn classify_fallback_key(fallback: &IrNode, ir: &GrammarIR) -> Option<KeyClass> {
    let mut visited = HashSet::new();
    let pattern = extract_leading_regex_pattern(fallback, ir, &mut visited)?;
    match classify_regex(pattern) {
        RegexClass::Identifier { .. } => Some(KeyClass::Identifier),
        RegexClass::QuotedString { quote_char, .. } => Some(KeyClass::QuotedString { quote_char }),
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
///
/// Follows `Ref` nodes to find the underlying `Seq` body, so branches
/// like `Ref(colorDecl)` whose body is `Seq([Ref(colorProps), Literal(":"), ...])`
/// correctly detect `":"` as the common separator.
fn extract_seq_separator(
    branches: &[AltBranch],
    all_literals: &[(usize, Vec<String>)],
    ir: &GrammarIR,
) -> Option<String> {
    let mut common_sep: Option<String> = None;
    for (idx, _) in all_literals {
        let branch = &branches[*idx];
        let mut visited = HashSet::new();
        if let Some(children) = resolve_to_seq(&branch.node, ir, &mut visited) {
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
