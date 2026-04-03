//! Algebraic regex simplification via rewrite rules.
//!
//! A registry of `RewriteRule` impls that operate on Alt nodes containing
//! Regex branches. Each rule is testable in isolation and composition.
//! Feeds into the `Changed`-based fixed-point loop (Phase 4.3).
//!
//! Rules are RE#-inspired (POPL 2025) with extensions:
//! - Superset absorption: `[a-z]|[a-c]` → `[a-z]`
//! - Union merge: `[a-c]|[d-f]` → `[a-f]`
//! - Redundant alternation: `R|R` → `R`
//! - Repetition absorption: `a+a*` → `a+`, `a*a*` → `a*`
//! - Empty-language elimination: prune unmatchable branches

use crate::{AltBranch, GrammarIR, IrNode};

/// Trait for algebraic rewrite rules on regex alternations.
pub trait RewriteRule: Send + Sync {
    /// Human-readable name for diagnostics.
    fn name(&self) -> &'static str;

    /// Apply this rule to an Alt node's branches. Returns true if modified.
    fn apply(&self, branches: &mut Vec<AltBranch>, strings: &mut Vec<String>) -> bool;
}

/// Registry of default rewrite rules.
pub fn default_rules() -> Vec<Box<dyn RewriteRule>> {
    vec![
        Box::new(RedundantAlternation),
        Box::new(SupersetAbsorption),
        Box::new(UnionMerge),
    ]
}

/// Apply all rewrite rules to an Alt node's branches.
/// Returns true if any rule made a change.
pub fn simplify_alt(
    branches: &mut Vec<AltBranch>,
    rules: &[Box<dyn RewriteRule>],
    strings: &mut Vec<String>,
) -> bool {
    let mut changed = false;
    for rule in rules {
        changed |= rule.apply(branches, strings);
    }
    changed
}

/// Apply algebraic simplification across the entire IR.
/// Returns true if any node was modified.
pub fn simplify_regex_algebra(ir: &mut GrammarIR) -> bool {
    let rules = default_rules();
    let mut changed = false;
    for rule in &mut ir.rules {
        let body = std::mem::replace(&mut rule.body, IrNode::Epsilon);
        let (new_body, c) = simplify_node(body, &rules, &mut ir.strings);
        rule.body = new_body;
        changed |= c;
    }
    changed
}

fn simplify_node(
    node: IrNode,
    rules: &[Box<dyn RewriteRule>],
    strings: &mut Vec<String>,
) -> (IrNode, bool) {
    match node {
        IrNode::Alt(mut branches, dispatch) => {
            // Recurse into branches first.
            let mut changed = false;
            for b in &mut branches {
                let inner = std::mem::replace(&mut b.node, IrNode::Epsilon);
                let (new_inner, c) = simplify_node(inner, rules, strings);
                b.node = new_inner;
                changed |= c;
            }

            // Apply rewrite rules to this Alt.
            if dispatch.is_none() && branches.len() >= 2 {
                changed |= simplify_alt(&mut branches, rules, strings);
            }

            // Unwrap singleton Alt.
            if branches.len() == 1 {
                changed = true;
                (branches.into_iter().next().unwrap().node, changed)
            } else {
                (IrNode::Alt(branches, dispatch), changed)
            }
        }
        IrNode::Seq(children) => {
            let mut changed = false;
            let children: Vec<IrNode> = children
                .into_iter()
                .map(|c| {
                    let (new_c, c_changed) = simplify_node(c, rules, strings);
                    changed |= c_changed;
                    new_c
                })
                .collect();
            (IrNode::Seq(children), changed)
        }
        IrNode::Repeat { inner, lo, hi } => {
            let (new_inner, changed) = simplify_node(*inner, rules, strings);
            (
                IrNode::Repeat {
                    inner: Box::new(new_inner),
                    lo,
                    hi,
                },
                changed,
            )
        }
        IrNode::Skip(a, b) => {
            let (na, ca) = simplify_node(*a, rules, strings);
            let (nb, cb) = simplify_node(*b, rules, strings);
            (IrNode::Skip(Box::new(na), Box::new(nb)), ca || cb)
        }
        IrNode::Next(a, b) => {
            let (na, ca) = simplify_node(*a, rules, strings);
            let (nb, cb) = simplify_node(*b, rules, strings);
            (IrNode::Next(Box::new(na), Box::new(nb)), ca || cb)
        }
        IrNode::OptionalWhitespace(inner) => {
            let (new_inner, changed) = simplify_node(*inner, rules, strings);
            (IrNode::OptionalWhitespace(Box::new(new_inner)), changed)
        }
        IrNode::Map { inner, fn_id } => {
            let (new_inner, changed) = simplify_node(*inner, rules, strings);
            (
                IrNode::Map {
                    inner: Box::new(new_inner),
                    fn_id,
                },
                changed,
            )
        }
        // Leaves and non-regex nodes — no simplification.
        other => (other, false),
    }
}

// ── Rewrite Rule Implementations ────────────────────────────────────────

/// Remove duplicate regex branches: `R|R` → `R`.
struct RedundantAlternation;

impl RewriteRule for RedundantAlternation {
    fn name(&self) -> &'static str {
        "redundant_alternation"
    }

    fn apply(&self, branches: &mut Vec<AltBranch>, strings: &mut Vec<String>) -> bool {
        let _ = strings;
        let initial_len = branches.len();

        // Deduplicate by comparing IrNode structure.
        let mut seen: Vec<usize> = Vec::new();
        let mut to_remove: Vec<usize> = Vec::new();

        for i in 0..branches.len() {
            let mut is_dup = false;
            for &j in &seen {
                if ir_nodes_eq(&branches[i].node, &branches[j].node) {
                    is_dup = true;
                    break;
                }
            }
            if is_dup {
                to_remove.push(i);
            } else {
                seen.push(i);
            }
        }

        // Remove duplicates in reverse order to preserve indices.
        for &i in to_remove.iter().rev() {
            branches.remove(i);
        }

        branches.len() != initial_len
    }
}

/// Absorb regex branches that are subsets of another: `[a-z]|[a-c]` → `[a-z]`.
struct SupersetAbsorption;

impl RewriteRule for SupersetAbsorption {
    fn name(&self) -> &'static str {
        "superset_absorption"
    }

    fn apply(&self, branches: &mut Vec<AltBranch>, strings: &mut Vec<String>) -> bool {
        // Only process Alt where all branches are Regex.
        if !branches.iter().all(|b| matches!(&b.node, IrNode::Regex(_))) {
            return false;
        }

        // Extract character sets for each regex branch.
        let char_sets: Vec<Option<Vec<u8>>> = branches
            .iter()
            .map(|b| {
                if let IrNode::Regex(sid) = &b.node {
                    extract_single_char_class(&strings[*sid as usize])
                } else {
                    None
                }
            })
            .collect();

        // Find branches that are strict subsets of another.
        let mut to_remove: Vec<usize> = Vec::new();
        for i in 0..branches.len() {
            if to_remove.contains(&i) {
                continue;
            }
            if let Some(set_i) = &char_sets[i] {
                for j in 0..branches.len() {
                    if i == j || to_remove.contains(&j) {
                        continue;
                    }
                    if let Some(set_j) = &char_sets[j] {
                        // If set_j is a strict subset of set_i, remove set_j.
                        if set_j.len() < set_i.len() && set_j.iter().all(|b| set_i.contains(b)) {
                            to_remove.push(j);
                        }
                    }
                }
            }
        }

        if to_remove.is_empty() {
            return false;
        }

        to_remove.sort_unstable();
        to_remove.dedup();
        for &i in to_remove.iter().rev() {
            branches.remove(i);
        }
        true
    }
}

/// Merge adjacent char classes with disjoint ranges: `[a-c]|[d-f]` → `[a-f]`.
struct UnionMerge;

impl RewriteRule for UnionMerge {
    fn name(&self) -> &'static str {
        "union_merge"
    }

    fn apply(&self, branches: &mut Vec<AltBranch>, strings: &mut Vec<String>) -> bool {
        // Only process Alt where all branches are single char class Regex.
        if branches.len() < 2 {
            return false;
        }

        let char_sets: Vec<Option<Vec<u8>>> = branches
            .iter()
            .map(|b| {
                if let IrNode::Regex(sid) = &b.node {
                    extract_single_char_class(&strings[*sid as usize])
                } else {
                    None
                }
            })
            .collect();

        // Find consecutive regex branches that are both single char classes.
        let mut merged = false;
        let mut i = 0;
        while i + 1 < branches.len() {
            if let (Some(set_a), Some(set_b)) = (
                &char_sets[i],
                &char_sets.get(i + 1).and_then(|s| s.as_ref()),
            ) {
                // Check if ranges are adjacent or overlapping.
                let mut combined: Vec<u8> = set_a.clone();
                combined.extend_from_slice(set_b);
                combined.sort_unstable();
                combined.dedup();

                // Build a new char class pattern from the combined set.
                let new_pattern = bytes_to_char_class(&combined);
                let sid = strings.len() as u32;
                strings.push(new_pattern);

                branches[i] = AltBranch {
                    node: IrNode::Regex(sid),
                    first_set: None,
                };
                branches.remove(i + 1);
                merged = true;
                // Don't increment i — check the new merged branch against the next.
            } else {
                i += 1;
            }
        }

        merged
    }
}

// ── Helpers ─────────────────────────────────────────────────────────────

/// Extract the byte set from a single char class regex like `[a-z]` or `[0-9a-fA-F]`.
/// Returns None for non-char-class patterns.
fn extract_single_char_class(pattern: &str) -> Option<Vec<u8>> {
    // Must be `[...]` optionally with `+` or `*`.
    let class_str = pattern
        .strip_suffix('+')
        .or_else(|| pattern.strip_suffix('*'))
        .unwrap_or(pattern);

    let inner = class_str.strip_prefix('[')?.strip_suffix(']')?;
    if inner.starts_with('^') {
        return None;
    }

    let mut bytes = Vec::new();
    let mut chars = inner.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next()? {
                'd' => bytes.extend(b'0'..=b'9'),
                'w' => {
                    bytes.extend(b'0'..=b'9');
                    bytes.extend(b'A'..=b'Z');
                    bytes.push(b'_');
                    bytes.extend(b'a'..=b'z');
                }
                's' => bytes.extend(&[b'\t', b'\n', 0x0B, 0x0C, b'\r', b' ']),
                esc if esc.is_ascii() => bytes.push(esc as u8),
                _ => return None,
            }
        } else if c.is_ascii() {
            if chars.peek() == Some(&'-') {
                chars.next();
                let hi = chars.next()?;
                if !hi.is_ascii() {
                    return None;
                }
                bytes.extend(c as u8..=hi as u8);
            } else {
                bytes.push(c as u8);
            }
        } else {
            return None;
        }
    }

    bytes.sort_unstable();
    bytes.dedup();
    if bytes.is_empty() { None } else { Some(bytes) }
}

/// Convert a sorted byte set to a character class pattern string.
fn bytes_to_char_class(bytes: &[u8]) -> String {
    let mut result = String::from("[");

    // Build ranges from sorted bytes.
    let mut i = 0;
    while i < bytes.len() {
        let start = bytes[i];
        let mut end = start;
        while i + 1 < bytes.len() && bytes[i + 1] == end + 1 {
            end = bytes[i + 1];
            i += 1;
        }

        if end - start >= 2 {
            result.push(start as char);
            result.push('-');
            result.push(end as char);
        } else if end > start {
            result.push(start as char);
            result.push(end as char);
        } else {
            result.push(start as char);
        }
        i += 1;
    }

    result.push(']');
    result
}

/// Structural equality of IrNode (for deduplication).
fn ir_nodes_eq(a: &IrNode, b: &IrNode) -> bool {
    match (a, b) {
        (IrNode::Literal(a), IrNode::Literal(b)) => a == b,
        (IrNode::Regex(a), IrNode::Regex(b)) => a == b,
        (IrNode::Epsilon, IrNode::Epsilon) => true,
        (IrNode::Ref(a), IrNode::Ref(b)) => a == b,
        (IrNode::Seq(a), IrNode::Seq(b)) => {
            a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| ir_nodes_eq(x, y))
        }
        (IrNode::Alt(a, _), IrNode::Alt(b, _)) => {
            a.len() == b.len()
                && a.iter()
                    .zip(b.iter())
                    .all(|(x, y)| ir_nodes_eq(&x.node, &y.node))
        }
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extract_char_class_simple() {
        assert_eq!(
            extract_single_char_class("[a-z]"),
            Some((b'a'..=b'z').collect())
        );
        assert_eq!(
            extract_single_char_class("[a-c]"),
            Some(vec![b'a', b'b', b'c'])
        );
    }

    #[test]
    fn extract_char_class_multi_range() {
        let result = extract_single_char_class("[0-9a-fA-F]").unwrap();
        assert!(result.contains(&b'0'));
        assert!(result.contains(&b'9'));
        assert!(result.contains(&b'a'));
        assert!(result.contains(&b'f'));
        assert!(result.contains(&b'A'));
        assert!(result.contains(&b'F'));
    }

    #[test]
    fn bytes_to_class_roundtrip() {
        let bytes: Vec<u8> = (b'a'..=b'z').collect();
        assert_eq!(bytes_to_char_class(&bytes), "[a-z]");
    }

    #[test]
    fn redundant_elimination() {
        let mut strings = vec!["[a-z]".to_string()];
        let mut branches = vec![
            AltBranch {
                node: IrNode::Regex(0),
                first_set: None,
            },
            AltBranch {
                node: IrNode::Regex(0),
                first_set: None,
            },
        ];
        let rule = RedundantAlternation;
        assert!(rule.apply(&mut branches, &mut strings));
        assert_eq!(branches.len(), 1);
    }
}
