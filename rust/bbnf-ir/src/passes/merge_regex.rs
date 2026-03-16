//! Pass: Merge adjacent regex alternatives into a single combined regex.
//!
//! Detects `Alt([Regex(a), Regex(b), ...])` where all branches are bare `Regex` nodes
//! and merges them into a single `Regex("a|b|...")`. This eliminates backtracking
//! overhead from chained `.or()` calls when a single regex engine call suffices.
//!
//! Primary target: CSS `propertyName` which has 3 regex alternatives that can be
//! fused into one pattern.

use crate::{AltBranch, GrammarIR, IrNode};

/// Merge adjacent regex alternatives across the entire IR.
pub fn merge_regex_alts(ir: &mut GrammarIR) {
    for rule in &mut ir.rules {
        rule.body = merge_regex_in_node(std::mem::replace(&mut rule.body, IrNode::Epsilon), &mut ir.strings);
    }
}

fn merge_regex_in_node(node: IrNode, strings: &mut Vec<String>) -> IrNode {
    match node {
        IrNode::Alt(branches, dispatch) => {
            // Check if ALL branches are bare Regex nodes (no Map wrappers, no dispatch).
            // Only merge when there's no dispatch table (dispatch handles perf already).
            let all_regex = dispatch.is_none()
                && branches.len() >= 2
                && branches.iter().all(|b| matches!(&b.node, IrNode::Regex(_)));

            if all_regex {
                // Fuse all regex patterns with `|` into a single combined pattern.
                let combined: String = branches
                    .iter()
                    .map(|b| {
                        let IrNode::Regex(sid) = &b.node else {
                            unreachable!()
                        };
                        // Wrap ALL patterns in non-capturing groups unconditionally
                        // to prevent capture group renumbering and ensure semantic
                        // equivalence regardless of internal structure.
                        let pattern = &strings[*sid as usize];
                        format!("(?:{})", pattern)
                    })
                    .collect::<Vec<_>>()
                    .join("|");

                let sid = strings.len() as u32;
                strings.push(combined);
                IrNode::Regex(sid)
            } else {
                // Recurse into branches.
                let branches = branches
                    .into_iter()
                    .map(|b| AltBranch {
                        node: merge_regex_in_node(b.node, strings),
                        first_set: b.first_set,
                    })
                    .collect();
                IrNode::Alt(branches, dispatch)
            }
        }
        IrNode::Seq(children) => {
            IrNode::Seq(children.into_iter().map(|c| merge_regex_in_node(c, strings)).collect())
        }
        IrNode::Repeat { inner, lo, hi } => IrNode::Repeat {
            inner: Box::new(merge_regex_in_node(*inner, strings)),
            lo,
            hi,
        },
        IrNode::Skip(a, b) => IrNode::Skip(
            Box::new(merge_regex_in_node(*a, strings)),
            Box::new(merge_regex_in_node(*b, strings)),
        ),
        IrNode::Next(a, b) => IrNode::Next(
            Box::new(merge_regex_in_node(*a, strings)),
            Box::new(merge_regex_in_node(*b, strings)),
        ),
        IrNode::Minus(a, b) => IrNode::Minus(
            Box::new(merge_regex_in_node(*a, strings)),
            Box::new(merge_regex_in_node(*b, strings)),
        ),
        IrNode::Negate(inner) => {
            IrNode::Negate(Box::new(merge_regex_in_node(*inner, strings)))
        }
        IrNode::OptionalWhitespace(inner) => {
            IrNode::OptionalWhitespace(Box::new(merge_regex_in_node(*inner, strings)))
        }
        IrNode::Map { inner, fn_id } => IrNode::Map {
            inner: Box::new(merge_regex_in_node(*inner, strings)),
            fn_id,
        },
        // Leaves — no recursion needed.
        node @ (IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_)) => node,
    }
}

/// Check if a regex pattern contains a top-level `|` (pipe) character.
/// We track parenthesis/bracket depth to avoid false positives from `|` inside
/// groups like `(?:a|b)` or character classes like `[a|b]`.
fn pattern_has_top_level_pipe(pattern: &str) -> bool {
    let mut depth = 0u32;
    let mut in_bracket = false;
    let mut escape = false;

    for c in pattern.chars() {
        if escape {
            escape = false;
            continue;
        }
        if c == '\\' {
            escape = true;
            continue;
        }
        if in_bracket {
            if c == ']' {
                in_bracket = false;
            }
            continue;
        }
        match c {
            '[' => in_bracket = true,
            '(' => depth += 1,
            ')' => depth = depth.saturating_sub(1),
            '|' if depth == 0 => return true,
            _ => {}
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn top_level_pipe_detection() {
        assert!(pattern_has_top_level_pipe("a|b"));
        assert!(!pattern_has_top_level_pipe("(?:a|b)"));
        assert!(!pattern_has_top_level_pipe("[a|b]"));
        assert!(pattern_has_top_level_pipe("(?:a|b)|c"));
        assert!(!pattern_has_top_level_pipe(r"a\|b"));
    }
}
