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
                        // Wrap each alternative in a non-capturing group if it
                        // contains an unescaped `|` at the top level.
                        let pattern = &strings[*sid as usize];
                        if pattern_has_top_level_pipe(pattern) {
                            format!("(?:{})", pattern)
                        } else {
                            pattern.clone()
                        }
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
    use std::collections::HashMap;
    use crate::{IrRule, RuleMeta};

    fn make_ir(body: IrNode, strings: Vec<String>) -> GrammarIR {
        GrammarIR {
            entry: 0,
            rules: vec![IrRule {
                id: 0,
                name: 0,
                body,
                meta: RuleMeta::default(),
            }],
            strings,
            fns: vec![],
            types: vec![],
            follow_sets: HashMap::new(),
        }
    }

    #[test]
    fn merges_two_regex_alts() {
        let mut ir = make_ir(
            IrNode::Alt(
                vec![
                    AltBranch { node: IrNode::Regex(1), first_set: None },
                    AltBranch { node: IrNode::Regex(2), first_set: None },
                ],
                None,
            ),
            vec!["rule".into(), "[a-z]+".into(), "[0-9]+".into()],
        );
        merge_regex_alts(&mut ir);
        match &ir.rules[0].body {
            IrNode::Regex(sid) => {
                assert_eq!(ir.get_string(*sid), "[a-z]+|[0-9]+");
            }
            other => panic!("Expected Regex, got {:?}", other),
        }
    }

    #[test]
    fn merges_three_css_property_regex() {
        let mut ir = make_ir(
            IrNode::Alt(
                vec![
                    AltBranch { node: IrNode::Regex(1), first_set: None },
                    AltBranch { node: IrNode::Regex(2), first_set: None },
                    AltBranch { node: IrNode::Regex(3), first_set: None },
                ],
                None,
            ),
            vec![
                "rule".into(),
                r"[a-zA-Z_][\w-]*".into(),
                r"--[\w-]+".into(),
                r"-[a-zA-Z][\w-]*".into(),
            ],
        );
        merge_regex_alts(&mut ir);
        match &ir.rules[0].body {
            IrNode::Regex(sid) => {
                assert_eq!(
                    ir.get_string(*sid),
                    r"[a-zA-Z_][\w-]*|--[\w-]+|-[a-zA-Z][\w-]*"
                );
            }
            other => panic!("Expected Regex, got {:?}", other),
        }
    }

    #[test]
    fn skips_non_regex_alts() {
        let mut ir = make_ir(
            IrNode::Alt(
                vec![
                    AltBranch { node: IrNode::Regex(1), first_set: None },
                    AltBranch { node: IrNode::Literal(2), first_set: None },
                ],
                None,
            ),
            vec!["rule".into(), "[a-z]+".into(), "x".into()],
        );
        merge_regex_alts(&mut ir);
        // Should not merge — not all branches are Regex.
        assert!(matches!(&ir.rules[0].body, IrNode::Alt(..)));
    }

    #[test]
    fn wraps_pipe_containing_patterns() {
        let mut ir = make_ir(
            IrNode::Alt(
                vec![
                    AltBranch { node: IrNode::Regex(1), first_set: None },
                    AltBranch { node: IrNode::Regex(2), first_set: None },
                ],
                None,
            ),
            vec!["rule".into(), "a|b".into(), "c".into()],
        );
        merge_regex_alts(&mut ir);
        match &ir.rules[0].body {
            IrNode::Regex(sid) => {
                assert_eq!(ir.get_string(*sid), "(?:a|b)|c");
            }
            other => panic!("Expected Regex, got {:?}", other),
        }
    }

    #[test]
    fn top_level_pipe_detection() {
        assert!(pattern_has_top_level_pipe("a|b"));
        assert!(!pattern_has_top_level_pipe("(?:a|b)"));
        assert!(!pattern_has_top_level_pipe("[a|b]"));
        assert!(pattern_has_top_level_pipe("(?:a|b)|c"));
        assert!(!pattern_has_top_level_pipe(r"a\|b"));
    }
}
