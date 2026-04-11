//! Alias detection for AST-level diagnostics.
//!
//! Tranche AC.2: rewritten against the tape-first view surface.

use std::collections::{HashMap, HashSet};

use crate::grammar::generated::{BbnfBootstrapNodeView, BbnfBootstrapRuleKind};
use crate::types::AST;

/// Find rules whose RHS is simply a reference to another nonterminal.
pub fn find_aliases<'a>(
    ast: &'a AST<'a>,
    cyclic_rules: &HashSet<&'a str>,
) -> HashMap<&'a str, &'a str> {
    let mut aliases = HashMap::new();

    for (&name, entry) in ast {
        if cyclic_rules.contains(name) {
            continue;
        }

        if let Some(target) = extract_alias_target(entry.rhs) {
            if ast.contains_key(target) {
                aliases.insert(name, target);
            }
        }
    }

    aliases
}

/// Extract the target nonterminal name if the expression is a simple
/// alias (possibly grouped).
fn extract_alias_target<'a>(node: BbnfBootstrapNodeView<'a>) -> Option<&'a str> {
    match node.rule_kind() {
        // Direct identifier reference
        BbnfBootstrapRuleKind::identifier => Some(node.span_text()),

        // term_1 without call args = plain nonterminal reference
        BbnfBootstrapRuleKind::term_1 => {
            let ident = node.child(0)?;
            let call_args = node.child(1);
            let no_call = call_args
                .map(|c| c.span().1 == c.span().0)
                .unwrap_or(true);
            if no_call {
                Some(ident.span_text())
            } else {
                None
            }
        }

        // Transparent structural wrapper.
        BbnfBootstrapRuleKind::term => {
            let inner = node.child(0)?;
            extract_alias_target(inner)
        }

        // factor = (comment_before?, term, modifier?, comment_after?)
        // — unwrap when all three optional slots are absent.
        BbnfBootstrapRuleKind::factor => {
            let term = node.child(1)?;
            let modifier = node.child(2);
            let comment_before = node.child(0);
            let comment_after = node.child(3);
            let all_bare = comment_before
                .map(|c| c.span().1 == c.span().0)
                .unwrap_or(true)
                && modifier.map(|m| m.span().1 == m.span().0).unwrap_or(true)
                && comment_after
                    .map(|c| c.span().1 == c.span().0)
                    .unwrap_or(true);
            if all_bare {
                extract_alias_target(term)
            } else {
                None
            }
        }

        // mapped_factor = (inner, mapping?) — unwrap when the
        // mapping slot is absent.
        BbnfBootstrapRuleKind::mapped_factor => {
            let inner = node.child(0)?;
            let mapping = node.child(1);
            let no_mapping = mapping
                .map(|m| m.span().1 == m.span().0)
                .unwrap_or(true);
            if no_mapping {
                extract_alias_target(inner)
            } else {
                None
            }
        }

        // Grouped expression: (rhs) — `term_2` with an opening `(`.
        BbnfBootstrapRuleKind::term_2 => {
            let open = node.child(0)?;
            if open.span_text() == "(" {
                let inner = node.child(1)?;
                extract_alias_target(inner)
            } else {
                None
            }
        }

        // Single-branch alternation / single-element concatenation
        // / single-operand binary factor — descend transparently.
        BbnfBootstrapRuleKind::alternation | BbnfBootstrapRuleKind::call_arg => {
            let mut iter = node.children();
            let first = iter.next()?;
            if iter.next().is_some() {
                return None;
            }
            let branch = first.child(0).unwrap_or(first);
            extract_alias_target(branch)
        }
        BbnfBootstrapRuleKind::concatenation => {
            let mut iter = node.children();
            let first = iter.next()?;
            if iter.next().is_some() {
                return None;
            }
            let part = first.child(0).unwrap_or(first);
            extract_alias_target(part)
        }
        BbnfBootstrapRuleKind::binary_factor => {
            let first = node.child(0)?;
            let rest = node.child(1);
            let rest_empty = rest.map(|r| r.children().next().is_none()).unwrap_or(true);
            if rest_empty {
                extract_alias_target(first)
            } else {
                None
            }
        }

        _ => None,
    }
}
