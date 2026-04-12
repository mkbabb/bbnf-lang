//! Alias detection for AST-level diagnostics.
//!
//! Tranche AC.2: rewritten against the tape-first view surface.
//!
//! Tranche AF.0: shape-agnostic `term` handling — grouped
//! expressions and plain-reference shapes are distinguished by
//! leading-byte dispatch and by the presence of substantive
//! non-identifier children, not by sub-variant wrapper kinds
//! that structural-mode dedup may or may not produce. Every
//! rule_kind reference here is a canonical grammar rule name.

use std::collections::{HashMap, HashSet};

use crate::grammar::generated::{BbnfBootstrapNodeView, BbnfBootstrapRuleKind};
use crate::lower::tape_walk::{find_child_by_kind, peel_transparent};
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

        // Transparent wrappers — descend into the single inner child.
        BbnfBootstrapRuleKind::rhs
        | BbnfBootstrapRuleKind::grammar_item
        | BbnfBootstrapRuleKind::directive
        | BbnfBootstrapRuleKind::lhs => {
            node.child(0).and_then(extract_alias_target)
        }

        // `term = ε | identifier (call_args)? | literal | regex
        //       | "(" rhs ")" | "[" rhs "]" | "{" rhs "}" | "@{" rhs "}"`
        //
        // Structural-mode dedup may collapse the per-branch `term_N`
        // wrappers, so dispatch on content:
        //
        // - Leading byte `(` / `[` / `{` / `@{` → grouped form. Only
        //   `(rhs)` preserves alias semantics (the other wrappers
        //   imply optional / repetition / host-call); descend into
        //   the inner `rhs` compound found by `rule_kind` lookup.
        // - Otherwise → bare term. If there's exactly one substantive
        //   non-wrapper child and it's an `identifier` with no
        //   accompanying call-args compound, the term is a plain
        //   reference.
        BbnfBootstrapRuleKind::term => {
            let leading = node.span_text().as_bytes().first().copied();
            if leading == Some(b'(') {
                // Grouped `( rhs )` form — descend into the inner
                // expression. Prefer the explicit `rhs` child under
                // structural mode; fall back to the first
                // substantive non-literal descendant otherwise.
                let inner = find_child_by_kind(node, BbnfBootstrapRuleKind::rhs)
                    .or_else(|| find_semantic_child(node))?;
                return extract_alias_target(peel_transparent(inner));
            }
            if matches!(leading, Some(b'[') | Some(b'{') | Some(b'@')) {
                // Optional / repetition / host-call — not an alias.
                return None;
            }
            // Bare term: `identifier (call_args)?` / `literal` / `regex` / `ε`.
            // Alias only when the sole substantive child is an identifier.
            let ident = find_child_by_kind(node, BbnfBootstrapRuleKind::identifier)?;
            let has_call_args = node.children().any(|c| {
                let k = c.rule_kind();
                k != BbnfBootstrapRuleKind::identifier
                    && k != BbnfBootstrapRuleKind::comment
                    && k != BbnfBootstrapRuleKind::big_comment
                    && c.span().1 > c.span().0
            });
            if has_call_args {
                None
            } else {
                Some(ident.span_text())
            }
        }

        // factor = (comment_before?, term, modifier?, comment_after?)
        // — unwrap when all three optional slots are absent.
        BbnfBootstrapRuleKind::factor => {
            let modifier = find_child_by_kind(node, BbnfBootstrapRuleKind::modifier);
            let has_modifier = modifier
                .map(|m| m.span().1 > m.span().0)
                .unwrap_or(false);
            if has_modifier {
                return None;
            }
            // Under tape-first, the `term` child may not exist as a
            // separate Rule record. Check for it explicitly, then fall
            // back to the factor's span text for a bare identifier.
            if let Some(term) = find_child_by_kind(node, BbnfBootstrapRuleKind::term) {
                extract_alias_target(term)
            } else {
                let text = node.span_text().trim();
                if !text.is_empty() && super::deps::is_ident(text.as_bytes()) {
                    Some(text)
                } else {
                    None
                }
            }
        }

        // mapped_factor = (inner, mapping?) — unwrap when the
        // mapping slot is absent.
        BbnfBootstrapRuleKind::mapped_factor => {
            let mapping = node.child(1);
            let no_mapping = mapping
                .map(|m| m.span().1 == m.span().0)
                .unwrap_or(true);
            if !no_mapping {
                return None;
            }
            // Under the tape-first parser, child(0) may be an empty
            // wrapper when the term body was consumed span-only.
            // Fall back to the mapped_factor's own span text.
            let inner = node.child(0)?;
            let (ilo, ihi) = inner.span();
            if ihi > ilo {
                extract_alias_target(inner)
            } else {
                // Empty wrapper — check the parent's span for a bare ident.
                let text = node.span_text().trim();
                if !text.is_empty() && super::deps::is_ident(text.as_bytes()) {
                    Some(text)
                } else {
                    None
                }
            }
        }

        // Single-branch alternation / single-element concatenation
        // / single-operand binary factor — descend transparently.
        BbnfBootstrapRuleKind::alternation | BbnfBootstrapRuleKind::call_arg => {
            let branches: Vec<_> = super::deps::iter_tape_iteration_views(node);
            if branches.len() != 1 {
                return None;
            }
            extract_alias_target(branches[0])
        }
        BbnfBootstrapRuleKind::concatenation => {
            let parts: Vec<_> = super::deps::iter_tape_iteration_views(node);
            if parts.len() != 1 {
                return None;
            }
            extract_alias_target(parts[0])
        }
        BbnfBootstrapRuleKind::binary_factor => {
            let operands: Vec<_> = super::deps::collect_tape_binary_operand_views(node);
            if operands.len() != 1 {
                return None;
            }
            extract_alias_target(operands[0])
        }

        // Anonymous wrapper compounds (variant_idx=0 collides with
        // `int_lit`): peel to the single substantive Rule child and
        // re-dispatch. When no Rule child exists but the span text
        // is a bare identifier, treat it as a direct reference.
        BbnfBootstrapRuleKind::int_lit | BbnfBootstrapRuleKind::Unknown => {
            use ::bbnf::runtime::tape::TapeKind;
            let substantive: Vec<BbnfBootstrapNodeView<'a>> = node
                .children()
                .filter(|c| c.kind() == TapeKind::Rule)
                .collect();
            match substantive.len() {
                1 => extract_alias_target(substantive[0]),
                0 => {
                    // No Rule children — check if span is a bare identifier.
                    let text = node.span_text().trim();
                    if !text.is_empty() && super::deps::is_ident(text.as_bytes()) {
                        Some(text)
                    } else {
                        None
                    }
                }
                _ => None,
            }
        }

        _ => None,
    }
}

/// Find the first substantive (non-literal-punctuation, non-empty)
/// child of a view — used as a fallback when a named `rhs` child
/// isn't available because structural-mode dedup elided the wrapper.
///
/// "Substantive" here means: compound children with non-zero span
/// whose rule_kind isn't one of the grammar's literal punctuation
/// slots (no `identifier`, `literal`, etc. filter — we want
/// whichever expression layer sits under the opening `(`).
fn find_semantic_child<'a>(
    view: BbnfBootstrapNodeView<'a>,
) -> Option<BbnfBootstrapNodeView<'a>> {
    view.children().find(|c| {
        let (lo, hi) = c.span();
        if hi <= lo {
            return false;
        }
        // Skip the opening / closing delimiter literals — they carry
        // a single byte of span and no rule_kind of interest.
        let text = c.span_text();
        !matches!(text, "(" | ")" | "[" | "]" | "{" | "}" | "@{")
    })
}
