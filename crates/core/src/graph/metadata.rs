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
use crate::lower::tape_walk::{find_descendant_by_kind, peel_transparent};
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
        BbnfBootstrapRuleKind::grammar_item
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
                // Grouped `( expr )` form — descend into the inner
                // expression. Find the first substantive non-literal
                // descendant.
                let inner = find_semantic_child(node)?;
                return extract_alias_target(peel_transparent(inner));
            }
            if matches!(leading, Some(b'[') | Some(b'{') | Some(b'@')) {
                // Optional / repetition / host-call — not an alias.
                return None;
            }
            // Bare term: `identifier (call_args)?` / `literal` / `regex` / `ε`.
            // Alias only when the sole substantive child is an identifier.
            //
            // Under DTA the identifier and any call_args compound
            // sit inside a Seq wrapper on the term compound, so a
            // direct-child scan misses them. Descend to the first
            // identifier descendant; call-args detection uses the
            // `call_arg` rule_kind directly, which is semantically
            // tighter than "any non-identifier substantive child"
            // and immune to the Seq-wrapper false positive.
            let ident =
                find_descendant_by_kind(node, BbnfBootstrapRuleKind::identifier)?;
            let has_call_args =
                find_descendant_by_kind(node, BbnfBootstrapRuleKind::call_arg).is_some();
            if has_call_args {
                None
            } else {
                Some(ident.span_text())
            }
        }

        // factor = (comment_before?, term, modifier?, comment_after?)
        // — unwrap when all three optional slots are absent.
        //
        // Under DTA the factor body is wrapped in a Seq compound, so
        // the modifier and term may sit one compound deeper than the
        // direct children. Descend to them by rule_kind.
        BbnfBootstrapRuleKind::factor => {
            let modifier = find_descendant_by_kind(node, BbnfBootstrapRuleKind::modifier);
            let has_modifier = modifier
                .map(|m| m.span().1 > m.span().0)
                .unwrap_or(false);
            if has_modifier {
                return None;
            }
            // Under tape-first, the `term` child may not exist as a
            // separate Rule record. Check for it explicitly, then fall
            // back to the factor's span text for a bare identifier.
            if let Some(term) = find_descendant_by_kind(node, BbnfBootstrapRuleKind::term) {
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
        //
        // Under DTA the mapped_factor body is wrapped in a Seq
        // compound; the positional child(0) / child(1) reads picked
        // up stale slots. The mapping's semantic leading rule is
        // `value_expr` — absence of a value_expr descendant is the
        // DTA-shape-stable test for "no mapping". The inner
        // expression is resolved via find_descendant_by_kind
        // against the inner layer (factor / term).
        BbnfBootstrapRuleKind::mapped_factor => {
            let has_mapping = find_descendant_by_kind(
                node,
                BbnfBootstrapRuleKind::value_expr,
            )
            .is_some();
            if has_mapping {
                return None;
            }
            // Look for the inner expression (factor / term /
            // identifier). `find_descendant_by_kind` hits the first
            // semantic rule in document order — factor precedes the
            // mapping slot. Fall back to a bare-ident span check.
            if let Some(inner) = find_descendant_by_kind(
                node,
                BbnfBootstrapRuleKind::factor,
            )
            .or_else(|| find_descendant_by_kind(node, BbnfBootstrapRuleKind::term))
            {
                return extract_alias_target(inner);
            }
            let text = node.span_text().trim();
            if !text.is_empty() && super::deps::is_ident(text.as_bytes()) {
                Some(text)
            } else {
                None
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
            use crate::runtime::tape::TapeKind;
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
