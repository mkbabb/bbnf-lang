//! Collect semantic tokens from a tape-first `BbnfBootstrapNodeView`
//! rule RHS.

use bbnf::grammar::generated::{BbnfBootstrapNodeView, BbnfBootstrapRuleKind};

use super::super::types::{SemanticTokenInfo, token_types};

/// Collect semantic tokens from a bootstrap tape view.
pub fn collect_semantic_tokens(
    node: BbnfBootstrapNodeView<'_>,
    tokens: &mut Vec<SemanticTokenInfo>,
) {
    match node.rule_kind() {
        // Identifier — nonterminal reference.
        BbnfBootstrapRuleKind::identifier => {
            let (lo, hi) = node.span();
            tokens.push(SemanticTokenInfo {
                span: (lo as usize, hi as usize),
                token_type: token_types::RULE_REFERENCE,
            });
        }

        // term_1: identifier + optional call args.
        BbnfBootstrapRuleKind::term_1 => {
            if let Some(ident) = node.child(0) {
                let (lo, hi) = ident.span();
                tokens.push(SemanticTokenInfo {
                    span: (lo as usize, hi as usize),
                    token_type: token_types::RULE_REFERENCE,
                });
            }
            for c in node.children().skip(1) {
                collect_semantic_tokens(c, tokens);
            }
        }

        // Literals.
        BbnfBootstrapRuleKind::literal => {
            let (lo, hi) = node.span();
            tokens.push(SemanticTokenInfo {
                span: (lo as usize, hi as usize),
                token_type: token_types::STRING,
            });
        }

        // Regex.
        BbnfBootstrapRuleKind::regex => {
            let (lo, hi) = node.span();
            tokens.push(SemanticTokenInfo {
                span: (lo as usize, hi as usize),
                token_type: token_types::REGEXP,
            });
        }

        // Epsilon: `ε` / `epsilon`.
        BbnfBootstrapRuleKind::term_0 => {
            let (lo, hi) = node.span();
            tokens.push(SemanticTokenInfo {
                span: (lo as usize, hi as usize),
                token_type: token_types::KEYWORD,
            });
        }

        // Structural: alternation, concatenation.
        BbnfBootstrapRuleKind::alternation | BbnfBootstrapRuleKind::concatenation => {
            for child in super::iter_iteration_views(node) {
                collect_semantic_tokens(child, tokens);
            }
        }

        // Binary factor: first + [(op, operand)].
        BbnfBootstrapRuleKind::binary_factor => {
            for operand in super::collect_binary_operand_views(node) {
                collect_semantic_tokens(operand, tokens);
            }
        }

        // Mapped factor: recurse into the inner factor child.
        BbnfBootstrapRuleKind::mapped_factor => {
            if let Some(inner) = node.child(0) {
                collect_semantic_tokens(inner, tokens);
            }
        }

        // Factor: find the term child and recurse.
        BbnfBootstrapRuleKind::factor => {
            for c in node.children() {
                if super::is_term_kind(c.rule_kind()) {
                    collect_semantic_tokens(c, tokens);
                    break;
                }
            }
        }

        // Transparent wrappers.
        BbnfBootstrapRuleKind::term
        | BbnfBootstrapRuleKind::rhs
        | BbnfBootstrapRuleKind::grammar_item
        | BbnfBootstrapRuleKind::directive
        | BbnfBootstrapRuleKind::lhs => {
            if let Some(inner) = node.child(0) {
                collect_semantic_tokens(inner, tokens);
            }
        }

        // Grouped: (`(`, inner, `)`) / (`[`, inner, `]`) / etc.
        BbnfBootstrapRuleKind::term_2 | BbnfBootstrapRuleKind::value_atom_0 => {
            if let Some(inner) = node.child(1) {
                collect_semantic_tokens(inner, tokens);
            }
        }

        // Closure: recurse into the body child.
        BbnfBootstrapRuleKind::closure => {
            if let Some(body) = node.child(4) {
                collect_semantic_tokens(body, tokens);
            }
        }

        // Everything else — no tokens to emit.
        _ => {}
    }
}
