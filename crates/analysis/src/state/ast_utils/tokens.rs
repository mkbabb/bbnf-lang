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

        // Anonymous wrapper compounds (variant_idx=0 collides with
        // `int_lit`): peel to substantive Rule children and recurse.
        // When no Rule children exist, check if the span text is a
        // bare identifier, literal, or regex for token emission.
        BbnfBootstrapRuleKind::int_lit | BbnfBootstrapRuleKind::Unknown => {
            use ::bbnf::runtime::tape::TapeKind;
            let mut found_rule = false;
            for c in node.children() {
                if c.kind() == TapeKind::Rule {
                    collect_semantic_tokens(c, tokens);
                    found_rule = true;
                }
            }
            if !found_rule {
                let text = node.span_text().trim();
                let (lo, hi) = node.span();
                if !text.is_empty() && lo < hi {
                    let lead_ws = node.span_text().len() - node.span_text().trim_start().len();
                    let trail_ws = node.span_text().len() - node.span_text().trim_end().len();
                    let tok_lo = lo as usize + lead_ws;
                    let tok_hi = hi as usize - trail_ws;
                    // Classify the leaf token by its first byte.
                    let first = text.as_bytes()[0];
                    if first == b'"' || first == b'\'' || first == b'`' {
                        tokens.push(SemanticTokenInfo {
                            span: (tok_lo, tok_hi),
                            token_type: token_types::STRING,
                        });
                    } else if first == b'/' {
                        tokens.push(SemanticTokenInfo {
                            span: (tok_lo, tok_hi),
                            token_type: token_types::REGEXP,
                        });
                    } else if first == b'_' || first.is_ascii_alphabetic() {
                        tokens.push(SemanticTokenInfo {
                            span: (tok_lo, tok_hi),
                            token_type: token_types::RULE_REFERENCE,
                        });
                    }
                }
            }
        }

        // Everything else — no tokens to emit.
        _ => {}
    }
}
