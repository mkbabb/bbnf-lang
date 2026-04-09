use bbnf::grammar::generated::BbnfBootstrapEnum;

use super::super::types::{token_types, SemanticTokenInfo};

/// Collect semantic tokens from a bootstrap AST node.
pub fn collect_semantic_tokens(node: &BbnfBootstrapEnum<'_>, tokens: &mut Vec<SemanticTokenInfo>) {
    match node {
        // Identifier — nonterminal reference
        BbnfBootstrapEnum::identifier(s) => {
            tokens.push(SemanticTokenInfo {
                span: (s.start, s.end),
                token_type: token_types::RULE_REFERENCE,
            });
        }

        // term_1: identifier + optional call args
        BbnfBootstrapEnum::term_1((ident, call_args)) => {
            let ident_span = match ident {
                BbnfBootstrapEnum::identifier(s) => (s.start, s.end),
                _ => (0, 0),
            };
            tokens.push(SemanticTokenInfo {
                span: ident_span,
                token_type: token_types::RULE_REFERENCE,
            });
            if let Some((_open, first_arg, rest_args, _close)) = call_args {
                collect_semantic_tokens(first_arg, tokens);
                for (_comma, arg) in *rest_args {
                    collect_semantic_tokens(arg, tokens);
                }
            }
        }

        // Literals
        BbnfBootstrapEnum::literal(s) => {
            tokens.push(SemanticTokenInfo {
                span: (s.start, s.end),
                token_type: token_types::STRING,
            });
        }

        // Regex
        BbnfBootstrapEnum::regex(s) => {
            tokens.push(SemanticTokenInfo {
                span: (s.start, s.end),
                token_type: token_types::REGEXP,
            });
        }

        // Epsilon
        BbnfBootstrapEnum::term_0(s) => {
            tokens.push(SemanticTokenInfo {
                span: (s.start, s.end),
                token_type: token_types::KEYWORD,
            });
        }

        // Structural: alternation, concatenation
        BbnfBootstrapEnum::alternation(branches) => {
            for (branch, _pipe) in *branches {
                collect_semantic_tokens(branch, tokens);
            }
        }
        BbnfBootstrapEnum::concatenation(parts) => {
            for (part, _comma) in *parts {
                collect_semantic_tokens(part, tokens);
            }
        }

        // Binary factor
        BbnfBootstrapEnum::binary_factor((first, rest)) => {
            collect_semantic_tokens(first, tokens);
            for (_, operand) in *rest {
                collect_semantic_tokens(operand, tokens);
            }
        }

        // Mapped factor
        BbnfBootstrapEnum::mapped_factor((inner, _mapping)) => {
            collect_semantic_tokens(inner, tokens);
        }

        // Factor
        BbnfBootstrapEnum::factor((_, term, _, _)) => {
            collect_semantic_tokens(term, tokens);
        }

        // Term variants
        BbnfBootstrapEnum::term(inner) => {
            collect_semantic_tokens(inner, tokens);
        }
        BbnfBootstrapEnum::term_2((_open, inner, _close)) => {
            collect_semantic_tokens(inner, tokens);
        }

        // Closure
        BbnfBootstrapEnum::closure((_pipe, _first_param, _params, _pipe2, body)) => {
            collect_semantic_tokens(body, tokens);
        }

        // Everything else — no tokens to emit
        _ => {}
    }
}
