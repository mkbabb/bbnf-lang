//! Host fold: lower `BbnfBootstrapEnum` → `ParsedGrammar`.
//!
//! Converts the generated parser's typed AST into the `Expression<'a>`
//! tree consumed by the rest of the compiler pipeline.

use std::borrow::Cow;

use parse_that::Span;

use super::generated::BbnfBootstrapEnum;
use crate::types::*;

/// Lower the generated parser output to a `ParsedGrammar`.
pub fn lower<'a>(ast: &'a BbnfBootstrapEnum<'a>) -> ParsedGrammar<'a> {
    let BbnfBootstrapEnum::grammar(items) = ast else {
        panic!("expected grammar variant, got {:?}", std::mem::discriminant(ast));
    };

    let mut imports = Vec::new();
    let mut recovers = Vec::new();
    let mut pretties = Vec::new();
    let mut ws_pattern = None;
    let mut debug_rules = Vec::new();
    let mut token_rules = Vec::new();
    let mut host_fns = Vec::new();
    let mut rules: AST<'a> = indexmap::IndexMap::new();

    for (_comment_before, item, _comment_after) in items.iter() {
        let inner = match item {
            BbnfBootstrapEnum::directive(d) => d,
            other => other,
        };
        lower_top_level_item(
            inner,
            &mut imports,
            &mut recovers,
            &mut pretties,
            &mut ws_pattern,
            &mut debug_rules,
            &mut token_rules,
            &mut host_fns,
            &mut rules,
        );
    }

    ParsedGrammar {
        imports,
        recovers,
        pretties,
        rules,
        ws_pattern,
        debug_rules,
        token_rules,
        host_fns,
    }
}

fn lower_top_level_item<'a>(
    item: &'a BbnfBootstrapEnum<'a>,
    imports: &mut Vec<ImportDirective<'a>>,
    recovers: &mut Vec<RecoverDirective<'a>>,
    pretties: &mut Vec<PrettyDirective<'a>>,
    ws_pattern: &mut Option<Cow<'a, str>>,
    debug_rules: &mut Vec<Cow<'a, str>>,
    token_rules: &mut Vec<Cow<'a, str>>,
    host_fns: &mut Vec<HostFnDecl<'a>>,
    rules: &mut AST<'a>,
) {
    match item {
        BbnfBootstrapEnum::rule((lhs_span, rhs, _terminator)) => {
            let lhs = Expression::Nonterminal(Token::new(
                Cow::Borrowed(lhs_span.as_str()),
                *lhs_span,
            ));
            let body = lower_rhs(rhs);
            let rule_expr = Expression::Rule(Box::new(body), None);
            rules.insert(lhs, rule_expr);
        }

        BbnfBootstrapEnum::import_directive((_kw, inner, _term)) => {
            lower_import(inner, imports);
        }

        BbnfBootstrapEnum::recover_directive((_kw, name, rhs, term)) => {
            let name_span = lower_identifier_span(name);
            let sync = lower_rhs(rhs);
            recovers.push(RecoverDirective {
                rule_name: Cow::Borrowed(name_span.as_str()),
                sync_expr: sync,
                span: *term,
            });
        }

        BbnfBootstrapEnum::pretty_directive((_kw, target, hints, term)) => {
            let target_str = match target {
                BbnfBootstrapEnum::pretty_directive_0(s) => Cow::Borrowed(s.as_str()),
                other => Cow::Owned(lower_identifier_str(other).to_string()),
            };
            let hint_strs: Vec<Cow<'a, str>> = hints
                .iter()
                .map(|h| Cow::Borrowed(lower_identifier_str(h)))
                .collect();
            pretties.push(PrettyDirective {
                rule_name: target_str,
                hints: hint_strs,
                span: *term,
            });
        }

        BbnfBootstrapEnum::ws_directive((_kw, regex_enum, _term)) => {
            if let BbnfBootstrapEnum::regex(s) = regex_enum {
                // Strip / delimiters
                let inner = &s.as_str()[1..s.as_str().len() - 1];
                *ws_pattern = Some(Cow::Borrowed(inner));
            }
        }

        BbnfBootstrapEnum::token_directive((_kw, name, _term)) => {
            token_rules.push(Cow::Owned(lower_identifier_str(name).to_string()));
        }

        BbnfBootstrapEnum::debug_directive((_kw, target, _term)) => {
            match target {
                BbnfBootstrapEnum::debug_directive_0(s) => {
                    debug_rules.push(Cow::Borrowed(s.as_str()));
                }
                other => {
                    debug_rules.push(Cow::Owned(lower_identifier_str(other).to_string()));
                }
            }
        }

        BbnfBootstrapEnum::host_directive((_kw, name, _term)) => {
            host_fns.push(HostFnDecl {
                name: Cow::Owned(lower_identifier_str(name).to_string()),
                return_type: None,
            });
        }

        // Directive wrapper — unwrap and recurse
        BbnfBootstrapEnum::directive(inner) => {
            lower_top_level_item(inner, imports, recovers, pretties, ws_pattern, debug_rules, token_rules, host_fns, rules);
        }

        _ => {} // Comments, etc.
    }
}

fn lower_import<'a>(inner: &'a BbnfBootstrapEnum<'a>, imports: &mut Vec<ImportDirective<'a>>) {
    match inner {
        BbnfBootstrapEnum::import_directive_0((items_or_path, path_span)) => {
            // Selective: { items } from "path"
            if let BbnfBootstrapEnum::import_items((_open, items, _close)) = items_or_path {
                let names: Vec<ImportedName<'a>> = std::iter::once(items.first())
                    .flatten()
                    .chain(items.iter().skip(1))
                    .map(|(_comma, name)| {
                        let s = lower_identifier_span(name);
                        ImportedName {
                            name: Cow::Borrowed(s.as_str()),
                            span: s,
                        }
                    })
                    .collect();
                let path = extract_import_path(path_span);
                imports.push(ImportDirective {
                    path: Cow::Borrowed(path),
                    span: *path_span,
                    items: Some(names),
                });
            }
        }
        BbnfBootstrapEnum::import_path(path_span) => {
            // Glob import: @import "path"
            let path = extract_import_path(path_span);
            imports.push(ImportDirective {
                path: Cow::Borrowed(path),
                span: *path_span,
                items: None,
            });
        }
        _ => {}
    }
}

/// Lower `rhs = closure | alternation`.
fn lower_rhs<'a>(rhs: &'a BbnfBootstrapEnum<'a>) -> Expression<'a> {
    match rhs {
        BbnfBootstrapEnum::closure((pipe1, params, pipe2, body)) => {
            let param_tokens: Vec<Token<'a, Cow<'a, str>>> = {
                let mut v = vec![Token::new(Cow::Borrowed(pipe1.as_str()), *pipe1)];
                // TODO: proper param extraction from the sep-by list
                v
            };
            let body_expr = lower_rhs(body);
            Expression::Closure(param_tokens, Box::new(Token::new(body_expr, *pipe2)))
        }
        BbnfBootstrapEnum::alternation(branches) => lower_alternation(branches),
        other => lower_expression(other),
    }
}

fn lower_alternation<'a>(
    branches: &'a [(&'a BbnfBootstrapEnum<'a>, Span<'a>)],
) -> Expression<'a> {
    if branches.len() == 1 {
        return lower_concatenation_dispatch(&branches[0].0);
    }
    let exprs: Vec<Expression<'a>> = branches
        .iter()
        .map(|(branch, _pipe)| lower_concatenation_dispatch(branch))
        .collect();
    let span = branches.first().map(|(_, s)| *s).unwrap_or_default();
    Expression::Alternation(Box::new(Token::new(exprs, span)))
}

fn lower_concatenation_dispatch<'a>(node: &'a BbnfBootstrapEnum<'a>) -> Expression<'a> {
    match node {
        BbnfBootstrapEnum::concatenation(parts) => lower_concatenation(parts),
        other => lower_expression(other),
    }
}

fn lower_concatenation<'a>(
    parts: &'a [(&'a BbnfBootstrapEnum<'a>, Span<'a>)],
) -> Expression<'a> {
    if parts.len() == 1 {
        return lower_binary_factor_dispatch(&parts[0].0);
    }
    let exprs: Vec<Expression<'a>> = parts
        .iter()
        .map(|(part, _comma)| lower_binary_factor_dispatch(part))
        .collect();
    let span = parts.first().map(|(_, s)| *s).unwrap_or_default();
    Expression::Concatenation(Box::new(Token::new(exprs, span)))
}

fn lower_binary_factor_dispatch<'a>(node: &'a BbnfBootstrapEnum<'a>) -> Expression<'a> {
    match node {
        BbnfBootstrapEnum::binary_factor((first, rest)) => {
            let mut result = lower_mapped_factor_dispatch(first);
            for (op, operand) in rest.iter() {
                let rhs = lower_mapped_factor_dispatch(operand);
                let op_str = lower_identifier_str(op);
                result = match op_str {
                    "<<" => Expression::Skip(
                        Box::new(Token::new(result, Span::default())),
                        Box::new(Token::new(rhs, Span::default())),
                    ),
                    ">>" => Expression::Next(
                        Box::new(Token::new(result, Span::default())),
                        Box::new(Token::new(rhs, Span::default())),
                    ),
                    "-" => Expression::Minus(
                        Box::new(Token::new(result, Span::default())),
                        Box::new(Token::new(rhs, Span::default())),
                    ),
                    _ => result,
                };
            }
            result
        }
        other => lower_expression(other),
    }
}

fn lower_mapped_factor_dispatch<'a>(node: &'a BbnfBootstrapEnum<'a>) -> Expression<'a> {
    match node {
        BbnfBootstrapEnum::mapped_factor((inner, mapping)) => {
            let base = lower_factor_dispatch(inner);
            if let Some((_arrow, (_value_expr, _type_ann))) = mapping {
                // TODO: full MapArrow lowering for -> syntax
                base
            } else {
                base
            }
        }
        other => lower_expression(other),
    }
}

fn lower_factor_dispatch<'a>(node: &'a BbnfBootstrapEnum<'a>) -> Expression<'a> {
    match node {
        BbnfBootstrapEnum::factor((_comment_before, term, modifier, _comment_after)) => {
            let base = lower_term_dispatch(term);
            match modifier {
                Some(BbnfBootstrapEnum::modifier(s)) => match s.as_str() {
                    "?" => Expression::Optional(Box::new(Token::new(base, *s))),
                    "*" => Expression::Many(Box::new(Token::new(base, *s))),
                    "+" => Expression::Many1(Box::new(Token::new(base, *s))),
                    "?w" => Expression::OptionalWhitespace(Box::new(Token::new(base, *s))),
                    _ => base,
                },
                _ => base,
            }
        }
        other => lower_expression(other),
    }
}

fn lower_term_dispatch<'a>(node: &'a BbnfBootstrapEnum<'a>) -> Expression<'a> {
    match node {
        BbnfBootstrapEnum::term(inner) => lower_term_dispatch(inner),

        // Epsilon: "ε" or "epsilon"
        BbnfBootstrapEnum::term_0(s) => {
            Expression::Epsilon(Token::new((), *s))
        }

        // identifier with optional call: identifier ( "(" rhs ("," rhs)* ")" )?
        BbnfBootstrapEnum::term_1((ident_span, call_args)) => {
            let name = Cow::Borrowed(ident_span.as_str());
            if let Some((_open, first_arg, rest_args, _close)) = call_args {
                let mut args = vec![lower_rhs(first_arg)];
                for (_comma, arg) in rest_args.iter() {
                    args.push(lower_rhs(arg));
                }
                Expression::GrammarCall(Token::new(name, *ident_span), args)
            } else {
                Expression::Nonterminal(Token::new(name, *ident_span))
            }
        }

        // Grouped: "(" rhs ")", "[" rhs "]", "{" rhs "}"
        BbnfBootstrapEnum::term_2((open, inner, _close)) => {
            let expr = lower_rhs(inner);
            match open.as_str() {
                "(" => Expression::Group(Box::new(Token::new(expr, *open))),
                "[" => Expression::Optional(Box::new(Token::new(expr, *open))),
                "{" => Expression::Many(Box::new(Token::new(expr, *open))),
                _ => expr,
            }
        }

        // Literal
        BbnfBootstrapEnum::literal(s) => {
            // Strip quote delimiters — the span includes them
            let raw = s.as_str();
            let inner = &raw[1..raw.len() - 1];
            Expression::Literal(Token::new(Cow::Borrowed(inner), *s))
        }

        // Regex
        BbnfBootstrapEnum::regex(s) => {
            let raw = s.as_str();
            let inner = &raw[1..raw.len() - 1];
            Expression::Regex(Token::new(Cow::Borrowed(inner), *s))
        }

        // Identifier (plain nonterminal reference)
        BbnfBootstrapEnum::identifier(s) => {
            Expression::Nonterminal(Token::new(Cow::Borrowed(s.as_str()), *s))
        }

        other => lower_expression(other),
    }
}

/// Fallback: try to extract meaningful content from any variant.
fn lower_expression<'a>(node: &'a BbnfBootstrapEnum<'a>) -> Expression<'a> {
    match node {
        BbnfBootstrapEnum::literal(s) => {
            let raw = s.as_str();
            let inner = &raw[1..raw.len() - 1];
            Expression::Literal(Token::new(Cow::Borrowed(inner), *s))
        }
        BbnfBootstrapEnum::regex(s) => {
            let raw = s.as_str();
            let inner = &raw[1..raw.len() - 1];
            Expression::Regex(Token::new(Cow::Borrowed(inner), *s))
        }
        BbnfBootstrapEnum::identifier(s) => {
            Expression::Nonterminal(Token::new(Cow::Borrowed(s.as_str()), *s))
        }
        BbnfBootstrapEnum::term(inner) => lower_term_dispatch(inner),
        BbnfBootstrapEnum::factor(f) => lower_factor_dispatch(node),
        BbnfBootstrapEnum::mapped_factor(_) => lower_mapped_factor_dispatch(node),
        BbnfBootstrapEnum::binary_factor(_) => lower_binary_factor_dispatch(node),
        BbnfBootstrapEnum::concatenation(parts) => lower_concatenation(parts),
        BbnfBootstrapEnum::alternation(branches) => lower_alternation(branches),
        BbnfBootstrapEnum::comment(s) | BbnfBootstrapEnum::big_comment(s) => {
            Expression::Epsilon(Token::new((), *s))
        }
        _ => Expression::Epsilon(Token::new((), Span::default())),
    }
}

// ── Helpers ──────────────────────────────────────────────────────────────

fn lower_identifier_str<'a>(node: &'a BbnfBootstrapEnum<'a>) -> &'a str {
    match node {
        BbnfBootstrapEnum::identifier(s) => s.as_str(),
        _ => "",
    }
}

fn lower_identifier_span<'a>(node: &'a BbnfBootstrapEnum<'a>) -> Span<'a> {
    match node {
        BbnfBootstrapEnum::identifier(s) => *s,
        _ => Span::default(),
    }
}

fn extract_import_path<'a>(span: &Span<'a>) -> &'a str {
    let raw = span.as_str();
    // Strip quotes
    if raw.starts_with('"') && raw.ends_with('"') {
        &raw[1..raw.len() - 1]
    } else {
        raw
    }
}
