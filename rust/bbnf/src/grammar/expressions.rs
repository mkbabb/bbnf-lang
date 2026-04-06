//! Grammar expression hierarchy: term → factor → mapped_factor → ... → alternation.
//!
//! Extended with closures (`|params| body`) and grammar calls (`name(args)`).

use std::borrow::Cow;

use parse_that::{Parser, ParserSpan, ParserState, Span, any_span, lazy, string, string_span};

use crate::types::*;

use super::tokens;
use super::value_expr;

// ─── Helpers ────────────────────────────────────────────────────────────────

fn map_factor<'a>(
    factor: (Expression<'a>, Vec<Span<'a>>),
    prev_offset: usize,
    state: &mut ParserState<'a>,
) -> Expression<'a> {
    let (mut expr, modifiers) = factor;
    for op in modifiers {
        let token = Token::new(expr, Span::new(prev_offset, state.offset, state.src));
        expr = match op.as_str() {
            "*" => Expression::Many(Box::new(token)),
            "+" => Expression::Many1(Box::new(token)),
            "?w" => Expression::OptionalWhitespace(Box::new(token)),
            "?" => Expression::Optional(Box::new(token)),
            _ => unreachable!("unhandled factor: {:?}", op.as_str()),
        };
    }
    expr
}

fn reduce_binary_expression<'a>(
    expr: (Expression<'a>, Vec<(Span<'a>, Expression<'a>)>),
    prev_offset: usize,
    state: &mut ParserState<'a>,
) -> Expression<'a> {
    let (left, right) = expr;
    if right.is_empty() {
        return left;
    }
    right.into_iter().fold(left, |acc, (op, right)| {
        let acc_token = Token::new(acc, Span::new(prev_offset, state.offset, state.src));
        let right_token = Token::new(right, Span::new(prev_offset, state.offset, state.src));
        match op.as_str() {
            "<<" => Expression::Skip(Box::new(acc_token), Box::new(right_token)),
            ">>" => Expression::Next(Box::new(acc_token), Box::new(right_token)),
            "-" => Expression::Minus(Box::new(acc_token), Box::new(right_token)),
            _ => unreachable!(),
        }
    })
}

// ─── Expression Hierarchy ───────────────────────────────────────────────────

fn group<'a>() -> Parser<'a, Expression<'a>> {
    lazy(|| {
        rhs()
            .trim_whitespace()
            .wrap(string_span("("), string_span(")"))
            .map_with_state(|expr, prev_offset, state| {
                let token = Token::new(expr, Span::new(prev_offset, state.offset, state.src));
                Expression::Group(Box::new(token))
            })
    })
}

fn optional_group<'a>() -> Parser<'a, Expression<'a>> {
    lazy(|| {
        rhs()
            .trim_whitespace()
            .wrap(string_span("["), string_span("]"))
            .map_with_state(|expr, prev_offset, state| {
                let token = Token::new(expr, Span::new(prev_offset, state.offset, state.src));
                Expression::Optional(Box::new(token))
            })
    })
}

fn many_group<'a>() -> Parser<'a, Expression<'a>> {
    lazy(|| {
        rhs()
            .trim_whitespace()
            .wrap(string_span("{"), string_span("}"))
            .map_with_state(|expr, prev_offset, state| {
                let token = Token::new(expr, Span::new(prev_offset, state.offset, state.src));
                Expression::Many(Box::new(token))
            })
    })
}

fn span_capture<'a>() -> Parser<'a, Expression<'a>> {
    lazy(|| {
        string_span("@{")
            .trim_whitespace()
            .next(rhs().trim_whitespace())
            .skip(string_span("}"))
            .map_with_state(|expr, prev_offset, state| {
                let token = Token::new(expr, Span::new(prev_offset, state.offset, state.src));
                Expression::SpanCapture(Box::new(token))
            })
    })
}

fn standalone_optional_whitespace<'a>() -> Parser<'a, Expression<'a>> {
    string_span("?w").map(|span| {
        let inner = Expression::Epsilon(Token::new((), span));
        Expression::OptionalWhitespace(Box::new(Token::new(inner, span)))
    })
}

/// Parse a nonterminal or grammar call: `ident` or `ident(arg1, arg2)`.
fn nonterminal_or_call<'a>() -> Parser<'a, Expression<'a>> {
    tokens::identifier()
        .then(
            lazy(|| {
                // Each arg is a binary_factor — commas separate args, not concatenate.
                binary_factor()
                    .trim_whitespace()
                    .sep_by(string(",").trim_whitespace(), 1..)
                    .trim_whitespace()
                    .wrap(string_span("("), string_span(")"))
            })
            .opt(),
        )
        .map(|(ident_span, args_opt)| {
            let name = ident_span.as_str();
            if let Some(args) = args_opt {
                Expression::GrammarCall(
                    Token::new(Cow::Borrowed(name), ident_span),
                    args,
                )
            } else {
                Expression::Nonterminal(Token::new(Cow::Borrowed(name), ident_span))
            }
        })
}

fn term<'a>() -> Parser<'a, Expression<'a>> {
    tokens::epsilon()
        | span_capture()
        | standalone_optional_whitespace()
        | group()
        | optional_group()
        | many_group()
        | nonterminal_or_call()
        | tokens::literal()
        | tokens::regex()
}

fn factor<'a>() -> Parser<'a, Expression<'a>> {
    tokens::trim_comment(
        term()
            .then(any_span(&["?w", "*", "+", "?"]).trim_whitespace().many(..))
            .map_with_state(map_factor),
        tokens::block_comment().opt(),
    )
}

/// Parse `factor -> value_expr : TYPE` — per-expression map.
fn mapped_factor<'a>() -> Parser<'a, Expression<'a>> {
    factor()
        .then(map_arrow().opt())
        .map_with_state(
            |pair: (Expression<'a>, Option<MapArrow<'a>>), prev_offset, state| {
                let (expr, arrow_opt) = pair;
                if let Some(arrow) = arrow_opt {
                    let expr_token =
                        Token::new(expr, Span::new(prev_offset, state.offset, state.src));
                    Expression::MappedExpression(Box::new(expr_token), arrow)
                } else {
                    expr
                }
            },
        )
}

/// Parse `->` followed by a structured value expression and optional type annotation.
fn map_arrow<'a>() -> Parser<'a, MapArrow<'a>> {
    string_span("->")
        .trim_whitespace()
        .next(value_expr::parse_map_arrow())
}

fn binary_factor<'a>() -> Parser<'a, Expression<'a>> {
    mapped_factor()
        .then(
            any_span(&["<<", ">>", "-"])
                .trim_whitespace()
                .then(factor())
                .many(..),
        )
        .map_with_state(reduce_binary_expression)
}

fn concatenation<'a>() -> Parser<'a, Expression<'a>> {
    binary_factor()
        .then(
            string_span(",")
                .trim_whitespace()
                .opt_span()
                .next(binary_factor().trim_whitespace())
                .many(..),
        )
        .map_with_state(|(first, rest), prev_offset, state| {
            let mut exprs = Vec::with_capacity(rest.len() + 1);
            exprs.push(first);
            exprs.extend(rest);
            if exprs.len() == 1 {
                exprs.into_iter().next().unwrap()
            } else {
                let token = Token::new(exprs, Span::new(prev_offset, state.offset, state.src));
                Expression::Concatenation(Box::new(token))
            }
        })
}

pub(super) fn alternation<'a>() -> Parser<'a, Expression<'a>> {
    let delim = string_span("|").trim_whitespace();

    concatenation()
        .sep_by(delim, ..)
        .map_with_state(|exprs, prev_offset, state| {
            if exprs.len() == 1 {
                exprs.into_iter().next().unwrap()
            } else {
                let token = Token::new(exprs, Span::new(prev_offset, state.offset, state.src));
                Expression::Alternation(Box::new(token))
            }
        })
}

/// Parse a grammar closure: `|params| body`.
///
/// Uses lookahead to disambiguate from alternation `|`:
/// if `|` is followed by identifier(s) separated by `,` then `|`, it's a closure.
fn grammar_closure<'a>() -> Parser<'a, Expression<'a>> {
    lazy(|| {
        Parser::new(move |state: &mut ParserState<'a>| {
            let bytes = state.src_bytes;
            let start = state.offset;

            if start >= state.end || bytes[start] != b'|' {
                return None;
            }

            let saved = state.offset;
            state.offset = start + 1;

            // Skip whitespace.
            while state.offset < state.end && bytes[state.offset].is_ascii_whitespace() {
                state.offset += 1;
            }

            // First param: simple identifier.
            let first_start = state.offset;
            if state.offset >= state.end
                || !(bytes[state.offset].is_ascii_alphabetic() || bytes[state.offset] == b'_')
            {
                state.offset = saved;
                return None;
            }
            while state.offset < state.end
                && (bytes[state.offset].is_ascii_alphanumeric() || bytes[state.offset] == b'_')
            {
                state.offset += 1;
            }
            let first_end = state.offset;

            while state.offset < state.end && bytes[state.offset].is_ascii_whitespace() {
                state.offset += 1;
            }

            // Must see `,` or `|` to confirm closure.
            if state.offset >= state.end
                || (bytes[state.offset] != b',' && bytes[state.offset] != b'|')
            {
                state.offset = saved;
                return None;
            }

            let mut params = vec![Token::new(
                Cow::Borrowed(&state.src[first_start..first_end]),
                Span::new(first_start, first_end, state.src),
            )];

            while state.offset < state.end && bytes[state.offset] == b',' {
                state.offset += 1;
                while state.offset < state.end && bytes[state.offset].is_ascii_whitespace() {
                    state.offset += 1;
                }
                let p_start = state.offset;
                if state.offset >= state.end
                    || !(bytes[state.offset].is_ascii_alphabetic()
                        || bytes[state.offset] == b'_')
                {
                    state.offset = saved;
                    return None;
                }
                while state.offset < state.end
                    && (bytes[state.offset].is_ascii_alphanumeric()
                        || bytes[state.offset] == b'_')
                {
                    state.offset += 1;
                }
                let p_end = state.offset;
                params.push(Token::new(
                    Cow::Borrowed(&state.src[p_start..p_end]),
                    Span::new(p_start, p_end, state.src),
                ));
                while state.offset < state.end && bytes[state.offset].is_ascii_whitespace() {
                    state.offset += 1;
                }
            }

            // Closing `|`.
            if state.offset >= state.end || bytes[state.offset] != b'|' {
                state.offset = saved;
                return None;
            }
            state.offset += 1;

            // Skip whitespace after closing `|`, then parse body.
            while state.offset < state.end && state.src_bytes[state.offset].is_ascii_whitespace() {
                state.offset += 1;
            }
            let body_parser = rhs();
            let body = body_parser.call(state)?;
            let body_token =
                Token::new(body, Span::new(start, state.offset, state.src));
            Some(Expression::Closure(params, Box::new(body_token)))
        })
    })
}

pub(super) fn lhs<'a>() -> Parser<'a, Expression<'a>> {
    tokens::nonterminal()
}

/// Top-level expression: closure | alternation.
///
/// Per the spec: `expr = closure | map_expr | grammar_expr ;`
/// Closures take priority over alternation (which subsumes map_expr via mapped_factor).
pub(super) fn rhs<'a>() -> Parser<'a, Expression<'a>> {
    grammar_closure() | alternation()
}
