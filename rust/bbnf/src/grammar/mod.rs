mod tokens;

use std::borrow::Cow;

use parse_that::{
    Parser, ParserSpan, ParserState, Span, any_span, lazy, next_span, string, string_span,
    take_while_span,
};

use crate::types::*;

// ─── Value Expression Parser (recursive descent) ────────────────────────────
//
// Precedence (lowest to highest):
//   ||  →  &&  →  == !=  →  < > <= >=  →  + -  →  * / %  →  ! - (unary)  →  .prop  →  atom

fn ve_skip_ws(state: &mut ParserState<'_>) {
    let bytes = state.src_bytes;
    while state.offset < state.end && bytes[state.offset].is_ascii_whitespace() {
        state.offset += 1;
    }
}

fn ve_peek(state: &ParserState<'_>) -> Option<u8> {
    if state.offset < state.end {
        Some(state.src_bytes[state.offset])
    } else {
        None
    }
}

fn ve_peek_at(state: &ParserState<'_>, off: usize) -> Option<u8> {
    let pos = state.offset + off;
    if pos < state.end {
        Some(state.src_bytes[pos])
    } else {
        None
    }
}

/// Try to consume an exact byte sequence. Returns true on success.
fn ve_try_consume(state: &mut ParserState<'_>, pattern: &[u8]) -> bool {
    let end = state.offset + pattern.len();
    if end <= state.end && &state.src_bytes[state.offset..end] == pattern {
        state.offset = end;
        true
    } else {
        false
    }
}

/// Parse a simple identifier: `[a-zA-Z_][a-zA-Z0-9_]*`.
fn ve_simple_ident<'a>(state: &mut ParserState<'a>) -> Option<Span<'a>> {
    let start = state.offset;
    let bytes = state.src_bytes;
    if start >= state.end || !(bytes[start].is_ascii_alphabetic() || bytes[start] == b'_') {
        return None;
    }
    let mut i = start + 1;
    while i < state.end && (bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_') {
        i += 1;
    }
    state.offset = i;
    Some(Span::new(start, i, state.src))
}

/// Parse a value-expression identifier (allows `::` path separators):
/// `[a-zA-Z_][a-zA-Z0-9_]* ("::" [a-zA-Z_][a-zA-Z0-9_]*)*`.
fn ve_ident<'a>(state: &mut ParserState<'a>) -> Option<Span<'a>> {
    let start = state.offset;
    ve_simple_ident(state)?;
    // Consume `::segment` suffixes.
    loop {
        let saved = state.offset;
        if ve_try_consume(state, b"::") {
            if ve_simple_ident(state).is_some() {
                continue;
            }
            // `::` without a following ident — backtrack.
            state.offset = saved;
        }
        break;
    }
    Some(Span::new(start, state.offset, state.src))
}

/// Parse a number literal (integer or float, with optional suffix and hex support).
fn ve_number<'a>(state: &mut ParserState<'a>) -> Option<ValueExpr<'a>> {
    let start = state.offset;
    let bytes = state.src_bytes;
    let mut i = start;
    let mut is_float = false;

    // Hex: 0x...
    if i + 1 < state.end && bytes[i] == b'0' && (bytes[i + 1] == b'x' || bytes[i + 1] == b'X') {
        i += 2;
        let hex_start = i;
        while i < state.end && bytes[i].is_ascii_hexdigit() {
            i += 1;
        }
        if i == hex_start {
            return None; // `0x` with no digits
        }
    } else {
        // Decimal digits.
        while i < state.end && bytes[i].is_ascii_digit() {
            i += 1;
        }
        if i == start {
            return None;
        }
        // Fractional part.
        if i < state.end && bytes[i] == b'.' {
            let dot_pos = i;
            i += 1;
            let frac_start = i;
            while i < state.end && bytes[i].is_ascii_digit() {
                i += 1;
            }
            if i == frac_start {
                // No digits after `.` — backtrack the dot (could be property access).
                i = dot_pos;
            } else {
                is_float = true;
            }
        }
        // Exponent.
        if i < state.end && (bytes[i] == b'e' || bytes[i] == b'E') {
            let exp_start = i;
            i += 1;
            if i < state.end && (bytes[i] == b'+' || bytes[i] == b'-') {
                i += 1;
            }
            let digit_start = i;
            while i < state.end && bytes[i].is_ascii_digit() {
                i += 1;
            }
            if i == digit_start {
                // No digits after exponent — backtrack.
                i = exp_start;
            } else {
                is_float = true;
            }
        }
    }

    // Optional type suffix: [a-zA-Z_][a-zA-Z0-9_]*
    if i < state.end && (bytes[i].is_ascii_alphabetic() || bytes[i] == b'_') {
        i += 1;
        while i < state.end && (bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_') {
            i += 1;
        }
    }

    state.offset = i;
    let span = Span::new(start, i, state.src);
    let text: Cow<'a, str> = Cow::Borrowed(span.as_str());
    let token = Token::new(text, span);
    Some(if is_float {
        ValueExpr::FloatLit(token)
    } else {
        ValueExpr::IntLit(token)
    })
}

/// Parse a double-quoted string literal.
fn ve_string<'a>(state: &mut ParserState<'a>) -> Option<ValueExpr<'a>> {
    let start = state.offset;
    let bytes = state.src_bytes;
    if ve_peek(state) != Some(b'"') {
        return None;
    }
    let mut i = start + 1;
    while i < state.end {
        match bytes[i] {
            b'\\' => {
                i += 2; // skip escape
            }
            b'"' => {
                i += 1;
                state.offset = i;
                // Inner content (without quotes).
                let inner = &state.src[start + 1..i - 1];
                let span = Span::new(start, i, state.src);
                return Some(ValueExpr::StringLit(Token::new(
                    Cow::Borrowed(inner),
                    span,
                )));
            }
            _ => i += 1,
        }
    }
    None // unterminated string
}

/// Parse a value expression atom.
fn ve_atom<'a>(state: &mut ParserState<'a>) -> Option<ValueExpr<'a>> {
    ve_skip_ws(state);

    // String literal.
    if ve_peek(state) == Some(b'"') {
        return ve_string(state);
    }

    // Parenthesized expression.
    if ve_peek(state) == Some(b'(') {
        state.offset += 1;
        let inner = ve_or(state)?;
        ve_skip_ws(state);
        if ve_peek(state) != Some(b')') {
            return None;
        }
        state.offset += 1;
        return Some(ValueExpr::Paren(Box::new(inner)));
    }

    // Number literal (must check before identifiers for `0x` hex).
    if ve_peek(state).is_some_and(|b| b.is_ascii_digit()) {
        return ve_number(state);
    }

    // Identifier, keyword, or function call.
    if ve_peek(state).is_some_and(|b| b.is_ascii_alphabetic() || b == b'_') {
        let ident_span = ve_ident(state)?;
        let name = ident_span.as_str();

        match name {
            "true" => {
                return Some(ValueExpr::BoolLit(Token::new(true, ident_span)));
            }
            "false" => {
                return Some(ValueExpr::BoolLit(Token::new(false, ident_span)));
            }
            "input" => {
                // Check for property access: `input.prop`.
                let input_span = ident_span;
                ve_skip_ws(state);
                if ve_peek(state) == Some(b'.') {
                    let saved = state.offset;
                    state.offset += 1;
                    if let Some(prop_span) = ve_simple_ident(state) {
                        let prop = Token::new(Cow::Borrowed(prop_span.as_str()), prop_span);
                        return Some(ValueExpr::InputProp(input_span, prop));
                    }
                    state.offset = saved;
                }
                return Some(ValueExpr::Input(input_span));
            }
            _ => {}
        }

        // Check for function call: `ident(args)`.
        ve_skip_ws(state);
        if ve_peek(state) == Some(b'(') {
            state.offset += 1;
            let mut args = Vec::new();
            ve_skip_ws(state);
            if ve_peek(state) != Some(b')') {
                args.push(ve_or(state)?);
                loop {
                    ve_skip_ws(state);
                    if ve_peek(state) != Some(b',') {
                        break;
                    }
                    state.offset += 1;
                    args.push(ve_or(state)?);
                }
            }
            ve_skip_ws(state);
            if ve_peek(state) != Some(b')') {
                return None;
            }
            state.offset += 1;
            let fn_name = Token::new(Cow::Borrowed(name), ident_span);
            return Some(ValueExpr::FnCall(fn_name, args));
        }

        return Some(ValueExpr::Ident(Token::new(Cow::Borrowed(name), ident_span)));
    }

    None
}

/// Parse postfix: `atom ("." IDENT)*` (property access on non-input is unsupported for now).
fn ve_postfix<'a>(state: &mut ParserState<'a>) -> Option<ValueExpr<'a>> {
    ve_atom(state)
    // Property access on `input` is handled inside ve_atom directly.
    // General property access (e.g., `result.len`) not supported yet.
}

/// Parse unary prefix: `("!" | "-") unary | postfix`.
fn ve_unary<'a>(state: &mut ParserState<'a>) -> Option<ValueExpr<'a>> {
    ve_skip_ws(state);
    match ve_peek(state) {
        Some(b'!') => {
            state.offset += 1;
            let inner = ve_unary(state)?;
            Some(ValueExpr::UnaryOp(UnaryOpKind::Not, Box::new(inner)))
        }
        Some(b'-') => {
            // Make sure it's not `->`.
            if ve_peek_at(state, 1) == Some(b'>') {
                return ve_postfix(state);
            }
            state.offset += 1;
            let inner = ve_unary(state)?;
            Some(ValueExpr::UnaryOp(UnaryOpKind::Neg, Box::new(inner)))
        }
        _ => ve_postfix(state),
    }
}

/// Left-associative binary operator helper.
fn ve_binop_left<'a>(
    state: &mut ParserState<'a>,
    operand: fn(&mut ParserState<'a>) -> Option<ValueExpr<'a>>,
    ops: &[(&[u8], BinOpKind)],
) -> Option<ValueExpr<'a>> {
    let mut left = operand(state)?;
    loop {
        ve_skip_ws(state);
        let saved = state.offset;
        let mut matched = None;
        // Try longer patterns first (they're listed first in each call).
        for &(pat, kind) in ops {
            if ve_try_consume(state, pat) {
                matched = Some(kind);
                break;
            }
        }
        let Some(kind) = matched else { break };
        let Some(right) = operand(state) else {
            state.offset = saved;
            break;
        };
        left = ValueExpr::BinOp(kind, Box::new(left), Box::new(right));
    }
    Some(left)
}

fn ve_mul<'a>(state: &mut ParserState<'a>) -> Option<ValueExpr<'a>> {
    ve_binop_left(state, ve_unary, &[
        (b"*", BinOpKind::Mul),
        (b"/", BinOpKind::Div),
        (b"%", BinOpKind::Mod),
    ])
}

fn ve_add<'a>(state: &mut ParserState<'a>) -> Option<ValueExpr<'a>> {
    ve_binop_left(state, ve_mul, &[
        (b"+", BinOpKind::Add),
        (b"-", BinOpKind::Sub),
    ])
}

fn ve_cmp<'a>(state: &mut ParserState<'a>) -> Option<ValueExpr<'a>> {
    ve_binop_left(state, ve_add, &[
        (b"<=", BinOpKind::Le),
        (b">=", BinOpKind::Ge),
        (b"<", BinOpKind::Lt),
        (b">", BinOpKind::Gt),
    ])
}

fn ve_eq<'a>(state: &mut ParserState<'a>) -> Option<ValueExpr<'a>> {
    ve_binop_left(state, ve_cmp, &[
        (b"==", BinOpKind::Eq),
        (b"!=", BinOpKind::Ne),
    ])
}

fn ve_and<'a>(state: &mut ParserState<'a>) -> Option<ValueExpr<'a>> {
    ve_binop_left(state, ve_eq, &[(b"&&", BinOpKind::And)])
}

fn ve_or<'a>(state: &mut ParserState<'a>) -> Option<ValueExpr<'a>> {
    ve_binop_left(state, ve_and, &[(b"||", BinOpKind::Or)])
}

/// Parse a type annotation: `: TYPE` where TYPE is an identifier.
fn ve_type_annotation<'a>(state: &mut ParserState<'a>) -> Option<Token<'a, Cow<'a, str>>> {
    ve_skip_ws(state);
    if ve_peek(state) != Some(b':') {
        return None;
    }
    // Don't consume `::` (path separator).
    if ve_peek_at(state, 1) == Some(b':') {
        return None;
    }
    state.offset += 1; // consume `:`
    ve_skip_ws(state);
    let span = ve_ident(state)?;
    Some(Token::new(Cow::Borrowed(span.as_str()), span))
}

/// Parse a complete map arrow: `-> value_expr (: TYPE)?`.
fn ve_parse_map_arrow<'a>(state: &mut ParserState<'a>) -> Option<MapArrow<'a>> {
    let start = state.offset;
    let expr = ve_or(state)?;
    let return_type = ve_type_annotation(state);
    let span = Span::new(start, state.offset, state.src);
    Some(MapArrow {
        expr,
        return_type,
        span,
    })
}

/// Helper enum for interleaving imports, recovers, pretties, and rules during parsing.
enum TopLevelItem<'a> {
    Import(ImportDirective<'a>),
    Recover(RecoverDirective<'a>),
    Pretty(PrettyDirective<'a>),
    /// `@ws /regex/ ;` — custom whitespace pattern for `?w`.
    WsPattern(Cow<'a, str>),
    /// `@debug ruleName ;` or `@debug * ;` — instrument a rule for debugging.
    Debug(Cow<'a, str>),
    /// `@token ruleName ;` — mark a rule as a lexical token.
    Token(Cow<'a, str>),
    Rule(Expression<'a>),
}

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
            _ => unreachable!(
                "unhandled factor: {:?}, {:?}",
                op.as_str(),
                token.span.as_str()
            ),
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

pub struct BBNFGrammar<'a> {
    _marker: std::marker::PhantomData<&'a ()>,
}

impl<'a> BBNFGrammar<'a> {
    fn group() -> Parser<'a, Expression<'a>> {
        lazy(|| {
            Self::rhs()
                .trim_whitespace()
                .wrap(string_span("("), string_span(")"))
                .map_with_state(|expr, prev_offset, state| {
                    let token = Token::new(expr, Span::new(prev_offset, state.offset, state.src));
                    Expression::Group(Box::new(token))
                })
        })
    }

    fn optional_group() -> Parser<'a, Expression<'a>> {
        lazy(|| {
            Self::rhs()
                .trim_whitespace()
                .wrap(string_span("["), string_span("]"))
                .map_with_state(|expr, prev_offset, state| {
                    let token = Token::new(expr, Span::new(prev_offset, state.offset, state.src));
                    Expression::Optional(Box::new(token))
                })
        })
    }

    fn many_group() -> Parser<'a, Expression<'a>> {
        lazy(|| {
            Self::rhs()
                .trim_whitespace()
                .wrap(string_span("{"), string_span("}"))
                .map_with_state(|expr, prev_offset, state| {
                    let token = Token::new(expr, Span::new(prev_offset, state.offset, state.src));
                    Expression::Many(Box::new(token))
                })
        })
    }

    fn span_capture() -> Parser<'a, Expression<'a>> {
        lazy(|| {
            string_span("@{")
                .trim_whitespace()
                .next(Self::rhs().trim_whitespace())
                .skip(string_span("}"))
                .map_with_state(|expr, prev_offset, state| {
                    let token = Token::new(expr, Span::new(prev_offset, state.offset, state.src));
                    Expression::SpanCapture(Box::new(token))
                })
        })
    }

    fn standalone_optional_whitespace() -> Parser<'a, Expression<'a>> {
        string_span("?w").map(|span| {
            let inner = Expression::Epsilon(Token::new((), span));
            Expression::OptionalWhitespace(Box::new(Token::new(inner, span)))
        })
    }

    fn term() -> Parser<'a, Expression<'a>> {
        tokens::epsilon()
            | Self::span_capture()
            | Self::standalone_optional_whitespace()
            | Self::group()
            | Self::optional_group()
            | Self::many_group()
            | tokens::nonterminal()
            | tokens::literal()
            | tokens::regex()
    }

    fn factor() -> Parser<'a, Expression<'a>> {
        tokens::trim_comment(
            Self::term()
                .then(any_span(&["?w", "*", "+", "?"]).trim_whitespace().many(..))
                .map_with_state(map_factor),
            tokens::block_comment().opt(),
        )
    }

    /// Parse `factor -> value_expr : TYPE` — per-expression map.
    /// Uses the structured value expression parser.
    fn mapped_factor() -> Parser<'a, Expression<'a>> {
        Self::factor()
            .then(Self::map_arrow().opt())
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

    /// Parse the `->` operator followed by a structured value expression
    /// and optional type annotation.
    fn map_arrow() -> Parser<'a, MapArrow<'a>> {
        string_span("->")
            .trim_whitespace()
            .next(Parser::new(|state: &mut ParserState<'a>| {
                ve_parse_map_arrow(state)
            }))
    }

    fn binary_factor() -> Parser<'a, Expression<'a>> {
        Self::mapped_factor()
            .then(
                any_span(&["<<", ">>", "-"])
                    .trim_whitespace()
                    .then(Self::factor())
                    .many(..),
            )
            .map_with_state(reduce_binary_expression)
    }

    fn concatenation() -> Parser<'a, Expression<'a>> {
        Self::binary_factor()
            .then(
                string_span(",")
                    .trim_whitespace()
                    .opt_span()
                    .next(Self::binary_factor().trim_whitespace())
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

    fn alternation() -> Parser<'a, Expression<'a>> {
        let delim = string_span("|").trim_whitespace();

        Self::concatenation()
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

    fn lhs() -> Parser<'a, Expression<'a>> {
        tokens::nonterminal()
    }

    fn rhs() -> Parser<'a, Expression<'a>> {
        Self::alternation()
    }

    fn production_rule() -> Parser<'a, Expression<'a>> {
        let comment = tokens::block_comment() | tokens::line_comment();
        let eq = string("=").trim_whitespace();

        let terminator = (any_span(&[";", "."])).trim_whitespace();

        let production_rule = Self::lhs()
            .skip(eq)
            .then(Self::rhs())
            .skip(terminator)
            .map(|(lhs, rhs)| {
                Expression::ProductionRule(
                    lhs.into(),
                    Expression::Rule(Box::new(rhs), None).into(),
                )
            });

        tokens::trim_comment(production_rule, comment.opt())
    }

    /// Parse a list of identifiers in `{ a, b, c }` form.
    fn import_items() -> Parser<'a, Vec<ImportedName<'a>>> {
        let ident = tokens::identifier().map(|s: Span<'a>| ImportedName {
            name: Cow::Borrowed(s.as_str()),
            span: s,
        });

        string("{")
            .trim_whitespace()
            .next(
                ident
                    .sep_by(string(",").trim_whitespace(), 1..)
                    .trim_whitespace(),
            )
            .skip(string("}").trim_whitespace())
    }

    /// Parse an `@import` directive:
    /// - `@import "path" ;`
    /// - `@import { a, b } from "path" ;`
    fn import_directive() -> Parser<'a, ImportDirective<'a>> {
        let selective = Self::import_items()
            .skip(string("from").trim_whitespace())
            .then(tokens::import_path())
            .map(|(items, path)| (Some(items), path));

        let glob = tokens::import_path().map(|path| (None, path));

        string("@import")
            .trim_whitespace()
            .next(selective | glob)
            .skip(any_span(&[";", "."]).opt().trim_whitespace())
            .map_with_state(|(items, path), prev_offset, state| ImportDirective {
                path,
                span: Span::new(prev_offset, state.offset, state.src),
                items,
            })
    }

    /// Parse an `@recover` directive:
    /// `@recover ruleName syncExpr ;`
    fn recover_directive() -> Parser<'a, RecoverDirective<'a>> {
        string("@recover")
            .trim_whitespace()
            .next(
                tokens::identifier()
                    .trim_whitespace()
                    .then(Self::rhs().trim_whitespace()),
            )
            .skip(any_span(&[";", "."]).opt().trim_whitespace())
            .map_with_state(
                |(name_span, sync_expr), prev_offset, state| RecoverDirective {
                    rule_name: Cow::Borrowed(name_span.as_str()),
                    sync_expr,
                    span: Span::new(prev_offset, state.offset, state.src),
                },
            )
    }

    /// Parse a `@debug ruleName ;` or `@debug * ;` directive — instrument rules for debugging.
    fn debug_directive() -> Parser<'a, Cow<'a, str>> {
        string("@debug")
            .trim_whitespace()
            .next(
                (string_span("*") | tokens::identifier())
                    .trim_whitespace()
                    .map(|span| Cow::Borrowed(span.as_str())),
            )
            .skip(any_span(&[";", "."]).opt().trim_whitespace())
    }

    /// Parse a `@token ruleName ;` directive — mark a rule as a lexical token.
    fn token_directive() -> Parser<'a, Cow<'a, str>> {
        string("@token")
            .trim_whitespace()
            .next(tokens::identifier().trim_whitespace())
            .skip(any_span(&[";", "."]).opt().trim_whitespace())
            .map(|name_span| Cow::Borrowed(name_span.as_str()))
    }

    /// Parse an `@ws /regex/ ;` directive — custom whitespace pattern for `?w`.
    fn ws_directive() -> Parser<'a, Cow<'a, str>> {
        string("@ws")
            .trim_whitespace()
            .next(tokens::regex().trim_whitespace())
            .skip(any_span(&[";", "."]).opt().trim_whitespace())
            .map(|regex_expr| {
                // Extract the regex pattern string from Expression::Regex.
                match regex_expr {
                    Expression::Regex(token) => token.value,
                    _ => unreachable!(),
                }
            })
    }

    /// Parse a `sep("...")` hint: `sep("literal string")`.
    fn sep_hint() -> Parser<'a, Span<'a>> {
        let not_quote = take_while_span(|c| c != '"' && c != '\\');
        let escaped = string_span(r"\").then_span(next_span(1));
        let quoted_content = (not_quote | escaped)
            .many_span(..)
            .wrap_span(string_span("\""), string_span("\""));
        // Match `sep(` ... `)` and return the whole span including sep("...").
        string_span("sep(")
            .then_span(quoted_content)
            .then_span(string_span(")"))
    }

    /// Parse a `split("...")` hint: `split("delimiter string")`.
    fn split_hint() -> Parser<'a, Span<'a>> {
        let not_quote = take_while_span(|c| c != '"' && c != '\\');
        let escaped = string_span(r"\").then_span(next_span(1));
        let quoted_content = (not_quote | escaped)
            .many_span(..)
            .wrap_span(string_span("\""), string_span("\""));
        // Match `split(` ... `)` and return the whole span including split("...").
        string_span("split(")
            .then_span(quoted_content)
            .then_span(string_span(")"))
    }

    /// Parse a `@pretty` directive:
    /// `@pretty ruleName hint1 hint2 ... ;`
    /// Hints can be identifiers (e.g. `group`), `sep("...")`, or `split("...")` expressions.
    fn pretty_directive() -> Parser<'a, PrettyDirective<'a>> {
        let hint = Self::sep_hint() | Self::split_hint() | tokens::identifier();

        string("@pretty")
            .trim_whitespace()
            .next(
                tokens::identifier()
                    .trim_whitespace()
                    .then(hint.trim_whitespace().many(1..)),
            )
            .skip(any_span(&[";", "."]).opt().trim_whitespace())
            .map_with_state(|(name_span, hints), prev_offset, state| PrettyDirective {
                rule_name: Cow::Borrowed(name_span.as_str()),
                hints: hints.iter().map(|s| Cow::Borrowed(s.as_str())).collect(),
                span: Span::new(prev_offset, state.offset, state.src),
            })
    }

    /// Parse a grammar file: interleaved import directives, recover directives, and rules.
    /// Returns a `ParsedGrammar` with imports, recovers, and the AST.
    pub fn grammar_with_imports() -> Parser<'a, ParsedGrammar<'a>> {
        let import = tokens::skip_comments()
            .next(Self::import_directive().trim_whitespace())
            .map(TopLevelItem::Import);
        let recover = tokens::skip_comments()
            .next(Self::recover_directive().trim_whitespace())
            .map(TopLevelItem::Recover);
        let pretty = tokens::skip_comments()
            .next(Self::pretty_directive().trim_whitespace())
            .map(TopLevelItem::Pretty);
        let ws_pat = tokens::skip_comments()
            .next(Self::ws_directive().trim_whitespace())
            .map(TopLevelItem::WsPattern);
        let debug_dir = tokens::skip_comments()
            .next(Self::debug_directive().trim_whitespace())
            .map(TopLevelItem::Debug);
        let token_dir = tokens::skip_comments()
            .next(Self::token_directive().trim_whitespace())
            .map(TopLevelItem::Token);
        let rule = tokens::skip_comments()
            .next(Self::production_rule().trim_whitespace())
            .map(TopLevelItem::Rule);

        let item = import | recover | pretty | ws_pat | debug_dir | token_dir | rule;

        tokens::skip_comments().next(item.many(..)).map(|items| {
            let mut imports = Vec::new();
            let mut recovers = Vec::new();
            let mut pretties = Vec::new();
            let mut ws_pattern = None;
            let mut debug_rules = Vec::new();
            let mut token_rules = Vec::new();
            let mut rules_vec = Vec::new();
            for item in items {
                match item {
                    TopLevelItem::Import(imp) => imports.push(imp),
                    TopLevelItem::Recover(rec) => recovers.push(rec),
                    TopLevelItem::Pretty(p) => pretties.push(p),
                    TopLevelItem::WsPattern(pat) => ws_pattern = Some(pat),
                    TopLevelItem::Debug(name) => debug_rules.push(name),
                    TopLevelItem::Token(name) => token_rules.push(name),
                    TopLevelItem::Rule(r) => rules_vec.push(r),
                }
            }
            let ast: AST<'a> = rules_vec
                .into_iter()
                .map(|expr| match expr {
                    Expression::ProductionRule(lhs, rhs) => (*lhs, *rhs),
                    _ => unreachable!(),
                })
                .collect();
            ParsedGrammar {
                imports,
                recovers,
                pretties,
                rules: ast,
                ws_pattern,
                debug_rules,
                token_rules,
            }
        })
    }

    /// Parse a grammar file (rules only, no imports). Original API for backward
    /// compatibility with proc-macro and existing consumers.
    pub fn grammar() -> Parser<'a, AST<'a>> {
        let rule = Self::production_rule().trim_whitespace().many(..);

        rule.trim_whitespace().map(|rules| {
            rules
                .into_iter()
                .map(|expr| match expr {
                    Expression::ProductionRule(lhs, rhs) => (*lhs, *rhs),
                    _ => unreachable!(),
                })
                .collect()
        })
    }
}
