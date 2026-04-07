//! Value expression parser for `->` map syntax.
//!
//! Precedence (lowest to highest):
//!   ||  →  &&  →  == !=  →  < > <= >=  →  + -  →  * / %  →  ! - (unary)  →  .prop  →  atom

use std::borrow::Cow;

use parse_that::{Parser, ParserState, Span};

use crate::types::*;

// ─── Utilities ──────────────────────────────────────────────────────────────

fn skip_ws(state: &mut ParserState<'_>) {
    let bytes = state.src_bytes;
    while state.offset < state.end && bytes[state.offset].is_ascii_whitespace() {
        state.offset += 1;
    }
}

fn peek(state: &ParserState<'_>) -> Option<u8> {
    if state.offset < state.end {
        Some(state.src_bytes[state.offset])
    } else {
        None
    }
}

fn peek_at(state: &ParserState<'_>, off: usize) -> Option<u8> {
    let pos = state.offset + off;
    if pos < state.end {
        Some(state.src_bytes[pos])
    } else {
        None
    }
}

/// Try to consume an exact byte sequence.
fn try_consume(state: &mut ParserState<'_>, pattern: &[u8]) -> bool {
    let end = state.offset + pattern.len();
    if end <= state.end && &state.src_bytes[state.offset..end] == pattern {
        state.offset = end;
        true
    } else {
        false
    }
}

// ─── Identifier Parsers ─────────────────────────────────────────────────────

/// Parse `[a-zA-Z_][a-zA-Z0-9_]*`.
fn simple_ident<'a>(state: &mut ParserState<'a>) -> Option<Span<'a>> {
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

/// Parse identifier with `::` path separators.
fn ident<'a>(state: &mut ParserState<'a>) -> Option<Span<'a>> {
    let start = state.offset;
    simple_ident(state)?;
    loop {
        let saved = state.offset;
        if try_consume(state, b"::") {
            if simple_ident(state).is_some() {
                continue;
            }
            state.offset = saved;
        }
        break;
    }
    Some(Span::new(start, state.offset, state.src))
}

// ─── Literal Parsers ────────────────────────────────────────────────────────

/// Parse integer or float literal with optional suffix and hex support.
fn number<'a>(state: &mut ParserState<'a>) -> Option<ValueExpr<'a>> {
    let start = state.offset;
    let bytes = state.src_bytes;
    let mut i = start;
    let mut is_float = false;

    if i + 1 < state.end && bytes[i] == b'0' && (bytes[i + 1] == b'x' || bytes[i + 1] == b'X') {
        i += 2;
        let hex_start = i;
        while i < state.end && bytes[i].is_ascii_hexdigit() {
            i += 1;
        }
        if i == hex_start {
            return None;
        }
    } else {
        while i < state.end && bytes[i].is_ascii_digit() {
            i += 1;
        }
        if i == start {
            return None;
        }
        // Fractional part — backtrack dot if no digits follow (could be property access).
        if i < state.end && bytes[i] == b'.' {
            let dot_pos = i;
            i += 1;
            let frac_start = i;
            while i < state.end && bytes[i].is_ascii_digit() {
                i += 1;
            }
            if i == frac_start {
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
                i = exp_start;
            } else {
                is_float = true;
            }
        }
    }

    // Optional type suffix.
    if i < state.end && (bytes[i].is_ascii_alphabetic() || bytes[i] == b'_') {
        i += 1;
        while i < state.end && (bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_') {
            i += 1;
        }
    }

    state.offset = i;
    let span = Span::new(start, i, state.src);
    let token = Token::new(Cow::Borrowed(span.as_str()), span);
    Some(if is_float {
        ValueExpr::FloatLit(token)
    } else {
        ValueExpr::IntLit(token)
    })
}

/// Parse a double-quoted string literal.
fn string_lit<'a>(state: &mut ParserState<'a>) -> Option<ValueExpr<'a>> {
    let start = state.offset;
    let bytes = state.src_bytes;
    if peek(state) != Some(b'"') {
        return None;
    }
    let mut i = start + 1;
    while i < state.end {
        match bytes[i] {
            b'\\' => i += 2,
            b'"' => {
                i += 1;
                state.offset = i;
                let inner = &state.src[start + 1..i - 1];
                let span = Span::new(start, i, state.src);
                return Some(ValueExpr::StringLit(Token::new(Cow::Borrowed(inner), span)));
            }
            _ => i += 1,
        }
    }
    None
}

// ─── Precedence Climbing ────────────────────────────────────────────────────

fn atom<'a>(state: &mut ParserState<'a>) -> Option<ValueExpr<'a>> {
    skip_ws(state);

    if peek(state) == Some(b'"') {
        return string_lit(state);
    }

    if peek(state) == Some(b'(') {
        state.offset += 1;
        let inner = or(state)?;
        skip_ws(state);
        if peek(state) != Some(b')') {
            return None;
        }
        state.offset += 1;
        return Some(ValueExpr::Paren(Box::new(inner)));
    }

    if peek(state).is_some_and(|b| b.is_ascii_digit()) {
        return number(state);
    }

    if peek(state).is_some_and(|b| b.is_ascii_alphabetic() || b == b'_') {
        let ident_span = ident(state)?;
        let name = ident_span.as_str();

        match name {
            "true" => return Some(ValueExpr::BoolLit(Token::new(true, ident_span))),
            "false" => return Some(ValueExpr::BoolLit(Token::new(false, ident_span))),
            "input" => {
                let input_span = ident_span;
                skip_ws(state);
                if peek(state) == Some(b'.') {
                    let saved = state.offset;
                    state.offset += 1;
                    if let Some(prop_span) = simple_ident(state) {
                        let prop = Token::new(Cow::Borrowed(prop_span.as_str()), prop_span);
                        return Some(ValueExpr::InputProp(input_span, prop));
                    }
                    state.offset = saved;
                }
                return Some(ValueExpr::Input(input_span));
            }
            _ => {}
        }

        skip_ws(state);
        if peek(state) == Some(b'(') {
            state.offset += 1;
            let mut args = Vec::new();
            skip_ws(state);
            if peek(state) != Some(b')') {
                args.push(or(state)?);
                loop {
                    skip_ws(state);
                    if peek(state) != Some(b',') {
                        break;
                    }
                    state.offset += 1;
                    args.push(or(state)?);
                }
            }
            skip_ws(state);
            if peek(state) != Some(b')') {
                return None;
            }
            state.offset += 1;
            return Some(ValueExpr::FnCall(
                Token::new(Cow::Borrowed(name), ident_span),
                args,
            ));
        }

        return Some(ValueExpr::Ident(Token::new(
            Cow::Borrowed(name),
            ident_span,
        )));
    }

    None
}

fn unary<'a>(state: &mut ParserState<'a>) -> Option<ValueExpr<'a>> {
    skip_ws(state);
    match peek(state) {
        Some(b'!') => {
            state.offset += 1;
            let inner = unary(state)?;
            Some(ValueExpr::UnaryOp(UnaryOpKind::Not, Box::new(inner)))
        }
        Some(b'-') => {
            if peek_at(state, 1) == Some(b'>') {
                return atom(state);
            }
            state.offset += 1;
            let inner = unary(state)?;
            Some(ValueExpr::UnaryOp(UnaryOpKind::Neg, Box::new(inner)))
        }
        _ => atom(state),
    }
}

/// Left-associative binary operator helper.
fn binop_left<'a>(
    state: &mut ParserState<'a>,
    operand: fn(&mut ParserState<'a>) -> Option<ValueExpr<'a>>,
    ops: &[(&[u8], BinOpKind)],
) -> Option<ValueExpr<'a>> {
    let mut left = operand(state)?;
    loop {
        skip_ws(state);
        let saved = state.offset;
        let mut matched = None;
        for &(pat, kind) in ops {
            if try_consume(state, pat) {
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

fn mul<'a>(state: &mut ParserState<'a>) -> Option<ValueExpr<'a>> {
    binop_left(state, unary, &[
        (b"*", BinOpKind::Mul),
        (b"/", BinOpKind::Div),
        (b"%", BinOpKind::Mod),
    ])
}

fn add<'a>(state: &mut ParserState<'a>) -> Option<ValueExpr<'a>> {
    binop_left(state, mul, &[(b"+", BinOpKind::Add), (b"-", BinOpKind::Sub)])
}

fn cmp<'a>(state: &mut ParserState<'a>) -> Option<ValueExpr<'a>> {
    binop_left(state, add, &[
        (b"<=", BinOpKind::Le),
        (b">=", BinOpKind::Ge),
        (b"<", BinOpKind::Lt),
        (b">", BinOpKind::Gt),
    ])
}

fn eq<'a>(state: &mut ParserState<'a>) -> Option<ValueExpr<'a>> {
    binop_left(state, cmp, &[(b"==", BinOpKind::Eq), (b"!=", BinOpKind::Ne)])
}

fn and<'a>(state: &mut ParserState<'a>) -> Option<ValueExpr<'a>> {
    binop_left(state, eq, &[(b"&&", BinOpKind::And)])
}

fn or<'a>(state: &mut ParserState<'a>) -> Option<ValueExpr<'a>> {
    binop_left(state, and, &[(b"||", BinOpKind::Or)])
}

// ─── Type Annotation ────────────────────────────────────────────────────────

/// Parse `: TYPE` where TYPE is an identifier.
fn type_annotation<'a>(state: &mut ParserState<'a>) -> Option<Token<'a, Cow<'a, str>>> {
    skip_ws(state);
    if peek(state) != Some(b':') {
        return None;
    }
    if peek_at(state, 1) == Some(b':') {
        return None; // `::` is a path separator, not a type annotation
    }
    state.offset += 1;
    skip_ws(state);
    let span = ident(state)?;
    Some(Token::new(Cow::Borrowed(span.as_str()), span))
}

// ─── Public API ─────────────────────────────────────────────────────────────

/// Parse a complete map arrow body: `value_expr (: TYPE)?`.
///
/// Called after `->` has been consumed by the grammar expression parser.
pub(super) fn parse_map_arrow<'a>() -> Parser<'a, MapArrow<'a>> {
    Parser::new(|state: &mut ParserState<'a>| {
        let start = state.offset;
        let expr = or(state)?;
        let return_type = type_annotation(state);
        let span = Span::new(start, state.offset, state.src);
        Some(MapArrow {
            expr,
            return_type,
            span,
        })
    })
}
