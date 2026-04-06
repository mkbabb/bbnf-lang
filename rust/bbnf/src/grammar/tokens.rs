//! Terminal parsers: identifiers, literals, regexes, epsilon, comments.

use std::borrow::Cow;

use parse_that::{
    Parser, ParserSpan, ParserState, Span, string_span, take_while_span,
};
use parse_that::parsers::utils::escaped_span;

use crate::types::*;

// ─── Comments ───────────────────────────────────────────────────────────────

pub(super) fn block_comment<'a>() -> Parser<'a, Comment<'a>> {
    let not_comment = take_while_span(|c| c != '*' && c != '/');
    let comment = not_comment.many_span(1..);
    comment
        .wrap_span(string_span("/*"), string_span("*/"))
        .trim_whitespace()
        .many_span(1..)
        .map(|s| Comment::Block(s.as_str().into()))
}

pub(super) fn line_comment<'a>() -> Parser<'a, Comment<'a>> {
    let not_newline = take_while_span(|c| c != '\n').opt_span();
    let end = string_span("\r").opt_span().then_span(string_span("\n"));
    not_newline
        .wrap_span(string_span("//"), end)
        .many_span(1..)
        .map(|s| Comment::Line(s.as_str().into()))
}

/// Skip any number of line/block comments (used between top-level items).
pub(super) fn skip_comments<'a>() -> Parser<'a, ()> {
    (block_comment() | line_comment())
        .trim_whitespace()
        .many(..)
        .map(|_| ())
}

/// Attach leading/trailing comments to an expression.
pub(super) fn trim_comment<'a>(
    p: Parser<'a, Expression<'a>>,
    comment_parser: Parser<'a, Option<Comment<'a>>>,
) -> Parser<'a, Expression<'a>> {
    p.trim_keep(comment_parser).map(|(left, mut expr, right)| {
        if left.is_some() || right.is_some() {
            let comments = Comments { left, right };
            set_expression_comments(&mut expr, comments);
        }
        expr
    })
}

// ─── Identifiers ────────────────────────────────────────────────────────────

pub(super) fn identifier<'a>() -> Parser<'a, Span<'a>> {
    let first_part = take_while_span(|c| c.is_alphabetic() || c == '_');
    let rest_part =
        take_while_span(|c| c.is_alphanumeric() || c == '_' || c == '-' || c == '.')
            .many_span(..);
    first_part.then_span(rest_part)
}

// ─── Terminals ──────────────────────────────────────────────────────────────

pub(super) fn literal<'a>() -> Parser<'a, Expression<'a>> {
    let quoted = |quote: &'a str| {
        let not_quote = take_while_span(|c| c != quote.chars().next().unwrap() && c != '\\');
        (not_quote | escaped_span())
            .many_span(..)
            .wrap_span(string_span(quote), string_span(quote))
    };
    (quoted("\"") | quoted("'") | quoted("`")).map(|s| {
        let token = Token::new(s.as_str().into(), s);
        Expression::Literal(token)
    })
}

pub(super) fn epsilon<'a>() -> Parser<'a, Expression<'a>> {
    string_span("epsilon").map(|s| {
        let token = Token::new((), s);
        Expression::Epsilon(token)
    })
}

pub(super) fn nonterminal<'a>() -> Parser<'a, Expression<'a>> {
    identifier().map(|s| {
        let token = Token::new(s.as_str().into(), s);
        Expression::Nonterminal(token)
    })
}

/// Scan a regex body between `/` delimiters, aware of character classes (`[...]`)
/// where `/` is literal and not a closing delimiter.
pub(super) fn regex_body<'a>() -> Parser<'a, Span<'a>> {
    Parser::new(move |state: &mut ParserState<'a>| {
        let start = state.offset;
        let bytes = state.src_bytes;
        let end = state.end;
        let mut i = start;
        let mut bracket_depth: u32 = 0;

        while i < end {
            match bytes[i] {
                b'\\' => {
                    i += 1;
                    if i < end {
                        i += 1;
                    }
                }
                b'[' if bracket_depth == 0 => {
                    bracket_depth += 1;
                    i += 1;
                }
                b']' if bracket_depth > 0 => {
                    bracket_depth -= 1;
                    i += 1;
                }
                b'/' if bracket_depth == 0 => break,
                _ => {
                    i += 1;
                }
            }
        }

        if i == start {
            return Some(Span::new(start, start, state.src));
        }
        state.offset = i;
        Some(Span::new(start, i, state.src))
    })
}

pub(super) fn regex<'a>() -> Parser<'a, Expression<'a>> {
    let string = regex_body().wrap_span(string_span("/"), string_span("/"));
    string.map(|s| {
        if let Err(e) = regex_syntax::Parser::new().parse(s.as_str()) {
            panic!("invalid regex: {:?}, {:?}", s.as_str(), e);
        }
        let token = Token::new(s.as_str().into(), s);
        Expression::Regex(token)
    })
}

/// Parse an import path string: `"path/to/file.bbnf"`.
pub(super) fn import_path<'a>() -> Parser<'a, Cow<'a, str>> {
    let not_quote = take_while_span(|c| c != '"' && c != '\\');
    let path_content = (not_quote | escaped_span()).many_span(..);
    path_content
        .wrap_span(string_span("\""), string_span("\""))
        .map(|s| Cow::Borrowed(s.as_str()))
}
