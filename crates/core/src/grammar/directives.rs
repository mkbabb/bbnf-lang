//! @-directive parsers: @import, @recover, @pretty, @ws, @debug, @token.

use std::borrow::Cow;

use parse_that::{
    Parser, ParserSpan, Span, any_span, next_span, string, string_span, take_while_span,
};

use crate::types::*;

use super::tokens;

/// Parse a list of identifiers in `{ a, b, c }` form.
fn import_items<'a>() -> Parser<'a, Vec<ImportedName<'a>>> {
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
pub(super) fn import_directive<'a>() -> Parser<'a, ImportDirective<'a>> {
    let selective = import_items()
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

/// Parse an `@recover` directive: `@recover ruleName syncExpr ;`
pub(super) fn recover_directive<'a>(
    rhs: Parser<'a, Expression<'a>>,
) -> Parser<'a, RecoverDirective<'a>> {
    string("@recover")
        .trim_whitespace()
        .next(
            tokens::identifier()
                .trim_whitespace()
                .then(rhs.trim_whitespace()),
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

/// Parse a `@debug ruleName ;` or `@debug * ;` directive.
pub(super) fn debug_directive<'a>() -> Parser<'a, Cow<'a, str>> {
    string("@debug")
        .trim_whitespace()
        .next(
            (string_span("*") | tokens::identifier())
                .trim_whitespace()
                .map(|span| Cow::Borrowed(span.as_str())),
        )
        .skip(any_span(&[";", "."]).opt().trim_whitespace())
}

/// Parse a `@token ruleName ;` directive.
pub(super) fn token_directive<'a>() -> Parser<'a, Cow<'a, str>> {
    string("@token")
        .trim_whitespace()
        .next(tokens::identifier().trim_whitespace())
        .skip(any_span(&[";", "."]).opt().trim_whitespace())
        .map(|name_span| Cow::Borrowed(name_span.as_str()))
}

/// Parse an `@ws /regex/ ;` directive — custom whitespace pattern for `?w`.
pub(super) fn ws_directive<'a>() -> Parser<'a, Cow<'a, str>> {
    string("@ws")
        .trim_whitespace()
        .next(tokens::regex().trim_whitespace())
        .skip(any_span(&[";", "."]).opt().trim_whitespace())
        .map(|regex_expr| match regex_expr {
            Expression::Regex(token) => token.value,
            _ => unreachable!(),
        })
}

/// Parse a `sep("...")` hint.
fn sep_hint<'a>() -> Parser<'a, Span<'a>> {
    let not_quote = take_while_span(|c| c != '"' && c != '\\');
    let escaped = string_span(r"\").then_span(next_span(1));
    let quoted_content = (not_quote | escaped)
        .many_span(..)
        .wrap_span(string_span("\""), string_span("\""));
    string_span("sep(")
        .then_span(quoted_content)
        .then_span(string_span(")"))
}

/// Parse a `split("...")` hint.
fn split_hint<'a>() -> Parser<'a, Span<'a>> {
    let not_quote = take_while_span(|c| c != '"' && c != '\\');
    let escaped = string_span(r"\").then_span(next_span(1));
    let quoted_content = (not_quote | escaped)
        .many_span(..)
        .wrap_span(string_span("\""), string_span("\""));
    string_span("split(")
        .then_span(quoted_content)
        .then_span(string_span(")"))
}

/// Parse a `@host funcName ;` or `@host funcName : ReturnType ;` directive.
///
/// Declares an external host function with an optional abstract return type.
/// The return type is backend-agnostic — each backend resolves the name to
/// its native type representation.
pub(super) fn host_directive<'a>() -> Parser<'a, HostFnDecl<'a>> {
    string("@host")
        .trim_whitespace()
        .next(tokens::identifier().trim_whitespace())
        .then(
            string(":")
                .trim_whitespace()
                .next(tokens::identifier().trim_whitespace())
                .opt(),
        )
        .skip(any_span(&[";", "."]).opt().trim_whitespace())
        .map(|(name_span, return_type_span)| HostFnDecl {
            name: Cow::Borrowed(name_span.as_str()),
            return_type: return_type_span.map(|s| Cow::Borrowed(s.as_str())),
        })
}

/// Parse a `@pretty` directive: `@pretty ruleName hint1 hint2 ... ;`
pub(super) fn pretty_directive<'a>() -> Parser<'a, PrettyDirective<'a>> {
    let hint = sep_hint() | split_hint() | tokens::identifier();

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
