//! BBNF grammar parser.
//!
//! Sub-modules:
//! - `tokens` — terminal parsers (identifiers, literals, regexes, comments)
//! - `expressions` — grammar expression hierarchy (term → alternation)
//! - `directives` — @-directive parsers (@import, @recover, @pretty, etc.)
//! - `value_expr` — value expression parser for `->` map syntax

mod directives;
mod expressions;
mod tokens;
mod value_expr;

use std::borrow::Cow;

use parse_that::{Parser, any_span, string};

use crate::types::*;

/// Helper enum for interleaving directives and rules during top-level parsing.
enum TopLevelItem<'a> {
    Import(ImportDirective<'a>),
    Recover(RecoverDirective<'a>),
    Pretty(PrettyDirective<'a>),
    WsPattern(Cow<'a, str>),
    Debug(Cow<'a, str>),
    Token(Cow<'a, str>),
    Rule(Expression<'a>),
}

fn production_rule<'a>() -> Parser<'a, Expression<'a>> {
    let comment = tokens::block_comment() | tokens::line_comment();
    let eq = string("=").trim_whitespace();
    let terminator = any_span(&[";", "."]).trim_whitespace();

    let rule = expressions::lhs()
        .skip(eq)
        .then(expressions::rhs())
        .skip(terminator)
        .map(|(lhs, rhs)| {
            Expression::ProductionRule(
                lhs.into(),
                Expression::Rule(Box::new(rhs), None).into(),
            )
        });

    tokens::trim_comment(rule, comment.opt())
}

/// Parse a BBNF grammar file with all directives and imports.
///
/// This is the single entry point for all grammar parsing.
pub fn parse<'a>() -> Parser<'a, ParsedGrammar<'a>> {
    let import = tokens::skip_comments()
        .next(directives::import_directive().trim_whitespace())
        .map(TopLevelItem::Import);
    let recover = tokens::skip_comments()
        .next(directives::recover_directive(expressions::rhs()).trim_whitespace())
        .map(TopLevelItem::Recover);
    let pretty = tokens::skip_comments()
        .next(directives::pretty_directive().trim_whitespace())
        .map(TopLevelItem::Pretty);
    let ws_pat = tokens::skip_comments()
        .next(directives::ws_directive().trim_whitespace())
        .map(TopLevelItem::WsPattern);
    let debug_dir = tokens::skip_comments()
        .next(directives::debug_directive().trim_whitespace())
        .map(TopLevelItem::Debug);
    let token_dir = tokens::skip_comments()
        .next(directives::token_directive().trim_whitespace())
        .map(TopLevelItem::Token);
    let rule = tokens::skip_comments()
        .next(production_rule().trim_whitespace())
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
