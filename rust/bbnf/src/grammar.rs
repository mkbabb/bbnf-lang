use std::borrow::Cow;

use parse_that::parsers::utils::escaped_span;
use parse_that::{
    Parser, ParserFlat, ParserSpan, ParserState, Span, any_span, lazy, next_span, string,
    string_span, take_while_span,
};

use crate::types::*;

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
    fn block_comment() -> Parser<'a, Comment<'a>> {
        let not_comment = take_while_span(|c| c != '*' && c != '/');

        let comment = not_comment.many_span(1..);

        comment
            .wrap_span(string_span("/*"), string_span("*/"))
            .trim_whitespace()
            .many_span(1..)
            .map(|s| Comment::Block(s.as_str().into()))
    }

    fn line_comment() -> Parser<'a, Comment<'a>> {
        let not_newline = take_while_span(|c| c != '\n').opt_span();
        let end = string_span("\r").opt_span().then_span(string_span("\n"));

        not_newline
            .wrap_span(string_span("//"), end)
            .many_span(1..)
            .map(|s| Comment::Line(s.as_str().into()))
    }

    fn identifier() -> Parser<'a, Span<'a>> {
        let first_part = take_while_span(|c| c.is_alphabetic() || c == '_');
        let rest_part =
            take_while_span(|c| c.is_alphanumeric() || c == '_' || c == '-' || c == '.')
                .many_span(..);
        first_part.then_span(rest_part)
    }

    fn literal() -> Parser<'a, Expression<'a>> {
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

    fn epsilon() -> Parser<'a, Expression<'a>> {
        string_span("epsilon").map(|s| {
            let token = Token::new((), s);
            Expression::Epsilon(token)
        })
    }

    fn nonterminal() -> Parser<'a, Expression<'a>> {
        Self::identifier().map(|s| {
            let token = Token::new(s.as_str().into(), s);
            Expression::Nonterminal(token)
        })
    }

    /// Scan a regex body between `/` delimiters, aware of character classes (`[...]`)
    /// where `/` is literal and not a closing delimiter.
    fn regex_body() -> Parser<'a, Span<'a>> {
        let body = move |state: &mut ParserState<'a>| {
            let start = state.offset;
            let bytes = state.src_bytes;
            let end = state.end;
            let mut i = start;
            let mut bracket_depth: u32 = 0;

            while i < end {
                match bytes[i] {
                    b'\\' => {
                        // Escape sequence: consume backslash + next char.
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
                    b'/' if bracket_depth == 0 => {
                        // Closing delimiter — stop before it.
                        break;
                    }
                    _ => {
                        i += 1;
                    }
                }
            }

            if i == start {
                // Empty body is valid (e.g. `//`), produce an empty span.
                return Some(Span::new(start, start, state.src));
            }
            state.offset = i;
            Some(Span::new(start, i, state.src))
        };
        Parser::new(body)
    }

    fn regex() -> Parser<'a, Expression<'a>> {
        let string = Self::regex_body().wrap_span(string_span("/"), string_span("/"));

        string.map(|s| {
            if let Err(e) = regex_syntax::Parser::new().parse(s.as_str()) {
                panic!("invalid regex: {:?}, {:?}", s.as_str(), e);
            }
            let token = Token::new(s.as_str().into(), s);
            Expression::Regex(token)
        })
    }

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
            let inner = Expression::Epsilon(Token::new((), span.clone()));
            Expression::OptionalWhitespace(Box::new(Token::new(inner, span)))
        })
    }

    fn term() -> Parser<'a, Expression<'a>> {
        Self::epsilon()
            | Self::span_capture()
            | Self::standalone_optional_whitespace()
            | Self::group()
            | Self::optional_group()
            | Self::many_group()
            | Self::nonterminal()
            | Self::literal()
            | Self::regex()
    }

    fn trim_comment(
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

    fn factor() -> Parser<'a, Expression<'a>> {
        Self::trim_comment(
            Self::term()
                .then(any_span(&["?w", "*", "+", "?"]).trim_whitespace().many(..))
                .map_with_state(map_factor),
            Self::block_comment().opt(),
        )
    }

    /// Parse `factor -> mapper_expr` — per-expression map.
    /// The mapper text is consumed greedily until the next delimiter.
    fn mapped_factor() -> Parser<'a, Expression<'a>> {
        Self::factor().then(Self::map_arrow().opt()).map_with_state(
            |pair: (Expression<'a>, Option<Span<'a>>), prev_offset, state| {
                let (expr, mapper_opt) = pair;
                if let Some(mapper_span) = mapper_opt {
                    let mapper_str = mapper_span.as_str().trim();
                    let mapper_token = Token::new(Cow::Borrowed(mapper_str), mapper_span);
                    let expr_token =
                        Token::new(expr, Span::new(prev_offset, state.offset, state.src));
                    let fn_token = Token::new(
                        Expression::MappingFn(mapper_token),
                        Span::new(prev_offset, state.offset, state.src),
                    );
                    Expression::MappedExpression((Box::new(expr_token), Box::new(fn_token)))
                } else {
                    expr
                }
            },
        )
    }

    /// Parse the `->` operator and its argument text.
    /// Handles three mapper forms:
    /// 1. Rust closure: `|params| -> RetType { body }` or `|params| expr`
    /// 2. Function path: `crate::module::func`
    /// 3. Literal value: `0u8`, `true`
    ///
    /// Balanced `{}`/`()`/`[]` are tracked. Closure `|...|` params are recognized
    /// when the mapper starts with `|`.
    fn map_arrow() -> Parser<'a, Span<'a>> {
        string_span("->")
            .trim_whitespace()
            .next(Parser::new(|state: &mut ParserState<'a>| {
                let src = &state.src[state.offset..];
                let start = state.offset;
                let bytes = src.as_bytes();
                let len = bytes.len();
                let mut i = 0;

                // If the mapper starts with `|`, it's a Rust closure.
                // Consume the parameter list `|...|` first, then the body.
                if i < len && bytes[i] == b'|' {
                    // Skip opening `|` and find the matching closing `|`.
                    i += 1;
                    while i < len && bytes[i] != b'|' {
                        i += 1;
                    }
                    if i < len {
                        i += 1; // skip closing `|`
                    }
                    // Now consume the closure body with balanced delimiters.
                    // Stop at depth-0 `,`, `|`, `;` (BBNF delimiters).
                    let mut depth: usize = 0;
                    while i < len {
                        match bytes[i] {
                            b'{' | b'(' | b'[' => depth += 1,
                            b'}' | b')' | b']' => {
                                if depth == 0 {
                                    break;
                                }
                                depth -= 1;
                            }
                            b',' | b'|' | b';' if depth == 0 => break,
                            _ => {}
                        }
                        i += 1;
                    }
                } else {
                    // Non-closure mapper: consume until depth-0 delimiter.
                    let mut depth: usize = 0;
                    while i < len {
                        match bytes[i] {
                            b'{' | b'(' | b'[' => depth += 1,
                            b'}' | b')' | b']' => {
                                if depth == 0 {
                                    break;
                                }
                                depth -= 1;
                            }
                            b',' | b'|' | b';' if depth == 0 => break,
                            _ => {}
                        }
                        i += 1;
                    }
                }

                if i == 0 {
                    return None;
                }
                state.offset += i;
                Some(Span::new(start, state.offset, state.src))
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
        Self::nonterminal()
    }

    fn rhs() -> Parser<'a, Expression<'a>> {
        Self::alternation()
    }

    fn mapping_fn() -> Parser<'a, Option<Box<Expression<'a>>>> {
        let lhs = string_span(";").skip(Self::lhs().trim_whitespace().skip(string_span("=")));
        let not_lhs = next_span(1).look_ahead(lhs.negate());

        string_span("=>")
            .trim_whitespace()
            .next(not_lhs.many_span(..).then_span(next_span(1)))
            .map(|s| {
                let token = Token::new(s.as_str().into(), s);
                let trimmed = s.as_str().trim();
                // Accept closures, constant literals, or path expressions.
                if syn::parse_str::<syn::ExprClosure>(trimmed).is_err()
                    && syn::parse_str::<syn::Expr>(trimmed).is_err()
                {
                    panic!("invalid mapper expression: {:?}", trimmed);
                }

                Box::new(Expression::MappingFn(token))
            })
            .opt()
    }

    fn production_rule() -> Parser<'a, Expression<'a>> {
        let comment = Self::block_comment() | Self::line_comment();
        let eq = string("=").trim_whitespace();

        let terminator = (any_span(&[";", "."])).trim_whitespace();

        let production_rule = Self::lhs()
            .skip(eq)
            .then(Self::rhs())
            .then_flat(Self::mapping_fn())
            .skip(terminator)
            .map(|(lhs, rhs, mapping_fn)| {
                Expression::ProductionRule(
                    lhs.into(),
                    Expression::Rule(Box::new(rhs), mapping_fn).into(),
                )
            });

        Self::trim_comment(production_rule, comment.opt())
    }

    /// Parse an import path string: `"path/to/file.bbnf"`.
    fn import_path() -> Parser<'a, Cow<'a, str>> {
        let not_quote = take_while_span(|c| c != '"' && c != '\\');
        let path_content = (not_quote | escaped_span()).many_span(..);
        path_content
            .wrap_span(string_span("\""), string_span("\""))
            .map(|s| Cow::Borrowed(s.as_str()))
    }

    /// Parse a list of identifiers in `{ a, b, c }` form.
    fn import_items() -> Parser<'a, Vec<ImportedName<'a>>> {
        let ident = Self::identifier().map(|s: Span<'a>| ImportedName {
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
            .then(Self::import_path())
            .map(|(items, path)| (Some(items), path));

        let glob = Self::import_path().map(|path| (None, path));

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
                Self::identifier()
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
                (string_span("*") | Self::identifier())
                    .trim_whitespace()
                    .map(|span| Cow::Borrowed(span.as_str())),
            )
            .skip(any_span(&[";", "."]).opt().trim_whitespace())
    }

    /// Parse a `@token ruleName ;` directive — mark a rule as a lexical token.
    fn token_directive() -> Parser<'a, Cow<'a, str>> {
        string("@token")
            .trim_whitespace()
            .next(Self::identifier().trim_whitespace())
            .skip(any_span(&[";", "."]).opt().trim_whitespace())
            .map(|name_span| Cow::Borrowed(name_span.as_str()))
    }

    /// Parse an `@ws /regex/ ;` directive — custom whitespace pattern for `?w`.
    fn ws_directive() -> Parser<'a, Cow<'a, str>> {
        string("@ws")
            .trim_whitespace()
            .next(Self::regex().trim_whitespace())
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
        let hint = Self::sep_hint() | Self::split_hint() | Self::identifier();

        string("@pretty")
            .trim_whitespace()
            .next(
                Self::identifier()
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

    /// Skip any number of line/block comments (used between top-level items).
    fn skip_comments() -> Parser<'a, ()> {
        (Self::block_comment() | Self::line_comment())
            .trim_whitespace()
            .many(..)
            .map(|_| ())
    }

    /// Parse a grammar file: interleaved import directives, recover directives, and rules.
    /// Returns a `ParsedGrammar` with imports, recovers, and the AST.
    pub fn grammar_with_imports() -> Parser<'a, ParsedGrammar<'a>> {
        let import = Self::skip_comments()
            .next(Self::import_directive().trim_whitespace())
            .map(TopLevelItem::Import);
        let recover = Self::skip_comments()
            .next(Self::recover_directive().trim_whitespace())
            .map(TopLevelItem::Recover);
        let pretty = Self::skip_comments()
            .next(Self::pretty_directive().trim_whitespace())
            .map(TopLevelItem::Pretty);
        let ws_pat = Self::skip_comments()
            .next(Self::ws_directive().trim_whitespace())
            .map(TopLevelItem::WsPattern);
        let debug_dir = Self::skip_comments()
            .next(Self::debug_directive().trim_whitespace())
            .map(TopLevelItem::Debug);
        let token_dir = Self::skip_comments()
            .next(Self::token_directive().trim_whitespace())
            .map(TopLevelItem::Token);
        let rule = Self::skip_comments()
            .next(Self::production_rule().trim_whitespace())
            .map(TopLevelItem::Rule);

        let item = import | recover | pretty | ws_pat | debug_dir | token_dir | rule;

        Self::skip_comments().next(item.many(..)).map(|items| {
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
