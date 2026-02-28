use std::borrow::Cow;

use parse_that::Span;

use pprint::{Doc, Pretty};

use indexmap::IndexMap;

#[derive(Pretty, Debug, Clone, Eq, PartialEq)]
pub enum Comment<'a> {
    Line(Cow<'a, str>),
    Block(Cow<'a, str>),
}

#[derive(Pretty, Debug, Clone, Eq, PartialEq)]
pub struct Comments<'a> {
    pub left: Option<Comment<'a>>,
    pub right: Option<Comment<'a>>,
}

type TokenExpression<'a, T = Expression<'a>> = Box<Token<'a, T>>;

#[derive(Pretty, Debug, Clone, Eq, PartialEq, Hash)]
pub enum Expression<'a> {
    Literal(Token<'a, Cow<'a, str>>),
    Nonterminal(Token<'a, Cow<'a, str>>),

    Regex(Token<'a, Cow<'a, str>>),

    MappingFn(Token<'a, Cow<'a, str>>),
    MappedExpression((TokenExpression<'a>, TokenExpression<'a>)),

    DebugExpression((TokenExpression<'a>, String)),

    Group(TokenExpression<'a>),
    Optional(TokenExpression<'a>),
    OptionalWhitespace(TokenExpression<'a>),

    Many(TokenExpression<'a>),
    Many1(TokenExpression<'a>),

    Skip(TokenExpression<'a>, TokenExpression<'a>),
    Next(TokenExpression<'a>, TokenExpression<'a>),
    Minus(TokenExpression<'a>, TokenExpression<'a>),

    Concatenation(TokenExpression<'a, Vec<Expression<'a>>>),
    Alternation(TokenExpression<'a, Vec<Expression<'a>>>),

    Rule(Box<Expression<'a>>, Option<Box<Expression<'a>>>),

    ProductionRule(Box<Expression<'a>>, Box<Expression<'a>>),

    Epsilon(Token<'a, ()>),
}

#[derive(Pretty, Debug, Clone, Eq)]
pub struct Token<'a, T> {
    pub value: T,

    #[pprint(skip)]
    pub span: Span<'a>,
    #[pprint(skip)]
    pub comments: Option<Comments<'a>>,
}

impl<'a, T> Token<'a, T> {
    pub fn new(value: T, span: Span<'a>) -> Self {
        Self {
            value,
            span,
            comments: None,
        }
    }

    pub fn new_without_span(value: T) -> Self {
        Self {
            value,
            span: Span::new(0, 0, ""),
            comments: None,
        }
    }

    pub fn inner(&self) -> &T {
        &self.value
    }
}

impl<'a, T: PartialEq> PartialEq for Token<'a, T> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}
impl<'a, T: std::hash::Hash> std::hash::Hash for Token<'a, T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.value.hash(state);
    }
}

pub type AST<'a> = IndexMap<Expression<'a>, Expression<'a>>;

/// An `@import` directive at the top of a grammar file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportDirective<'a> {
    /// The path string from the import (relative to importing file).
    pub path: Cow<'a, str>,
    /// The byte-offset span of the entire import directive.
    pub span: Span<'a>,
    /// If `Some`, selective import: only these rule names are imported.
    /// If `None`, glob import: all rules are imported.
    pub items: Option<Vec<Cow<'a, str>>>,
}

/// The result of parsing a complete grammar file: imports + rules.
#[derive(Debug, Clone)]
pub struct ParsedGrammar<'a> {
    pub imports: Vec<ImportDirective<'a>>,
    pub rules: AST<'a>,
}

pub fn set_expression_comments<'a>(expr: &mut Expression<'a>, comments: Comments<'a>) {
    match expr {
        Expression::Literal(token) | Expression::Nonterminal(token) => {
            token.comments = Some(comments)
        }

        Expression::Regex(token) => token.comments = Some(comments),

        Expression::Epsilon(token) => token.comments = Some(comments),

        Expression::Group(token)
        | Expression::Optional(token)
        | Expression::Many(token)
        | Expression::Many1(token)
        | Expression::Skip(token, _)
        | Expression::Next(token, _)
        | Expression::Minus(token, _) => token.comments = Some(comments),

        Expression::Concatenation(token) | Expression::Alternation(token) => {
            token.comments = Some(comments)
        }

        _ => {}
    }
}
