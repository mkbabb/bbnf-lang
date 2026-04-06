use std::borrow::Cow;
use std::fmt;

use parse_that::Span;

use pprint::Pretty;

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

// ─── Value Expression AST ───────────────────────────────────────────────────

/// Binary operators in value expressions after `->`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    And,
    Or,
}

impl fmt::Display for BinOpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Mod => "%",
            Self::Eq => "==",
            Self::Ne => "!=",
            Self::Lt => "<",
            Self::Gt => ">",
            Self::Le => "<=",
            Self::Ge => ">=",
            Self::And => "&&",
            Self::Or => "||",
        })
    }
}

/// Unary operators in value expressions after `->`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOpKind {
    Neg,
    Not,
}

impl fmt::Display for UnaryOpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Neg => "-",
            Self::Not => "!",
        })
    }
}

/// Structured value expression parsed from `->` map syntax.
///
/// Stores raw source text for numeric literals (to preserve suffixes like `u8`, `f64`)
/// and converts to typed `MapExpr` during lowering.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ValueExpr<'a> {
    /// Integer literal: `42`, `0u8`, `0xFF`, `0xF0F8FFFFu32`.
    IntLit(Token<'a, Cow<'a, str>>),
    /// Float literal: `3.14`, `1.0f64`.
    FloatLit(Token<'a, Cow<'a, str>>),
    /// Boolean literal: `true`, `false`.
    BoolLit(Token<'a, bool>),
    /// String literal: `"hello"`.
    StringLit(Token<'a, Cow<'a, str>>),
    /// `input` keyword — the parse result.
    Input(#[allow(dead_code)] Span<'a>),
    /// Property access on input: `input.len`, `input.as_str`.
    InputProp(#[allow(dead_code)] Span<'a>, Token<'a, Cow<'a, str>>),
    /// Bare identifier (type name, variable, or path): `f64`, `crate::module::func`.
    Ident(Token<'a, Cow<'a, str>>),
    /// Function call: `parse_hex(input)`, `crate::module::func(input)`.
    FnCall(Token<'a, Cow<'a, str>>, Vec<ValueExpr<'a>>),
    /// Binary operation: `a + b`, `input == "on"`.
    BinOp(BinOpKind, Box<ValueExpr<'a>>, Box<ValueExpr<'a>>),
    /// Unary operation: `-a`, `!flag`.
    UnaryOp(UnaryOpKind, Box<ValueExpr<'a>>),
    /// Parenthesized expression: `(a + b)`.
    Paren(Box<ValueExpr<'a>>),
    /// Value closure: `|params| body` — explicit parameter binding in value context.
    Closure(Vec<Token<'a, Cow<'a, str>>>, Box<ValueExpr<'a>>),
}

/// A `->` value-expression mapping with optional type annotation.
///
/// Produced by `factor -> value_expr : TYPE` syntax.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MapArrow<'a> {
    pub expr: ValueExpr<'a>,
    pub return_type: Option<Token<'a, Cow<'a, str>>>,
    pub span: Span<'a>,
}

impl<'a> pprint::Pretty<'a> for MapArrow<'a> {
    fn pretty(&'a self, _b: &mut pprint::FmtBuilder<'a>) {
        // MapArrow is always #[pprint(skip)] in Expression variants,
        // but the derive macro still requires the trait bound.
    }
}

// ─── Grammar Expression AST ────────────────────────────────────────────────

type TokenExpression<'a, T = Expression<'a>> = Box<Token<'a, T>>;

#[derive(Pretty, Debug, Clone, Eq, PartialEq, Hash)]
pub enum Expression<'a> {
    Literal(Token<'a, Cow<'a, str>>),
    Nonterminal(Token<'a, Cow<'a, str>>),

    Regex(Token<'a, Cow<'a, str>>),

    /// Per-expression mapping: `factor -> value_expr : TYPE`.
    MappedExpression(TokenExpression<'a>, #[pprint(skip)] MapArrow<'a>),

    DebugExpression((TokenExpression<'a>, String)),

    Group(TokenExpression<'a>),
    Optional(TokenExpression<'a>),
    OptionalWhitespace(TokenExpression<'a>),
    /// Span capture: `@{expr}` — parse inner expression, discard result, return Span.
    SpanCapture(TokenExpression<'a>),

    Many(TokenExpression<'a>),
    Many1(TokenExpression<'a>),

    Skip(TokenExpression<'a>, TokenExpression<'a>),
    Next(TokenExpression<'a>, TokenExpression<'a>),
    Minus(TokenExpression<'a>, TokenExpression<'a>),

    Concatenation(TokenExpression<'a, Vec<Expression<'a>>>),
    Alternation(TokenExpression<'a, Vec<Expression<'a>>>),

    /// Grammar closure: `|params| body` — compile-time grammar function.
    Closure(Vec<Token<'a, Cow<'a, str>>>, TokenExpression<'a>),

    /// Grammar function call: `name(arg1, arg2)` — invokes a closure-bound rule.
    GrammarCall(Token<'a, Cow<'a, str>>, Vec<Expression<'a>>),

    Rule(Box<Expression<'a>>, Option<MapArrow<'a>>),

    ProductionRule(Box<Expression<'a>>, Box<Expression<'a>>),

    Epsilon(#[pprint(skip)] Token<'a, ()>),
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

/// A single imported name in a selective `@import { a, b } from "path"` directive.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportedName<'a> {
    pub name: Cow<'a, str>,
    pub span: Span<'a>,
}

/// An `@import` directive at the top of a grammar file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportDirective<'a> {
    /// The path string from the import (relative to importing file).
    pub path: Cow<'a, str>,
    /// The byte-offset span of the entire import directive.
    pub span: Span<'a>,
    /// If `Some`, selective import: only these rule names are imported.
    /// If `None`, glob import: all rules are imported.
    pub items: Option<Vec<ImportedName<'a>>>,
}

/// An `@recover` directive that annotates a rule with error recovery.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecoverDirective<'a> {
    /// The name of the rule to wrap with recovery.
    pub rule_name: Cow<'a, str>,
    /// The sync expression to use for error recovery (typically a regex).
    pub sync_expr: Expression<'a>,
    /// The byte-offset span of the entire recover directive.
    pub span: Span<'a>,
}

/// An `@pretty` directive that provides formatting hints for a rule.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PrettyDirective<'a> {
    /// The name of the rule to apply formatting hints to.
    pub rule_name: Cow<'a, str>,
    /// Formatting hints (e.g. "group", "indent", "block", "blankline", "nobreak", "fast").
    pub hints: Vec<Cow<'a, str>>,
    /// The byte-offset span of the entire pretty directive.
    pub span: Span<'a>,
}

/// The result of parsing a complete grammar file: imports + rules.
#[derive(Debug, Clone)]
pub struct ParsedGrammar<'a> {
    pub imports: Vec<ImportDirective<'a>>,
    pub recovers: Vec<RecoverDirective<'a>>,
    pub pretties: Vec<PrettyDirective<'a>>,
    pub rules: AST<'a>,
    /// Custom whitespace pattern from `@ws /regex/ ;` directive.
    /// When set, `?w` compiles to this regex instead of the default `\s*` trim.
    pub ws_pattern: Option<Cow<'a, str>>,
    /// Rules to instrument for debugging from `@debug ruleName ;` directives.
    /// `"*"` means all rules.
    pub debug_rules: Vec<Cow<'a, str>>,
    /// Rules marked as lexical tokens from `@token ruleName ;` directives.
    pub token_rules: Vec<Cow<'a, str>>,
    /// Host function declarations from `@host funcName ;` directives.
    pub host_fns: Vec<Cow<'a, str>>,
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
        | Expression::SpanCapture(token)
        | Expression::Skip(token, _)
        | Expression::Next(token, _)
        | Expression::Minus(token, _) => token.comments = Some(comments),

        Expression::Concatenation(token) | Expression::Alternation(token) => {
            token.comments = Some(comments)
        }

        Expression::MappedExpression(inner, _) | Expression::Closure(_, inner) => {
            inner.comments = Some(comments)
        }

        _ => {}
    }
}
