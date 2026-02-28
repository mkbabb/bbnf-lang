use std::borrow::Cow;

use bbnf::types::{Expression, Token};
use parse_that::Span;

/// Helper: create a Nonterminal expression with the given name.
pub fn nt(name: &str) -> Expression<'_> {
    Expression::Nonterminal(Token::new(Cow::Borrowed(name), Span::new(0, 0, "")))
}

/// Helper: create a Literal expression.
pub fn lit(value: &str) -> Expression<'_> {
    Expression::Literal(Token::new(Cow::Borrowed(value), Span::new(0, 0, "")))
}
