//! Dependency graph construction and AST traversal.

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::types::{Expression, Token, AST};

pub type Visitor<'a> = dyn FnMut(&'a Expression<'a>, &'a Expression<'a>) + 'a;

pub type Dependencies<'a> = HashMap<Expression<'a>, HashSet<Expression<'a>>>;

pub fn get_nonterminal_name<'a>(expr: &'a Expression<'a>) -> Option<&'a str> {
    if let Expression::Nonterminal(Token { value, .. }) = expr {
        Some(value)
    } else {
        None
    }
}

pub fn traverse_ast<'a>(ast: &'a AST, visitor: Option<&mut Visitor<'a>>) {
    fn visit<'a>(
        nonterminal: &'a Expression<'a>,
        expr: &'a Expression<'a>,
        visitor: &mut Visitor<'a>,
    ) {
        visitor(nonterminal, expr);
        match expr {
            Expression::Alternation(inner_exprs) => {
                let inner_exprs = inner_exprs.inner();
                for inner_expr in inner_exprs {
                    visit(nonterminal, inner_expr, visitor)
                }
            }
            Expression::Concatenation(inner_exprs) => {
                let inner_exprs = inner_exprs.inner();
                for inner_expr in inner_exprs {
                    visit(nonterminal, inner_expr, visitor)
                }
            }
            Expression::Skip(left_expr, right_expr)
            | Expression::Next(left_expr, right_expr)
            | Expression::Minus(left_expr, right_expr) => {
                let left_expr = left_expr.inner();
                let right_expr = right_expr.inner();
                visit(nonterminal, left_expr, visitor);
                visit(nonterminal, right_expr, visitor);
            }
            Expression::Group(inner_expr)
            | Expression::Optional(inner_expr)
            | Expression::Many(inner_expr)
            | Expression::Many1(inner_expr)
            | Expression::OptionalWhitespace(inner_expr) => {
                let inner_expr = inner_expr.inner();
                visit(nonterminal, inner_expr, visitor);
            }
            Expression::Rule(rhs, _) => {
                visit(nonterminal, rhs, visitor);
            }
            _ => {}
        }
    }

    let mut visitor_default = |_, _| {};
    let visitor = visitor.unwrap_or(&mut visitor_default);
    ast.into_iter()
        .for_each(|(lhs, rhs)| visit(lhs, rhs, visitor));
}

pub fn calculate_ast_deps<'a>(ast: &'a AST<'a>) -> Dependencies<'a> {
    let deps = Rc::new(RefCell::new(HashMap::new()));
    let mut visitor = {
        let deps = deps.clone();
        move |nonterminal: &'a Expression, expr: &'a Expression| {
            let mut deps = deps.borrow_mut();
            let sub_deps = deps.entry(nonterminal.clone()).or_insert(HashSet::new());
            if let Expression::Nonterminal(_) = expr {
                sub_deps.insert(expr.clone());
            }
        }
    };
    traverse_ast(ast, Some(&mut visitor));
    deps.take()
}
