mod common;

use bbnf::optimize::remove_direct_left_recursion;
use bbnf::types::{Expression, Token, AST};

use common::{lit, nt};

#[test]
fn test_no_left_recursion() {
    let mut ast = AST::new();
    let a = nt("A");
    let rhs = Expression::Rule(Box::new(lit("x")), None);
    ast.insert(a.clone(), rhs.clone());

    let result = remove_direct_left_recursion(&ast);
    assert_eq!(result.len(), 1);
    assert!(result.get(&a).is_some());
}

#[test]
fn test_direct_left_recursion() {
    let mut ast = AST::new();
    let a = nt("A");

    // A = A "x" | "y"
    let alt1 = Expression::Concatenation(Box::new(Token::new_without_span(vec![
        nt("A"),
        lit("x"),
    ])));
    let alt2 = lit("y");
    let rhs = Expression::Rule(
        Box::new(Expression::Alternation(Box::new(
            Token::new_without_span(vec![alt1, alt2]),
        ))),
        None,
    );
    ast.insert(a.clone(), rhs);

    let result = remove_direct_left_recursion(&ast);
    // Should have 2 rules: A and A_tail
    assert_eq!(result.len(), 2);

    let tail = nt("A_tail");
    assert!(result.get(&tail).is_some());
}
