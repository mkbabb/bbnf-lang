//! Left-recursion removal and left-factoring for BBNF grammars.
//!
//! Direct left-recursion elimination follows the standard algorithm:
//! For a rule `A = A α₁ | A α₂ | ... | β₁ | β₂ | ...`
//! Transform to:
//!   `A  = β₁ A' | β₂ A' | ...`
//!   `A' = α₁ A' | α₂ A' | ... | ε`
//!
//! Indirect left-recursion uses Paull's algorithm: for rules ordered
//! A₁, A₂, ..., Aₙ within each multi-member SCC, substitute earlier
//! rules' bodies forward, converting indirect cycles to direct ones.
//!
//! Both are opt-in via flag, matching the TypeScript design.

use std::borrow::Cow;

use crate::types::{Expression, Token, AST};
/// Remove all direct left-recursion from the grammar.
///
/// For each rule, if any alternative in its alternation starts with a reference
/// to itself, apply the standard left-recursion elimination algorithm.
///
/// This does NOT handle indirect left-recursion (A -> B -> A). For that,
/// the grammar would need to be topologically reordered first (which the
/// SCC analysis already provides).
pub fn remove_direct_left_recursion<'a>(ast: &AST<'a>) -> AST<'a> {
    let mut new_ast = AST::new();

    for (lhs, rhs) in ast {
        let lhs_name = match lhs {
            Expression::Nonterminal(token) => token.value.as_ref(),
            _ => {
                new_ast.insert(lhs.clone(), rhs.clone());
                continue;
            }
        };

        // Unwrap Rule to get the actual expression
        let (inner_expr, mapping_fn) = match rhs {
            Expression::Rule(inner, mapping) => (inner.as_ref(), mapping.clone()),
            other => (other, None),
        };

        // Only process alternations
        let alternatives = match inner_expr {
            Expression::Alternation(token) => &token.value,
            _ => {
                new_ast.insert(lhs.clone(), rhs.clone());
                continue;
            }
        };

        // Partition into left-recursive (alpha) and non-left-recursive (beta) alternatives
        let mut alphas: Vec<Expression<'a>> = Vec::new();
        let mut betas: Vec<Expression<'a>> = Vec::new();

        for alt in alternatives {
            if is_left_recursive(alt, lhs_name) {
                // Strip the leading self-reference
                if let Some(stripped) = strip_leading_nonterminal(alt, lhs_name) {
                    alphas.push(stripped);
                } else {
                    betas.push(alt.clone());
                }
            } else {
                betas.push(alt.clone());
            }
        }

        if alphas.is_empty() {
            // No left-recursion — keep original
            new_ast.insert(lhs.clone(), rhs.clone());
            continue;
        }

        // Create the tail rule name: A' (A_tail)
        let tail_name = format!("{}_tail", lhs_name);

        let tail_nt = Expression::Nonterminal(Token::new_without_span(Cow::Owned(tail_name.clone())));
        let tail_lhs = Expression::Nonterminal(Token::new_without_span(Cow::Owned(tail_name)));

        // A = β₁ A' | β₂ A' | ...
        let new_betas: Vec<Expression<'a>> = betas
            .into_iter()
            .map(|beta| {
                // β A'
                let token = Token::new_without_span(vec![beta, tail_nt.clone()]);
                Expression::Concatenation(Box::new(token))
            })
            .collect();

        let new_rhs = if new_betas.len() == 1 {
            new_betas.into_iter().next().unwrap()
        } else {
            Expression::Alternation(Box::new(Token::new_without_span(new_betas)))
        };

        new_ast.insert(
            lhs.clone(),
            Expression::Rule(Box::new(new_rhs), mapping_fn),
        );

        // A' = α₁ A' | α₂ A' | ... | ε
        let mut tail_alts: Vec<Expression<'a>> = alphas
            .into_iter()
            .map(|alpha| {
                let token = Token::new_without_span(vec![alpha, tail_nt.clone()]);
                Expression::Concatenation(Box::new(token))
            })
            .collect();

        // Add epsilon alternative
        tail_alts.push(Expression::Epsilon(Token::new_without_span(())));

        let tail_rhs = Expression::Alternation(Box::new(Token::new_without_span(tail_alts)));

        new_ast.insert(
            tail_lhs,
            Expression::Rule(Box::new(tail_rhs), None),
        );
    }

    new_ast
}

// ── Indirect Left-Recursion (Paull's Algorithm) ─────────────────────────────

/// Eliminate indirect left-recursion using Paull's algorithm.
///
/// For each multi-member SCC (where indirect cycles occur), processes rules
/// in topological order: for A_i, substitute all earlier A_j (j < i) bodies
/// where A_i starts with A_j. After substitution, any remaining left-recursion
/// in A_i is direct and handled by `remove_direct_left_recursion`.
///
/// `indirect_sccs` is a list of multi-member SCCs, each containing the rule
/// names in topological order. Pre-extracted from SccResult to avoid lifetime
/// entanglement with the dependency graph.
pub fn remove_indirect_left_recursion<'a>(
    ast: &AST<'a>,
    indirect_sccs: &[Vec<String>],
) -> AST<'a> {
    let mut ast = ast.clone();

    if indirect_sccs.is_empty() {
        return ast;
    }

    for scc_names in indirect_sccs {

        // For each rule A_i in topological order within the SCC.
        for i in 0..scc_names.len() {
            let name_i = &scc_names[i];

            // Find the LHS key for rule i.
            let lhs_i =
                Expression::Nonterminal(Token::new_without_span(Cow::Owned(name_i.clone())));
            let body_i = match ast.get(&lhs_i) {
                Some(body) => body.clone(),
                None => continue,
            };

            // Unwrap Rule wrapper if present.
            let (inner_i, mapping_fn) = match &body_i {
                Expression::Rule(inner, mapping) => (inner.as_ref().clone(), mapping.clone()),
                other => (other.clone(), None),
            };

            // For each earlier rule A_j in the SCC.
            let mut current = inner_i;
            for name_j in scc_names.iter().take(i) {
                let lhs_j =
                    Expression::Nonterminal(Token::new_without_span(Cow::Owned(name_j.clone())));
                let body_j = match ast.get(&lhs_j) {
                    Some(body) => body.clone(),
                    None => continue,
                };

                // Unwrap Rule wrapper for target body.
                let inner_j = match &body_j {
                    Expression::Rule(inner, _) => inner.as_ref().clone(),
                    other => other.clone(),
                };

                // Substitute A_j's body where A_i starts with A_j.
                if let Some(substituted) =
                    substitute_leading_nonterminal(&current, name_j, &inner_j)
                {
                    current = substituted;
                }
            }

            // Write back the substituted body.
            let new_body = Expression::Rule(Box::new(current), mapping_fn);
            ast.insert(lhs_i, new_body);
        }
    }

    ast
}

/// If `expr` begins with nonterminal `target_name`, substitute `target_expr`
/// in place of that leading reference. Returns `Some(substituted)` or `None`
/// if no substitution was made.
fn substitute_leading_nonterminal<'a>(
    expr: &Expression<'a>,
    target_name: &str,
    target_expr: &Expression<'a>,
) -> Option<Expression<'a>> {
    match expr {
        Expression::Alternation(token) => {
            let branches = &token.value;
            let mut any_changed = false;
            let mut new_branches: Vec<Expression<'a>> = Vec::new();

            for branch in branches {
                if let Some(sub) =
                    substitute_leading_nonterminal(branch, target_name, target_expr)
                {
                    // Flatten alternation results.
                    if let Expression::Alternation(inner) = &sub {
                        new_branches.extend(inner.value.iter().cloned());
                    } else {
                        new_branches.push(sub);
                    }
                    any_changed = true;
                } else {
                    new_branches.push(branch.clone());
                }
            }

            if any_changed {
                Some(Expression::Alternation(Box::new(Token::new_without_span(
                    new_branches,
                ))))
            } else {
                None
            }
        }

        Expression::Concatenation(token) => {
            let elems = &token.value;
            if elems.is_empty() {
                return None;
            }

            let first = &elems[0];
            if let Expression::Nonterminal(tok) = first {
                if tok.value.as_ref() == target_name {
                    let rest: Vec<Expression<'a>> = elems[1..].to_vec();

                    // Replace leading nonterminal with target expression's alternatives.
                    if let Expression::Alternation(alt_token) = target_expr {
                        let new_branches: Vec<Expression<'a>> = alt_token
                            .value
                            .iter()
                            .map(|alt| {
                                let mut combined = vec![alt.clone()];
                                combined.extend(rest.iter().cloned());
                                Expression::Concatenation(Box::new(Token::new_without_span(
                                    combined,
                                )))
                            })
                            .collect();
                        return Some(Expression::Alternation(Box::new(
                            Token::new_without_span(new_branches),
                        )));
                    } else {
                        let mut combined = vec![target_expr.clone()];
                        combined.extend(rest);
                        return Some(Expression::Concatenation(Box::new(
                            Token::new_without_span(combined),
                        )));
                    }
                }
            }
            None
        }

        // Single nonterminal reference.
        Expression::Nonterminal(tok) if tok.value.as_ref() == target_name => {
            Some(target_expr.clone())
        }

        _ => None,
    }
}

// ── Direct Left-Recursion ───────────────────────────────────────────────────

/// Check if an expression starts with a reference to the given nonterminal name.
fn is_left_recursive(expr: &Expression<'_>, name: &str) -> bool {
    match expr {
        Expression::Nonterminal(token) => token.value.as_ref() == name,
        Expression::Concatenation(token) => {
            if let Some(first) = token.value.first() {
                is_left_recursive(first, name)
            } else {
                false
            }
        }
        Expression::Group(inner) => is_left_recursive(&inner.value, name),
        _ => false,
    }
}

/// Strip the leading nonterminal reference from an expression.
/// For concatenation `[A, x, y]`, returns `Concatenation([x, y])` or just `x` if only one remains.
/// For bare `A`, returns `Epsilon`.
fn strip_leading_nonterminal<'a>(expr: &Expression<'a>, name: &str) -> Option<Expression<'a>> {
    match expr {
        Expression::Nonterminal(token) if token.value.as_ref() == name => {
            Some(Expression::Epsilon(Token::new_without_span(())))
        }
        Expression::Concatenation(token) => {
            let exprs = &token.value;
            if exprs.is_empty() {
                return None;
            }
            if !is_left_recursive(&exprs[0], name) {
                return None;
            }
            let rest: Vec<Expression<'a>> = exprs[1..].to_vec();
            if rest.is_empty() {
                Some(Expression::Epsilon(Token::new_without_span(())))
            } else if rest.len() == 1 {
                Some(rest.into_iter().next().unwrap())
            } else {
                Some(Expression::Concatenation(Box::new(Token::new_without_span(rest))))
            }
        }
        Expression::Group(inner) => strip_leading_nonterminal(&inner.value, name),
        _ => None,
    }
}

// ── Tests ───────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use indexmap::IndexMap;

    fn nt(name: &str) -> Expression<'static> {
        Expression::Nonterminal(Token::new_without_span(Cow::Owned(name.to_string())))
    }

    fn lit(s: &str) -> Expression<'static> {
        Expression::Literal(Token::new_without_span(Cow::Owned(s.to_string())))
    }

    fn cat(exprs: Vec<Expression<'static>>) -> Expression<'static> {
        Expression::Concatenation(Box::new(Token::new_without_span(exprs)))
    }

    fn alt(exprs: Vec<Expression<'static>>) -> Expression<'static> {
        Expression::Alternation(Box::new(Token::new_without_span(exprs)))
    }

    fn rule(inner: Expression<'static>) -> Expression<'static> {
        Expression::Rule(Box::new(inner), None)
    }

    fn make_ast(rules: Vec<(&str, Expression<'static>)>) -> AST<'static> {
        let mut ast = IndexMap::new();
        for (name, body) in rules {
            ast.insert(nt(name), rule(body));
        }
        ast
    }

    #[test]
    fn direct_lr_eliminated() {
        // A = A "+" "x" | "x"
        let ast = make_ast(vec![(
            "A",
            alt(vec![cat(vec![nt("A"), lit("+"), lit("x")]), lit("x")]),
        )]);

        let result = remove_direct_left_recursion(&ast);
        // Should produce A and A_tail rules.
        assert_eq!(result.len(), 2);
        assert!(result.contains_key(&nt("A_tail")));
    }

    #[test]
    fn no_lr_unchanged() {
        let ast = make_ast(vec![("A", alt(vec![lit("x"), lit("y")]))]);

        let result = remove_direct_left_recursion(&ast);
        assert_eq!(result.len(), 1);
        assert!(!result.contains_key(&nt("A_tail")));
    }

    #[test]
    fn indirect_lr_substituted() {
        // A = B "x"
        // B = A "y" | "z"
        // SCC: [A, B] — A starts with B, B starts with A (indirect cycle).
        let ast = make_ast(vec![
            ("A", cat(vec![nt("B"), lit("x")])),
            ("B", alt(vec![cat(vec![nt("A"), lit("y")]), lit("z")])),
        ]);

        // SCC order: [A, B] — when processing B (i=1), substitute A (j=0).
        let sccs = vec![vec!["A".to_string(), "B".to_string()]];
        let result = remove_indirect_left_recursion(&ast, &sccs);

        // B should now have A's body substituted for leading A reference.
        // B was: A "y" | "z"  →  (B "x") "y" | "z"  (after substituting A = B "x")
        let b_body = result.get(&nt("B")).unwrap();
        let inner = match b_body {
            Expression::Rule(inner, _) => inner.as_ref(),
            other => other,
        };

        // B was: A "y" | "z". After substituting A = (B "x"):
        //   → (B "x") "y" | "z"
        // The first branch should be a concatenation whose first element
        // is A's body (B "x"), making B directly left-recursive via B.
        if let Expression::Alternation(tok) = inner {
            assert_eq!(tok.value.len(), 2, "Expected 2 branches after substitution");
            let first = &tok.value[0];
            if let Expression::Concatenation(cat_tok) = first {
                // First elem is A's body: Concatenation([B, "x"])
                if let Expression::Concatenation(inner_cat) = &cat_tok.value[0] {
                    assert!(
                        matches!(&inner_cat.value[0], Expression::Nonterminal(t) if t.value.as_ref() == "B"),
                        "Expected nested B, got {:?}",
                        inner_cat.value[0]
                    );
                } else {
                    panic!("Expected nested concatenation, got {:?}", cat_tok.value[0]);
                }
            } else {
                panic!("Expected concatenation, got {:?}", first);
            }
        } else {
            panic!("Expected alternation after substitution, got {:?}", inner);
        }
    }

    #[test]
    fn indirect_lr_no_multi_sccs_is_noop() {
        let ast = make_ast(vec![
            ("A", alt(vec![cat(vec![nt("A"), lit("+")]), lit("x")])),
        ]);

        // No multi-member SCCs.
        let sccs: Vec<Vec<String>> = vec![];
        let result = remove_indirect_left_recursion(&ast, &sccs);
        assert_eq!(result.len(), ast.len());
    }

    #[test]
    fn substitute_single_nonterminal() {
        // expr = B, target = "B", replacement = lit("z")
        let expr = nt("B");
        let target = lit("z");
        let result = substitute_leading_nonterminal(&expr, "B", &target);
        assert_eq!(result, Some(lit("z")));
    }

    #[test]
    fn substitute_concatenation_leading() {
        // expr = B "x", target = "B", replacement = lit("z")
        let expr = cat(vec![nt("B"), lit("x")]);
        let target = lit("z");
        let result = substitute_leading_nonterminal(&expr, "B", &target);

        // Should become: "z" "x"
        match result {
            Some(Expression::Concatenation(tok)) => {
                assert_eq!(tok.value.len(), 2);
                assert!(matches!(&tok.value[0], Expression::Literal(t) if t.value.as_ref() == "z"));
                assert!(matches!(&tok.value[1], Expression::Literal(t) if t.value.as_ref() == "x"));
            }
            other => panic!("Expected concatenation, got {:?}", other),
        }
    }

    #[test]
    fn substitute_with_alternation_target() {
        // expr = B "x", target = "B", replacement = "y" | "z"
        let expr = cat(vec![nt("B"), lit("x")]);
        let target = alt(vec![lit("y"), lit("z")]);
        let result = substitute_leading_nonterminal(&expr, "B", &target);

        // Should become: ("y" "x") | ("z" "x")
        match result {
            Some(Expression::Alternation(tok)) => {
                assert_eq!(tok.value.len(), 2);
            }
            other => panic!("Expected alternation, got {:?}", other),
        }
    }

    #[test]
    fn substitute_no_match_returns_none() {
        let expr = cat(vec![nt("C"), lit("x")]);
        let target = lit("z");
        assert!(substitute_leading_nonterminal(&expr, "B", &target).is_none());
    }
}