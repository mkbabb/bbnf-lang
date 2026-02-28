//! Reference counting, alias detection, transparent alternations, and span-eligible rules.

use std::collections::{HashMap, HashSet};

use crate::types::{Expression, AST};
use super::deps::Dependencies;
use super::first_sets::unwrap_rule;

/// Count how many times each nonterminal appears as a dependency of other rules.
pub fn compute_ref_counts<'a>(deps: &'a Dependencies<'a>) -> HashMap<&'a Expression<'a>, usize> {
    let mut counts: HashMap<&'a Expression<'a>, usize> = HashMap::new();

    for lhs in deps.keys() {
        counts.entry(lhs).or_insert(0);
    }

    for sub_deps in deps.values() {
        for dep in sub_deps {
            if let Some((key, _)) = deps.get_key_value(dep) {
                *counts.entry(key).or_insert(0) += 1;
            }
        }
    }

    counts
}

/// Find rules whose RHS is simply a reference to another nonterminal.
pub fn find_aliases<'a>(
    ast: &'a AST<'a>,
    cyclic_rules: &HashSet<Expression<'a>>,
) -> HashMap<&'a Expression<'a>, &'a Expression<'a>> {
    let mut aliases = HashMap::new();

    for (lhs, rhs) in ast {
        if cyclic_rules.contains(lhs) {
            continue;
        }

        let inner = unwrap_rule(rhs);
        if let Some(Expression::Nonterminal(ref_token)) = extract_alias_target(inner) {
            let target_name: &str = &ref_token.value;
            for (key, _) in ast {
                if let Expression::Nonterminal(k_token) = key {
                    if k_token.value.as_ref() == target_name {
                        aliases.insert(lhs, key);
                        break;
                    }
                }
            }
        }
    }

    aliases
}

fn extract_alias_target<'a>(expr: &'a Expression<'a>) -> Option<&'a Expression<'a>> {
    match expr {
        Expression::Nonterminal(_) => Some(expr),
        Expression::Group(inner) => extract_alias_target(&inner.value),
        _ => None,
    }
}

/// Find rules that are pure alternations of nonterminals.
pub fn find_transparent_alternations<'a>(
    ast: &'a AST<'a>,
    cyclic_rules: &HashSet<Expression<'a>>,
) -> HashSet<String> {
    let mut transparent = HashSet::new();

    for (lhs, rhs) in ast {
        let name = match lhs {
            Expression::Nonterminal(token) => token.value.as_ref(),
            _ => continue,
        };

        if !cyclic_rules.contains(lhs) {
            continue;
        }

        let inner = unwrap_rule(rhs);

        let branches = match inner {
            Expression::Alternation(token) => &token.value,
            _ => continue,
        };

        let all_simple = branches.iter().all(|branch| {
            matches!(branch, Expression::Nonterminal(_))
        });

        if all_simple {
            transparent.insert(name.to_string());
        }
    }

    transparent
}

fn expr_is_span_eligible<'a>(
    expr: &'a Expression<'a>,
    cyclic_rules: &HashSet<Expression<'a>>,
    ast: &'a AST<'a>,
) -> bool {
    match expr {
        Expression::Literal(_) | Expression::Regex(_) | Expression::Epsilon(_) => true,
        Expression::Group(inner)
        | Expression::Optional(inner)
        | Expression::OptionalWhitespace(inner)
        | Expression::Many(inner)
        | Expression::Many1(inner) => {
            expr_is_span_eligible(&inner.value, cyclic_rules, ast)
        }
        Expression::Skip(left, right) | Expression::Next(left, right) => {
            expr_is_span_eligible(&left.value, cyclic_rules, ast)
                && expr_is_span_eligible(&right.value, cyclic_rules, ast)
        }
        Expression::Concatenation(inner) => inner
            .value
            .iter()
            .all(|e| expr_is_span_eligible(e, cyclic_rules, ast)),
        Expression::Alternation(inner) => inner
            .value
            .iter()
            .all(|e| expr_is_span_eligible(e, cyclic_rules, ast)),
        Expression::Nonterminal(token) => {
            let target_name: &str = &token.value;
            if cyclic_rules.contains(expr) {
                return false;
            }
            let target_rhs = ast.iter().find_map(|(k, v)| {
                if let Expression::Nonterminal(t) = k {
                    if t.value.as_ref() == target_name {
                        Some(v)
                    } else {
                        None
                    }
                } else {
                    None
                }
            });
            match target_rhs {
                Some(rhs) => expr_is_span_eligible(unwrap_rule(rhs), cyclic_rules, ast),
                None => false,
            }
        }
        Expression::Rule(inner, _) => expr_is_span_eligible(inner, cyclic_rules, ast),
        _ => false,
    }
}

/// Find rules whose entire body can be expressed as a SpanParser.
pub fn find_span_eligible_rules<'a>(
    ast: &'a AST<'a>,
    cyclic_rules: &HashSet<Expression<'a>>,
) -> HashSet<String> {
    let mut eligible = HashSet::new();

    for (lhs, rhs) in ast {
        let name = match lhs {
            Expression::Nonterminal(token) => token.value.as_ref(),
            _ => continue,
        };

        if cyclic_rules.contains(lhs) {
            continue;
        }

        let inner = unwrap_rule(rhs);
        if expr_is_span_eligible(inner, cyclic_rules, ast) {
            eligible.insert(name.to_string());
        }
    }

    eligible
}
