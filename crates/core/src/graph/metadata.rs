//! Alias detection for AST-level diagnostics.

use std::collections::{HashMap, HashSet};

use crate::types::{AST, Expression};

fn unwrap_rule<'a>(expr: &'a Expression<'a>) -> &'a Expression<'a> {
    match expr {
        Expression::Rule(inner, _) => inner,
        other => other,
    }
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
