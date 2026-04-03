//! Reference counting and alias detection for AST-level diagnostics.

use std::collections::{HashMap, HashSet};

use super::deps::Dependencies;
use super::first_sets::unwrap_rule;
use crate::types::{AST, Expression};

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
