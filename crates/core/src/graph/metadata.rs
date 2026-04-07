//! Alias detection for AST-level diagnostics.

use std::collections::{HashMap, HashSet};

use crate::grammar::generated::BbnfBootstrapEnum;
use crate::types::AST;

/// Find rules whose RHS is simply a reference to another nonterminal.
pub fn find_aliases<'a>(
    ast: &'a AST<'a>,
    cyclic_rules: &HashSet<&'a str>,
) -> HashMap<&'a str, &'a str> {
    let mut aliases = HashMap::new();

    for (&name, entry) in ast {
        if cyclic_rules.contains(name) {
            continue;
        }

        if let Some(target) = extract_alias_target(entry.rhs) {
            if ast.contains_key(target) {
                aliases.insert(name, target);
            }
        }
    }

    aliases
}

/// Extract the target nonterminal name if the expression is a simple alias (possibly grouped).
fn extract_alias_target<'a>(node: &'a BbnfBootstrapEnum<'a>) -> Option<&'a str> {
    match node {
        // Direct identifier reference
        BbnfBootstrapEnum::identifier(s) => Some(s.as_str()),

        // term_1 without call args = plain nonterminal reference
        BbnfBootstrapEnum::term_1((ident, None)) => {
            Some(crate::grammar::host::extract_span_text(ident))
        }

        // Unwrap structural wrappers
        BbnfBootstrapEnum::term(inner)
        | BbnfBootstrapEnum::factor((None, inner, None, None))
        | BbnfBootstrapEnum::mapped_factor((inner, None)) => extract_alias_target(inner),

        // Grouped expression: (rhs)
        BbnfBootstrapEnum::term_2((open, inner, _close)) if open.as_str() == "(" => {
            extract_alias_target(inner)
        }

        // Single-element containers
        BbnfBootstrapEnum::alternation(branches) if branches.len() == 1 => {
            extract_alias_target(branches[0].0)
        }
        BbnfBootstrapEnum::concatenation(parts) if parts.len() == 1 => {
            extract_alias_target(parts[0].0)
        }
        BbnfBootstrapEnum::binary_factor((first, rest)) if rest.is_empty() => {
            extract_alias_target(first)
        }

        _ => None,
    }
}
