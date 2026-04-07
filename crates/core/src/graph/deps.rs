//! Dependency graph construction from grammar AST.

use std::collections::{HashMap, HashSet};

use crate::grammar::generated::BbnfBootstrapEnum;
use crate::types::AST;

/// Rule name → set of referenced rule names.
pub type Dependencies<'a> = HashMap<&'a str, HashSet<&'a str>>;

/// Build a dependency graph from the grammar AST.
pub fn calculate_ast_deps<'a>(ast: &AST<'a>) -> Dependencies<'a> {
    let mut deps = HashMap::new();
    for (&name, entry) in ast.iter() {
        let mut refs = HashSet::new();
        collect_nonterminal_refs(entry.rhs, &mut refs);
        deps.insert(name, refs);
    }
    deps
}

/// Recursively collect nonterminal (identifier) references from a bootstrap AST node.
pub fn collect_nonterminal_refs<'a>(
    node: &'a BbnfBootstrapEnum<'a>,
    refs: &mut HashSet<&'a str>,
) {
    match node {
        // Leaf: identifier reference (nonterminal)
        BbnfBootstrapEnum::identifier(s) => {
            refs.insert(s.as_str());
        }

        // Structural: alternation, concatenation
        BbnfBootstrapEnum::alternation(branches) => {
            for (branch, _pipe) in *branches {
                collect_nonterminal_refs(branch, refs);
            }
        }
        BbnfBootstrapEnum::concatenation(parts) => {
            for (part, _comma) in *parts {
                collect_nonterminal_refs(part, refs);
            }
        }

        // Binary factor: first + [(op, operand)]
        BbnfBootstrapEnum::binary_factor((first, rest)) => {
            collect_nonterminal_refs(first, refs);
            for (_, operand) in *rest {
                collect_nonterminal_refs(operand, refs);
            }
        }

        // Mapped factor: inner + optional mapping
        BbnfBootstrapEnum::mapped_factor((inner, _mapping)) => {
            collect_nonterminal_refs(inner, refs);
        }

        // Factor: (comment?, term, modifier?, comment?)
        BbnfBootstrapEnum::factor((_, term, _, _)) => {
            collect_nonterminal_refs(term, refs);
        }

        // Term variants
        BbnfBootstrapEnum::term(inner) => {
            collect_nonterminal_refs(inner, refs);
        }
        BbnfBootstrapEnum::term_1((ident, call_args)) => {
            refs.insert(ident.as_str());
            if let Some((_open, first_arg, rest_args, _close)) = call_args {
                collect_nonterminal_refs(first_arg, refs);
                for (_comma, arg) in *rest_args {
                    collect_nonterminal_refs(arg, refs);
                }
            }
        }
        BbnfBootstrapEnum::term_2((_open, inner, _close))
        | BbnfBootstrapEnum::value_atom_0((_open, inner, _close)) => {
            collect_nonterminal_refs(inner, refs);
        }

        // RHS: closure | alternation
        BbnfBootstrapEnum::closure((_pipe, _params, _pipe2, body)) => {
            collect_nonterminal_refs(body, refs);
        }

        // Transparent wrappers
        BbnfBootstrapEnum::directive(inner) => {
            collect_nonterminal_refs(inner, refs);
        }

        // Terminals: literal, regex, modifier, epsilon, comments — no refs
        BbnfBootstrapEnum::literal(_)
        | BbnfBootstrapEnum::regex(_)
        | BbnfBootstrapEnum::modifier(_)
        | BbnfBootstrapEnum::term_0(_)
        | BbnfBootstrapEnum::comment(_)
        | BbnfBootstrapEnum::big_comment(_)
        | BbnfBootstrapEnum::binary_operators(_) => {}

        // Value expression variants — no grammar nonterminal refs
        BbnfBootstrapEnum::value_or(_)
        | BbnfBootstrapEnum::value_and(_)
        | BbnfBootstrapEnum::value_cmp(_)
        | BbnfBootstrapEnum::value_add(_)
        | BbnfBootstrapEnum::value_mul(_)
        | BbnfBootstrapEnum::value_unary(_)
        | BbnfBootstrapEnum::value_unary_0(_)
        | BbnfBootstrapEnum::value_atom(_)
        | BbnfBootstrapEnum::value_atom_0(_)
        | BbnfBootstrapEnum::value_input(_)
        | BbnfBootstrapEnum::value_fn_call(_)
        | BbnfBootstrapEnum::value_closure(_)
        | BbnfBootstrapEnum::int_lit(_)
        | BbnfBootstrapEnum::float_lit(_)
        | BbnfBootstrapEnum::bool_lit(_)
        | BbnfBootstrapEnum::string_lit(_)
        | BbnfBootstrapEnum::value_ident(_)
        | BbnfBootstrapEnum::type_annotation(_)
        | BbnfBootstrapEnum::type_name(_)
        | BbnfBootstrapEnum::cmp_op(_)
        | BbnfBootstrapEnum::mul_op(_)
        | BbnfBootstrapEnum::add_op(_) => {}

        // Directive variants — no grammar nonterminal refs
        BbnfBootstrapEnum::import_directive(_)
        | BbnfBootstrapEnum::import_directive_0(_)
        | BbnfBootstrapEnum::import_path(_)
        | BbnfBootstrapEnum::import_items(_)
        | BbnfBootstrapEnum::recover_directive(_)
        | BbnfBootstrapEnum::pretty_directive(_)
        | BbnfBootstrapEnum::pretty_directive_0(_)
        | BbnfBootstrapEnum::ws_directive(_)
        | BbnfBootstrapEnum::token_directive(_)
        | BbnfBootstrapEnum::debug_directive(_)
        | BbnfBootstrapEnum::debug_directive_0(_)
        | BbnfBootstrapEnum::host_directive(_) => {}

        // Grammar/rule level — should not appear inside rule RHS
        BbnfBootstrapEnum::grammar(_) | BbnfBootstrapEnum::rule(_) => {}

        // Phantom
        BbnfBootstrapEnum::__Phantom(_) => {}

        // Catch any remaining generated sub-variants
        _ => {}
    }
}
