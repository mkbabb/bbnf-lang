use bbnf::grammar::generated::BbnfBootstrapEnum;

use super::super::types::ReferenceInfo;

/// Recursively collect nonterminal references from a bootstrap AST node.
pub fn collect_references(node: &BbnfBootstrapEnum<'_>, refs: &mut Vec<ReferenceInfo>) {
    match node {
        // Leaf: identifier reference
        BbnfBootstrapEnum::identifier(s) => {
            refs.push(ReferenceInfo {
                name: s.as_str().to_string(),
                span: (s.start, s.end),
            });
        }

        // term_1: identifier + optional call args
        BbnfBootstrapEnum::term_1((ident, call_args)) => {
            let name = bbnf::grammar::generated::BbnfBootstrapEnum::span_text(ident);
            let ident_span = match ident {
                BbnfBootstrapEnum::identifier(s) => (s.start, s.end),
                _ => (0, 0),
            };
            refs.push(ReferenceInfo {
                name: name.to_string(),
                span: ident_span,
            });
            if let Some((_open, first_arg, rest_args, _close)) = call_args {
                collect_references(first_arg, refs);
                for (_comma, arg) in *rest_args {
                    collect_references(arg, refs);
                }
            }
        }

        // Structural: alternation, concatenation
        BbnfBootstrapEnum::alternation(branches) => {
            for (branch, _pipe) in *branches {
                collect_references(branch, refs);
            }
        }
        BbnfBootstrapEnum::concatenation(parts) => {
            for (part, _comma) in *parts {
                collect_references(part, refs);
            }
        }

        // Binary factor: first + [(op, operand)]
        BbnfBootstrapEnum::binary_factor((first, rest)) => {
            collect_references(first, refs);
            for (_, operand) in *rest {
                collect_references(operand, refs);
            }
        }

        // Mapped factor: inner + optional mapping
        BbnfBootstrapEnum::mapped_factor((inner, _mapping)) => {
            collect_references(inner, refs);
        }

        // Factor: (comment?, term, modifier?, comment?)
        BbnfBootstrapEnum::factor((_, term, _, _)) => {
            collect_references(term, refs);
        }

        // Term variants
        BbnfBootstrapEnum::term(inner) => {
            collect_references(inner, refs);
        }
        BbnfBootstrapEnum::term_2((_open, inner, _close)) => {
            collect_references(inner, refs);
        }

        // Closure: |params| body
        BbnfBootstrapEnum::closure((_pipe, _first_param, _params, _pipe2, body)) => {
            collect_references(body, refs);
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
        BbnfBootstrapEnum::grammar(_)
        | BbnfBootstrapEnum::rule(_)
        | BbnfBootstrapEnum::directive(_) => {}

        // Phantom + catch-all
        _ => {}
    }
}
