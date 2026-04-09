use bbnf::grammar::generated::BbnfBootstrapEnum;

/// Public wrapper for `compute_expression_end` (used by selection_range).
pub fn compute_expression_end_pub(node: &BbnfBootstrapEnum<'_>) -> Option<usize> {
    compute_expression_end(node)
}

/// Compute the end byte offset of a bootstrap AST node.
pub fn compute_expression_end(node: &BbnfBootstrapEnum<'_>) -> Option<usize> {
    match node {
        // Span leaves
        BbnfBootstrapEnum::identifier(s)
        | BbnfBootstrapEnum::literal(s)
        | BbnfBootstrapEnum::regex(s)
        | BbnfBootstrapEnum::term_0(s)
        | BbnfBootstrapEnum::modifier(s)
        | BbnfBootstrapEnum::binary_operators(s)
        | BbnfBootstrapEnum::comment(s)
        | BbnfBootstrapEnum::big_comment(s) => Some(s.end),

        // Alternation: end of last branch's pipe separator
        BbnfBootstrapEnum::alternation(branches) => {
            branches.last().map(|(branch, pipe)| {
                // Use the pipe separator end if it's non-empty, otherwise the branch end
                if pipe.end > pipe.start {
                    pipe.end
                } else {
                    compute_expression_end(branch).unwrap_or(pipe.end)
                }
            })
        }

        // Concatenation: end of last part's comma separator
        BbnfBootstrapEnum::concatenation(parts) => parts.last().map(|(part, comma)| {
            if comma.end > comma.start {
                comma.end
            } else {
                compute_expression_end(part).unwrap_or(comma.end)
            }
        }),

        // Binary factor: end of last operand, or first if no rest
        BbnfBootstrapEnum::binary_factor((first, rest)) => {
            if let Some((_, last_operand)) = rest.last() {
                compute_expression_end(last_operand)
            } else {
                compute_expression_end(first)
            }
        }

        // Mapped factor: end of mapping if present, else inner
        BbnfBootstrapEnum::mapped_factor((inner, mapping)) => {
            if let Some((_arrow, (value_expr, type_ann))) = mapping {
                if let Some(ta) = type_ann {
                    compute_expression_end(ta)
                } else {
                    compute_expression_end(value_expr)
                }
            } else {
                compute_expression_end(inner)
            }
        }

        // Factor: rightmost non-None component
        BbnfBootstrapEnum::factor((_, term, modifier, trailing_comment)) => {
            if let Some(tc) = trailing_comment {
                compute_expression_end(tc)
            } else if let Some(m) = modifier {
                compute_expression_end(m)
            } else {
                compute_expression_end(term)
            }
        }

        // Term: delegate to inner
        BbnfBootstrapEnum::term(inner) => compute_expression_end(inner),

        // term_1: identifier + optional call
        BbnfBootstrapEnum::term_1((ident, call_args)) => {
            if let Some((_open, _first, _rest, close)) = call_args {
                Some(close.end)
            } else {
                compute_expression_end(ident)
            }
        }

        // term_2: grouped (open, inner, close)
        BbnfBootstrapEnum::term_2((_open, _inner, close)) => Some(close.end),

        // Closure: end of body
        BbnfBootstrapEnum::closure((_pipe, _first_param, _params, _pipe2, body)) => compute_expression_end(body),

        // Value expression leaves
        BbnfBootstrapEnum::int_lit(s)
        | BbnfBootstrapEnum::float_lit(s)
        | BbnfBootstrapEnum::bool_lit(s)
        | BbnfBootstrapEnum::string_lit(s)
        | BbnfBootstrapEnum::value_ident(s) => Some(s.end),

        // Value expression compounds — recurse to find end
        BbnfBootstrapEnum::value_or((first, rest))
        | BbnfBootstrapEnum::value_and((first, rest)) => {
            if let Some((_, last)) = rest.last() {
                compute_expression_end(last)
            } else {
                compute_expression_end(first)
            }
        }
        BbnfBootstrapEnum::value_cmp((first, rest))
        | BbnfBootstrapEnum::value_add((first, rest))
        | BbnfBootstrapEnum::value_mul((first, rest)) => {
            if let Some((_, last)) = rest.last() {
                compute_expression_end(last)
            } else {
                compute_expression_end(first)
            }
        }
        BbnfBootstrapEnum::value_unary(inner) | BbnfBootstrapEnum::value_atom(inner) => {
            compute_expression_end(inner)
        }
        BbnfBootstrapEnum::value_unary_0((_op, inner)) => compute_expression_end(inner),
        BbnfBootstrapEnum::value_atom_0((_open, _inner, close)) => Some(close.end),
        BbnfBootstrapEnum::value_fn_call((_name, _open, _args, close)) => Some(close.end),
        BbnfBootstrapEnum::value_input((ident, props)) => {
            if let Some((_, last)) = props.last() {
                compute_expression_end(last)
            } else {
                Some(ident.end)
            }
        }
        BbnfBootstrapEnum::value_closure((_pipe, _first_param, _params, _pipe2, body)) => {
            compute_expression_end(body)
        }
        BbnfBootstrapEnum::type_annotation((_colon, ty)) => compute_expression_end(ty),
        BbnfBootstrapEnum::type_name(s) => Some(s.end),

        // Directives, grammar-level — shouldn't appear in RHS but handle gracefully
        _ => None,
    }
}
