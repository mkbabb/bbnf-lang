use bbnf::grammar::generated::BbnfBootstrapEnum;

/// Quick one-line formatting of a bootstrap AST node for hover text.
pub fn format_expression_short(node: &BbnfBootstrapEnum<'_>) -> String {
    match node {
        BbnfBootstrapEnum::identifier(s) => s.as_str().to_string(),
        BbnfBootstrapEnum::literal(s) => s.as_str().to_string(),
        BbnfBootstrapEnum::regex(s) => s.as_str().to_string(),
        BbnfBootstrapEnum::term_0(_) => "\u{03b5}".into(),

        BbnfBootstrapEnum::alternation(branches) => branches
            .iter()
            .map(|(branch, _)| format_expression_short(branch))
            .collect::<Vec<_>>()
            .join(" | "),

        BbnfBootstrapEnum::concatenation(parts) => parts
            .iter()
            .map(|(part, _)| format_expression_short(part))
            .collect::<Vec<_>>()
            .join(", "),

        BbnfBootstrapEnum::binary_factor((first, rest)) => {
            if rest.is_empty() {
                format_expression_short(first)
            } else {
                let mut s = format_expression_short(first);
                for (op, operand) in *rest {
                    s.push_str(&format!(
                        " {} {}",
                        format_expression_short(op),
                        format_expression_short(operand)
                    ));
                }
                s
            }
        }

        BbnfBootstrapEnum::mapped_factor((inner, mapping)) => {
            let inner_str = format_expression_short(inner);
            if let Some((_arrow, (value_expr, type_ann))) = mapping {
                let val_str = format_value_expr_short(value_expr);
                if let Some(ta) = type_ann {
                    format!(
                        "{} -> {} : {}",
                        inner_str,
                        val_str,
                        format_value_expr_short(ta)
                    )
                } else {
                    format!("{} -> {}", inner_str, val_str)
                }
            } else {
                inner_str
            }
        }

        BbnfBootstrapEnum::factor((_comment, term, modifier, _trailing)) => {
            let term_str = format_expression_short(term);
            if let Some(m) = modifier {
                format!("{}{}", term_str, format_expression_short(m))
            } else {
                term_str
            }
        }

        BbnfBootstrapEnum::term(inner) => format_expression_short(inner),

        BbnfBootstrapEnum::term_1((ident, call_args)) => {
            let name = bbnf::grammar::generated::BbnfBootstrapEnum::span_text(ident);
            if let Some((_open, first_arg, rest_args, _close)) = call_args {
                let mut args = vec![format_expression_short(first_arg)];
                for (_comma, arg) in *rest_args {
                    args.push(format_expression_short(arg));
                }
                format!("{}({})", name, args.join(", "))
            } else {
                name.to_string()
            }
        }

        BbnfBootstrapEnum::term_2((open, inner, _close)) => {
            let inner_str = format_expression_short(inner);
            let bracket = open.as_str();
            match bracket {
                "(" => format!("({})", inner_str),
                "[" => format!("[{}]", inner_str),
                "{" => format!("{{{}}}", inner_str),
                _ => format!("({})", inner_str),
            }
        }

        BbnfBootstrapEnum::modifier(s) => s.as_str().to_string(),
        BbnfBootstrapEnum::binary_operators(s) => s.as_str().to_string(),

        BbnfBootstrapEnum::closure((_pipe, first_param, rest_params, _pipe2, body)) => {
            let first_name = bbnf::grammar::generated::BbnfBootstrapEnum::span_text(first_param);
            let mut param_names: Vec<&str> = vec![first_name];
            for (_comma, p) in *rest_params {
                if let BbnfBootstrapEnum::identifier(s) = p {
                    param_names.push(s.as_str());
                }
            }
            format!(
                "|{}| {}",
                param_names.join(", "),
                format_expression_short(body)
            )
        }

        // Value expression formatting
        BbnfBootstrapEnum::int_lit(s)
        | BbnfBootstrapEnum::float_lit(s)
        | BbnfBootstrapEnum::bool_lit(s)
        | BbnfBootstrapEnum::string_lit(s)
        | BbnfBootstrapEnum::value_ident(s) => s.as_str().to_string(),

        BbnfBootstrapEnum::comment(_) | BbnfBootstrapEnum::big_comment(_) => String::new(),

        _ => "...".into(),
    }
}

/// Quick formatting of a value expression for hover text.
pub fn format_value_expr_short(node: &BbnfBootstrapEnum<'_>) -> String {
    match node {
        BbnfBootstrapEnum::int_lit(s)
        | BbnfBootstrapEnum::float_lit(s)
        | BbnfBootstrapEnum::bool_lit(s)
        | BbnfBootstrapEnum::string_lit(s)
        | BbnfBootstrapEnum::value_ident(s) => s.as_str().to_string(),

        BbnfBootstrapEnum::value_input((ident, props)) => {
            if props.is_empty() {
                ident.as_str().to_string()
            } else {
                let mut s = ident.as_str().to_string();
                for (_dot, prop) in *props {
                    s.push('.');
                    s.push_str(&format_value_expr_short(prop));
                }
                s
            }
        }

        BbnfBootstrapEnum::value_fn_call((name, _open, args, _close)) => {
            let name_str = bbnf::grammar::generated::BbnfBootstrapEnum::span_text(name);
            if let Some((first, rest)) = args {
                let mut arg_strs = vec![format_value_expr_short(first)];
                for (_comma, arg) in *rest {
                    arg_strs.push(format_value_expr_short(arg));
                }
                format!("{}({})", name_str, arg_strs.join(", "))
            } else {
                format!("{}()", name_str)
            }
        }

        BbnfBootstrapEnum::value_or((first, rest))
        | BbnfBootstrapEnum::value_and((first, rest)) => {
            if rest.is_empty() {
                format_value_expr_short(first)
            } else {
                let mut s = format_value_expr_short(first);
                for (op, operand) in *rest {
                    s.push_str(&format!(
                        " {} {}",
                        op.as_str(),
                        format_value_expr_short(operand)
                    ));
                }
                s
            }
        }

        BbnfBootstrapEnum::value_cmp((first, rest))
        | BbnfBootstrapEnum::value_add((first, rest))
        | BbnfBootstrapEnum::value_mul((first, rest)) => {
            if rest.is_empty() {
                format_value_expr_short(first)
            } else {
                let mut s = format_value_expr_short(first);
                for (op, operand) in *rest {
                    s.push_str(&format!(
                        " {} {}",
                        format_value_expr_short(op),
                        format_value_expr_short(operand)
                    ));
                }
                s
            }
        }

        BbnfBootstrapEnum::value_unary(inner) | BbnfBootstrapEnum::value_atom(inner) => {
            format_value_expr_short(inner)
        }

        BbnfBootstrapEnum::value_unary_0((op, inner)) => {
            format!("{}{}", op.as_str(), format_value_expr_short(inner))
        }

        BbnfBootstrapEnum::value_atom_0((_open, inner, _close)) => {
            format!("({})", format_value_expr_short(inner))
        }

        BbnfBootstrapEnum::value_closure((_pipe, first_param, rest_params, _pipe2, body)) => {
            let first_name = bbnf::grammar::generated::BbnfBootstrapEnum::span_text(first_param);
            let mut param_names: Vec<&str> = vec![first_name];
            for (_comma, p) in *rest_params {
                if let BbnfBootstrapEnum::value_ident(s) = p {
                    param_names.push(s.as_str());
                } else if let BbnfBootstrapEnum::identifier(s) = p {
                    param_names.push(s.as_str());
                }
            }
            format!(
                "|{}| {}",
                param_names.join(", "),
                format_value_expr_short(body)
            )
        }

        BbnfBootstrapEnum::type_annotation((_colon, ty)) => {
            format!(": {}", format_value_expr_short(ty))
        }
        BbnfBootstrapEnum::type_name(s) => s.as_str().to_string(),

        BbnfBootstrapEnum::cmp_op(s)
        | BbnfBootstrapEnum::mul_op(s)
        | BbnfBootstrapEnum::add_op(s) => s.as_str().to_string(),

        _ => "...".into(),
    }
}
