use std::collections::HashSet;

use crate::pipeline::CompileError;
use crate::types::{AST, Expression, Token};

pub(crate) fn validate_pretty_directives<'a>(
    ast: &AST<'a>,
    pretties: Option<&std::collections::HashMap<String, Vec<String>>>,
) -> Result<(), CompileError> {
    let Some(pretties) = pretties else {
        return Ok(());
    };

    let defined_rules: HashSet<String> = ast
        .keys()
        .filter_map(|lhs| match lhs {
            Expression::Nonterminal(token) => Some(token.value.to_string()),
            _ => None,
        })
        .collect();

    for (rule, hints) in pretties {
        if rule == "*" {
            for hint in hints {
                if !matches!(hint.as_str(), "auto" | "minimal" | "off") {
                    return Err(CompileError::UnknownPrettyHint {
                        rule: rule.clone(),
                        hint: hint.clone(),
                    });
                }
            }
            continue;
        }

        if !defined_rules.contains(rule) {
            return Err(CompileError::UndefinedPrettyRule { rule: rule.clone() });
        }

        for hint in hints {
            if !is_valid_pretty_hint(hint) {
                return Err(CompileError::UnknownPrettyHint {
                    rule: rule.clone(),
                    hint: hint.clone(),
                });
            }
        }
    }

    Ok(())
}

fn is_valid_pretty_hint(hint: &str) -> bool {
    matches!(
        hint,
        "group"
            | "indent"
            | "dedent"
            | "block"
            | "blankline"
            | "nobreak"
            | "softbreak"
            | "hardbreak"
            | "compact"
            | "fast"
            | "off"
    ) || bbnf_ir::parse_sep_hint(hint).is_some()
        || bbnf_ir::parse_split_hint(hint).is_some()
}

pub(crate) fn validate_ast<'a>(
    ast: &AST<'a>,
    validate_unknown_nonterminals: bool,
) -> Result<(), CompileError> {
    let defined_rules: HashSet<&str> = ast
        .keys()
        .filter_map(|lhs| match lhs {
            Expression::Nonterminal(token) => Some(token.value.as_ref()),
            _ => None,
        })
        .collect();

    for (lhs, rhs) in ast {
        let rule_name = match lhs {
            Expression::Nonterminal(Token { value, .. }) => value.as_ref(),
            _ => continue,
        };
        validate_expr(
            rule_name,
            rhs,
            &defined_rules,
            validate_unknown_nonterminals,
        )?;
    }

    Ok(())
}

fn validate_expr(
    rule_name: &str,
    expr: &Expression<'_>,
    defined_rules: &HashSet<&str>,
    validate_unknown_nonterminals: bool,
) -> Result<(), CompileError> {
    match expr {
        Expression::Literal(_) | Expression::Regex(_) | Expression::Epsilon(_) => Ok(()),
        Expression::Nonterminal(token) => {
            if !validate_unknown_nonterminals || defined_rules.contains(token.value.as_ref()) {
                Ok(())
            } else {
                Err(CompileError::UnknownNonterminal {
                    rule: rule_name.to_string(),
                    name: token.value.to_string(),
                })
            }
        }
        Expression::MappedExpression(inner, _arrow) => {
            validate_expr(
                rule_name,
                &inner.value,
                defined_rules,
                validate_unknown_nonterminals,
            )
        }
        Expression::DebugExpression((inner, _))
        | Expression::Group(inner)
        | Expression::Optional(inner)
        | Expression::OptionalWhitespace(inner)
        | Expression::SpanCapture(inner)
        | Expression::Many(inner)
        | Expression::Many1(inner) => validate_expr(
            rule_name,
            &inner.value,
            defined_rules,
            validate_unknown_nonterminals,
        ),
        Expression::Skip(left, right)
        | Expression::Next(left, right)
        | Expression::Minus(left, right) => {
            validate_expr(
                rule_name,
                &left.value,
                defined_rules,
                validate_unknown_nonterminals,
            )?;
            validate_expr(
                rule_name,
                &right.value,
                defined_rules,
                validate_unknown_nonterminals,
            )
        }
        Expression::Concatenation(token) | Expression::Alternation(token) => {
            token.value.iter().try_for_each(|child| {
                validate_expr(
                    rule_name,
                    child,
                    defined_rules,
                    validate_unknown_nonterminals,
                )
            })
        }
        Expression::Rule(inner, _arrow) => {
            validate_expr(
                rule_name,
                inner,
                defined_rules,
                validate_unknown_nonterminals,
            )
        }
        Expression::Closure(_params, body) => {
            validate_expr(
                rule_name,
                &body.value,
                defined_rules,
                validate_unknown_nonterminals,
            )
        }
        Expression::GrammarCall(name_tok, args) => {
            // Validate the called name exists (treat like a nonterminal reference).
            if validate_unknown_nonterminals && !defined_rules.contains(name_tok.value.as_ref()) {
                return Err(CompileError::UnknownNonterminal {
                    rule: rule_name.to_string(),
                    name: name_tok.value.to_string(),
                });
            }
            // Validate each argument expression.
            args.iter().try_for_each(|arg| {
                validate_expr(
                    rule_name,
                    arg,
                    defined_rules,
                    validate_unknown_nonterminals,
                )
            })
        }
        Expression::ProductionRule(_, _) => Err(CompileError::InvalidProductionRule {
            rule: rule_name.to_string(),
        }),
    }
}
