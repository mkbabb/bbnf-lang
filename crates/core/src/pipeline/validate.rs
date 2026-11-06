use std::collections::HashSet;

use crate::graph::deps::collect_nonterminal_refs;
use crate::pipeline::CompileError;
use crate::types::AST;

pub(crate) fn validate_ir_alt_width(ir: &bbnf_ir::GrammarIR) -> Result<(), CompileError> {
    for rule in &ir.rules {
        let name = ir.get_string(rule.name);
        validate_node_alt_width(&rule.body, name, bbnf_ir::AltBranchId::MAX_COUNT)?;
        if let Some(recover) = &rule.meta.directives.recover {
            validate_node_alt_width(recover, name, bbnf_ir::AltBranchId::MAX_COUNT)?;
        }
    }
    Ok(())
}

pub(crate) fn validate_node_alt_width(
    node: &bbnf_ir::IrNode,
    rule: &str,
    max: usize,
) -> Result<(), CompileError> {
    if let bbnf_ir::IrNode::Alt(branches, _) = node {
        let count = branches.len();
        if count > max {
            return Err(CompileError::TooManyAlternatives {
                rule: rule.to_string(),
                count,
                max,
            });
        }
    }

    let mut result = Ok(());
    node.for_each_child(&mut |child| {
        if result.is_ok() {
            result = validate_node_alt_width(child, rule, max);
        }
    });
    result
}

pub(crate) fn validate_pretty_directives<'a>(
    ast: &AST<'a>,
    pretties: Option<&std::collections::HashMap<String, Vec<String>>>,
) -> Result<(), CompileError> {
    let Some(pretties) = pretties else {
        return Ok(());
    };

    let defined_rules: HashSet<&str> = ast.keys().copied().collect();

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

        if !defined_rules.contains(rule.as_str()) {
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
    extra_known: &HashSet<&str>,
) -> Result<(), CompileError> {
    if !validate_unknown_nonterminals {
        return Ok(());
    }

    let defined_rules: HashSet<&str> = ast.keys().copied().collect();

    for (&rule_name, entry) in ast {
        let mut refs = indexmap::IndexSet::new();
        collect_nonterminal_refs(entry.rhs, &mut refs);

        for &referenced in &refs {
            if !defined_rules.contains(referenced) && !extra_known.contains(referenced) {
                return Err(CompileError::UnknownNonterminal {
                    rule: rule_name.to_string(),
                    name: referenced.to_string(),
                });
            }
        }
    }

    Ok(())
}
