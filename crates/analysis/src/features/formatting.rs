use ls_types::*;

use crate::state::ast_utils::format_value_expr_short;
use bbnf::grammar::generated::BbnfBootstrapEnum;
use bbnf::types::AST;

use crate::state::DocumentState;

const MAX_WIDTH: usize = 66;

pub fn format_document(state: &DocumentState) -> Option<Vec<TextEdit>> {
    let ast = state.ast()?;

    let formatted = format_ast(ast);

    // Replace entire document.
    let end = offset_to_end(&state.text);
    Some(vec![TextEdit {
        range: Range::new(Position::new(0, 0), end),
        new_text: formatted,
    }])
}

/// Format only rules that overlap the selected range.
pub fn format_range(state: &DocumentState, range: Range) -> Option<Vec<TextEdit>> {
    let ast = state.ast()?;

    let range_start = state.line_index.position_to_offset(range.start);
    let range_end = state.line_index.position_to_offset(range.end);

    let mut edits = Vec::new();

    for (&name, entry) in ast.iter() {
        let rule_start = entry.name_span.start;
        let rule_end = crate::state::compute_expression_end_pub(entry.rhs).unwrap_or_else(|| {
            panic!(
                "format_range could not compute expression end for rule `{}`",
                name
            )
        });

        // Skip rules that don't overlap the selection.
        if rule_end <= range_start || rule_start >= range_end {
            continue;
        }

        let rhs_str = format_expression(entry.rhs, 0);
        let formatted = format!("{} = {};\n", name, rhs_str);

        // Find the full rule span including the semicolon and any trailing whitespace.
        let text_after_rule = &state.text[rule_end..];
        let extra = text_after_rule
            .find(';')
            .map(|i| i + 1)
            .unwrap_or_else(|| panic!("format_range expected `;` terminator for rule `{}`", name));
        let full_end = rule_end + extra;

        // Skip trailing whitespace/newlines after semicolon.
        let trailing = state.text[full_end..]
            .chars()
            .take_while(|c| c.is_whitespace())
            .count();
        let full_end = full_end + trailing;

        let edit_range = state.line_index.span_to_range(rule_start, full_end);
        edits.push(TextEdit {
            range: edit_range,
            new_text: formatted,
        });
    }

    if edits.is_empty() {
        None
    } else {
        Some(edits)
    }
}

/// Format the rule that was just completed (triggered by typing `;`).
pub fn format_on_type(state: &DocumentState, position: Position) -> Option<Vec<TextEdit>> {
    let offset = state.line_index.position_to_offset(position);

    // Find which rule the cursor is in.
    for rule in &state.info.rules {
        if offset >= rule.full_span.0 && offset <= rule.full_span.1 + 2 {
            // Found the rule -- format just this one by delegating to format_range.
            let rule_range = state
                .line_index
                .span_to_range(rule.full_span.0, rule.full_span.1);
            return format_range(state, rule_range);
        }
    }

    None
}

fn offset_to_end(text: &str) -> Position {
    let mut line: u32 = 0;
    let mut col: u32 = 0;
    for byte in text.bytes() {
        if byte == b'\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    Position::new(line, col)
}

fn format_ast(ast: &AST<'_>) -> String {
    let mut lines = Vec::new();
    for (&name, entry) in ast.iter() {
        let rhs_str = format_expression(entry.rhs, 0);
        let rule_line = format!("{} = {}", name, rhs_str);

        // Add terminator.
        lines.push(format!("{};", rule_line));
        lines.push(String::new()); // blank line between rules
    }

    // Remove trailing blank line.
    if lines.last().is_some_and(|l| l.is_empty()) {
        lines.pop();
    }

    lines.join("\n") + "\n"
}

fn format_expression(node: &BbnfBootstrapEnum<'_>, indent_level: usize) -> String {
    match node {
        BbnfBootstrapEnum::literal(s) => {
            let text = s.as_str();
            // The literal span includes quotes already from the bootstrap parser.
            text.to_string()
        }

        BbnfBootstrapEnum::identifier(s) => s.as_str().to_string(),

        BbnfBootstrapEnum::regex(s) => s.as_str().to_string(),

        BbnfBootstrapEnum::term_0(_) => "epsilon".into(),

        BbnfBootstrapEnum::term(inner) => format_expression(inner, indent_level),

        BbnfBootstrapEnum::term_1((ident, call_args)) => {
            let name = bbnf::grammar::host::extract_span_text(ident);
            if let Some((_open, first_arg, rest_args, _close)) = call_args {
                let mut args = vec![format_expression(first_arg, indent_level)];
                for (_comma, arg) in *rest_args {
                    args.push(format_expression(arg, indent_level));
                }
                format!("{}({})", name, args.join(", "))
            } else {
                name.to_string()
            }
        }

        BbnfBootstrapEnum::term_2((open, inner, _close)) => {
            let inner_str = format_expression(inner, indent_level + 1);
            let bracket = open.as_str();
            match bracket {
                "(" => format!("({})", inner_str),
                "[" => format!("[{}]", inner_str),
                "{" => format!("{{{}}}", inner_str),
                "@{" => format!("@{{{}}}", inner_str),
                _ => format!("({})", inner_str),
            }
        }

        BbnfBootstrapEnum::factor((_comment, term, modifier, _trailing)) => {
            let term_str = format_expression(term, indent_level);
            if let Some(m) = modifier {
                format!("{}{}", term_str, format_expression(m, indent_level))
            } else {
                term_str
            }
        }

        BbnfBootstrapEnum::modifier(s) => s.as_str().to_string(),

        BbnfBootstrapEnum::mapped_factor((inner, mapping)) => {
            let inner_str = format_expression(inner, indent_level);
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

        BbnfBootstrapEnum::binary_factor((first, rest)) => {
            if rest.is_empty() {
                format_expression(first, indent_level)
            } else {
                let mut s = format_expression(first, indent_level);
                for (op, operand) in *rest {
                    s.push_str(&format!(
                        " {} {}",
                        format_expression(op, indent_level),
                        format_expression(operand, indent_level)
                    ));
                }
                s
            }
        }

        BbnfBootstrapEnum::binary_operators(s) => s.as_str().to_string(),

        BbnfBootstrapEnum::concatenation(parts) => {
            let formatted: Vec<String> = parts
                .iter()
                .map(|(part, _comma)| format_expression(part, indent_level))
                .collect();
            let flat = formatted.join(", ");
            if flat.len() + indent_level * 4 <= MAX_WIDTH {
                flat
            } else {
                let indent = "    ".repeat(indent_level + 1);
                let sep = format!(",\n{}", indent);
                format!("\n{}{}", indent, formatted.join(&sep))
            }
        }

        BbnfBootstrapEnum::alternation(branches) => {
            let formatted: Vec<String> = branches
                .iter()
                .map(|(branch, _pipe)| format_expression(branch, indent_level))
                .collect();
            let flat = formatted.join(" | ");
            if flat.len() + indent_level * 4 <= MAX_WIDTH {
                flat
            } else {
                let indent = "    ".repeat(indent_level + 1);
                let sep = format!("\n{}| ", indent);
                format!("\n{}{}", indent, formatted.join(&sep))
            }
        }

        BbnfBootstrapEnum::closure((_pipe, first_param, rest_params, _pipe2, body)) => {
            let first_name = bbnf::grammar::host::extract_span_text(first_param);
            let mut param_names: Vec<&str> = vec![first_name];
            for (_comma, p) in *rest_params {
                if let BbnfBootstrapEnum::identifier(s) = p {
                    param_names.push(s.as_str());
                }
            }
            format!(
                "|{}| {}",
                param_names.join(", "),
                format_expression(body, indent_level),
            )
        }

        BbnfBootstrapEnum::comment(_) | BbnfBootstrapEnum::big_comment(_) => String::new(),

        // Anything else: use the span text directly if available, otherwise "..."
        _ => "...".into(),
    }
}
