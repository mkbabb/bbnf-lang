use tower_lsp_server::ls_types::*;

use bbnf::types::{Expression, Token, AST};

/// Extract the inner value from a TokenExpression.
fn get_inner_expression<'a, T>(tok: &'a Token<'a, T>) -> &'a T {
    &tok.value
}

use crate::analysis::{position_to_offset, span_to_range};
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

    let range_start = position_to_offset(&state.text, range.start);
    let range_end = position_to_offset(&state.text, range.end);

    let mut edits = Vec::new();

    for (lhs, rhs) in ast.iter() {
        if let Expression::Nonterminal(Token { value: name, span: name_span, .. }) = lhs {
            let rule_start = name_span.start;
            let rule_end = crate::state::compute_expression_end_pub(rhs)
                .unwrap_or(name_span.end);

            // Skip rules that don't overlap the selection.
            if rule_end < range_start || rule_start > range_end {
                continue;
            }

            let rhs_str = format_expression(rhs, 0);
            let formatted = format!("{} = {};\n", name, rhs_str);

            // Find the full rule span including the semicolon and any trailing whitespace.
            let text_after_rule = &state.text[rule_end..];
            let extra = text_after_rule
                .find(';')
                .map(|i| i + 1)
                .unwrap_or(0);
            let full_end = rule_end + extra;

            // Skip trailing whitespace/newlines after semicolon.
            let trailing = state.text[full_end..]
                .chars()
                .take_while(|c| c.is_whitespace())
                .count();
            let full_end = full_end + trailing;

            let edit_range = span_to_range(&state.text, rule_start, full_end);
            edits.push(TextEdit {
                range: edit_range,
                new_text: formatted,
            });
        }
    }

    if edits.is_empty() {
        None
    } else {
        Some(edits)
    }
}

/// Format the rule that was just completed (triggered by typing `;`).
pub fn format_on_type(state: &DocumentState, position: Position) -> Option<Vec<TextEdit>> {
    let offset = position_to_offset(&state.text, position);

    // Find which rule the cursor is in.
    for rule in &state.info.rules {
        if offset >= rule.full_span.0 && offset <= rule.full_span.1 + 2 {
            // Found the rule — format just this one by delegating to format_range.
            let rule_range = span_to_range(&state.text, rule.full_span.0, rule.full_span.1);
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
    for (lhs, rhs) in ast.iter() {
        if let Expression::Nonterminal(Token { value: name, .. }) = lhs {
            let rhs_str = format_expression(rhs, 0);
            let rule_line = format!("{} = {}", name, rhs_str);

            // Add terminator.
            lines.push(format!("{};", rule_line));
            lines.push(String::new()); // blank line between rules
        }
    }

    // Remove trailing blank line.
    if lines.last().is_some_and(|l| l.is_empty()) {
        lines.pop();
    }

    lines.join("\n") + "\n"
}

fn format_expression(expr: &Expression<'_>, indent_level: usize) -> String {
    match expr {
        Expression::Literal(tok) => {
            let s = &tok.value;
            if s.contains('"') && !s.contains('\'') {
                format!("'{}'", s)
            } else {
                format!("\"{}\"", s)
            }
        }
        Expression::Nonterminal(tok) => tok.value.to_string(),
        Expression::Regex(tok) => format!("/{}/", tok.value),
        Expression::Epsilon(_) => "epsilon".into(),
        Expression::Group(inner) => {
            let inner_str = format_expression(get_inner_expression(inner), indent_level + 1);
            format!("({})", inner_str)
        }
        Expression::Optional(inner) => {
            let inner_str = format_expression(get_inner_expression(inner), indent_level + 1);
            format!("[{}]", inner_str)
        }
        Expression::Many(inner) => {
            let inner_str = format_expression(get_inner_expression(inner), indent_level + 1);
            format!("{{{}}}", inner_str)
        }
        Expression::Many1(inner) => {
            let inner_str = format_expression(get_inner_expression(inner), indent_level);
            format!("{}+", inner_str)
        }
        Expression::OptionalWhitespace(inner) => {
            let inner_str = format_expression(get_inner_expression(inner), indent_level);
            format!("{}?w", inner_str)
        }
        Expression::Skip(l, r) => {
            format!(
                "{} << {}",
                format_expression(get_inner_expression(l), indent_level),
                format_expression(get_inner_expression(r), indent_level),
            )
        }
        Expression::Next(l, r) => {
            format!(
                "{} >> {}",
                format_expression(get_inner_expression(l), indent_level),
                format_expression(get_inner_expression(r), indent_level),
            )
        }
        Expression::Minus(l, r) => {
            format!(
                "{} - {}",
                format_expression(get_inner_expression(l), indent_level),
                format_expression(get_inner_expression(r), indent_level),
            )
        }
        Expression::Concatenation(inner) => {
            let parts: Vec<String> = get_inner_expression(inner)
                .iter()
                .map(|e| format_expression(e, indent_level))
                .collect();
            let flat = parts.join(", ");
            if flat.len() + indent_level * 4 <= MAX_WIDTH {
                flat
            } else {
                let indent = "    ".repeat(indent_level + 1);
                let sep = format!(",\n{}", indent);
                format!("\n{}{}", indent, parts.join(&sep))
            }
        }
        Expression::Alternation(inner) => {
            let parts: Vec<String> = get_inner_expression(inner)
                .iter()
                .map(|e| format_expression(e, indent_level))
                .collect();
            let flat = parts.join(" | ");
            if flat.len() + indent_level * 4 <= MAX_WIDTH {
                flat
            } else {
                let indent = "    ".repeat(indent_level + 1);
                let sep = format!("\n{}| ", indent);
                format!("\n{}{}", indent, parts.join(&sep))
            }
        }
        Expression::Rule(rhs, mapping) => {
            let rhs_str = format_expression(rhs, indent_level);
            if let Some(m) = mapping {
                format!("{} {}", rhs_str, format_expression(m, indent_level))
            } else {
                rhs_str
            }
        }
        Expression::ProductionRule(lhs, rhs) => {
            format!(
                "{} = {}",
                format_expression(lhs, indent_level),
                format_expression(rhs, indent_level),
            )
        }
        Expression::MappedExpression((expr_tok, mapping_tok)) => {
            format!(
                "{} {}",
                format_expression(get_inner_expression(expr_tok), indent_level),
                format_expression(get_inner_expression(mapping_tok), indent_level),
            )
        }
        Expression::DebugExpression((expr_tok, label)) => {
            format!(
                "{}#{}",
                format_expression(get_inner_expression(expr_tok), indent_level),
                label,
            )
        }
        Expression::MappingFn(tok) => format!("=> {}", tok.value),
    }
}
