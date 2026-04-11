use ls_types::*;

use bbnf::grammar::generated::{BbnfBootstrapNodeView, BbnfBootstrapRuleKind};
use bbnf::types::AST;

use crate::state::DocumentState;
use crate::state::ast_utils::format_value_expr_short;

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

/// Recursively render a tape-first view into formatted BBNF source.
///
/// Walks structural compounds (alternation, concatenation,
/// binary_factor, mapped_factor, factor, term_*, closure) via the
/// same shape-agnostic helpers as the rest of the analysis layer,
/// and falls back to `span_text()` for everything else — which is
/// exactly the input slice the user typed.
fn format_expression(node: BbnfBootstrapNodeView<'_>, indent_level: usize) -> String {
    use crate::state::ast_utils::{collect_binary_operand_views, is_term_kind, iter_iteration_views};

    match node.rule_kind() {
        BbnfBootstrapRuleKind::literal | BbnfBootstrapRuleKind::regex => {
            // Spans already cover the delimiters.
            node.span_text().to_string()
        }

        BbnfBootstrapRuleKind::identifier => node.span_text().to_string(),

        BbnfBootstrapRuleKind::term_0 => "epsilon".into(),

        BbnfBootstrapRuleKind::term => node
            .child(0)
            .map(|c| format_expression(c, indent_level))
            .unwrap_or_else(|| node.span_text().to_string()),

        BbnfBootstrapRuleKind::term_1 => {
            let ident = match node.child(0) {
                Some(i) => i,
                None => return node.span_text().to_string(),
            };
            let name = ident.span_text();
            if let Some(call) = node.child(1) {
                let (lo, hi) = call.span();
                if hi > lo {
                    let args: Vec<String> = call
                        .children()
                        .filter_map(|c| {
                            let (clo, chi) = c.span();
                            if chi <= clo {
                                return None;
                            }
                            let text = c.span_text();
                            if text == "(" || text == ")" || text == "," {
                                return None;
                            }
                            Some(format_expression(c, indent_level))
                        })
                        .collect();
                    return format!("{}({})", name, args.join(", "));
                }
            }
            name.to_string()
        }

        BbnfBootstrapRuleKind::term_2 | BbnfBootstrapRuleKind::value_atom_0 => {
            let open = node
                .child(0)
                .map(|c| c.span_text().to_string())
                .unwrap_or_else(|| "(".into());
            let inner_str = node
                .child(1)
                .map(|c| format_expression(c, indent_level + 1))
                .unwrap_or_default();
            match open.as_str() {
                "(" => format!("({})", inner_str),
                "[" => format!("[{}]", inner_str),
                "{" => format!("{{{}}}", inner_str),
                "@{" => format!("@{{{}}}", inner_str),
                _ => format!("({})", inner_str),
            }
        }

        BbnfBootstrapRuleKind::factor => {
            let term = node.children().find(|c| is_term_kind(c.rule_kind()));
            let modifier = node
                .children()
                .find(|c| c.rule_kind() == BbnfBootstrapRuleKind::modifier);
            let term_str = term
                .map(|t| format_expression(t, indent_level))
                .unwrap_or_else(|| node.span_text().to_string());
            if let Some(m) = modifier {
                let (lo, hi) = m.span();
                if hi > lo {
                    return format!("{}{}", term_str, m.span_text());
                }
            }
            term_str
        }

        BbnfBootstrapRuleKind::modifier => node.span_text().to_string(),

        BbnfBootstrapRuleKind::mapped_factor => {
            let inner = match node.child(0) {
                Some(c) => c,
                None => return node.span_text().to_string(),
            };
            let inner_str = format_expression(inner, indent_level);
            let mapping = node.child(1);
            let has_arrow = mapping.is_some_and(|m| {
                let (lo, hi) = m.span();
                hi > lo && m.span_text().contains("->")
            });
            if !has_arrow {
                return inner_str;
            }
            // Extract the value_expr / type annotation from the
            // mapping group's children via rule_kind dispatch —
            // mirrors `lower/expression.rs::find_value_expr_child`.
            let mapping_node = mapping.unwrap();
            let value_expr = find_value_expr_child(mapping_node);
            let type_ann = find_type_annotation_child(mapping_node);
            match (value_expr, type_ann) {
                (Some(v), Some(t)) => format!(
                    "{} -> {} : {}",
                    inner_str,
                    format_value_expr_short(v),
                    format_value_expr_short(t)
                ),
                (Some(v), None) => {
                    format!("{} -> {}", inner_str, format_value_expr_short(v))
                }
                _ => mapping_node.span_text().trim().to_string(),
            }
        }

        BbnfBootstrapRuleKind::binary_factor => {
            let operands: Vec<_> = collect_binary_operand_views(node).collect();
            if operands.is_empty() {
                node.span_text().to_string()
            } else if operands.len() == 1 {
                format_expression(operands[0], indent_level)
            } else {
                let input = node.input();
                let mut out = format_expression(operands[0], indent_level);
                let mut prev_end = operands[0].span().1;
                for op in operands.iter().skip(1) {
                    let gap = &input[prev_end as usize..op.span().0 as usize];
                    let trimmed = gap.trim();
                    out.push(' ');
                    out.push_str(trimmed);
                    out.push(' ');
                    out.push_str(&format_expression(*op, indent_level));
                    prev_end = op.span().1;
                }
                out
            }
        }

        BbnfBootstrapRuleKind::binary_operators => node.span_text().to_string(),

        BbnfBootstrapRuleKind::concatenation => {
            let formatted: Vec<String> = iter_iteration_views(node)
                .map(|c| format_expression(c, indent_level))
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

        BbnfBootstrapRuleKind::alternation => {
            let formatted: Vec<String> = iter_iteration_views(node)
                .map(|c| format_expression(c, indent_level))
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

        BbnfBootstrapRuleKind::closure => {
            // Closure source slice is already `|params| body`.
            node.span_text().trim().to_string()
        }

        BbnfBootstrapRuleKind::comment | BbnfBootstrapRuleKind::big_comment => String::new(),

        // Transparent wrappers.
        BbnfBootstrapRuleKind::rhs
        | BbnfBootstrapRuleKind::grammar_item
        | BbnfBootstrapRuleKind::directive
        | BbnfBootstrapRuleKind::lhs => node
            .child(0)
            .map(|c| format_expression(c, indent_level))
            .unwrap_or_else(|| node.span_text().to_string()),

        // Anything else: use the raw source slice.
        _ => {
            let text = node.span_text().trim();
            if text.is_empty() {
                "...".into()
            } else {
                text.to_string()
            }
        }
    }
}

/// Walk the children of a mapping group to find the value expression root.
fn find_value_expr_child<'p>(
    node: BbnfBootstrapNodeView<'p>,
) -> Option<BbnfBootstrapNodeView<'p>> {
    for c in node.children() {
        match c.rule_kind() {
            BbnfBootstrapRuleKind::value_expr
            | BbnfBootstrapRuleKind::value_or
            | BbnfBootstrapRuleKind::value_and
            | BbnfBootstrapRuleKind::value_cmp
            | BbnfBootstrapRuleKind::value_add
            | BbnfBootstrapRuleKind::value_mul
            | BbnfBootstrapRuleKind::value_unary
            | BbnfBootstrapRuleKind::value_unary_0
            | BbnfBootstrapRuleKind::value_atom
            | BbnfBootstrapRuleKind::value_atom_0
            | BbnfBootstrapRuleKind::value_fn_call
            | BbnfBootstrapRuleKind::value_path
            | BbnfBootstrapRuleKind::value_ident
            | BbnfBootstrapRuleKind::value_input
            | BbnfBootstrapRuleKind::value_closure
            | BbnfBootstrapRuleKind::int_lit
            | BbnfBootstrapRuleKind::float_lit
            | BbnfBootstrapRuleKind::bool_lit
            | BbnfBootstrapRuleKind::string_lit => return Some(c),
            _ => {
                if let Some(found) = find_value_expr_child(c) {
                    return Some(found);
                }
            }
        }
    }
    None
}

fn find_type_annotation_child<'p>(
    node: BbnfBootstrapNodeView<'p>,
) -> Option<BbnfBootstrapNodeView<'p>> {
    for c in node.children() {
        if c.rule_kind() == BbnfBootstrapRuleKind::type_annotation {
            return Some(c);
        }
        if let Some(found) = find_type_annotation_child(c) {
            return Some(found);
        }
    }
    None
}
