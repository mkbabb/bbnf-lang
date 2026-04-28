//! AZ-II.cutover.D4 — document / range / on-type formatting over
//! the struct-direct [`BbnfView`] surface.

use ls_types::*;

use bbnf::runtime::RuntimeView;
use bbnf::runtime::bbnf::{BbnfCompoundKind, BbnfView};
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

/// Recursively render a [`BbnfView`] into formatted BBNF source.
fn format_expression(node: BbnfView<'_, '_>, indent_level: usize) -> String {
    use crate::state::ast_utils::{collect_binary_operand_views, iter_iteration_views};

    // Span leaves — the source slice IS the formatted form.
    if !node.is_compound() {
        return node
            .span_text_opt()
            .map(str::to_string)
            .unwrap_or_default();
    }

    match node.compound_kind() {
        Some(BbnfCompoundKind::Term) => match node.branch_tag() {
            Some(0) => "epsilon".into(),
            Some(1) => format_term_call(node, indent_level),
            Some(b @ 4..=7) => format_term_grouped(node, b, indent_level),
            _ => node
                .child(0)
                .map(|c| format_expression(c, indent_level))
                .unwrap_or_else(|| span_or_dots(node)),
        },

        Some(BbnfCompoundKind::Factor) => {
            let term = node
                .children()
                .find(|c| c.compound_kind() == Some(BbnfCompoundKind::Term));
            // Modifier is a non-compound Span-bearing leaf adjacent
            // to the Term.
            let modifier_text = node
                .children()
                .filter(|c| c.compound_kind().is_none())
                .find_map(|c| {
                    let t = c.span_text_opt()?.trim();
                    if matches!(t, "?" | "?w" | "*" | "+") {
                        Some(t.to_string())
                    } else {
                        None
                    }
                });
            let term_str = term
                .map(|t| format_expression(t, indent_level))
                .unwrap_or_else(|| span_or_dots(node));
            match modifier_text {
                Some(m) => format!("{}{}", term_str, m),
                None => term_str,
            }
        }

        Some(BbnfCompoundKind::MappedFactor) => {
            let inner = match node.child(0) {
                Some(c) => c,
                None => return span_or_dots(node),
            };
            let has_arrow = node.span_text_opt().is_some_and(|s| s.contains("->"));
            if !has_arrow {
                return format_expression(inner, indent_level);
            }
            let inner_str = format_expression(inner, indent_level);
            // Find value_expr / type_annotation companions among the
            // mapped_factor's children. The struct-direct alphabet
            // emits expressions.bbnf rules under BbnfCompoundKind::Other;
            // distinguish by checking is_compound() and walking depth.
            let value_expr = find_value_expr_child(node);
            let type_ann = find_type_annotation_child(node);
            match (value_expr, type_ann) {
                (Some(v), Some(t)) => format!(
                    "{} -> {} : {}",
                    inner_str,
                    format_value_expr_short(v),
                    format_value_expr_short(t)
                ),
                (Some(v), None) => format!("{} -> {}", inner_str, format_value_expr_short(v)),
                _ => node
                    .span_text_opt()
                    .map(|s| s.trim().to_string())
                    .unwrap_or_else(|| span_or_dots(node)),
            }
        }

        Some(BbnfCompoundKind::BinaryFactor) => {
            let operands: Vec<_> = collect_binary_operand_views(node).collect();
            if operands.is_empty() {
                span_or_dots(node)
            } else if operands.len() == 1 {
                format_expression(operands[0], indent_level)
            } else {
                let input = node.input();
                let mut out = format_expression(operands[0], indent_level);
                let mut prev_end = operands[0].span_range().map(|(_, hi)| hi);
                for op in operands.iter().skip(1) {
                    let op_lo = op.span_range().map(|(lo, _)| lo);
                    if let (Some(end), Some(start)) = (prev_end, op_lo) {
                        if end <= start {
                            let gap = &input[end..start];
                            let trimmed = gap.trim();
                            out.push(' ');
                            out.push_str(trimmed);
                            out.push(' ');
                        } else {
                            out.push(' ');
                        }
                    } else {
                        out.push(' ');
                    }
                    out.push_str(&format_expression(*op, indent_level));
                    prev_end = op.span_range().map(|(_, hi)| hi);
                }
                out
            }
        }

        Some(BbnfCompoundKind::Concatenation) => {
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

        Some(BbnfCompoundKind::Alternation) => {
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

        Some(BbnfCompoundKind::Closure) => node
            .span_text_opt()
            .map(|s| s.trim().to_string())
            .unwrap_or_else(|| span_or_dots(node)),

        // Transparent wrappers — descend.
        Some(BbnfCompoundKind::Rhs)
        | Some(BbnfCompoundKind::GrammarItem)
        | Some(BbnfCompoundKind::Directive)
        | Some(BbnfCompoundKind::Lhs) => node
            .child(0)
            .map(|c| format_expression(c, indent_level))
            .unwrap_or_else(|| span_or_dots(node)),

        // CallArg: render the substantive expression.
        Some(BbnfCompoundKind::CallArg) => node
            .span_text_opt()
            .map(|s| s.trim().to_string())
            .unwrap_or_else(|| span_or_dots(node)),

        // Everything else: use the recovered source slice.
        _ => span_or_dots(node),
    }
}

fn span_or_dots(node: BbnfView<'_, '_>) -> String {
    let text = node.span_text_opt().unwrap_or("").trim();
    if text.is_empty() {
        "...".into()
    } else {
        text.to_string()
    }
}

/// Format `term_1` (identifier with optional call-args).
fn format_term_call(node: BbnfView<'_, '_>, indent_level: usize) -> String {
    let ident = match node.child(0) {
        Some(c) => c,
        None => return span_or_dots(node),
    };
    let name = ident.span_text_opt().unwrap_or("").to_string();
    let args: Vec<String> = node
        .children()
        .filter(|c| c.compound_kind() == Some(BbnfCompoundKind::CallArg))
        .map(|c| format_expression(c, indent_level))
        .filter(|s| !s.trim().is_empty())
        .collect();
    if args.is_empty() {
        name
    } else {
        format!("{}({})", name, args.join(", "))
    }
}

/// Format a grouped term — `( rhs )` / `[ rhs ]` / `{ rhs }` /
/// `@{ rhs }`. The `branch` argument is the term's branch_tag (4..=7).
fn format_term_grouped(node: BbnfView<'_, '_>, branch: u32, indent_level: usize) -> String {
    let inner_view = node
        .children()
        .find(|c| c.compound_kind() == Some(BbnfCompoundKind::Rhs));
    let inner = inner_view
        .map(|i| format_expression(i, indent_level + 1))
        .unwrap_or_else(|| span_or_dots(node));
    match branch {
        4 => format!("@{{{}}}", inner),
        5 => format!("({})", inner),
        6 => format!("[{}]", inner),
        7 => format!("{{{}}}", inner),
        _ => format!("({})", inner),
    }
}

/// Walk the children of a mapped_factor to find the value-expression
/// root. The struct-direct alphabet collapses every imported
/// `expressions.bbnf` rule onto [`BbnfCompoundKind::Other`]; the
/// value-expression sits as the first non-Factor compound child of
/// the mapped_factor.
fn find_value_expr_child<'a, 'p>(node: BbnfView<'a, 'p>) -> Option<BbnfView<'a, 'p>> {
    for c in node.children() {
        match c.compound_kind() {
            Some(BbnfCompoundKind::Factor) => continue,
            Some(BbnfCompoundKind::Other) => return Some(c),
            Some(_) => {
                if let Some(found) = find_value_expr_child(c) {
                    return Some(found);
                }
            }
            None => {
                // Non-compound child (Span / numeric leaf): recurse
                // through the parent's structural children only.
            }
        }
    }
    None
}

/// Locate the type_annotation child of a mapped_factor. The
/// type_annotation rule lowers to BbnfCompoundKind::Other; we
/// disambiguate by matching the trailing `:` token in the source
/// gap before it.
fn find_type_annotation_child<'a, 'p>(node: BbnfView<'a, 'p>) -> Option<BbnfView<'a, 'p>> {
    // Walk children to find one whose leading source byte is `:`.
    let input = node.input();
    let mut prev_end: Option<usize> = None;
    for c in node.children() {
        let span = c.span_range()?;
        if let Some(end) = prev_end {
            if end < span.0 {
                let gap = input[end..span.0].trim_start();
                if gap.starts_with(':') {
                    return Some(c);
                }
            }
        }
        prev_end = Some(span.1);
        if let Some(found) = find_type_annotation_child(c) {
            return Some(found);
        }
    }
    None
}
