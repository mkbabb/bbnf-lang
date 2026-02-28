use tower_lsp_server::ls_types::*;

use bbnf::types::{Expression, Token};

use crate::analysis::{position_to_offset, span_to_range};
use crate::state::DocumentState;

/// Extract the inner value from a TokenExpression.
fn get_inner_expression<'a, T>(tok: &'a Token<'a, T>) -> &'a T {
    &tok.value
}

/// Compute selection ranges for each requested position.
///
/// Builds a chain of nested ranges from innermost (token) to outermost (full rule),
/// enabling "Expand/Shrink Selection" in the editor.
pub fn selection_ranges(state: &DocumentState, positions: Vec<Position>) -> Vec<SelectionRange> {
    // Use the cached AST (no re-parsing needed).
    let Some(ast) = state.ast() else {
        return positions.iter().map(|p| trivial_range(*p)).collect();
    };

    positions
        .iter()
        .map(|&pos| {
            let offset = position_to_offset(&state.text, pos);
            compute_selection_range(&state.text, ast, offset)
                .unwrap_or_else(|| trivial_range(pos))
        })
        .collect()
}

fn trivial_range(pos: Position) -> SelectionRange {
    SelectionRange {
        range: Range::new(pos, pos),
        parent: None,
    }
}

/// Walk the AST to find all spans containing the offset, ordered innermost-first.
fn compute_selection_range(
    text: &str,
    ast: &bbnf::types::AST<'_>,
    offset: usize,
) -> Option<SelectionRange> {
    // Find which rule contains this offset.
    for (lhs, rhs) in ast.iter() {
        if let Expression::Nonterminal(Token { span: name_span, .. }) = lhs {
            let rule_start = name_span.start;
            let rule_end = crate::state::compute_expression_end_pub(rhs).unwrap_or(name_span.end);

            if offset < rule_start || offset > rule_end {
                continue;
            }

            // Collect nested spans from the RHS expression tree.
            let mut spans = Vec::new();
            collect_spans(rhs, offset, &mut spans);

            // Add the full rule span as the outermost.
            spans.push((rule_start, rule_end));

            // Sort spans innermost-first (smallest to largest).
            spans.sort_by_key(|(start, end)| *end - *start);
            spans.dedup();

            // Build the chain from innermost to outermost.
            let mut result: Option<SelectionRange> = None;
            for (start, end) in spans.into_iter().rev() {
                let range = span_to_range(text, start, end);
                result = Some(SelectionRange {
                    range,
                    parent: result.map(Box::new),
                });
            }

            return result;
        }
    }
    None
}

/// Recursively collect all expression spans that contain the given offset.
fn collect_spans(expr: &Expression<'_>, offset: usize, spans: &mut Vec<(usize, usize)>) {
    match expr {
        Expression::Literal(tok) | Expression::Nonterminal(tok) | Expression::Regex(tok) => {
            if offset >= tok.span.start && offset <= tok.span.end {
                spans.push((tok.span.start, tok.span.end));
            }
        }
        Expression::Epsilon(tok) => {
            if offset >= tok.span.start && offset <= tok.span.end {
                spans.push((tok.span.start, tok.span.end));
            }
        }
        Expression::Alternation(inner) | Expression::Concatenation(inner) => {
            if offset >= inner.span.start && offset <= inner.span.end {
                spans.push((inner.span.start, inner.span.end));
            }
            for child in get_inner_expression(inner) {
                collect_spans(child, offset, spans);
            }
        }
        Expression::Group(inner)
        | Expression::Optional(inner)
        | Expression::Many(inner)
        | Expression::Many1(inner)
        | Expression::OptionalWhitespace(inner) => {
            if offset >= inner.span.start && offset <= inner.span.end {
                spans.push((inner.span.start, inner.span.end));
            }
            collect_spans(get_inner_expression(inner), offset, spans);
        }
        Expression::Skip(l, r) | Expression::Next(l, r) | Expression::Minus(l, r) => {
            // Use the combined span of both operands.
            let start = l.span.start;
            let end = r.span.end;
            if offset >= start && offset <= end {
                spans.push((start, end));
            }
            collect_spans(get_inner_expression(l), offset, spans);
            collect_spans(get_inner_expression(r), offset, spans);
        }
        Expression::Rule(rhs, _) => {
            collect_spans(rhs, offset, spans);
        }
        Expression::MappedExpression((expr_tok, _)) => {
            collect_spans(get_inner_expression(expr_tok), offset, spans);
        }
        Expression::DebugExpression((expr_tok, _)) => {
            collect_spans(get_inner_expression(expr_tok), offset, spans);
        }
        _ => {}
    }
}
