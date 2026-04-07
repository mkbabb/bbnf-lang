use ls_types::*;

use bbnf::grammar::generated::BbnfBootstrapEnum;

use crate::state::DocumentState;

/// Compute selection ranges for each requested position.
///
/// Builds a chain of nested ranges from innermost (token) to outermost (full rule),
/// enabling "Expand/Shrink Selection" in the editor.
pub fn selection_ranges(state: &DocumentState, positions: Vec<Position>) -> Vec<SelectionRange> {
    // Use the cached AST (no re-parsing needed).
    let ast = state
        .ast()
        .unwrap_or_else(|| panic!("selection_ranges requested for document with no parsed AST"));

    positions
        .iter()
        .map(|&pos| {
            let offset = state.line_index.position_to_offset(pos);
            compute_selection_range(&state.line_index, ast, offset).unwrap_or_else(|| {
                panic!(
                    "selection_ranges could not resolve a span chain for position {}:{}",
                    pos.line, pos.character
                )
            })
        })
        .collect()
}

/// Walk the AST to find all spans containing the offset, ordered innermost-first.
fn compute_selection_range(
    line_index: &crate::analysis::LineIndex,
    ast: &bbnf::types::AST<'_>,
    offset: usize,
) -> Option<SelectionRange> {
    // Find which rule contains this offset.
    for (_name, entry) in ast.iter() {
        let name_span = &entry.name_span;
        let rule_start = name_span.start;
        let rule_end = crate::state::compute_expression_end_pub(entry.rhs).unwrap_or_else(|| {
            panic!(
                "compute_selection_range could not compute expression end for rule at {}",
                name_span.start
            )
        });

        if offset < rule_start || offset > rule_end {
            continue;
        }

        // Collect nested spans from the RHS expression tree.
        let mut spans = Vec::new();
        collect_spans(entry.rhs, offset, &mut spans);

        // Add the full rule span as the outermost.
        spans.push((rule_start, rule_end));

        // Sort spans innermost-first (smallest to largest).
        spans.sort_by_key(|(start, end)| *end - *start);
        spans.dedup();

        // Build the chain from innermost to outermost.
        let mut result: Option<SelectionRange> = None;
        for (start, end) in spans.into_iter().rev() {
            let range = line_index.span_to_range(start, end);
            result = Some(SelectionRange {
                range,
                parent: result.map(Box::new),
            });
        }

        return result;
    }
    None
}

/// Recursively collect all expression spans that contain the given offset.
fn collect_spans(node: &BbnfBootstrapEnum<'_>, offset: usize, spans: &mut Vec<(usize, usize)>) {
    match node {
        // Span leaves
        BbnfBootstrapEnum::identifier(s)
        | BbnfBootstrapEnum::literal(s)
        | BbnfBootstrapEnum::regex(s)
        | BbnfBootstrapEnum::modifier(s)
        | BbnfBootstrapEnum::binary_operators(s)
        | BbnfBootstrapEnum::comment(s)
        | BbnfBootstrapEnum::big_comment(s) => {
            if offset >= s.start && offset <= s.end {
                spans.push((s.start, s.end));
            }
        }

        BbnfBootstrapEnum::term_0(s) => {
            if offset >= s.start && offset <= s.end {
                spans.push((s.start, s.end));
            }
        }

        // Alternation
        BbnfBootstrapEnum::alternation(branches) => {
            // Compute overall span from first to last branch.
            if let (Some(first), Some(last)) = (branches.first(), branches.last()) {
                if let (Some(first_start), Some(last_end)) =
                    (span_start(first.0), span_end_or_pipe(last))
                {
                    if offset >= first_start && offset <= last_end {
                        spans.push((first_start, last_end));
                    }
                }
            }
            for (branch, _pipe) in *branches {
                collect_spans(branch, offset, spans);
            }
        }

        // Concatenation
        BbnfBootstrapEnum::concatenation(parts) => {
            if let (Some(first), Some(last)) = (parts.first(), parts.last()) {
                if let (Some(first_start), Some(last_end)) =
                    (span_start(first.0), span_end_or_comma(last))
                {
                    if offset >= first_start && offset <= last_end {
                        spans.push((first_start, last_end));
                    }
                }
            }
            for (part, _comma) in *parts {
                collect_spans(part, offset, spans);
            }
        }

        // Binary factor
        BbnfBootstrapEnum::binary_factor((first, rest)) => {
            collect_spans(first, offset, spans);
            for (op, operand) in *rest {
                collect_spans(op, offset, spans);
                collect_spans(operand, offset, spans);
            }
        }

        // Mapped factor
        BbnfBootstrapEnum::mapped_factor((inner, _mapping)) => {
            collect_spans(inner, offset, spans);
        }

        // Factor
        BbnfBootstrapEnum::factor((_, term, modifier, _)) => {
            collect_spans(term, offset, spans);
            if let Some(m) = modifier {
                collect_spans(m, offset, spans);
            }
        }

        // Term
        BbnfBootstrapEnum::term(inner) => {
            collect_spans(inner, offset, spans);
        }

        // term_1: identifier + optional call
        BbnfBootstrapEnum::term_1((ident, call_args)) => {
            if let BbnfBootstrapEnum::identifier(s) = ident {
                if offset >= s.start && offset <= s.end {
                    spans.push((s.start, s.end));
                }
            }
            if let Some((_open, first_arg, rest_args, _close)) = call_args {
                collect_spans(first_arg, offset, spans);
                for (_comma, arg) in *rest_args {
                    collect_spans(arg, offset, spans);
                }
            }
        }

        // term_2: grouped
        BbnfBootstrapEnum::term_2((open, inner, close)) => {
            let start = open.start;
            let end = close.end;
            if offset >= start && offset <= end {
                spans.push((start, end));
            }
            collect_spans(inner, offset, spans);
        }

        // Closure
        BbnfBootstrapEnum::closure((_pipe, _first_param, _params, _pipe2, body)) => {
            collect_spans(body, offset, spans);
        }

        // Value expression leaves
        BbnfBootstrapEnum::int_lit(s)
        | BbnfBootstrapEnum::float_lit(s)
        | BbnfBootstrapEnum::bool_lit(s)
        | BbnfBootstrapEnum::string_lit(s)
        | BbnfBootstrapEnum::value_ident(s) => {
            if offset >= s.start && offset <= s.end {
                spans.push((s.start, s.end));
            }
        }

        // Everything else — no spans to contribute
        _ => {}
    }
}

/// Get the start offset of a node (best-effort).
fn span_start(node: &BbnfBootstrapEnum<'_>) -> Option<usize> {
    match node {
        BbnfBootstrapEnum::identifier(s)
        | BbnfBootstrapEnum::literal(s)
        | BbnfBootstrapEnum::regex(s)
        | BbnfBootstrapEnum::term_0(s)
        | BbnfBootstrapEnum::modifier(s)
        | BbnfBootstrapEnum::binary_operators(s)
        | BbnfBootstrapEnum::comment(s)
        | BbnfBootstrapEnum::big_comment(s) => Some(s.start),

        BbnfBootstrapEnum::term(inner) => span_start(inner),
        BbnfBootstrapEnum::factor((_, term, _, _)) => span_start(term),
        BbnfBootstrapEnum::mapped_factor((inner, _)) => span_start(inner),
        BbnfBootstrapEnum::binary_factor((first, _)) => span_start(first),
        BbnfBootstrapEnum::term_1((ident, _)) => span_start(ident),
        BbnfBootstrapEnum::term_2((open, _, _)) => Some(open.start),

        BbnfBootstrapEnum::alternation(branches) => {
            branches.first().and_then(|(b, _)| span_start(b))
        }
        BbnfBootstrapEnum::concatenation(parts) => parts.first().and_then(|(p, _)| span_start(p)),

        _ => None,
    }
}

/// Get the end offset for an alternation element (branch, pipe).
fn span_end_or_pipe(elem: &(&BbnfBootstrapEnum<'_>, bbnf::Span<'_>)) -> Option<usize> {
    let (branch, pipe) = elem;
    if pipe.end > pipe.start {
        Some(pipe.end)
    } else {
        crate::state::ast_utils::compute_expression_end(branch)
    }
}

/// Get the end offset for a concatenation element (part, comma).
fn span_end_or_comma(elem: &(&BbnfBootstrapEnum<'_>, bbnf::Span<'_>)) -> Option<usize> {
    let (part, comma) = elem;
    if comma.end > comma.start {
        Some(comma.end)
    } else {
        crate::state::ast_utils::compute_expression_end(part)
    }
}
