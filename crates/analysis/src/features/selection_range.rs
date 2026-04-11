use ls_types::*;

use bbnf::grammar::generated::{BbnfBootstrapNodeView, BbnfBootstrapRuleKind};

use crate::state::DocumentState;
use crate::state::ast_utils::{
    collect_binary_operand_views, compute_expression_end, is_term_kind, iter_iteration_views,
};

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
        let rule_end = compute_expression_end(entry.rhs).unwrap_or_else(|| {
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
///
/// Under the tape-first view API every rule compound carries its
/// own `(lo, hi)` span on the cursor, so the collection is just a
/// shape-agnostic pre-order walk that pushes `(lo, hi)` when the
/// offset lands inside and descends into structural children
/// relevant to the grammar hierarchy.
fn collect_spans(
    node: BbnfBootstrapNodeView<'_>,
    offset: usize,
    spans: &mut Vec<(usize, usize)>,
) {
    let (lo, hi) = node.span();
    let lo = lo as usize;
    let hi = hi as usize;
    let contains = offset >= lo && offset <= hi;

    match node.rule_kind() {
        // Span leaves — push on contain.
        BbnfBootstrapRuleKind::identifier
        | BbnfBootstrapRuleKind::literal
        | BbnfBootstrapRuleKind::regex
        | BbnfBootstrapRuleKind::modifier
        | BbnfBootstrapRuleKind::binary_operators
        | BbnfBootstrapRuleKind::comment
        | BbnfBootstrapRuleKind::big_comment
        | BbnfBootstrapRuleKind::term_0
        | BbnfBootstrapRuleKind::int_lit
        | BbnfBootstrapRuleKind::float_lit
        | BbnfBootstrapRuleKind::bool_lit
        | BbnfBootstrapRuleKind::string_lit
        | BbnfBootstrapRuleKind::value_ident => {
            if contains {
                spans.push((lo, hi));
            }
        }

        // Alternation.
        BbnfBootstrapRuleKind::alternation => {
            if contains {
                spans.push((lo, hi));
            }
            for branch in iter_iteration_views(node) {
                collect_spans(branch, offset, spans);
            }
        }

        // Concatenation.
        BbnfBootstrapRuleKind::concatenation => {
            if contains {
                spans.push((lo, hi));
            }
            for part in iter_iteration_views(node) {
                collect_spans(part, offset, spans);
            }
        }

        // Binary factor.
        BbnfBootstrapRuleKind::binary_factor => {
            if contains {
                spans.push((lo, hi));
            }
            for operand in collect_binary_operand_views(node) {
                collect_spans(operand, offset, spans);
            }
        }

        // Mapped factor: inner + optional mapping.
        BbnfBootstrapRuleKind::mapped_factor => {
            if contains {
                spans.push((lo, hi));
            }
            if let Some(inner) = node.child(0) {
                collect_spans(inner, offset, spans);
            }
        }

        // Factor.
        BbnfBootstrapRuleKind::factor => {
            if contains {
                spans.push((lo, hi));
            }
            for c in node.children() {
                if is_term_kind(c.rule_kind())
                    || c.rule_kind() == BbnfBootstrapRuleKind::modifier
                {
                    collect_spans(c, offset, spans);
                }
            }
        }

        // term: transparent wrapper.
        BbnfBootstrapRuleKind::term => {
            if let Some(inner) = node.child(0) {
                collect_spans(inner, offset, spans);
            }
        }

        // term_1: identifier + optional call args.
        BbnfBootstrapRuleKind::term_1 => {
            if let Some(ident) = node.child(0) {
                collect_spans(ident, offset, spans);
            }
            for c in node.children().skip(1) {
                collect_spans(c, offset, spans);
            }
        }

        // term_2 / value_atom_0: grouped expression.
        BbnfBootstrapRuleKind::term_2 | BbnfBootstrapRuleKind::value_atom_0 => {
            if contains {
                spans.push((lo, hi));
            }
            if let Some(inner) = node.child(1) {
                collect_spans(inner, offset, spans);
            }
        }

        // Closure: recurse into body.
        BbnfBootstrapRuleKind::closure => {
            if contains {
                spans.push((lo, hi));
            }
            if let Some(body) = node.child(4) {
                collect_spans(body, offset, spans);
            }
        }

        // Transparent wrappers.
        BbnfBootstrapRuleKind::rhs
        | BbnfBootstrapRuleKind::grammar_item
        | BbnfBootstrapRuleKind::directive
        | BbnfBootstrapRuleKind::lhs => {
            if let Some(inner) = node.child(0) {
                collect_spans(inner, offset, spans);
            }
        }

        // Everything else — no spans to contribute.
        _ => {}
    }
}
