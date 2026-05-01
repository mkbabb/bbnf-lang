//! AZ-II.cutover.D4 — selection-range computation over the
//! struct-direct [`BbnfView`] surface.

use ls_types::*;

use bbnf::runtime::RuntimeView;
use bbnf::runtime::bbnf::{BbnfCompoundKind, BbnfView};

use crate::state::DocumentState;
use crate::state::ast_utils::{
    collect_binary_operand_views, compute_expression_end, iter_iteration_views,
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
fn collect_spans(node: BbnfView<'_, '_>, offset: usize, spans: &mut Vec<(usize, usize)>) {
    let span = node.span_range();
    let contains = span.is_some_and(|(lo, hi)| offset >= lo && offset <= hi);

    // Span-leaf focus: push and return.
    if !node.is_compound() {
        if contains {
            if let Some((lo, hi)) = span {
                spans.push((lo, hi));
            }
        }
        return;
    }

    match node.compound_kind() {
        // Alternation.
        Some(BbnfCompoundKind::Alternation) => {
            if contains {
                if let Some((lo, hi)) = span {
                    spans.push((lo, hi));
                }
            }
            for branch in iter_iteration_views(node) {
                collect_spans(branch, offset, spans);
            }
        }

        // Concatenation.
        Some(BbnfCompoundKind::Concatenation) => {
            if contains {
                if let Some((lo, hi)) = span {
                    spans.push((lo, hi));
                }
            }
            for part in iter_iteration_views(node) {
                collect_spans(part, offset, spans);
            }
        }

        // Binary factor.
        Some(BbnfCompoundKind::BinaryFactor) => {
            if contains {
                if let Some((lo, hi)) = span {
                    spans.push((lo, hi));
                }
            }
            for operand in collect_binary_operand_views(node) {
                collect_spans(operand, offset, spans);
            }
        }

        // Mapped factor: inner + optional mapping.
        Some(BbnfCompoundKind::MappedFactor) => {
            if contains {
                if let Some((lo, hi)) = span {
                    spans.push((lo, hi));
                }
            }
            if let Some(inner) = node.child(0) {
                collect_spans(inner, offset, spans);
            }
        }

        // Factor.
        Some(BbnfCompoundKind::Factor) => {
            if contains {
                if let Some((lo, hi)) = span {
                    spans.push((lo, hi));
                }
            }
            for c in node.children() {
                collect_spans(c, offset, spans);
            }
        }

        // Term: dispatch on branch_tag.
        Some(BbnfCompoundKind::Term) => match node.branch_tag() {
            Some(b @ 4..=7) => {
                let _ = b;
                // Grouped: span is meaningful for the wrapping
                // brackets.
                if contains {
                    if let Some((lo, hi)) = span {
                        spans.push((lo, hi));
                    }
                }
                if let Some(inner) = node
                    .children()
                    .find(|c| c.compound_kind() == Some(BbnfCompoundKind::Rhs))
                {
                    collect_spans(inner, offset, spans);
                }
            }
            Some(1) => {
                // Identifier with optional call-args.
                if let Some(ident) = node.child(0) {
                    collect_spans(ident, offset, spans);
                }
                for c in node.children().skip(1) {
                    collect_spans(c, offset, spans);
                }
            }
            _ => {
                if let Some(inner) = node.child(0) {
                    collect_spans(inner, offset, spans);
                }
            }
        },

        // Closure: recurse into body.
        Some(BbnfCompoundKind::Closure) => {
            if contains {
                if let Some((lo, hi)) = span {
                    spans.push((lo, hi));
                }
            }
            if let Some(body) = node
                .children()
                .find(|c| c.compound_kind() == Some(BbnfCompoundKind::Rhs))
            {
                collect_spans(body, offset, spans);
            }
        }

        // Transparent wrappers.
        Some(BbnfCompoundKind::Rhs)
        | Some(BbnfCompoundKind::GrammarItem)
        | Some(BbnfCompoundKind::Directive)
        | Some(BbnfCompoundKind::Lhs) => {
            if let Some(inner) = node.child(0) {
                collect_spans(inner, offset, spans);
            }
        }

        // Everything else — no spans to contribute.
        _ => {}
    }
}
