//! Short-form expression / value-expression formatting for hover text.
//!
//! AZ-II.cutover.D4 — walks a struct-direct [`BbnfView`] and produces
//! a compact human-readable string. Structural cases (alternation,
//! concatenation, binary_factor, mapped_factor, factor, term,
//! closure) reconstruct the grammar surface syntax explicitly so the
//! hover output remains stable regardless of the optimizer's
//! wrapper-elision decisions. Everything else falls through to the
//! view's [`BbnfView::span_text`], which is the source slice the
//! rule body covered (recovered via pointer arithmetic on descendant
//! Span leaves).

use bbnf::runtime::RuntimeView;
use bbnf::runtime::bbnf::{BbnfCompoundKind, BbnfView};

/// Quick one-line formatting of a [`BbnfView`] for hover text.
pub fn format_expression_short(node: BbnfView<'_, '_>) -> String {
    // Span leaves — the source slice IS the formatted form.
    if !node.is_compound() {
        return node.span_text_opt().map(str::to_string).unwrap_or_default();
    }

    match node.compound_kind() {
        Some(BbnfCompoundKind::Term) => match node.branch_tag() {
            Some(0) => "\u{03b5}".into(), // epsilon
            Some(1) => format_term_call(node),
            Some(b @ 4..=7) => format_term_grouped(node, b),
            _ => node
                .child(0)
                .map(format_expression_short)
                .unwrap_or_else(|| span_or_ellipsis(node)),
        },

        Some(BbnfCompoundKind::Alternation) => super::iter_iteration_views(node)
            .map(format_expression_short)
            .collect::<Vec<_>>()
            .join(" | "),

        Some(BbnfCompoundKind::Concatenation) => super::iter_iteration_views(node)
            .map(format_expression_short)
            .collect::<Vec<_>>()
            .join(", "),

        Some(BbnfCompoundKind::BinaryFactor) => {
            // Concatenate operand text separated by the source-recovered
            // operator gap. The exact operator stays faithful via
            // gap-text lookup on the parent's input slice between
            // operand byte spans.
            let operands: Vec<_> = super::collect_binary_operand_views(node).collect();
            if operands.is_empty() {
                span_or_ellipsis(node)
            } else if operands.len() == 1 {
                format_expression_short(operands[0])
            } else {
                let input = node.input();
                let mut out = format_expression_short(operands[0]);
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
                    out.push_str(&format_expression_short(*op));
                    prev_end = op.span_range().map(|(_, hi)| hi);
                }
                out
            }
        }

        Some(BbnfCompoundKind::MappedFactor) => {
            let inner = match node.child(0) {
                Some(c) => c,
                None => return span_or_ellipsis(node),
            };
            // Heuristic for mapping-presence: the source slice the
            // compound covers contains "->" between the inner factor's
            // end and the compound's end.
            let has_arrow = node
                .span_text_opt()
                .is_some_and(|s| s.contains("->"));
            if has_arrow {
                return node
                    .span_text_opt()
                    .map(|s| s.trim().to_string())
                    .unwrap_or_else(|| span_or_ellipsis(node));
            }
            format_expression_short(inner)
        }

        Some(BbnfCompoundKind::Factor) => {
            let term = node
                .children()
                .find(|c| c.compound_kind() == Some(BbnfCompoundKind::Term));
            // The modifier child is a non-compound Span-bearing leaf
            // adjacent to the Term in the factor's child list.
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
                .map(format_expression_short)
                .unwrap_or_else(|| span_or_ellipsis(node));
            match modifier_text {
                Some(m) => format!("{}{}", term_str, m),
                None => term_str,
            }
        }

        Some(BbnfCompoundKind::Closure) => {
            // closure source slice is already `|params| body`.
            node.span_text_opt()
                .map(|s| s.trim().to_string())
                .unwrap_or_else(|| span_or_ellipsis(node))
        }

        // Transparent wrappers — descend.
        Some(BbnfCompoundKind::Rhs)
        | Some(BbnfCompoundKind::GrammarItem)
        | Some(BbnfCompoundKind::Directive)
        | Some(BbnfCompoundKind::Lhs) => node
            .child(0)
            .map(format_expression_short)
            .unwrap_or_else(|| span_or_ellipsis(node)),

        // Comments contribute no text to expression display.
        // (Comments project to standalone Span leaves in the BBNF
        // runtime; they appear as direct children of compound rules
        // only as fluff between productions and are not reached
        // through this function's expression dispatch.)

        // Everything else: fall back to the recovered source slice.
        _ => span_or_ellipsis(node),
    }
}

/// Quick formatting of a value expression for hover text.
///
/// Value-expression compounds (`value_expr`, `value_or`,
/// `value_and`, `value_cmp`, `value_add`, `value_mul`,
/// `value_unary`, `value_atom`, `value_fn_call`, `value_path`,
/// `value_input`, `value_closure`, `value_ident`, `int_lit`,
/// `float_lit`, `bool_lit`, `string_lit`, `type_annotation`) all
/// resolve to BbnfCompoundKind::Other or to non-compound leaves in
/// the BBNF struct-direct alphabet (the alphabet only enumerates
/// rules from `bbnf.bbnf` proper; the imported `expressions.bbnf`
/// rules collapse to `Other`). Their source slice already reads as
/// the formatted expression the user typed, so formatting just
/// returns the recovered slice trimmed.
pub fn format_value_expr_short(node: BbnfView<'_, '_>) -> String {
    node.span_text_opt()
        .map(|s| {
            let t = s.trim();
            if t.is_empty() {
                "...".into()
            } else {
                t.to_string()
            }
        })
        .unwrap_or_else(|| "...".into())
}

/// Shared fallback: trimmed span_text or "…" placeholder.
fn span_or_ellipsis(node: BbnfView<'_, '_>) -> String {
    let text = node.span_text_opt().unwrap_or("").trim();
    if text.is_empty() {
        "...".into()
    } else {
        text.to_string()
    }
}

/// Format a `term_1` (identifier with optional call-args).
fn format_term_call(node: BbnfView<'_, '_>) -> String {
    // Branch shape: child(0) = identifier Span. The optional call
    // group, if present, follows as additional children carrying the
    // call_arg compounds.
    let ident = match node.child(0) {
        Some(c) => c,
        None => return span_or_ellipsis(node),
    };
    let name = ident.span_text_opt().unwrap_or("").to_string();
    // Collect call_arg compounds (the optional `( ... )` group).
    let args: Vec<String> = node
        .children()
        .filter(|c| c.compound_kind() == Some(BbnfCompoundKind::CallArg))
        .map(|c| c.span_text_opt().unwrap_or("").trim().to_string())
        .filter(|s| !s.is_empty())
        .collect();
    if args.is_empty() {
        name
    } else {
        format!("{}({})", name, args.join(", "))
    }
}

/// Format a grouped term — `( rhs )` / `[ rhs ]` / `{ rhs }` /
/// `@{ rhs }`. The `branch` argument is the term's branch_tag (4..=7).
fn format_term_grouped(node: BbnfView<'_, '_>, branch: u32) -> String {
    let inner_view = node
        .children()
        .find(|c| c.compound_kind() == Some(BbnfCompoundKind::Rhs));
    let inner = inner_view
        .map(format_expression_short)
        .unwrap_or_else(|| span_or_ellipsis(node));
    match branch {
        4 => format!("@{{{}}}", inner),
        5 => format!("({})", inner),
        6 => format!("[{}]", inner),
        7 => format!("{{{}}}", inner),
        _ => format!("({})", inner),
    }
}
