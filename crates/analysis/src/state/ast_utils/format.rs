//! Short-form expression / value-expression formatting for hover text.
//!
//! Walks a tape-first `BbnfBootstrapNodeView` and produces a compact
//! human-readable string. The structural cases (alternation,
//! concatenation, binary_factor, term_1, term_2, mapped_factor,
//! factor, closure) reconstruct the grammar surface syntax
//! explicitly so the hover output remains stable regardless of the
//! optimizer's wrapper-elision decisions. Everything else falls
//! through to `span_text()`, which is exactly the source slice the
//! rule body covers.

use bbnf::grammar::generated::{BbnfBootstrapNodeView, BbnfBootstrapRuleKind};

/// Quick one-line formatting of a bootstrap tape view for hover text.
pub fn format_expression_short(node: BbnfBootstrapNodeView<'_>) -> String {
    match node.rule_kind() {
        BbnfBootstrapRuleKind::identifier
        | BbnfBootstrapRuleKind::literal
        | BbnfBootstrapRuleKind::regex => node.span_text().to_string(),

        BbnfBootstrapRuleKind::term_0 => "\u{03b5}".into(),

        BbnfBootstrapRuleKind::alternation => super::iter_iteration_views(node)
            .map(format_expression_short)
            .collect::<Vec<_>>()
            .join(" | "),

        BbnfBootstrapRuleKind::concatenation => super::iter_iteration_views(node)
            .map(format_expression_short)
            .collect::<Vec<_>>()
            .join(", "),

        BbnfBootstrapRuleKind::binary_factor => {
            // For brevity we concatenate operand text separated by a
            // space. The exact operator is recoverable from the
            // source gap but adds code weight for a hover-only
            // summary; the source slice is already available via the
            // wider fallback and the summary is approximate.
            let operands: Vec<_> = super::collect_binary_operand_views(node).collect();
            if operands.is_empty() {
                node.span_text().to_string()
            } else if operands.len() == 1 {
                format_expression_short(operands[0])
            } else {
                // Reconstruct the exact chain text from the source
                // slice between operand spans so the operator stays
                // faithful.
                let input = node.input();
                let mut out = format_expression_short(operands[0]);
                let mut prev_end = operands[0].span().1;
                for op in operands.iter().skip(1) {
                    let gap = &input[prev_end as usize..op.span().0 as usize];
                    let trimmed = gap.trim();
                    out.push(' ');
                    out.push_str(trimmed);
                    out.push(' ');
                    out.push_str(&format_expression_short(*op));
                    prev_end = op.span().1;
                }
                out
            }
        }

        BbnfBootstrapRuleKind::mapped_factor => {
            let mapping = node.child(1);
            let has_arrow = mapping.is_some_and(|m| {
                let (lo, hi) = m.span();
                hi > lo && m.span_text().contains("->")
            });
            if has_arrow {
                // Fall back to the exact mapping source slice; it
                // already reads as `expr -> value_expr : ty`.
                return node.span_text().trim().to_string();
            }
            // The inner child (child(0)) may be an empty wrapper under
            // the tape-first parser when the term body was consumed as
            // a span-only operation. Detect this and fall back to the
            // parent's span text.
            let inner = match node.child(0) {
                Some(c) => c,
                None => return node.span_text().trim().to_string(),
            };
            let (ilo, ihi) = inner.span();
            if ihi <= ilo {
                // Empty wrapper — the actual content is in the parent span.
                return node.span_text().trim().to_string();
            }
            format_expression_short(inner)
        }

        BbnfBootstrapRuleKind::factor => {
            let term = node
                .children()
                .find(|c| super::is_term_kind(c.rule_kind()));
            let modifier = node
                .children()
                .find(|c| c.rule_kind() == BbnfBootstrapRuleKind::modifier);
            let term_str = term
                .map(format_expression_short)
                .unwrap_or_else(|| node.span_text().to_string());
            if let Some(m) = modifier {
                let (lo, hi) = m.span();
                if hi > lo {
                    return format!("{}{}", term_str, m.span_text());
                }
            }
            term_str
        }

        BbnfBootstrapRuleKind::term => node
            .child(0)
            .map(format_expression_short)
            .unwrap_or_else(|| node.span_text().to_string()),

        BbnfBootstrapRuleKind::term_1 => {
            let ident = match node.child(0) {
                Some(i) => i,
                None => return node.span_text().to_string(),
            };
            let name = ident.span_text();
            // The optional call-args group is child(1); when absent
            // its span collapses to `(lo, lo)`.
            if let Some(call) = node.child(1) {
                let (lo, hi) = call.span();
                if hi > lo {
                    // call = "(" first (, rest)* ")". Walk the direct
                    // children of `call` and render each nontrivial
                    // one as an argument.
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
                            Some(format_expression_short(c))
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
            let inner = node
                .child(1)
                .map(format_expression_short)
                .unwrap_or_default();
            match open.as_str() {
                "(" => format!("({})", inner),
                "[" => format!("[{}]", inner),
                "{" => format!("{{{}}}", inner),
                "@{" => format!("@{{{}}}", inner),
                _ => format!("({})", inner),
            }
        }

        BbnfBootstrapRuleKind::modifier | BbnfBootstrapRuleKind::binary_operators => {
            node.span_text().to_string()
        }

        BbnfBootstrapRuleKind::closure => {
            // closure = "|", first_param, (",", rest_param)*, "|", body
            // Fall back to the literal source slice — it's already
            // `|params| body` as written.
            node.span_text().trim().to_string()
        }

        // Value expression leaves / compounds — delegate.
        BbnfBootstrapRuleKind::int_lit
        | BbnfBootstrapRuleKind::float_lit
        | BbnfBootstrapRuleKind::bool_lit
        | BbnfBootstrapRuleKind::string_lit
        | BbnfBootstrapRuleKind::value_ident
        | BbnfBootstrapRuleKind::value_expr
        | BbnfBootstrapRuleKind::value_or
        | BbnfBootstrapRuleKind::value_and
        | BbnfBootstrapRuleKind::value_cmp
        | BbnfBootstrapRuleKind::value_add
        | BbnfBootstrapRuleKind::value_mul
        | BbnfBootstrapRuleKind::value_unary
        | BbnfBootstrapRuleKind::value_unary_0
        | BbnfBootstrapRuleKind::value_atom
        | BbnfBootstrapRuleKind::value_fn_call
        | BbnfBootstrapRuleKind::value_input
        | BbnfBootstrapRuleKind::value_closure
        | BbnfBootstrapRuleKind::type_annotation
        | BbnfBootstrapRuleKind::type_name => format_value_expr_short(node),

        BbnfBootstrapRuleKind::comment | BbnfBootstrapRuleKind::big_comment => String::new(),

        // Transparent wrappers.
        BbnfBootstrapRuleKind::rhs
        | BbnfBootstrapRuleKind::grammar_item
        | BbnfBootstrapRuleKind::directive
        | BbnfBootstrapRuleKind::lhs => node
            .child(0)
            .map(format_expression_short)
            .unwrap_or_else(|| node.span_text().to_string()),

        // Everything else: fall back to the raw source slice.
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

/// Quick formatting of a value expression for hover text.
pub fn format_value_expr_short(node: BbnfBootstrapNodeView<'_>) -> String {
    match node.rule_kind() {
        BbnfBootstrapRuleKind::int_lit
        | BbnfBootstrapRuleKind::float_lit
        | BbnfBootstrapRuleKind::bool_lit
        | BbnfBootstrapRuleKind::string_lit
        | BbnfBootstrapRuleKind::value_ident
        | BbnfBootstrapRuleKind::type_name
        | BbnfBootstrapRuleKind::cmp_op
        | BbnfBootstrapRuleKind::mul_op
        | BbnfBootstrapRuleKind::add_op => node.span_text().to_string(),

        // Value-expression compounds — the raw source slice already
        // reads as the formatted expression the user typed. Trimmed
        // so that a trailing `;` or whitespace doesn't leak in.
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
        | BbnfBootstrapRuleKind::value_input
        | BbnfBootstrapRuleKind::value_closure
        | BbnfBootstrapRuleKind::type_annotation => node.span_text().trim().to_string(),

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
