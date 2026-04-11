//! AST helper functions over tape-first `BbnfBootstrapNodeView` nodes.
//!
//! Pure functions consumed by `state/diagnostics.rs` and `features/*.rs`.

use bbnf::grammar::generated::{BbnfBootstrapNodeView, BbnfBootstrapRuleKind};

pub mod cycles;
pub mod format;
pub mod references;
pub mod spans;
pub mod tokens;

pub use cycles::{build_cycle_path, compute_reachable_rules};
pub use format::{format_expression_short, format_value_expr_short};
pub use references::collect_references;
pub use spans::{compute_expression_end, compute_expression_end_pub};
pub use tokens::collect_semantic_tokens;

/// Format a byte as a display-friendly character for inlay hints.
pub fn format_char(b: u8) -> String {
    match b {
        b'\t' => "\\t".into(),
        b'\n' => "\\n".into(),
        b'\r' => "\\r".into(),
        b' ' => "SP".into(),
        0x0b => "\\v".into(),
        0x0c => "\\f".into(),
        c if c.is_ascii_graphic() => format!("'{}'", c as char),
        c => format!("0x{:02x}", c),
    }
}

/// Check if a rule RHS is effectively empty (epsilon only).
///
/// Walks the tape view structure, peeling transparent wrappers
/// (`term`, `factor` without a modifier, single-branch
/// `alternation`/`concatenation`) to their leaf, and returns true
/// when the leaf is the `term_0` epsilon marker.
pub fn is_empty_rhs(node: BbnfBootstrapNodeView<'_>) -> bool {
    match node.rule_kind() {
        BbnfBootstrapRuleKind::term_0 => true,

        // Transparent wrappers — descend into the single inner child.
        BbnfBootstrapRuleKind::term
        | BbnfBootstrapRuleKind::rhs
        | BbnfBootstrapRuleKind::grammar_item
        | BbnfBootstrapRuleKind::directive
        | BbnfBootstrapRuleKind::lhs => node
            .child(0)
            .map(is_empty_rhs)
            .unwrap_or(false),

        // Factor: epsilon iff the inner term is epsilon AND there's no modifier.
        BbnfBootstrapRuleKind::factor => {
            let term = node
                .children()
                .find(|c| is_term_kind(c.rule_kind()));
            let modifier_present = node
                .children()
                .any(|c| c.rule_kind() == BbnfBootstrapRuleKind::modifier);
            match (term, modifier_present) {
                (Some(t), false) => is_empty_rhs(t),
                _ => false,
            }
        }

        // Mapped factor: epsilon iff inner is epsilon AND there's no `->` mapping.
        BbnfBootstrapRuleKind::mapped_factor => {
            let inner = match node.child(0) {
                Some(c) => c,
                None => return false,
            };
            let mapping_present = node.child(1).is_some_and(|m| {
                let (lo, hi) = m.span();
                hi > lo && m.span_text().contains("->")
            });
            if mapping_present {
                false
            } else {
                is_empty_rhs(inner)
            }
        }

        // Binary factor: epsilon iff single operand and that operand is epsilon.
        BbnfBootstrapRuleKind::binary_factor => {
            let operands: Vec<_> = collect_binary_operand_views(node).collect();
            operands.len() == 1 && is_empty_rhs(operands[0])
        }

        // Concatenation / alternation: epsilon iff exactly one branch and that
        // branch is epsilon.
        BbnfBootstrapRuleKind::concatenation | BbnfBootstrapRuleKind::alternation => {
            let parts: Vec<_> = iter_iteration_views(node).collect();
            parts.len() == 1 && is_empty_rhs(parts[0])
        }

        _ => false,
    }
}

/// True iff `kind` belongs to the `term` family (term or one of
/// its sub-variants term_0/1/2, plus value_atom_0 and the leaf
/// terminals that the lowering dispatches through `lower_term_dispatch`).
pub(crate) fn is_term_kind(kind: BbnfBootstrapRuleKind) -> bool {
    matches!(
        kind,
        BbnfBootstrapRuleKind::term
            | BbnfBootstrapRuleKind::term_0
            | BbnfBootstrapRuleKind::term_1
            | BbnfBootstrapRuleKind::term_2
            | BbnfBootstrapRuleKind::value_atom_0
            | BbnfBootstrapRuleKind::literal
            | BbnfBootstrapRuleKind::regex
            | BbnfBootstrapRuleKind::identifier
    )
}

/// Collect binary-factor operand views, peeling a single-Repeat
/// wrapper when present (mirrors `iter_rep_children` from the
/// lowering, but for the operand list specifically).
pub(crate) fn collect_binary_operand_views<'a>(
    node: BbnfBootstrapNodeView<'a>,
) -> impl Iterator<Item = BbnfBootstrapNodeView<'a>> + 'a {
    use ::bbnf::runtime::tape::TapeKind;
    let mut children = node.children();
    let first = children.next();
    let rest: Vec<_> = children.collect();
    let expanded: Vec<BbnfBootstrapNodeView<'a>> = if let Some(f) = first {
        let mut v = vec![f];
        if rest.len() == 1 && rest[0].kind() == TapeKind::Repeat {
            v.extend(rest[0].children());
        } else {
            v.extend(rest);
        }
        v
    } else {
        Vec::new()
    };
    expanded.into_iter()
}

/// Iterate the operand views of an iteration-pair compound. Mirrors
/// `lower/expression.rs::iter_iteration_pairs`: peels a single
/// `TapeKind::Repeat` wrapper, then descends into any per-iteration
/// `TapeKind::Seq` wrapper to reach the operand.
pub(crate) fn iter_iteration_views<'a>(
    node: BbnfBootstrapNodeView<'a>,
) -> impl Iterator<Item = BbnfBootstrapNodeView<'a>> + 'a {
    use ::bbnf::runtime::tape::TapeKind;
    iter_rep_children(node).filter_map(|pair| match pair.kind() {
        TapeKind::Seq => pair.child(0),
        _ => Some(pair),
    })
}

/// Iterate iteration children, unwrapping a single top-level
/// `TapeKind::Repeat` wrapper if present. Matches the lowering's
/// `tape_walk::iter_rep_children` helper.
pub(crate) fn iter_rep_children<'a>(
    view: BbnfBootstrapNodeView<'a>,
) -> Box<dyn Iterator<Item = BbnfBootstrapNodeView<'a>> + 'a> {
    use ::bbnf::runtime::tape::TapeKind;
    let mut children = view.children();
    let first = match children.next() {
        Some(c) => c,
        None => return Box::new(std::iter::empty()),
    };
    if children.next().is_none() && first.kind() == TapeKind::Repeat {
        return Box::new(first.children());
    }
    Box::new(view.children())
}
