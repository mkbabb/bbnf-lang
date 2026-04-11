//! Recursively collect nonterminal references from a tape-first
//! `BbnfBootstrapNodeView` rule RHS.
//!
//! Walks the tape view shape-agnostically: `term_1` identifier-call
//! compounds, nested `term`/`factor`/`mapped_factor` wrappers,
//! grouped `term_2` expressions, alternation / concatenation, and
//! binary-factor chains all cascade into a recursive descent that
//! bottoms out at the `identifier` leaf.

use bbnf::grammar::generated::{BbnfBootstrapNodeView, BbnfBootstrapRuleKind};

use super::super::types::ReferenceInfo;

/// Recursively collect nonterminal references from a bootstrap tape view.
pub fn collect_references(node: BbnfBootstrapNodeView<'_>, refs: &mut Vec<ReferenceInfo>) {
    match node.rule_kind() {
        // Leaf: identifier reference. The bare `identifier` leaf
        // variant pushes a rule compound whose span covers the
        // identifier text; the name is its span_text.
        BbnfBootstrapRuleKind::identifier => {
            let (lo, hi) = node.span();
            refs.push(ReferenceInfo {
                name: node.span_text().to_string(),
                span: (lo as usize, hi as usize),
            });
        }

        // term_1: identifier + optional call args.
        //   term_1 = identifier ( "(" first_arg ( "," arg )* ")" )?
        // Child layout under the tape view:
        //   child(0) = identifier
        //   child(1) = optional call-args group
        BbnfBootstrapRuleKind::term_1 => {
            if let Some(ident) = node.child(0) {
                let (lo, hi) = ident.span();
                refs.push(ReferenceInfo {
                    name: ident.span_text().to_string(),
                    span: (lo as usize, hi as usize),
                });
            }
            // Any remaining children (the optional call-args group)
            // are descended into — the args themselves may carry
            // nonterminal references.
            for c in node.children().skip(1) {
                collect_references(c, refs);
            }
        }

        // Structural: alternation, concatenation. Walk every
        // iteration child and recurse.
        BbnfBootstrapRuleKind::alternation | BbnfBootstrapRuleKind::concatenation => {
            for child in super::iter_iteration_views(node) {
                collect_references(child, refs);
            }
        }

        // Binary factor: first operand + rest operands.
        BbnfBootstrapRuleKind::binary_factor => {
            for operand in super::collect_binary_operand_views(node) {
                collect_references(operand, refs);
            }
        }

        // Mapped factor: recurse into the inner factor child.
        // The optional `-> value_expr` mapping carries no grammar
        // nonterminal refs.
        BbnfBootstrapRuleKind::mapped_factor => {
            if let Some(inner) = node.child(0) {
                collect_references(inner, refs);
            }
        }

        // Factor: find the term child by rule_kind and recurse.
        BbnfBootstrapRuleKind::factor => {
            for c in node.children() {
                if super::is_term_kind(c.rule_kind()) {
                    collect_references(c, refs);
                    break;
                }
            }
        }

        // Transparent wrappers — descend into the single inner child.
        BbnfBootstrapRuleKind::term
        | BbnfBootstrapRuleKind::rhs
        | BbnfBootstrapRuleKind::grammar_item
        | BbnfBootstrapRuleKind::directive
        | BbnfBootstrapRuleKind::lhs => {
            if let Some(inner) = node.child(0) {
                collect_references(inner, refs);
            }
        }

        // Grouped: "(" rhs ")" / "[" rhs "]" / "{" rhs "}" / "@{" rhs "}"
        //   term_2 / value_atom_0 layout: child(0) = open, child(1) = inner, child(2) = close
        BbnfBootstrapRuleKind::term_2 | BbnfBootstrapRuleKind::value_atom_0 => {
            if let Some(inner) = node.child(1) {
                collect_references(inner, refs);
            }
        }

        // Closure: |params| body — recurse into the body child
        // (trailing child of the closure compound).
        BbnfBootstrapRuleKind::closure => {
            if let Some(body) = node.child(4) {
                collect_references(body, refs);
            }
        }

        // Terminals and value-expression compounds don't carry
        // grammar nonterminal references. Everything else is a
        // leaf, directive, or grammar-level construct and
        // contributes nothing.
        _ => {}
    }
}
