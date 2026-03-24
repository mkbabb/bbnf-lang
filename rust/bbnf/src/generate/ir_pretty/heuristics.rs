//! Heuristic hint inference and mode resolution for IR pretty-printer codegen.
//!
//! Infers `@pretty` hints from rule shape when no explicit hints are provided,
//! and resolves the global heuristic mode from grammar-level directives.

use bbnf_ir::{GrammarIR, IrNode, IrRule};

use super::patterns::contains_structured_ref_ir;
use super::utils::{pretty_hints_to_strings, unwrap_ir_map};
use crate::generate::ir_types::{type_is_span, IrCodegenCtx};
use crate::generate::prettify::prettify_utils::*;

/// Infer @pretty hints from rule shape when no explicit hints exist.
pub(crate) fn infer_hints_ir(
    rule: &IrRule,
    ty: &syn::Type,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx<'_>,
) -> Vec<String> {
    use crate::generate::prettify::heuristics::HeuristicMode;

    // Resolve mode from grammar-level @pretty * directive.
    let mode = resolve_heuristic_mode_ir(ir);
    match mode {
        HeuristicMode::Off | HeuristicMode::Minimal => Vec::new(),
        HeuristicMode::Auto => {
            let name = ir.get_string(rule.name);
            let inner = unwrap_ir_map(&rule.body);

            // 1. Top-level detection by name.
            const TOPLEVEL_NAMES: &[&str] = &[
                "grammar",
                "program",
                "stylesheet",
                "module",
                "document",
                "file",
                "root",
            ];
            if TOPLEVEL_NAMES.contains(&name) {
                return vec!["block".to_string()];
            }

            // Shape-based: Vec of nonterminals at root.
            if is_vec_type(ty) && is_nonterminal_repetition_ir(inner) {
                return vec!["block".to_string()];
            }

            // 2. Block-delimited detection.
            if contains_brace_wrapped_ir(inner, ir) {
                return vec!["group".to_string(), "indent".to_string()];
            }

            // 3. Large compound detection -- only when the rule body contains
            //    nonterminal references. Rules that are purely terminal
            //    concatenations (literals, regexes, optional/repeated terminals)
            //    are opaque tokens and should not receive formatting hints.
            if let syn::Type::Tuple(tuple) = ty {
                if tuple.elems.len() > 3 && contains_structured_ref_ir(inner, ctx) {
                    return vec!["group".to_string()];
                }
            }
            if is_recursive_enum_type(ty) && !type_is_span(ty) {
                if let IrNode::Alt(branches, _) = inner {
                    if branches.len() > 2 {
                        return vec!["group".to_string()];
                    }
                }
            }

            Vec::new()
        }
    }
}

/// Resolve the heuristic mode from the IR grammar.
pub(crate) fn resolve_heuristic_mode_ir(
    ir: &GrammarIR,
) -> crate::generate::prettify::heuristics::HeuristicMode {
    use crate::generate::prettify::heuristics::HeuristicMode;

    // Look for a rule named "*" with pretty hints (grammar-level @pretty * mode).
    for rule in &ir.rules {
        if ir.get_string(rule.name) == "*" {
            if let Some(ref ph) = rule.meta.pretty {
                let hints = pretty_hints_to_strings(ph);
                if let Some(mode_str) = hints.first() {
                    return HeuristicMode::from_str(mode_str).unwrap_or(HeuristicMode::Auto);
                }
            }
        }
    }
    HeuristicMode::Auto
}

/// Resolve hints for a rule: explicit @pretty > heuristic inference.
pub(crate) fn resolve_ir_hints(
    rule: &IrRule,
    ty: &syn::Type,
    ctx: &IrCodegenCtx<'_>,
) -> Vec<String> {
    if let Some(ref ph) = rule.meta.pretty {
        let hints = pretty_hints_to_strings(ph);
        if hints.iter().any(|h| h == "off") {
            return vec!["off".to_string()];
        }
        hints
    } else {
        infer_hints_ir(rule, ty, ctx.ir, ctx)
    }
}

/// Check if a node is a repetition of nonterminals.
fn is_nonterminal_repetition_ir(node: &IrNode) -> bool {
    match node {
        IrNode::Repeat { inner, .. } => is_or_contains_nonterminal_ir(inner),
        IrNode::OptionalWhitespace(inner) => is_nonterminal_repetition_ir(inner),
        _ => false,
    }
}

/// Check if a node is or contains a nonterminal Ref.
fn is_or_contains_nonterminal_ir(node: &IrNode) -> bool {
    match node {
        IrNode::Ref(_) => true,
        IrNode::OptionalWhitespace(inner) => is_or_contains_nonterminal_ir(inner),
        IrNode::Seq(children) => children.iter().any(is_or_contains_nonterminal_ir),
        _ => false,
    }
}

/// Check if a node contains a brace-wrapped pattern `"{" >> ... << "}"`.
fn contains_brace_wrapped_ir(node: &IrNode, ir: &GrammarIR) -> bool {
    use super::utils::unwrap_ir_whitespace;

    match node {
        IrNode::Skip(left, right) => {
            let left = unwrap_ir_whitespace(left);
            let right_node = unwrap_ir_whitespace(right);
            if let IrNode::Next(next_left, _) = left {
                let next_left_inner = unwrap_ir_whitespace(next_left);
                if let IrNode::Literal(l_sid) = next_left_inner {
                    if let IrNode::Literal(r_sid) = right_node {
                        return ir.get_string(*l_sid) == "{" && ir.get_string(*r_sid) == "}";
                    }
                }
            }
            false
        }
        IrNode::Seq(children) => children.iter().any(|c| contains_brace_wrapped_ir(c, ir)),
        IrNode::OptionalWhitespace(inner) => contains_brace_wrapped_ir(inner, ir),
        _ => false,
    }
}
