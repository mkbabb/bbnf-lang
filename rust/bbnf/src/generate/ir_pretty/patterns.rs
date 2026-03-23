//! IR pattern detection for pretty-printer codegen.
//!
//! Detects structural patterns in IR nodes: wrapped repetitions, key-value pairs,
//! separator resolution, and structured reference detection.

use bbnf_ir::{GrammarIR, IrNode};

use super::utils::{unwrap_ir_map, unwrap_ir_whitespace};
use crate::generate::ir_types::IrCodegenCtx;

/// Detect `"L" >> middle << "R"` (wrapped repetition) in IR.
///
/// Matches `Skip(Next(Literal(L), inner), Literal(R))` or variants with
/// OptionalWhitespace wrappers.
pub(crate) fn detect_wrapped_pattern_ir(
    node: &IrNode,
    _ir: &GrammarIR,
) -> Option<(String, String)> {
    let node = unwrap_ir_whitespace(node);
    if let IrNode::Skip(left, right) = node {
        let left = unwrap_ir_whitespace(left);
        let right_node = unwrap_ir_whitespace(right);
        if let IrNode::Next(next_left, _) = left {
            let next_left_inner = unwrap_ir_whitespace(next_left);
            if let IrNode::Literal(l_sid) = next_left_inner {
                if let IrNode::Literal(r_sid) = right_node {
                    return Some((
                        _ir.get_string(*l_sid).to_string(),
                        _ir.get_string(*r_sid).to_string(),
                    ));
                }
            }
        }
    }
    None
}

/// Resolve a Ref and detect wrapped pattern in the referenced rule.
pub(crate) fn resolve_and_detect_wrapped_ir(
    node: &IrNode,
    ir: &GrammarIR,
) -> Option<(String, String)> {
    if let IrNode::Ref(rule_id) = node {
        let rule = &ir.rules[*rule_id as usize];
        let inner = unwrap_ir_map(&rule.body);
        detect_wrapped_pattern_ir(inner, ir)
    } else {
        None
    }
}

/// Detect `key, sep >> value` (key-value pair) pattern in IR.
///
/// Matches `Seq([elem, Next(Literal(sep), value)])`.
pub(crate) fn detect_key_value_pattern_ir(
    node: &IrNode,
    ir: &GrammarIR,
) -> Option<(String, String)> {
    if let IrNode::Seq(children) = node {
        if children.len() == 2 {
            if let IrNode::Next(sep, _) = &children[1] {
                let sep_inner = unwrap_ir_whitespace(sep);
                if let IrNode::Literal(sep_sid) = sep_inner {
                    return Some(("key".to_string(), ir.get_string(*sep_sid).to_string()));
                }
                // sep can be a Ref to a rule that is a literal.
                if let IrNode::Ref(sep_rule_id) = sep_inner {
                    let sep_name = ir.get_string(ir.rules[*sep_rule_id as usize].name);
                    return Some(("key".to_string(), sep_name.to_string()));
                }
            }
        }
    }
    None
}

/// Resolve a rule name's body to find a literal value (for separator resolution).
pub(crate) fn resolve_separator_literal_ir(name: &str, ir: &GrammarIR) -> Option<String> {
    for rule in &ir.rules {
        if ir.get_string(rule.name) == name {
            let inner = unwrap_ir_map(&rule.body);
            let unwrapped = unwrap_ir_whitespace(inner);
            if let IrNode::Literal(sid) = unwrapped {
                return Some(ir.get_string(*sid).to_string());
            }
        }
    }
    None
}

/// Check if an IR node tree contains nonterminal references to structured
/// (non-span) rules. References to span-producing rules don't count as
/// "structural" content -- they're effectively terminal patterns.
pub(crate) fn contains_structured_ref_ir(node: &IrNode, ctx: &IrCodegenCtx<'_>) -> bool {
    use crate::generate::ir_types::type_is_span;

    match node {
        IrNode::Ref(rule_id) => {
            // Check if the referenced rule produces a Span type.
            if let Some(ty) = ctx.rule_types.get(rule_id) {
                !type_is_span(ty)
            } else {
                true // Unknown type -> assume structured.
            }
        }
        IrNode::Seq(children) => children.iter().any(|c| contains_structured_ref_ir(c, ctx)),
        IrNode::Alt(branches, _) => branches
            .iter()
            .any(|b| contains_structured_ref_ir(&b.node, ctx)),
        IrNode::Repeat { inner, .. } => contains_structured_ref_ir(inner, ctx),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            contains_structured_ref_ir(a, ctx) || contains_structured_ref_ir(b, ctx)
        }
        IrNode::OptionalWhitespace(inner) | IrNode::Negate(inner) | IrNode::Map { inner, .. } => {
            contains_structured_ref_ir(inner, ctx)
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => false,
    }
}
