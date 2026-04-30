//! Value-Ref extraction helpers shared by the StructDirect array
//! shapes.

use bbnf_ir::GrammarIR;

/// Extract the value-position Ref target from an array rule body.
///
/// AW-V.W5.2 — the canonical JSON array body is
/// `"[" >> ((value << comma?)*)?w << "]"`, which lowers to
/// `Skip(Next("[", OW(Repeat(Skip(value, Repeat(comma, 0..=1))))), "]")`.
/// The value Ref sits inside the outer Repeat. The list-rule entry
/// variant (CSS `stylesheet = ruleList ?w`, BBNF `grammar = (item ?w)*`)
/// has a simpler shape: `Repeat(ref_or_alt, lo, hi)` with OW wrappers.
///
/// Strategy: walk the body, find the outer `Repeat`, then find the
/// first value-position Ref inside the iteration body.
pub(super) fn extract_array_value_ref(
    node: &bbnf_ir::IrNode,
    ir: &GrammarIR,
) -> Option<bbnf_ir::RuleId> {
    use bbnf_ir::IrNode;
    fn find_repeat_inner<'a>(n: &'a IrNode) -> Option<&'a IrNode> {
        match n {
            IrNode::Repeat { inner, .. } => Some(inner),
            IrNode::OptionalWhitespace(inner) | IrNode::Map { inner, .. } => {
                find_repeat_inner(inner)
            }
            IrNode::Seq(children) => children.iter().find_map(find_repeat_inner),
            IrNode::Next(lhs, rhs) | IrNode::Skip(lhs, rhs) => {
                find_repeat_inner(lhs).or_else(|| find_repeat_inner(rhs))
            }
            _ => None,
        }
    }
    fn first_value_ref(n: &IrNode, ir: &GrammarIR) -> Option<bbnf_ir::RuleId> {
        // Punctuation-rule predicate: a rule whose body is a single literal.
        fn is_punct(rid: bbnf_ir::RuleId, ir: &GrammarIR) -> bool {
            let rule = match ir.rules.iter().find(|r| r.id == rid) {
                Some(r) => r,
                None => return false,
            };
            fn unwrap<'a>(n: &'a IrNode) -> &'a IrNode {
                match n {
                    IrNode::OptionalWhitespace(i) | IrNode::Map { inner: i, .. } => unwrap(i),
                    _ => n,
                }
            }
            matches!(unwrap(&rule.body), IrNode::Literal(_))
        }
        match n {
            IrNode::Ref(rid) => {
                if is_punct(*rid, ir) {
                    None
                } else {
                    Some(*rid)
                }
            }
            IrNode::OptionalWhitespace(inner) | IrNode::Map { inner, .. } => {
                first_value_ref(inner, ir)
            }
            IrNode::Seq(children) => children.iter().find_map(|c| first_value_ref(c, ir)),
            IrNode::Skip(lhs, _) => first_value_ref(lhs, ir),
            IrNode::Next(lhs, rhs) => first_value_ref(lhs, ir).or_else(|| first_value_ref(rhs, ir)),
            IrNode::Alt(branches, _) => {
                // For Alt-of-Refs at the value position (uncommon but
                // legal), route through the dispatcher — return None.
                // A single-Ref Alt could be unwrapped, but that's not the
                // canonical shape.
                let _ = branches;
                None
            }
            _ => None,
        }
    }
    let repeat_inner = find_repeat_inner(node)?;
    first_value_ref(repeat_inner, ir)
}
