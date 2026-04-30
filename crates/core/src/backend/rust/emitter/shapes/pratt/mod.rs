//! Pratt-shape emitter.
//!
//! # Role
//!
//! Emits per-grammar Pratt-shape parse functions for operator-chain
//! head rules. The emitted body is driven entirely by grammar-derived
//! facts: the per-rule `PRECEDENCE_LUT_<rule>` table and the sparse
//! `PRECEDENCE_ENTRIES_<rule>` slice are mined from the grammar's
//! declared operator precedence + associativity at codegen time; no
//! grammar-name dispatch survives in the runtime body.
//!
//! The emitted code is a monolithic StructDirect body:
//!
//! - Operand dispatch calls through the per-grammar value-position
//!   dispatcher — the same dispatcher Object / Array shape emitters
//!   re-enter for nested values.
//! - Operator reduction uses the per-grammar `PRECEDENCE_LUT` const
//!   the [`crate::backend::rust::emitter::precedence`] emitter
//!   already lowers.
//! - Reducer nodes are built through the StructBuilder surface so the
//!   operator tree is projected directly into the target typed
//!   structure without an intermediate column substrate.

mod dispatch;
mod struct_direct;

pub use dispatch::emit_parse_pratt;

/// Find the operand-position Ref in a Pratt rule's body for direct
/// shape-fn dispatch.
///
/// AW-V.W5.2 — canonical Pratt body is `operand (op operand)*` which
/// lowers through Seq / Next / Repeat to place the operand Ref at
/// the head of the body. Walks left-most to find the first Ref.
pub(super) fn extract_first_ref(node: &bbnf_ir::IrNode) -> Option<bbnf_ir::RuleId> {
    use bbnf_ir::IrNode;
    match node {
        IrNode::Ref(rid) => Some(*rid),
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => extract_first_ref(inner),
        IrNode::Seq(children) => children.iter().find_map(extract_first_ref),
        IrNode::Next(lhs, rhs) | IrNode::Skip(lhs, rhs) => {
            extract_first_ref(lhs).or_else(|| extract_first_ref(rhs))
        }
        IrNode::Repeat { inner, .. } => extract_first_ref(inner),
        _ => None,
    }
}
