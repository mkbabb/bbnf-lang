//! Pratt-shape emitter — `parse_pratt_<grammar>_<rule>`.
//!
//! # Role — AW-V.W4.1 (substrate) / AY.W6.c (write-time retarget)
//!
//! Emits per-grammar Pratt-shape parse functions for operator-chain
//! head rules. The emitted body is driven entirely by grammar-derived
//! facts: the per-rule `PRECEDENCE_LUT_<rule>` table and the sparse
//! `PRECEDENCE_ENTRIES_<rule>` slice are mined from the grammar's
//! declared operator precedence + associativity at codegen time; no
//! grammar-name dispatch survives in the runtime body.
//!
//! Unlike the walker's indirected `dispatch_one` arm, the emitted
//! code is a monolithic inline body:
//!
//! - Operand dispatch calls through the per-grammar value-position
//!   dispatcher — the same dispatcher Object / Array shape emitters
//!   re-enter for nested values.
//! - Operator reduction uses the per-grammar `PRECEDENCE_LUT` const
//!   the [`crate::backend::rust::emitter::precedence`] emitter
//!   already lowers.
//! - The outer Pratt compound opens pre-order via
//!   [`Tape<R>::begin_compound`] and closes via
//!   [`Tape<R>::end_compound_with_child_off`] (B5.W4 substrate), so
//!   its direct children (operands, op-leaf Spans, reducer compounds)
//!   land with write-time `sib_skip` stamping. The close primitive
//!   stamps the outer row's `child_off` directly to the final reducer
//!   root — preserving the walker's ShuntingYard invariant (outer
//!   compound's `child_off` names the reduced operator-tree root) in
//!   one substrate call, with no post-close surgery.
//! - Reducer inner compounds remain `push_compound` post-order: a
//!   reducer is synthesised at reduce time, AFTER its lhs + op-leaf +
//!   rhs have already been pushed, with its `child_off` pointing at
//!   the lhs row. The reducer shape is inherently post-order; open/
//!   close does not fit.
//!
//! # Tape shape (for `a + b * c`)
//!
//! ```text
//! [ 0] Rule    (Pratt outer)         span=0..N  child=<final-reducer-idx>
//! [ 1] ...operand 'a' records...
//! [ N] Span    variant=0 payload='+' span=p..p+1
//! [N+1] ...operand 'b' records...
//! [ M] Span    variant=0 payload='*' span=q..q+1
//! [M+1] ...operand 'c' records...
//! [ K] Rule    (reducer b*c)         child=pointer-to-'b'  variant='*'_disc
//! [K+1] Rule    (reducer a+(b*c))    child=pointer-to-'a'  variant='+'_disc
//! ```
//!
//! The outer compound lands at the TOP of the run (pre-order) with
//! `open_compound`; its `child_off` is overridden post-close to name
//! record `[K+1]` — the final reducer. Intermediate rows (operands,
//! op leaves, reducers) carry W5.b `SIB_SKIP_STAMPED_BIT` stamping.

mod struct_direct;
mod tape;
mod visitor;

pub use tape::emit_parse_pratt;
pub use visitor::emit_parse_pratt_visitor;

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
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            extract_first_ref(inner)
        }
        IrNode::Seq(children) => children.iter().find_map(extract_first_ref),
        IrNode::Next(lhs, rhs) | IrNode::Skip(lhs, rhs) => {
            extract_first_ref(lhs).or_else(|| extract_first_ref(rhs))
        }
        IrNode::Repeat { inner, .. } => extract_first_ref(inner),
        _ => None,
    }
}
