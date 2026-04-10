//! Seq node compilation.
//!
//! Decisions (shared across all backends):
//! - operator-chain detection via `NodeFacts.operator_chain`
//! - all-Span compression from `TypeMap`
//! - Span grouping: consecutive Span children merge into compressed groups
//! - Vec flattening: `(T, Vec<T>)` or `(Vec<T>, T)` pairs flatten to `Vec<T>`

use bbnf_ir::{GrammarIR, IrNode, TypeDesc};

use super::DriverState;
use super::node::compile_node;
use crate::backend::types::decisions;
use crate::backend::{Emitter, SeqChildGroup, ValuePlacement};

/// Compile a Seq node (first tries operator-chain optimization via
/// `NodeFacts`; falls back to type-guided grouping).
pub(super) fn compile_seq<E: Emitter>(
    node: &IrNode,
    alloc: ValuePlacement,
    ir: &GrammarIR,
    dstate: &mut DriverState,
    emitter: &mut E,
    ctx: &mut E::Ctx,
) -> E::Output {
    let IrNode::Seq(children) = node else {
        unreachable!("compile_seq called on non-Seq node");
    };

    // Try operator-chain emission (rule-level fast path).
    if let Some(output) = try_operator_chain(node, children, ir, dstate, emitter, ctx) {
        return output;
    }

    compile_seq_grouped(node, children, alloc, ir, dstate, emitter, ctx)
}

/// If this Seq is an operator chain (per `NodeFacts`) and the emitter
/// supports the rewrite, emit the chain form and return `Some(output)`.
/// Returns `None` otherwise so the caller falls through to the generic
/// Seq path.
fn try_operator_chain<E: Emitter>(
    node: &IrNode,
    children: &[IrNode],
    ir: &GrammarIR,
    dstate: &mut DriverState,
    emitter: &mut E,
    ctx: &mut E::Ctx,
) -> Option<E::Output> {
    let is_op_chain = ir
        .dag
        .as_ref()
        .and_then(|dag| dag.node_for(node))
        .and_then(|id| ir.node_facts.get(&id))
        .is_some_and(|f| f.operator_chain);
    if !is_op_chain {
        return None;
    }

    let (head, link, op, rhs) = extract_operator_chain(children)?;

    // Compute types from TypeMap via the DAG-keyed convenience
    // accessors. Fall back to `node_type` if `seq_result` is
    // unavailable.
    let seq_result = ir.seq_result_type(node).cloned();
    let head_type = seq_result
        .as_ref()
        .and_then(|t| match t {
            TypeDesc::Tuple(elems) if elems.len() == 2 => Some(elems[0].clone()),
            _ => None,
        })
        .or_else(|| ir.node_type(head).cloned())
        .unwrap_or(TypeDesc::Span);

    // Skip if head is Span — operator chain not beneficial for
    // all-Span chains.
    if head_type == TypeDesc::Span {
        return None;
    }

    let link_elem_type = ir.vec_elem_type(link).cloned().unwrap_or(TypeDesc::Span);

    let link_elem_types = match &link_elem_type {
        TypeDesc::Tuple(elems) if elems.len() == 2 => Some((&elems[0], &elems[1])),
        _ => None,
    };

    // BoxedEnum → Alloc, else Inline.
    fn alloc_for(ty: &TypeDesc) -> ValuePlacement {
        if matches!(ty, TypeDesc::BoxedEnum) {
            ValuePlacement::Alloc
        } else {
            ValuePlacement::Inline
        }
    }

    let head_alloc = alloc_for(&head_type);
    let (op_alloc, rhs_alloc) = link_elem_types
        .map(|(o, r)| (alloc_for(o), alloc_for(r)))
        .unwrap_or((ValuePlacement::Inline, ValuePlacement::Inline));

    // Compile each child. Span-projected children get span capture wrapping:
    // parse the child for side effects, return Span.
    let head_out = compile_chain_child(head, &head_type, head_alloc, ir, dstate, emitter, ctx);
    let op_out = link_elem_types
        .map(|(ot, _)| compile_chain_child(op, ot, op_alloc, ir, dstate, emitter, ctx))
        .unwrap_or_else(|| compile_node(op, ValuePlacement::Inline, ir, dstate, emitter, ctx));
    let rhs_out = link_elem_types
        .map(|(_, rt)| compile_chain_child(rhs, rt, rhs_alloc, ir, dstate, emitter, ctx))
        .unwrap_or_else(|| compile_node(rhs, ValuePlacement::Inline, ir, dstate, emitter, ctx));

    emitter.emit_operator_chain(
        head_out,
        op_out,
        rhs_out,
        &head_type,
        &link_elem_type,
        ir,
        ctx,
    )
}

fn compile_seq_grouped<E: Emitter>(
    node: &IrNode,
    children: &[IrNode],
    alloc: ValuePlacement,
    ir: &GrammarIR,
    dstate: &mut DriverState,
    emitter: &mut E,
    ctx: &mut E::Ctx,
) -> E::Output {
    let decision = decisions::decide_seq(node, ir);

    if decision.all_span {
        let outputs: Vec<_> = children
            .iter()
            .map(|c| compile_node(c, ValuePlacement::Inline, ir, dstate, emitter, ctx))
            .collect();
        return emitter.emit_seq_all_span(outputs, ctx);
    }

    let mut groups = Vec::new();
    let mut span_run: Vec<E::Output> = Vec::new();

    for (child, ty) in children.iter().zip(decision.child_types.iter()) {
        if *ty == TypeDesc::Span {
            let out = compile_node(child, ValuePlacement::Inline, ir, dstate, emitter, ctx);
            span_run.push(out);
        } else {
            if !span_run.is_empty() {
                groups.push(SeqChildGroup::SpanCompressed {
                    outputs: std::mem::take(&mut span_run),
                });
            }
            let ca = decisions::child_alloc(ty, alloc);
            let out = compile_node(child, ca, ir, dstate, emitter, ctx);
            groups.push(SeqChildGroup::Single {
                output: out,
                ty: ty.clone(),
            });
        }
    }
    if !span_run.is_empty() {
        groups.push(SeqChildGroup::SpanCompressed { outputs: span_run });
    }

    emitter.emit_seq_grouped(groups, &decision.result_type, decision.flatten, ctx)
}

/// Compile an operator-chain child with type-aware alloc and Span
/// projection. When the projected type is Span, the child is compiled
/// for side effects and wrapped in a Span capture.
fn compile_chain_child<E: Emitter>(
    child: &IrNode,
    projected_ty: &TypeDesc,
    alloc: ValuePlacement,
    ir: &GrammarIR,
    dstate: &mut DriverState,
    emitter: &mut E,
    ctx: &mut E::Ctx,
) -> E::Output {
    if *projected_ty == TypeDesc::Span {
        let inner = compile_node(child, ValuePlacement::Inline, ir, dstate, emitter, ctx);
        emitter.emit_span_capture(inner, ctx)
    } else {
        compile_node(child, alloc, ir, dstate, emitter, ctx)
    }
}

/// Extract operator chain components from a Seq already known to be
/// an operator chain. Returns `(head, link_node, op, rhs)`. `link_node`
/// is the `Seq(op, rhs)` inside `Repeat`.
fn extract_operator_chain(
    children: &[IrNode],
) -> Option<(&IrNode, &IrNode, &IrNode, &IrNode)> {
    if children.len() != 2 {
        return None;
    }
    if let IrNode::Repeat {
        inner,
        lo: 0,
        hi: u32::MAX,
    } = &children[1]
    {
        if let IrNode::Seq(link) = inner.as_ref() {
            if link.len() == 2 {
                return Some((&children[0], inner.as_ref(), &link[0], &link[1]));
            }
        }
    }
    None
}
