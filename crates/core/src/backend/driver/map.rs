//! Map node compilation — classifies `FnDescriptor` and delegates to
//! the appropriate emitter entry point.

use bbnf_ir::{FnDescriptor, FnId, GrammarIR, IrNode};

use super::DriverState;
use super::node::compile_node;
use crate::backend::{Emitter, ValuePlacement};

/// Compile a `Map { inner, fn_id }` node.
///
/// Decisions:
/// - Map fusion: if `inner` is also a Map, try to fuse both operations.
/// - FnDescriptor classification: dispatches to the appropriate
///   emitter entry point (number convert, enum wrap, box wrap, span
///   capture, hex convert, arbitrary expr).
pub(super) fn compile_map<E: Emitter>(
    inner: &IrNode,
    fn_id: FnId,
    alloc: ValuePlacement,
    ir: &GrammarIR,
    dstate: &mut DriverState,
    emitter: &mut E,
    ctx: &mut E::Ctx,
) -> E::Output {
    let fn_desc = &ir.fns[fn_id as usize];

    // Map fusion: if inner is also a Map, try to fuse both operations.
    if let IrNode::Map {
        inner: inner2,
        fn_id: fn_id2,
    } = inner
    {
        let inner_fd = &ir.fns[*fn_id2 as usize];
        let inner_out = compile_node(inner2, ValuePlacement::Inline, ir, dstate, emitter, ctx);
        if let Some(fused) = emitter.emit_fused_map(inner_out, inner_fd, fn_desc, alloc, ir, ctx) {
            return fused;
        }
        // Fusion not handled — fall through to single-map path with
        // re-compiled inner.
    }

    match fn_desc {
        FnDescriptor::NumberConvert { allow_leading_dot } => {
            emitter.emit_number_convert(*allow_leading_dot, ctx)
        }

        FnDescriptor::EnumWrap { variant } => {
            let variant_name = ir.get_string(*variant);
            let inner_out = compile_node(inner, ValuePlacement::Inline, ir, dstate, emitter, ctx);
            emitter.emit_enum_wrap(inner_out, variant_name, alloc, ctx)
        }

        FnDescriptor::BoxWrap => compile_node(inner, alloc, ir, dstate, emitter, ctx),

        FnDescriptor::SpanCapture => {
            let inner_out = compile_node(inner, ValuePlacement::Inline, ir, dstate, emitter, ctx);
            emitter.emit_span_capture(inner_out, ctx)
        }

        FnDescriptor::HexConvert { fn_path } => {
            let path_str = ir.get_string(*fn_path);
            let inner_out = compile_node(inner, ValuePlacement::Inline, ir, dstate, emitter, ctx);
            emitter.emit_hex_convert(inner_out, path_str, ctx)
        }

        FnDescriptor::Expr { expr, return_type } => {
            let inner_out = compile_node(inner, ValuePlacement::Inline, ir, dstate, emitter, ctx);
            emitter.emit_map_expr(inner_out, expr, return_type.as_ref(), alloc, ir, ctx)
        }
    }
}
