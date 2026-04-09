//! Wrap pattern compilation: `open >> middle << close`.
//!
//! Decisions:
//! - Delimited sep_by with terminator:
//!   `open >> OW(Repeat(Skip(element, Optional(separator)))) << close`
//! - Delimiter-scan optimization (forward memchr pivot).
//! - Generic wrap fallback.

use bbnf_ir::{GrammarIR, IrNode, TypeDesc};

use super::DriverState;
use super::node::compile_node;
use crate::backend::patterns::decisions;
use crate::backend::{Emitter, SepByConfig, ValuePlacement};

/// Compile a `Skip(Next(open, middle), close)` or equivalent wrap
/// pattern.
pub(super) fn compile_wrap<E: Emitter>(
    open: &IrNode,
    middle: &IrNode,
    close: &IrNode,
    alloc: ValuePlacement,
    ir: &GrammarIR,
    dstate: &mut DriverState,
    emitter: &mut E,
    ctx: &mut E::Ctx,
) -> E::Output {
    // Delimited sep_by with terminator:
    // `open >> OW(Repeat(Skip(element, Optional(separator)))) << close`
    // where close is a single-byte Literal.
    if let Some((inner_repeat, is_ow)) = unwrap_ow(middle) {
        if let IrNode::Repeat {
            inner,
            lo,
            hi: u32::MAX,
        } = inner_repeat
        {
            if let Some((element, separator)) = decisions::detect_sep_by(inner) {
                let terminator_bytes = if let IrNode::Literal(sid) = close {
                    let raw = ir.get_string(*sid);
                    Some(raw.to_string().into_bytes())
                } else {
                    None
                };

                let elem_type = ir
                    .vec_elem_type(element)
                    .cloned()
                    .or_else(|| {
                        let ty = ir.node_type(element).cloned()?;
                        Some(if ty == TypeDesc::BoxedEnum {
                            TypeDesc::Enum
                        } else {
                            ty
                        })
                    })
                    .unwrap_or(TypeDesc::Span);

                let elem_alloc = if elem_type == TypeDesc::BoxedEnum {
                    ValuePlacement::Alloc
                } else {
                    ValuePlacement::Inline
                };
                let open_out =
                    compile_node(open, ValuePlacement::Inline, ir, dstate, emitter, ctx);
                let element_out =
                    compile_node(element, elem_alloc, ir, dstate, emitter, ctx);
                let sep_out =
                    compile_node(separator, ValuePlacement::Inline, ir, dstate, emitter, ctx);
                let close_out =
                    compile_node(close, ValuePlacement::Inline, ir, dstate, emitter, ctx);

                let config = SepByConfig {
                    ws: is_ow,
                    lo: *lo,
                    terminator_bytes,
                };

                let ws_pattern = ir.ws_pattern.map(|sid| ir.get_string(sid));
                let sep_by_out =
                    emitter.emit_sep_by(element_out, sep_out, &config, &elem_type, ctx);

                // open >> ws_trim(sep_by) << close
                let middle_out = if is_ow {
                    emitter.emit_with_ws_trim(sep_by_out, ws_pattern, ctx)
                } else {
                    sep_by_out
                };
                let after_open = emitter.emit_next(open_out, middle_out, ctx);
                return emitter.emit_skip(after_open, close_out, ctx);
            }
        }
    }

    // Delimiter-scan optimization. Skip when alloc=Alloc — delim scan
    // always produces Span, but BoxedEnum rules need the full typed
    // result for variant wrapping.
    if alloc == ValuePlacement::Inline {
        if let Some(config) =
            crate::backend::patterns::delim_scan::try_detect(open, middle, close, ir)
        {
            if let Some(output) = emitter.emit_delim_scan(&config, ctx) {
                return output;
            }
        }
    }

    // Generic wrap: open >> middle << close.
    let open_out = compile_node(open, ValuePlacement::Inline, ir, dstate, emitter, ctx);
    let middle_out = compile_node(middle, alloc, ir, dstate, emitter, ctx);
    let close_out = compile_node(close, ValuePlacement::Inline, ir, dstate, emitter, ctx);
    let after_open = emitter.emit_next(open_out, middle_out, ctx);
    emitter.emit_skip(after_open, close_out, ctx)
}

/// Unwrap an `OptionalWhitespace` wrapper. Returns `(inner, is_ow)`.
fn unwrap_ow(node: &IrNode) -> Option<(&IrNode, bool)> {
    match node {
        IrNode::OptionalWhitespace(inner) => Some((inner.as_ref(), true)),
        other => Some((other, false)),
    }
}
