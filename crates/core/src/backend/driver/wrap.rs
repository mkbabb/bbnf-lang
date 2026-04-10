//! Wrap pattern compilation: `open >> middle << close`.
//!
//! Decisions:
//! - Delimited sep_by with terminator:
//!   `open >> OW(Repeat(Skip(element, Optional(separator)))) << close`
//! - Delimiter-scan optimization (forward memchr pivot).
//! - Generic wrap fallback.

use bbnf_ir::passes::csp_strategy::WrapMode;
use bbnf_ir::{GrammarIR, IrNode, TypeDesc};

use super::DriverState;
use super::derive_vec_elem_type;
use super::node::compile_node;
use crate::backend::types::decisions;
use crate::backend::{Emitter, SepByConfig, ValuePlacement};

/// Compile a `Skip(Next(open, middle), close)` or equivalent wrap
/// pattern. `wrap_root` is the outer Skip/Next node — used to look
/// up the pre-solved delim-scan configuration from the DriverState
/// cache.
pub(super) fn compile_wrap<E: Emitter>(
    wrap_root: &IrNode,
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

                let elem_type = derive_vec_elem_type(ir, element);

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

    // Delimiter-scan optimization (Tranche X.8c AOT consumption).
    //
    // Skip when alloc=Alloc — delim scan always produces Span, but
    // BoxedEnum rules need the full typed result for variant
    // wrapping. The pre-solved configs live in
    // `DriverState.delim_scan_configs` (cloned from
    // `ir.delim_scan_configs` during `BackendPreparation::from_ir`),
    // keyed by the wrap root's `NodeId`.
    //
    // The IR's per-NodeId `WrapMode` decision in
    // `ir.recognizer_decisions` is the authoritative CSP-level
    // strategy flag. When both the config and the decision agree
    // that DelimScan applies, the backend emits the scanner body.
    // If the CSP decided against DelimScan the backend honors the
    // decision and falls through to the generic wrap emitter — this
    // is the single data path the §8c gate asserts.
    if alloc == ValuePlacement::Inline {
        if let Some(config) = dstate.delim_scan_config(wrap_root, ir) {
            let csp_wrap_mode = ir
                .dag
                .as_ref()
                .and_then(|dag| dag.node_for(wrap_root))
                .and_then(|id| ir.recognizer_decisions.get(&id))
                .and_then(|d| d.wrap_mode.as_ref());
            let csp_allows_delim_scan = match csp_wrap_mode {
                // CSP explicitly chose a delim-scan flavor or left
                // the decision to the backend (no WrapMode var).
                Some(WrapMode::DelimScan) | Some(WrapMode::BalancedScan) | None => true,
                // CSP chose Generic or SepBy — honor the decision.
                Some(WrapMode::Generic) | Some(WrapMode::SepBy) => false,
            };
            if csp_allows_delim_scan {
                if let Some(output) = emitter.emit_delim_scan(config, ctx) {
                    return output;
                }
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
