//! Map (FnDescriptor) reversal for emission.
//!
//! During parsing, Map nodes transform the parse result via a host function.
//! During emission, we reverse that transformation to recover the textual form.

use bbnf_ir::{FnDescriptor, GrammarIR, IrNode, MapExpr};
use proc_macro2::TokenStream;
use quote::quote;

use crate::generate::ir_types::IrCodegenCtx;

/// Generate emit code for a Map node.
pub fn emit_map(
    inner: &IrNode,
    fn_id: u32,
    val: &TokenStream,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> TokenStream {
    let fn_desc = &ir.fns[fn_id as usize];

    match fn_desc {
        // ── Numeric: f64 → text ──────────────────────────────────────
        FnDescriptor::NumberConvert => {
            quote! { __sink.f64(*#val); }
        }

        // ── Hex: u32 → hex text ──────────────────────────────────────
        FnDescriptor::HexConvert { .. } => {
            quote! {
                {
                    use ::std::fmt::Write as _;
                    let mut __buf = ::std::string::String::new();
                    let _ = write!(__buf, "{:x}", #val);
                    __sink.text(&__buf);
                }
            }
        }

        // ── Span capture: value is Span, emit its text ──────────────
        FnDescriptor::SpanCapture => {
            quote! { __sink.text(#val.as_str()); }
        }

        // ── Enum/Box wrap: transparent — emit the inner value ────────
        FnDescriptor::EnumWrap { .. } | FnDescriptor::BoxWrap => {
            // These wrappings are type-level — the value is the inner result.
            super::node::emit_node(inner, val, ir, ctx)
        }

        // ── Expr: inspect the MapExpr to determine reversibility ─────
        FnDescriptor::Expr { expr, return_type } => {
            emit_map_expr(inner, expr, return_type.as_ref(), val, ir, ctx)
        }
    }
}

/// Emit code for a Map with an Expr descriptor.
fn emit_map_expr(
    inner: &IrNode,
    expr: &MapExpr,
    return_type: Option<&bbnf_ir::TypeDesc>,
    val: &TokenStream,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> TokenStream {
    match expr {
        // Constant literal: the parse matched a literal and returned a constant.
        // Reverse: emit the original literal from the IR.
        MapExpr::IntLit(_) | MapExpr::FloatLit(_) | MapExpr::StringLit(_) => {
            // The inner node IS the literal that was matched. Emit it.
            super::node::emit_node(inner, val, ir, ctx)
        }

        // Boolean constant: "true" -> true | "false" -> false
        MapExpr::BoolLit(_) => {
            // Inner is the matched literal; emit it directly from IR.
            super::node::emit_node(inner, val, ir, ctx)
        }

        // Identity: value is just the parse result passed through.
        MapExpr::Input => {
            super::node::emit_node(inner, val, ir, ctx)
        }

        // General function call or complex expression: not automatically reversible.
        // The inner node's text isn't recoverable from the mapped value.
        // Emit the value via Display as a best-effort fallback.
        _ => {
            quote! {
                {
                    use ::std::fmt::Write as _;
                    let mut __buf = ::std::string::String::new();
                    let _ = write!(__buf, "{}", #val);
                    __sink.text(&__buf);
                }
            }
        }
    }
}
