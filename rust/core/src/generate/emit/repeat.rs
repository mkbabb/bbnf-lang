//! Repeat emit codegen with sep_by pattern detection.
//!
//! Detects the same sep_by IR pattern the parse driver uses:
//! `Repeat(Skip(element, Repeat(sep, 0, 1)), lo, MAX)`.

use bbnf_ir::{GrammarIR, IrNode};
use proc_macro2::TokenStream;
use quote::quote;

use crate::generate::ir_types::IrCodegenCtx;

/// Generate emit code for a Repeat node.
pub fn emit_repeat(
    inner: &IrNode,
    lo: u32,
    hi: u32,
    val: &TokenStream,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> TokenStream {
    if lo == 0 && hi == 1 {
        // Optional: Option<T>
        let inner_val = quote! { __opt_v };
        let inner_emit = super::node::emit_node(inner, &inner_val, ir, ctx);
        return quote! {
            if let Some(#inner_val) = #val {
                #inner_emit
            }
        };
    }

    // Try sep_by detection: Skip(element, Repeat(separator, 0, 1))
    if let Some((element, separator)) = detect_sep_by(inner) {
        let item_val = quote! { __item };
        let elem_emit = super::node::emit_node(element, &item_val, ir, ctx);
        let sep_emit = emit_separator(separator, ir);

        return quote! {
            let mut __first = true;
            for #item_val in #val.iter() {
                if !__first {
                    #sep_emit
                }
                __first = false;
                #elem_emit
            }
        };
    }

    // Plain repetition: Vec<T>
    let item_val = quote! { __item };
    let inner_emit = super::node::emit_node(inner, &item_val, ir, ctx);
    quote! {
        for #item_val in #val.iter() {
            #inner_emit
        }
    }
}

/// Detect the sep_by IR pattern: `Skip(element, Repeat(separator, 0, 1))`.
///
/// Mirrors `driver.rs:detect_sep_by()`.
fn detect_sep_by(inner: &IrNode) -> Option<(&IrNode, &IrNode)> {
    if let IrNode::Skip(element, opt_sep) = inner {
        if let IrNode::Repeat {
            inner: separator,
            lo: 0,
            hi: 1,
        } = opt_sep.as_ref()
        {
            return Some((element.as_ref(), separator.as_ref()));
        }
    }
    None
}

/// Emit a separator from its IR node (structural content).
fn emit_separator(sep: &IrNode, ir: &GrammarIR) -> TokenStream {
    match sep {
        IrNode::Literal(sid) => {
            let s = ir.get_string(*sid);
            if s.len() == 1 {
                let byte = s.as_bytes()[0];
                quote! { __sink.char(#byte); }
            } else {
                quote! { __sink.text(#s); }
            }
        }
        IrNode::Seq(children) => {
            let parts: Vec<_> = children.iter().map(|c| emit_separator(c, ir)).collect();
            quote! { #(#parts)* }
        }
        IrNode::OptionalWhitespace(inner) => emit_separator(inner, ir),
        _ => quote! {},
    }
}
