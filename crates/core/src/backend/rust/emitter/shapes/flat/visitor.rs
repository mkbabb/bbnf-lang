//! Visitor-path per-position emission for the Flat shape.
//!
//! Mirrors the tape-path emitter structure ([`super::tape`]). Literal
//! positions byte-match without emitting a visitor event; Ref / Regex
//! / Alt positions recurse through the visitor dispatcher.

use bbnf_ir::{GrammarIR, IrNode};
use proc_macro2::TokenStream;
use quote::quote;

use super::super::dispatcher::emit_ref_call_visitor;
use super::PositionedNode;

/// Emit the visitor-path body-position sequence.
pub(super) fn emit_visitor_positions(
    positions: &[PositionedNode],
    support_mod: &proc_macro2::Ident,
    dispatcher_ident: &proc_macro2::Ident,
    ir: &GrammarIR,
) -> TokenStream {
    let mut emissions = Vec::with_capacity(positions.len());
    for pos in positions {
        let leading = if pos.leading_ws {
            quote! { let _ = #support_mod::skip_space(input, p, state); }
        } else {
            quote! {}
        };
        let trailing = if pos.trailing_ws {
            quote! { let _ = #support_mod::skip_space(input, p, state); }
        } else {
            quote! {}
        };
        let core = emit_visitor_position_core(pos.node, support_mod, dispatcher_ident, ir);
        emissions.push(quote! {
            {
                #leading
                #core
                #trailing
            }
        });
    }
    quote! { #(#emissions)* }
}

fn emit_visitor_position_core(
    node: &IrNode,
    support_mod: &proc_macro2::Ident,
    dispatcher_ident: &proc_macro2::Ident,
    ir: &GrammarIR,
) -> TokenStream {
    // AW-V.W5.2 — per-Ref routing. Extract grammar_suffix from support_mod.
    let grammar_suffix = support_mod
        .to_string()
        .strip_prefix("__shape_support_")
        .unwrap_or("")
        .to_string();
    match node {
        IrNode::Literal(sid) => {
            let bytes = ir.get_string(*sid).as_bytes();
            let len = bytes.len();
            let byte_lits: Vec<TokenStream> = bytes.iter().map(|b| quote! { #b }).collect();
            quote! {
                let at = *p;
                let end = at + #len;
                if input.len() < end || input[at..end] != [#(#byte_lits),*] {
                    return Err(crate::runtime::ParseErr::Syntax {
                        offset: at as u32, rule: None,
                    });
                }
                *p = end;
            }
        }
        IrNode::Ref(rid) => {
            // AW-V.W5.2 — direct per-Ref routing for visitor path.
            if let Some(call) = emit_ref_call_visitor(&grammar_suffix, *rid, ir) {
                quote! { (#call)?; }
            } else {
                quote! {
                    #dispatcher_ident(input, p, state, visitor)?;
                }
            }
        }
        IrNode::Regex(_)
        | IrNode::Alt(_, _)
        | IrNode::Negate(_)
        | IrNode::Minus(_, _)
        | IrNode::TokenDispatch { .. } => {
            // AX.W0a.2.e — inline-position emission (visitor path).
            // See tape-path note above for rationale.
            let _ = dispatcher_ident;
            super::super::inline::emit_inline_position_visitor(
                node,
                support_mod,
                &grammar_suffix,
                ir,
            )
        }
        IrNode::Repeat { inner, lo, hi } => {
            let inner_emit = emit_visitor_position_core(inner, support_mod, dispatcher_ident, ir);
            let lo_lit = *lo as usize;
            if *hi == 1 && *lo == 0 {
                quote! {
                    let save_p = *p;
                    let res = (|| -> ::core::result::Result<(), crate::runtime::ParseErr> {
                        #inner_emit
                        Ok(())
                    })();
                    if res.is_err() {
                        *p = save_p;
                    }
                }
            } else {
                quote! {
                    let mut iter_count: u32 = 0;
                    loop {
                        let save_p = *p;
                        let res = (|| -> ::core::result::Result<(), crate::runtime::ParseErr> {
                            #inner_emit
                            Ok(())
                        })();
                        if res.is_err() {
                            *p = save_p;
                            break;
                        }
                        if *p == save_p { break; }
                        iter_count = iter_count.saturating_add(1);
                    }
                    if iter_count < (#lo_lit as u32) {
                        return Err(crate::runtime::ParseErr::Syntax {
                            offset: *p as u32, rule: None,
                        });
                    }
                }
            }
        }
        IrNode::Seq(children) => {
            let mut out = Vec::with_capacity(children.len());
            for c in children {
                out.push(emit_visitor_position_core(
                    c,
                    support_mod,
                    dispatcher_ident,
                    ir,
                ));
            }
            quote! { #(#out)* }
        }
        IrNode::Next(lhs, rhs) | IrNode::Skip(lhs, rhs) => {
            let l = emit_visitor_position_core(lhs, support_mod, dispatcher_ident, ir);
            let r = emit_visitor_position_core(rhs, support_mod, dispatcher_ident, ir);
            quote! { #l #r }
        }
        IrNode::Map { inner, .. } => {
            emit_visitor_position_core(inner, support_mod, dispatcher_ident, ir)
        }
        IrNode::OptionalWhitespace(inner) => {
            let inner_emit = emit_visitor_position_core(inner, support_mod, dispatcher_ident, ir);
            quote! {
                let _ = #support_mod::skip_space(input, p, state);
                #inner_emit
                let _ = #support_mod::skip_space(input, p, state);
            }
        }
        IrNode::Epsilon => quote! {},
    }
}
