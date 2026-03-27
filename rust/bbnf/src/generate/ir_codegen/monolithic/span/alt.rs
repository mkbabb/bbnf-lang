//! Span-only alternation emission: dispatch-table and flat checkpoint chain.

use bbnf_ir::{IrNode, GrammarIR};

use proc_macro2::TokenStream;
use quote::quote;

use super::super::super::super::ir_types::IrCodegenCtx;
use super::super::super::unescape_literal;
use super::super::MonoCtx;
use super::emit_span_expr;

/// Emit a span-only Alt.
pub(super) fn emit_span_alt(
    branches: &[bbnf_ir::AltBranch],
    dispatch: Option<&bbnf_ir::AltDispatch>,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    if branches.is_empty() {
        return quote! { None };
    }
    if branches.len() == 1 {
        return emit_span_expr(&branches[0].node, ir, ctx, mctx);
    }

    // All-literal → direct byte matching.
    let all_literal = branches.iter().all(|b| matches!(&b.node, IrNode::Literal(_)));
    if all_literal {
        let lit_strings: Vec<String> = branches
            .iter()
            .map(|b| {
                let IrNode::Literal(sid) = &b.node else { unreachable!() };
                unescape_literal(ir.get_string(*sid))
            })
            .collect();

        if lit_strings.len() <= 8 {
            let cp_var = mctx.fresh("lit_cp");
            let mut arms: Vec<TokenStream> = Vec::new();
            for (i, s) in lit_strings.iter().enumerate() {
                let bytes = s.as_bytes();
                let len = bytes.len();
                let byte_lits: Vec<proc_macro2::Literal> =
                    bytes.iter().map(|b| proc_macro2::Literal::byte_character(*b)).collect();
                let check = if len == 1 {
                    quote! {
                        if state.src_bytes.get(state.offset).copied() == Some(#(#byte_lits)*) {
                            state.offset += 1;
                            return Some(::parse_that::Span::new(#cp_var, state.offset, state.src));
                        }
                    }
                } else {
                    quote! {
                        {
                            let __end = state.offset + #len;
                            if state.src_bytes.get(state.offset..__end) == Some(&[#(#byte_lits),*] as &[u8]) {
                                state.offset = __end;
                                return Some(::parse_that::Span::new(#cp_var, __end, state.src));
                            }
                        }
                    }
                };
                if i < lit_strings.len() - 1 {
                    arms.push(check);
                } else {
                    arms.push(quote! { #check None });
                }
            }
            return quote! {
                (|| -> Option<::parse_that::Span<'a>> {
                    let #cp_var = state.offset;
                    #(#arms)*
                })()
            };
        }

        // Large all-literal sets: inline sequential byte matching (no combinator).
        {
            let cp_var2 = mctx.fresh("lit_cp");
            let mut arms2: Vec<TokenStream> = Vec::new();
            for (i, s) in lit_strings.iter().enumerate() {
                let bytes = s.as_bytes();
                let len = bytes.len();
                let byte_lits: Vec<proc_macro2::Literal> =
                    bytes.iter().map(|b| proc_macro2::Literal::byte_character(*b)).collect();
                let check = if len == 1 {
                    quote! {
                        if state.src_bytes.get(state.offset).copied() == Some(#(#byte_lits)*) {
                            state.offset += 1;
                            return Some(::parse_that::Span::new(#cp_var2, state.offset, state.src));
                        }
                    }
                } else {
                    quote! {
                        {
                            let __end = state.offset + #len;
                            if state.src_bytes.get(state.offset..__end) == Some(&[#(#byte_lits),*] as &[u8]) {
                                state.offset = __end;
                                return Some(::parse_that::Span::new(#cp_var2, __end, state.src));
                            }
                        }
                    }
                };
                if i < lit_strings.len() - 1 {
                    arms2.push(check);
                } else {
                    arms2.push(quote! { #check None });
                }
            }
            return quote! {
                (|| -> Option<::parse_that::Span<'a>> {
                    let #cp_var2 = state.offset;
                    #(#arms2)*
                })()
            };
        }
    }

    if let Some(disp) = dispatch {
        emit_span_dispatch(branches, disp, ir, ctx, mctx)
    } else {
        emit_span_flat_alt(branches, ir, ctx, mctx)
    }
}

/// Dispatch-table Alt — O(1) byte match.
fn emit_span_dispatch(
    branches: &[bbnf_ir::AltBranch],
    disp: &bbnf_ir::AltDispatch,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    let mut match_arms: Vec<TokenStream> = Vec::new();
    let mut used = vec![false; branches.len()];

    for (idx, branch) in branches.iter().enumerate() {
        if used[idx] { continue; }
        used[idx] = true;
        let bytes: Vec<u8> = (0u8..128)
            .filter(|&c| disp.table.get(c as usize).copied() == Some(idx as u8))
            .collect();
        if bytes.is_empty() { continue; }

        let byte_patterns: Vec<TokenStream> = bytes
            .iter()
            .map(|&b| {
                let b_lit = proc_macro2::Literal::byte_character(b);
                quote! { #b_lit }
            })
            .collect();

        mctx.dispatch_guaranteed_byte = if bytes.len() == 1 { Some(bytes[0]) } else { None };
        let branch_expr = emit_span_expr(&branch.node, ir, ctx, mctx);
        mctx.dispatch_guaranteed_byte = None;

        match_arms.push(quote! { #(#byte_patterns)|* => { #branch_expr }, });
    }

    match_arms.push(quote! { _ => None, });

    quote! {
        {
            let __byte = *state.src_bytes.get(state.offset)?;
            match __byte {
                #(#match_arms)*
            }
        }
    }
}

/// Flat checkpoint-chain Alt.
fn emit_span_flat_alt(
    branches: &[bbnf_ir::AltBranch],
    ir: &GrammarIR,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    let cp_var = mctx.fresh("alt_cp");
    let mut arms: Vec<TokenStream> = Vec::new();

    for (i, branch) in branches.iter().enumerate() {
        let branch_expr = emit_span_expr(&branch.node, ir, ctx, mctx);
        if i < branches.len() - 1 {
            arms.push(quote! {
                if let Some(__v) = (|| -> Option<::parse_that::Span<'a>> { #branch_expr })() {
                    return Some(__v);
                }
                state.furthest_offset = state.furthest_offset.max(state.offset);
                state.offset = #cp_var;
            });
        } else {
            arms.push(quote! { (|| -> Option<::parse_that::Span<'a>> { #branch_expr })() });
        }
    }

    quote! {
        (|| -> Option<::parse_that::Span<'a>> {
            let #cp_var = state.offset;
            #(#arms)*
        })()
    }
}
