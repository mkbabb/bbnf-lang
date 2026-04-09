//! Prettify codegen for alternation (dispatch + sequential).

use bbnf_ir::AltDispatch;
use proc_macro2::TokenStream;
use quote::quote;

use crate::backend::rust::emitter::{RustEmitCtx, RustEmitter};

impl RustEmitter {
    pub(in crate::backend::rust::emitter) fn emit_prettify_alt_dispatch_impl(
        &mut self,
        table: &AltDispatch,
        branches: Vec<TokenStream>,
        _fallback: Option<TokenStream>,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let mut match_arms: Vec<TokenStream> = Vec::new();
        let mut used = vec![false; branches.len()];

        for (idx, branch) in branches.iter().enumerate() {
            if used[idx] {
                continue;
            }
            used[idx] = true;
            let bytes: Vec<u8> = (0u8..128)
                .filter(|&c| table.table.get(c as usize).copied() == Some(idx as u8))
                .collect();
            if bytes.is_empty() {
                continue;
            }
            let byte_patterns: Vec<TokenStream> = bytes
                .iter()
                .map(|&b| {
                    let b_lit = proc_macro2::Literal::byte_character(b);
                    quote! { #b_lit }
                })
                .collect();
            let branch_expr = branch;
            match_arms.push(quote! { #(#byte_patterns)|* => { #branch_expr; } });
        }

        // Find nullable branch for default + EOF handling.
        let nullable_idx = table.fallback_idx.map(|i| i as usize);

        let default_arm = if let Some(nul_idx) = nullable_idx {
            let nul_expr = &branches[nul_idx];
            quote! { _ => { #nul_expr; } }
        } else {
            quote! { _ => { return false; } }
        };
        match_arms.push(default_arm);

        let eof_handler = if let Some(nul_idx) = nullable_idx {
            let nul_expr = &branches[nul_idx];
            quote! {
                let Some(&__byte) = state.src_bytes.get(state.offset) else {
                    #nul_expr;
                    return true;
                };
            }
        } else {
            quote! {
                let __byte = match state.src_bytes.get(state.offset) {
                    Some(&b) => b,
                    None => return false,
                };
            }
        };

        quote! { {
            #eof_handler
            match __byte {
                #(#match_arms)*
            }
        } }
    }

    pub(in crate::backend::rust::emitter) fn emit_prettify_alt_sequential_impl(
        &mut self,
        branches: Vec<(TokenStream, bool)>,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        if branches.len() == 2 {
            let (first_body, first_atomic) = &branches[0];
            let first_try = self.emit_prettify_attempt_impl(
                first_body.clone(),
                !first_atomic,
                false, // conservative: full checkpoint
                ctx,
            );
            let second_body = &branches[1].0;
            return quote! { {
                if !#first_try {
                    #second_body;
                }
            } };
        }

        // General case: try each branch in order, restore builder on failure.
        let mut result = quote! { return false; };
        for (branch_body, is_atomic) in branches.iter().rev() {
            let branch_try = self.emit_prettify_attempt_impl(
                branch_body.clone(),
                !is_atomic,
                false,
                ctx,
            );
            result = quote! {
                {
                    if !#branch_try {
                        #result
                    }
                }
            };
        }
        result
    }
}
