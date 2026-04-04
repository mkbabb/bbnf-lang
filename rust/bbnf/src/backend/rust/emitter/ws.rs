//! Whitespace trimming emission for the shared-driver Rust emitter.

use proc_macro2::TokenStream;
use quote::quote;

use super::RustEmitCtx;
use super::RustEmitter;

impl RustEmitter {
    pub(super) fn emit_ws_trim_impl(
        &mut self,
        ws_pattern: Option<&str>,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        if let Some(pattern) = ws_pattern {
            let opts =
                crate::generate::regex::EmitOpts::new(&crate::generate::regex::CostModel::DEFAULT);
            let code = crate::generate::regex::emit_regex(pattern, &opts);
            quote! { { #code; Some(()) } }
        } else {
            quote! { { ::parse_that::trim_leading_whitespace_mut(state); Some(()) } }
        }
    }

    pub(super) fn emit_with_ws_trim_impl(
        &mut self,
        inner: TokenStream,
        ws_pattern: Option<&str>,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let trim = if let Some(pattern) = ws_pattern {
            let opts =
                crate::generate::regex::EmitOpts::new(&crate::generate::regex::CostModel::DEFAULT);
            let code = crate::generate::regex::emit_regex(pattern, &opts);
            quote! { #code; }
        } else {
            quote! { ::parse_that::trim_leading_whitespace_mut(state); }
        };
        quote! {
            {
                #trim
                let __ws_inner = #inner;
                #trim
                __ws_inner
            }
        }
    }
}
