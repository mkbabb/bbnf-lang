//! Prettify codegen for Seq, Skip, Next, and OptionalWhitespace nodes.

use proc_macro2::TokenStream;
use quote::quote;

use super::emit_whitespace_segment;
use crate::backend::rust::emitter::{RustEmitCtx, RustEmitter};

impl RustEmitter {
    pub(in crate::backend::rust::emitter) fn emit_prettify_seq_impl(
        &mut self,
        children: Vec<TokenStream>,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        if children.is_empty() {
            return quote! {};
        }
        if children.len() == 1 {
            return children.into_iter().next().unwrap();
        }
        quote! { { #(#children;)* } }
    }

    pub(in crate::backend::rust::emitter) fn emit_prettify_skip_impl(
        &mut self,
        left: TokenStream,
        right: TokenStream,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // Both sides emitted for their effects (parse + format).
        quote! { {
            #left;
            #right;
        } }
    }

    pub(in crate::backend::rust::emitter) fn emit_prettify_next_impl(
        &mut self,
        left: TokenStream,
        right: TokenStream,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // Both sides emitted for their effects (parse + format).
        quote! { {
            #left;
            #right;
        } }
    }

    pub(in crate::backend::rust::emitter) fn emit_prettify_optional_ws_impl(
        &mut self,
        inner: TokenStream,
        is_atomic: bool,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // Emit ws trim code. In prettify context, OptionalWhitespace nodes
        // contain the ws pattern baked into the IR via the @ws directive lowering.
        // The actual trim comes from the standard whitespace trimmer.
        let ws_trim = quote! {
            ::parse_that::trim_leading_whitespace_mut(state);
        };

        if is_atomic {
            // Deferred pattern: scan leading ws, try inner, then emit ws only
            // after inner succeeds. No checkpoint needed because the inner
            // expression fails atomically (before emitting any ops).
            let ws1 = ctx.fresh("ows");
            let ws2 = ctx.fresh("ows");
            let ws3 = ctx.fresh("ows");
            quote! { {
                let #ws1 = state.offset;
                #ws_trim
                let #ws2 = state.offset;
                #inner;
                __builder.text_inline_ws(&state.src[#ws1..#ws2]);
                let #ws3 = state.offset;
                #ws_trim
                __builder.text_inline_ws(&state.src[#ws3..state.offset]);
            } }
        } else {
            // Non-atomic inner: must checkpoint because the inner expression
            // might emit ops before failing. Leading ws is emitted eagerly.
            let ws_start1 = ctx.fresh("ows");
            let ws_start2 = ctx.fresh("ows");
            let ws_emit1 = emit_whitespace_segment(&ws_start1);
            let ws_emit2 = emit_whitespace_segment(&ws_start2);
            let body = quote! {{
                let #ws_start1 = state.offset;
                #ws_trim
                #ws_emit1
                #inner;
                let #ws_start2 = state.offset;
                #ws_trim
                #ws_emit2
            }};
            let body_try = self.emit_prettify_attempt_impl(body, true, false, ctx);
            quote! { {
                if !#body_try {
                    return false;
                }
            } }
        }
    }
}
