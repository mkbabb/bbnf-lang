//! Prettify codegen for Literal and Regex leaf expressions.

use bbnf_ir::GrammarIR;
use proc_macro2::TokenStream;
use quote::quote;

use crate::backend::rust::emitter::{RustEmitCtx, RustEmitter};

impl RustEmitter {
    pub(in crate::backend::rust::emitter) fn emit_prettify_literal_impl(
        &mut self,
        value: &str,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let unescaped = value.to_string();
        let bytes = unescaped.as_bytes();
        if bytes.len() == 1 {
            let b = bytes[0];
            let lit = proc_macro2::Literal::byte_character(b);
            quote! { {
                if state.src_bytes.get(state.offset).copied() != Some(#lit) {
                    return false;
                }
                state.offset += 1;
                __builder.char(#lit);
            } }
        } else {
            let s = proc_macro2::Literal::string(&unescaped);
            let len = unescaped.len();
            quote! { {
                let __s = #s;
                let __bytes = __s.as_bytes();
                let __slc = match state.src_bytes.get(state.offset..) {
                    Some(s) if s.len() >= #len => s,
                    _ => return false,
                };
                if &__slc[..#len] != __bytes {
                    return false;
                }
                __builder.text(&state.src[state.offset..state.offset + #len]);
                state.offset += #len;
            } }
        }
    }

    pub(in crate::backend::rust::emitter) fn emit_prettify_regex_impl(
        &mut self,
        pattern: &str,
        _regex_id: usize,
        ir: &GrammarIR,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let ws_pat = ir.ws_pattern.map(|sid| ir.get_string(sid));
        let opts =
            crate::generate::regex::EmitOpts::new(&crate::generate::regex::CostModel::DEFAULT)
                .with_ir(ir)
                .with_ws_pattern(ws_pat);
        let code = crate::generate::regex::emit_regex(pattern, &opts);
        if ws_pat == Some(pattern) {
            let emit = super::emit_trivia_segment(&syn::Ident::new(
                "__start",
                proc_macro2::Span::call_site(),
            ));
            return quote! { {
                let __start = state.offset;
                if #code.is_none() { return false; };
                #emit
            } };
        }
        // A leaf whose match ends in a whitespace run holding a line feed
        // (an opaque span such as css's `valueSpan` before `}`) prints that
        // run as one space: line feeds belong to the layout, and a raw one
        // beside the layout's own grew a blank line on every pass (sk-v27
        // LW3.S3c-PP). The space keeps the leaf apart from the next token;
        // the renderer trims it at a line end.
        quote! { {
            let __start = state.offset;
            if #code.is_none() { return false; };
            let __matched = &state.src[__start..state.offset];
            let __body = __matched.trim_end_matches(char::is_whitespace);
            if __matched[__body.len()..].contains('\n') {
                __builder.text(__body);
                __builder.text(" ");
            } else if !__matched.is_empty() {
                __builder.text(__matched);
            }
        } }
    }
}
