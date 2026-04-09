//! Leaf-op emission for the Rust backend: literals, regex, epsilon, and the
//! all-Span Seq fast path.
//!
//! Each method is `pub(super)` so the trait impl in `mod.rs` can delegate
//! to it via `self.emit_xxx_impl(...)`.

use bbnf_ir::GrammarIR;
use proc_macro2::TokenStream;
use quote::quote;

use super::{RustEmitCtx, RustEmitter};

impl RustEmitter {
    pub(super) fn emit_literal_match_impl(
        &mut self,
        value: &str,
        guaranteed_byte: Option<u8>,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let unescaped = value.to_string();
        let bytes = unescaped.as_bytes();

        if let Some(_byte) = guaranteed_byte {
            // Dispatch already proved this byte — just advance.
            return quote! {
                {
                    let __start = state.offset;
                    state.offset += 1;
                    Some(::parse_that::Span::new(__start, state.offset, state.src))
                }
            };
        }

        if bytes.len() == 1 {
            let byte = bytes[0];
            quote! {
                if state.offset < state.src.len()
                    && state.src.as_bytes()[state.offset] == #byte
                {
                    let __start = state.offset;
                    state.offset += 1;
                    Some(::parse_that::Span::new(__start, state.offset, state.src))
                } else {
                    None
                }
            }
        } else {
            let lit = proc_macro2::Literal::string(&unescaped);
            let len = bytes.len();
            quote! {
                if state.src[state.offset..].starts_with(#lit) {
                    let __start = state.offset;
                    state.offset += #len;
                    Some(::parse_that::Span::new(__start, state.offset, state.src))
                } else {
                    None
                }
            }
        }
    }

    pub(super) fn emit_regex_match_impl(
        &mut self,
        pattern: &str,
        _regex_id: usize,
        ir: &GrammarIR,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let opts =
            crate::generate::regex::EmitOpts::new(&crate::generate::regex::CostModel::DEFAULT)
                .with_fuse(!self.effective_prettify)
                .with_ir(ir);
        crate::generate::regex::emit_regex(pattern, &opts)
    }

    pub(super) fn emit_epsilon_impl(&mut self, _ctx: &mut RustEmitCtx) -> TokenStream {
        quote! { Some(::parse_that::Span::new(state.offset, state.offset, state.src)) }
    }

    pub(super) fn emit_seq_all_span_impl(
        &mut self,
        child_outputs: Vec<TokenStream>,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // All children are Span — emit for side effects, return combined Span.
        quote! {
            (|| {
                let __sp_start = state.offset;
                #( #child_outputs?; )*
                Some(::parse_that::Span::new(__sp_start, state.offset, state.src))
            })()
        }
    }
}
