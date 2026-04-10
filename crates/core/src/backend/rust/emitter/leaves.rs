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
                if state.offset < state.src_bytes.len()
                    && state.src_bytes[state.offset] == #byte
                {
                    let __start = state.offset;
                    state.offset += 1;
                    Some(::parse_that::Span::new(__start, state.offset, state.src))
                } else {
                    None
                }
            }
        } else {
            // Byte-array literal equality: load N bytes as `[u8; N]`
            // and compare with `*b"..."`. LLVM lowers this to a
            // single iN load + icmp for N in {2,4,8} and a
            // half-word + tail-byte combo for N in {3,5,6,7},
            // never invoking memcmp. UTF-8 length validation and
            // slice bounds checking are bypassed.
            //
            // Prior attempts using `state.src[..].starts_with(#str)`
            // or `state.src.as_bytes()[..].starts_with(#bytes as &[u8])`
            // both lose the compile-time length at the
            // `slice::starts_with` specialization and fall through
            // to runtime-length `memcmp`, which LLVM's memcmp-
            // expansion pass only occasionally folds back.
            let len = bytes.len();
            let lit = proc_macro2::Literal::byte_string(bytes);
            quote! {
                if state.offset + #len <= state.src_bytes.len()
                    && unsafe {
                        *(state.src_bytes.as_ptr().add(state.offset)
                            as *const [u8; #len])
                    } == *#lit
                {
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
