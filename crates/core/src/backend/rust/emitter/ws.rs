//! Whitespace trimming emission for the Rust backend.
//!
//! Tranche AC.2 tape-first, AN.3.1 SIMD routing fix.
//! Whitespace is always side-effecting: the scan/trim advances
//! `state.offset` without producing a tape record.  Every shape
//! returns `Option<()>`.
//!
//! When the `@ws` pattern classifies as `WhitespaceWithBlockComment`
//! (CSS comment-aware whitespace), we emit the kernel call
//! (`scan_ws_block_comments`) directly — bypassing the full
//! `emit_regex` pipeline that previously fell through to a
//! 147-line inline HIR expansion duplicated at every call site.

use proc_macro2::TokenStream;
use quote::quote;

use super::RustEmitCtx;
use super::RustEmitter;

/// Emit the ws-trim code for a custom `@ws` pattern.  Returns the
/// kernel call when the pattern is the comment-aware whitespace
/// family, otherwise falls through to `emit_regex`.
fn emit_ws_pattern(pattern: &str) -> TokenStream {
    use parse_that::regex::classify::{RegexClass, classify_regex};
    if matches!(classify_regex(pattern), RegexClass::WhitespaceWithBlockComment) {
        let call = crate::backend::kernels::comment_ws::emit_call();
        return quote! { let _ = #call; };
    }
    let opts =
        crate::generate::regex::EmitOpts::new(&crate::generate::regex::CostModel::DEFAULT);
    let code = crate::generate::regex::emit_regex(pattern, &opts);
    quote! { let _ = #code; }
}

impl RustEmitter {
    pub(super) fn emit_ws_trim_impl(
        &mut self,
        ws_pattern: Option<&str>,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        if let Some(pattern) = ws_pattern {
            let trim = emit_ws_pattern(pattern);
            quote! { { #trim Some(()) } }
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
            emit_ws_pattern(pattern)
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
