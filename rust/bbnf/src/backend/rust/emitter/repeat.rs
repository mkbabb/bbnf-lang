//! Repetition emission for the shared-driver Rust emitter.

use bbnf_ir::TypeDesc;
use proc_macro2::TokenStream;
use quote::quote;

use crate::backend::{AllocStrategy, SepByConfig};

use super::RustEmitCtx;
use super::RustEmitter;

impl RustEmitter {
    pub(super) fn emit_repeat_many_impl(
        &mut self,
        body: TokenStream,
        lo: u32,
        _hi: u32,
        _elem_type: &TypeDesc,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let count_var = ctx.fresh("count");
        let lo_lit = lo as usize;
        quote! {
            (|| {
                let __sp_start = state.offset;
                let mut #count_var: usize = 0;
                loop {
                    let __prev = state.offset;
                    match #body {
                        Some(_) => {
                            #count_var += 1;
                            if state.offset == __prev { break; }
                        }
                        None => break,
                    }
                }
                if #count_var >= #lo_lit {
                    Some(::parse_that::Span::new(__sp_start, state.offset, state.src))
                } else {
                    None
                }
            })()
        }
    }

    pub(super) fn emit_repeat_optional_impl(
        &mut self,
        body: TokenStream,
        _inner_type: &TypeDesc,
        _alloc: AllocStrategy,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        quote! {
            {
                let __cp = state.offset;
                match #body {
                    Some(__v) => Some(Some(__v)),
                    None => {
                        state.offset = __cp;
                        Some(None)
                    }
                }
            }
        }
    }

    pub(super) fn emit_sep_by_impl(
        &mut self,
        element: TokenStream,
        separator: TokenStream,
        config: &SepByConfig,
        _elem_type: &TypeDesc,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let count_var = ctx.fresh("count");
        let lo_lit = config.lo as usize;

        // Terminator byte early-exit check.
        let terminator_check = if let Some(ref tb) = config.terminator_bytes {
            if tb.len() == 1 {
                let byte = tb[0];
                quote! {
                    if state.offset < state.src.len()
                        && state.src.as_bytes()[state.offset] == #byte
                    {
                        break;
                    }
                }
            } else {
                quote! {}
            }
        } else {
            quote! {}
        };

        quote! {
            (|| {
                let __sp_start = state.offset;
                let mut #count_var: usize = 0;

                // First element.
                #terminator_check
                match #element {
                    Some(_) => { #count_var += 1; }
                    None => {
                        return if #count_var >= #lo_lit {
                            Some(::parse_that::Span::new(__sp_start, state.offset, state.src))
                        } else {
                            None
                        };
                    }
                }

                // Separator + element loop.
                loop {
                    #terminator_check
                    let __cp = state.offset;
                    match #separator {
                        Some(_) => {}
                        None => break,
                    }
                    match #element {
                        Some(_) => { #count_var += 1; }
                        None => {
                            state.offset = __cp;
                            break;
                        }
                    }
                }

                if #count_var >= #lo_lit {
                    Some(::parse_that::Span::new(__sp_start, state.offset, state.src))
                } else {
                    None
                }
            })()
        }
    }
}
