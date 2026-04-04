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
        elem_type: &TypeDesc,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let lo_lit = lo as usize;

        // Span case: collapse to a single Span covering all iterations.
        if *elem_type == TypeDesc::Span {
            let count_var = ctx.fresh("count");
            let check = if lo == 0 {
                quote! { Some(::parse_that::Span::new(__sp_start, state.offset, state.src)) }
            } else {
                quote! {
                    if #count_var >= #lo_lit {
                        Some(::parse_that::Span::new(__sp_start, state.offset, state.src))
                    } else {
                        None
                    }
                }
            };
            return quote! {
                {
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
                    #check
                }
            };
        }

        // Typed case: scratch-based slab collection.
        let ir_ctx = ctx.ir_ctx();
        let depth_var = ctx.fresh("depth");
        let prev_var = ctx.fresh("prev");
        let init_code = ir_ctx.emit_scratch_init(elem_type, &depth_var);
        let push_code = ir_ctx.emit_scratch_push(elem_type, &quote! { __value });
        let count_expr = ir_ctx.emit_scratch_count(elem_type, &depth_var);
        let collect_expr = ir_ctx.emit_scratch_collect(elem_type, &depth_var);
        let truncate_expr = ir_ctx.emit_scratch_truncate(elem_type, &depth_var);

        let check = if lo == 0 {
            quote! { Some(#collect_expr) }
        } else {
            quote! {
                if #count_expr >= #lo_lit {
                    Some(#collect_expr)
                } else {
                    #truncate_expr
                    None
                }
            }
        };

        quote! {
            {
                #init_code
                loop {
                    let #prev_var = state.offset;
                    match #body {
                        Some(__value) => {
                            #push_code;
                            if state.offset == #prev_var { break; }
                        }
                        None => {
                            state.offset = #prev_var;
                            break;
                        }
                    }
                }
                #check
            }
        }
    }

    pub(super) fn emit_repeat_optional_impl(
        &mut self,
        body: TokenStream,
        inner_type: &TypeDesc,
        alloc: AllocStrategy,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // Span case: Optional(Span) collapses to Span — zero-width on failure.
        if *inner_type == TypeDesc::Span {
            return quote! {
                {
                    let __cp = state.offset;
                    if (|| #body)().is_none() {
                        state.offset = __cp;
                    }
                    Some(::parse_that::Span::new(__cp, state.offset, state.src))
                }
            };
        }

        // BoxedEnum case: slab-allocate on success when alloc is requested.
        if matches!(inner_type, TypeDesc::BoxedEnum) && alloc == AllocStrategy::Alloc {
            let ir_ctx = ctx.ir_ctx();
            let alloc_expr = ir_ctx.emit_alloc(&quote! { __v });
            return quote! {
                {
                    let __cp = state.offset;
                    match (|| #body)() {
                        Some(__v) => Some(Some(#alloc_expr)),
                        None => {
                            state.offset = __cp;
                            Some(None)
                        }
                    }
                }
            };
        }

        // General case: Option<T>.
        quote! {
            {
                let __cp = state.offset;
                match (|| #body)() {
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
        elem_type: &TypeDesc,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
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

        // Span case: collapse to a single Span.
        if *elem_type == TypeDesc::Span {
            let count_var = ctx.fresh("count");
            return quote! {
                (|| {
                    let __sp_start = state.offset;
                    let mut #count_var: usize = 0;

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
            };
        }

        // Typed case: scratch-based slab collection.
        let ir_ctx = ctx.ir_ctx();
        let depth_var = ctx.fresh("depth");
        let count_var = ctx.fresh("count");
        let init_code = ir_ctx.emit_scratch_init(elem_type, &depth_var);
        let push_code = ir_ctx.emit_scratch_push(elem_type, &quote! { __value });
        let count_expr = ir_ctx.emit_scratch_count(elem_type, &depth_var);
        let collect_expr = ir_ctx.emit_scratch_collect(elem_type, &depth_var);
        let truncate_expr = ir_ctx.emit_scratch_truncate(elem_type, &depth_var);

        quote! {
            (|| {
                #init_code
                let mut #count_var: usize = 0;

                #terminator_check
                match #element {
                    Some(__value) => {
                        #push_code;
                        #count_var += 1;
                    }
                    None => {
                        return if #count_var >= #lo_lit {
                            Some(#collect_expr)
                        } else {
                            #truncate_expr
                            None
                        };
                    }
                }

                loop {
                    #terminator_check
                    let __cp = state.offset;
                    match #separator {
                        Some(_) => {}
                        None => break,
                    }
                    match #element {
                        Some(__value) => {
                            #push_code;
                            #count_var += 1;
                        }
                        None => {
                            state.offset = __cp;
                            break;
                        }
                    }
                }

                if #count_expr >= #lo_lit {
                    Some(#collect_expr)
                } else {
                    #truncate_expr
                    None
                }
            })()
        }
    }
}
