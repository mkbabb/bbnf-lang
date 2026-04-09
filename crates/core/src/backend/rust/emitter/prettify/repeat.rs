//! Prettify codegen for repetition (many, optional, bounded).

use proc_macro2::TokenStream;
use quote::quote;

use super::emit_separator_ops;
use crate::backend::prettify::{PrettyPolicy, SeparatorPolicy};
use crate::backend::rust::emitter::{RustEmitCtx, RustEmitter};

impl RustEmitter {
    pub(in crate::backend::rust::emitter) fn emit_prettify_repeat_impl(
        &mut self,
        body: TokenStream,
        lo: u32,
        hi: u32,
        policy: &PrettyPolicy,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let lo = lo as usize;
        let hi = hi as usize;

        // Optional: lo=0, hi=1
        if lo == 0 && hi == 1 {
            let inner_try = self.emit_prettify_attempt_impl(body, true, false, ctx);
            return quote! { {
                let _ = #inner_try;
                true
            } };
        }

        let sep_expr = emit_separator_ops(policy);
        let has_separator = !matches!(policy.separator, SeparatorPolicy::None);

        let inner_try = if has_separator {
            // With separator: always use full checkpoint to undo sep on fail.
            self.emit_prettify_attempt_impl(body, false, false, ctx)
        } else {
            self.emit_prettify_attempt_impl(body, true, false, ctx)
        };

        let count_var = ctx.fresh("rep_count");
        let cp_var = ctx.fresh("rep_cp");
        let loop_start_state = if lo > 0 {
            Some(ctx.fresh("rep_start"))
        } else {
            None
        };
        let loop_start_builder = if lo > 0 {
            Some(ctx.fresh("rep_bcp"))
        } else {
            None
        };

        let lo_check = if lo > 0 {
            let lo_lit = proc_macro2::Literal::usize_unsuffixed(lo);
            if let (Some(start_state), Some(start_builder)) =
                (&loop_start_state, &loop_start_builder)
            {
                quote! {
                    if #count_var < #lo_lit {
                        state.offset = #start_state;
                        __builder.restore(#start_builder);
                        return false;
                    }
                }
            } else {
                quote! {
                    if #count_var < #lo_lit {
                        return false;
                    }
                }
            }
        } else {
            quote! {}
        };

        let hi_check = if hi < usize::MAX {
            let hi_lit = proc_macro2::Literal::usize_unsuffixed(hi);
            quote! { #count_var < #hi_lit }
        } else {
            quote! { true }
        };

        let loop_cp = if let (Some(start_state), Some(start_builder)) =
            (&loop_start_state, &loop_start_builder)
        {
            quote! {
                let #start_state = state.offset;
                let #start_builder = __builder.checkpoint();
            }
        } else {
            quote! {}
        };

        if has_separator {
            // With separator: checkpoint covers sep + inner so we can undo the
            // separator if the inner expression fails on the next iteration.
            quote! { {
                #loop_cp
                let mut #count_var = 0usize;
                while #hi_check {
                    let #cp_var = state.offset;
                    let __iter_cp = if #count_var > 0 {
                        Some(__builder.checkpoint())
                    } else {
                        None
                    };
                    if #count_var > 0 {
                        #sep_expr
                    };
                    if !#inner_try {
                        state.offset = #cp_var;
                        if let Some(__bcp) = __iter_cp {
                            __builder.restore(__bcp);
                        }
                        break;
                    }
                    if state.offset == #cp_var {
                        break;
                    }
                    #count_var += 1;
                }
                #lo_check
            } }
        } else {
            // No separator: simplified loop with just state checkpoint.
            quote! { {
                #loop_cp
                let mut #count_var = 0usize;
                while #hi_check {
                    let #cp_var = state.offset;
                    if !#inner_try {
                        state.offset = #cp_var;
                        break;
                    }
                    if state.offset == #cp_var {
                        break;
                    }
                    #count_var += 1;
                }
                #lo_check
            } }
        }
    }
}
