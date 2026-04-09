//! Prettify codegen for atomic attempts and non-terminal references.

use bbnf_ir::{GrammarIR, RuleId};
use proc_macro2::TokenStream;
use quote::quote;

use super::prettify_fn_ident;
use crate::backend::prettify::PrettyRulePlan;
use crate::backend::rust::emitter::{RustEmitCtx, RustEmitter};

impl RustEmitter {
    pub(in crate::backend::rust::emitter) fn emit_prettify_ref_impl(
        &mut self,
        _rule_id: RuleId,
        rule_name: &str,
        plan: &PrettyRulePlan,
        _ir: &GrammarIR,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // For the emitter-trait path, the driver handles inlining by compiling
        // the rule body directly and not calling this method for inline rules.
        // This method generates function calls for non-inlined rules and
        // ws-rule references.
        //
        // Ws-rule refs: the generated __ws_prettify already does the ws scan +
        // discard + text_inline_ws pattern, so a direct function call is correct.
        // Inline refs: the driver compiles the body and returns it directly
        // without reaching this method.

        if plan.inline {
            // The driver should have compiled the body inline. If we reach here,
            // generate the function call as a safety net.
        }

        let fn_ident = prettify_fn_ident(rule_name);
        quote! {
            if !Self::#fn_ident(state, __builder) {
                return false;
            }
        }
    }

    pub(in crate::backend::rust::emitter) fn emit_prettify_attempt_impl(
        &mut self,
        expr: TokenStream,
        rollback_builder: bool,
        use_light: bool,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let state_cp = ctx.fresh("pretty_cp");
        if rollback_builder {
            let builder_cp = ctx.fresh("pretty_bcp");
            if use_light {
                quote! {{
                    let #state_cp = state.offset;
                    let #builder_cp = __builder.light_checkpoint();
                    let __ok = (|| -> bool { #expr; true })();
                    if !__ok {
                        state.offset = #state_cp;
                        __builder.light_restore(#builder_cp);
                    }
                    __ok
                }}
            } else {
                quote! {{
                    let #state_cp = state.offset;
                    let #builder_cp = __builder.checkpoint();
                    let __ok = (|| -> bool { #expr; true })();
                    if !__ok {
                        state.offset = #state_cp;
                        __builder.restore(#builder_cp);
                    }
                    __ok
                }}
            }
        } else {
            quote! {{
                let #state_cp = state.offset;
                let __ok = (|| -> bool { #expr; true })();
                if !__ok {
                    state.offset = #state_cp;
                }
                __ok
            }}
        }
    }
}
