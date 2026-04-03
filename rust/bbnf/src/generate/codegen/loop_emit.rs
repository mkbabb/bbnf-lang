use proc_macro2::TokenStream;
use quote::quote;

/// Shared skeleton for parse loops that checkpoint `state.offset`, try one
/// iteration body, restore on failure, and then run a caller-provided finish
/// expression.
pub(super) struct RestoringLoop<'a> {
    pub init: TokenStream,
    pub prev_var: &'a syn::Ident,
    pub step: TokenStream,
    pub on_success: TokenStream,
    pub on_failure: TokenStream,
    pub finish: TokenStream,
}

pub(super) fn emit_restoring_loop(spec: RestoringLoop<'_>) -> TokenStream {
    let RestoringLoop {
        init,
        prev_var,
        step,
        on_success,
        on_failure,
        finish,
    } = spec;

    quote! {
        {
            #init
            loop {
                let #prev_var = state.offset;
                match #step {
                    Some(__loop_value) => {
                        let __value = __loop_value;
                        #on_success
                    }
                    None => {
                        state.offset = #prev_var;
                        #on_failure
                    }
                }
            }
            #finish
        }
    }
}
