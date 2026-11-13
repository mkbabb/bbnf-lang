//! Prettify codegen for rule functions and whole-grammar assembly.

use bbnf_ir::{GrammarIR, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::{
    emit_rule_wrapper, emit_trivia_fn, emit_trivia_segment, prettify_fn_ident,
    split_compile_error,
};
use crate::backend::prettify::PrettyPolicy;
use crate::backend::rust::emitter::{RustEmitCtx, RustEmitter};

impl RustEmitter {
    pub(in crate::backend::rust::emitter) fn emit_prettify_rule_function_impl(
        &mut self,
        rule: &IrRule,
        body: TokenStream,
        policy: &PrettyPolicy,
        ir: &GrammarIR,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let name = ir.get_string(rule.name);
        let fn_ident = prettify_fn_ident(name);
        let pub_ident = format_ident!("{}_prettify", name);

        let fn_body = if let Some(split) = policy.split.as_ref() {
            split_compile_error(name, split)
        } else if policy.is_ws_rule {
            // Ws rule: run body under light checkpoint to discard builder ops,
            // then re-emit the consumed span as trivia.
            let ws_start = ctx.fresh("ws_start");
            let ws_cp = ctx.fresh("ws_cp");
            let ws_emit = emit_trivia_segment(&ws_start);
            quote! {{
                let #ws_start = state.offset;
                let #ws_cp = __builder.light_checkpoint();
                let __ok = (|| -> bool { #body; true })();
                __builder.light_restore(#ws_cp);
                if !__ok {
                    return false;
                }
                #ws_emit
                true
            }}
        } else {
            emit_rule_wrapper(
                policy,
                quote! {{
                    #body;
                    true
                }},
            )
        };

        let mut methods = TokenStream::new();

        methods.extend(quote! {
            fn #fn_ident<'a>(
                state: &mut ::parse_that::ParserState<'a>,
                __builder: &mut ::pprint::FmtBuilder<'a>,
            ) -> bool {
                #fn_body
            }
        });

        methods.extend(quote! {
            pub fn #pub_ident<'a>() -> Parser<'a, ::pprint::Doc<'a>> {
                Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                    let mut __builder =
                        ::pprint::FmtBuilder::with_capacity(state.src.len().saturating_mul(2));
                    if !Self::#fn_ident(state, &mut __builder) {
                        return None;
                    }
                    __builder.finish().ok()
                })
            }
        });

        methods
    }

    pub(in crate::backend::rust::emitter) fn emit_prettify_grammar_impl(
        &mut self,
        rule_functions: Vec<TokenStream>,
        ir: &GrammarIR,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let trivia_fn = ir.ws_pattern.map(|sid| {
            let pattern = ir.get_string(sid);
            let opts =
                crate::generate::regex::EmitOpts::new(&crate::generate::regex::CostModel::DEFAULT)
                    .with_ir(ir)
                    .with_ws_pattern(Some(pattern));
            emit_trivia_fn(crate::generate::regex::emit_regex(pattern, &opts))
        });
        quote! { #trivia_fn #(#rule_functions)* }
    }
}
