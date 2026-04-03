//! Prettify entry-point generation.

use bbnf_ir::GrammarIR;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::policy::{build_rule_plans, emit_rule_wrapper, emit_whitespace_segment, emit_ws_rule_body, split_compile_error};
use super::super::ir_types::IrCodegenCtx;
use super::{emit_prettify_expr, new_prettify_ctx, new_prettify_mctx, prettify_fn_ident};

pub(crate) fn generate_monolithic_prettify(ir: &GrammarIR, ctx: &IrCodegenCtx<'_>) -> TokenStream {
    let plans = build_rule_plans(ir);
    let pctx = new_prettify_ctx(ir, ctx, &plans);
    let mut methods = Vec::with_capacity(ir.rules.len());

    for rule in &ir.rules {
        let name = ir.get_string(rule.name);
        let fn_ident = prettify_fn_ident(name);
        let pub_ident = format_ident!("{}_prettify", name);
        let mut mctx = new_prettify_mctx();
        let plan = pctx.plan(rule.id);

        let body = if let Some(split) = plan.policy.split.as_ref() {
            split_compile_error(name, split)
        } else if plan.policy.is_ws_rule {
            let ws_start = mctx.fresh("ws_start");
            let body_expr = emit_prettify_expr(&rule.body, &pctx, rule.id, &mut mctx);
            let body_try = emit_ws_rule_body(body_expr, &mut mctx);
            let ws_emit = emit_whitespace_segment(&ws_start);
            quote! {{
                let #ws_start = state.offset;
                if !#body_try {
                    return false;
                }
                #ws_emit
                true
            }}
        } else {
            let body_expr = emit_prettify_expr(&rule.body, &pctx, rule.id, &mut mctx);
            emit_rule_wrapper(&plan.policy, quote! {{
                #body_expr;
                true
            }})
        };

        methods.push(quote! {
            #[allow(non_snake_case)]
            fn #fn_ident<'a>(
                state: &mut ::parse_that::ParserState<'a>,
                __builder: &mut ::pprint::FmtBuilder<'a>,
            ) -> bool {
                #body
            }
        });

        methods.push(quote! {
            pub fn #pub_ident<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
                Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                    let mut __builder =
                        ::pprint::FmtBuilder::with_capacity(state.src.len().saturating_mul(2));
                    if !Self::#fn_ident(state, &mut __builder) {
                        return None;
                    }
                    Some(__builder.finish())
                })
            }
        });
    }

    quote! {
        #(#methods)*
    }
}
