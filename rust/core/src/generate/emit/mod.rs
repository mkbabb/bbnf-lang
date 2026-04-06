//! Emission codegen: shared-decision EmitPlan + trivial codegen walk.
//!
//! 1. `plan.rs` consumes `decisions::decide_*()` to build an EmitPlan tree
//! 2. `codegen.rs` walks the EmitPlan → TokenStream (zero type queries)

pub mod plan;
pub mod codegen;

use bbnf_ir::GrammarIR;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::generate::ir_types::IrCodegenCtx;

/// Generate emit methods for all rules in the grammar.
pub fn generate_emit_methods(ir: &GrammarIR, ctx: &IrCodegenCtx) -> TokenStream {
    let mut methods = Vec::new();
    let enum_ident = &ctx.enum_ident;

    for rule in &ir.rules {
        let rule_name = ir.get_string(rule.name);
        let fn_ident = format_ident!("{}_emit", rule_name);
        let rule_type = ctx.rule_types.get(&rule.id)
            .cloned()
            .unwrap_or_else(|| ctx.enum_type.clone());

        let emit_plan = plan::compute_rule_plan(rule, ir, ctx);
        let val = quote! { __v };
        let body = codegen::emit_from_plan(&emit_plan, &val, ctx);

        methods.push(quote! {
            pub fn #fn_ident<'a, __S: ::bbnf_emit::EmitSink<'a>>(
                #val: &#rule_type,
                __sink: &mut __S,
            ) {
                #body
            }
        });
    }

    // Entry point.
    {
        let entry_rule = &ir.rules[ir.entry as usize];
        let entry_name = ir.get_string(entry_rule.name);
        let emit_fn = format_ident!("{}_emit", entry_name);
        let boxed_enum = &ctx.boxed_enum_type;

        if entry_rule.meta.is_transparent {
            methods.push(quote! {
                pub fn emit_compact<'a>(__v: #boxed_enum) -> String {
                    let mut __sink = ::bbnf_emit::StringSink::new();
                    Self::#emit_fn(&__v, &mut __sink);
                    __sink.finish()
                }
                pub fn emit<'a, __S: ::bbnf_emit::EmitSink<'a>>(
                    __v: #boxed_enum, __sink: &mut __S,
                ) {
                    Self::#emit_fn(&__v, __sink);
                }
            });
        } else {
            let variant = format_ident!("{}", entry_name);
            methods.push(quote! {
                pub fn emit_compact<'a>(__v: #boxed_enum) -> String {
                    let mut __sink = ::bbnf_emit::StringSink::new();
                    if let #enum_ident::#variant(__inner) = __v {
                        Self::#emit_fn(__inner, &mut __sink);
                    }
                    __sink.finish()
                }
                pub fn emit<'a, __S: ::bbnf_emit::EmitSink<'a>>(
                    __v: #boxed_enum, __sink: &mut __S,
                ) {
                    if let #enum_ident::#variant(__inner) = __v {
                        Self::#emit_fn(__inner, __sink);
                    }
                }
            });
        }
    }

    quote! { #(#methods)* }
}
