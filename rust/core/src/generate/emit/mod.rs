//! Type-only emission codegen.
//!
//! Recurse on collapsed TypeDesc. The type IS the value.
//! No plan. No decisions. No IR walking.

mod emit;

use bbnf_ir::GrammarIR;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::generate::ir_types::{IrCodegenCtx, type_desc_to_syn};

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

        // The type of this rule's value — drives the entire emit.
        let type_desc = ir.types.iter()
            .find(|(id, _)| *id == rule.id)
            .map(|(_, td)| td.clone())
            .unwrap_or(bbnf_ir::TypeDesc::Span);

        let val = quote! { __v };
        let body = emit::emit_type(&type_desc, &val, ir, ctx);

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
