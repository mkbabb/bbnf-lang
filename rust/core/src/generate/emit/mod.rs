//! Concrete-type-driven emission codegen.
//!
//! Recurses on syn::Type (the actual Rust type). No abstraction gap.

mod emit;

use bbnf_ir::GrammarIR;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::generate::ir_types::IrCodegenCtx;

pub fn generate_emit_methods(ir: &GrammarIR, ctx: &IrCodegenCtx) -> TokenStream {
    let mut methods = Vec::new();
    let enum_ident = &ctx.enum_ident;

    for rule in &ir.rules {
        let rule_name = ir.get_string(rule.name);
        let fn_ident = format_ident!("{}_emit", rule_name);
        let rule_type = ctx.rule_types.get(&rule.id)
            .cloned()
            .unwrap_or_else(|| ctx.enum_type.clone());

        let type_desc = ir.types.iter()
            .find(|(id, _)| *id == rule.id)
            .map(|(_, td)| td.clone())
            .unwrap_or(bbnf_ir::TypeDesc::Span);

        let val = quote! { __v };
        let body = emit::emit_for_type(&type_desc, &val, ir, ctx);

        // If the rule type is already a reference (BoxedEnum → &'a Enum),
        // take it directly. Otherwise take &rule_type.
        let is_ref = matches!(&type_desc, bbnf_ir::TypeDesc::BoxedEnum);
        let param = if is_ref {
            quote! { #val: #rule_type }
        } else {
            quote! { #val: &#rule_type }
        };

        methods.push(quote! {
            pub fn #fn_ident<'a, __S: ::bbnf_emit::EmitSink<'a>>(
                #param,
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
            // Transparent entry: value IS the enum.
            // The entry emit fn takes &&Enum (ref to the ref).
            // We need to bind __v to a let so the & borrow lives long enough.
            // Transparent entry: rule type is BoxedEnum (already a ref).
            // emit fn takes it directly (no extra &).
            methods.push(quote! {
                pub fn emit_compact<'a>(__v: #boxed_enum) -> String {
                    let mut __sink = ::bbnf_emit::StringSink::new();
                    Self::#emit_fn(__v, &mut __sink);
                    __sink.finish()
                }
                pub fn emit<'a, __S: ::bbnf_emit::EmitSink<'a>>(
                    __v: #boxed_enum, __sink: &mut __S,
                ) {
                    Self::#emit_fn(__v, __sink);
                }
            });
        } else {
            let variant = format_ident!("{}", entry_name);
            methods.push(quote! {
                pub fn emit_compact<'a>(__v: #boxed_enum) -> String {
                    let mut __sink = ::bbnf_emit::StringSink::new();
                    if let #enum_ident::#variant(ref __inner) = *__v {
                        Self::#emit_fn(__inner, &mut __sink);
                    }
                    __sink.finish()
                }
                pub fn emit<'a, __S: ::bbnf_emit::EmitSink<'a>>(
                    __v: #boxed_enum, __sink: &mut __S,
                ) {
                    if let #enum_ident::#variant(ref __inner) = *__v {
                        Self::#emit_fn(__inner, __sink);
                    }
                }
            });
        }
    }

    quote! { #(#methods)* }
}
