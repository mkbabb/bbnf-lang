//! Concrete-type-driven emission codegen.
//!
//! Recurses on syn::Type (the actual Rust type). No abstraction gap.

mod serialize;

use bbnf_ir::GrammarIR;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::generate::ir_types::IrCodegenCtx;

pub fn generate_serialize_methods(ir: &GrammarIR, ctx: &IrCodegenCtx) -> TokenStream {
    let mut methods = Vec::new();
    let enum_ident = &ctx.enum_ident;
    let boxed_enum = &ctx.boxed_enum_type;

    // Per-rule emit functions.
    for rule in &ir.rules {
        let rule_name = ir.get_string(rule.name);
        let fn_ident = format_ident!("serialize_{}", rule_name);
        let rule_type = ctx.rule_types.get(&rule.id)
            .cloned()
            .unwrap_or_else(|| ctx.enum_type.clone());

        let type_desc = ir.types.iter()
            .find(|(id, _)| *id == rule.id)
            .map(|(_, td)| td.clone())
            .unwrap_or(bbnf_ir::TypeDesc::Span);

        let val = quote! { __v };
        let body = serialize::serialize_for_node(&rule.body, &type_desc, &val, ir, ctx);

        // Ref-type rules (BoxedEnum, Vec) take the type directly.
        // Value-type rules take &type.
        let param = if serialize::type_desc_is_ref(&type_desc) {
            quote! { #val: #rule_type }
        } else {
            quote! { #val: &#rule_type }
        };

        methods.push(quote! {
            pub fn #fn_ident<'a, __S: ::bbnf_ser::Serializer>(
                #param,
                __ser: &mut __S,
            ) {
                #body
            }
        });
    }

    // __dispatch_serialize: takes &'a Enum<'a>, matches all variants.
    // Used by emit_for_syn_type and emit_leaf_syn_type to avoid infinite recursion.
    {
        let arms = serialize::generate_dispatch_arms(ir, ctx);
        methods.push(quote! {
            fn __dispatch_serialize<'a, __S: ::bbnf_ser::Serializer>(
                __v: #boxed_enum,
                __ser: &mut __S,
            ) {
                match __v {
                    #(#arms)*
                    _ => {}
                }
            }
        });
    }

    // Entry points.
    {
        let entry_rule = &ir.rules[ir.entry as usize];
        let entry_name = ir.get_string(entry_rule.name);
        let emit_fn = format_ident!("serialize_{}", entry_name);

        let entry_td = ir.types.iter()
            .find(|(id, _)| *id == entry_rule.id)
            .map(|(_, td)| td);
        let entry_is_ref = entry_td.map_or(false, |td| serialize::type_desc_is_ref(td));

        // Check if the entry rule's type is Enum/BoxedEnum (transparent behavior)
        // or a specific type (non-transparent, wrapped in variant).
        let entry_is_transparent = entry_td
            .map_or(true, |td| matches!(td, bbnf_ir::TypeDesc::Enum | bbnf_ir::TypeDesc::BoxedEnum))
            || entry_rule.meta.is_transparent;

        if entry_is_transparent {
            // Transparent entry: type is BoxedEnum (&'a Enum). serialize fn takes it directly.
            methods.push(quote! {
                pub fn serialize_compact<'a>(__v: #boxed_enum) -> String {
                    let mut __ser = ::bbnf_ser::StringSerializer::new();
                    Self::#emit_fn(__v, &mut __ser);
                    __ser.finish()
                }
                pub fn serialize<'a, __S: ::bbnf_ser::Serializer>(
                    __v: #boxed_enum, __ser: &mut __S,
                ) {
                    Self::#emit_fn(__v, __ser);
                }
            });
        } else {
            // Non-transparent entry: unwrap enum variant.
            let variant = format_ident!("{}", entry_name);

            // Match ergonomics: __inner from `if let Enum::V(__inner) = __v` is &FieldType.
            // For ref-type rules, pass *__inner. For value-type rules, pass __inner.
            let call = if entry_is_ref {
                quote! { Self::#emit_fn(*__inner, &mut __ser); }
            } else {
                quote! { Self::#emit_fn(__inner, &mut __ser); }
            };
            let call2 = if entry_is_ref {
                quote! { Self::#emit_fn(*__inner, __ser); }
            } else {
                quote! { Self::#emit_fn(__inner, __ser); }
            };

            methods.push(quote! {
                pub fn serialize_compact<'a>(__v: #boxed_enum) -> String {
                    let mut __ser = ::bbnf_ser::StringSerializer::new();
                    if let #enum_ident::#variant(__inner) = __v {
                        #call
                    }
                    __ser.finish()
                }
                pub fn serialize<'a, __S: ::bbnf_ser::Serializer>(
                    __v: #boxed_enum, __ser: &mut __S,
                ) {
                    if let #enum_ident::#variant(__inner) = __v {
                        #call2
                    }
                }
            });
        }
    }

    quote! { #(#methods)* }
}
