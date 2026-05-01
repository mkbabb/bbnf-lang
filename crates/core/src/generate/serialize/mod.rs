//! Tape-first serialize codegen.
//!
//! Walks the IR tree to generate serialization code that reads data
//! from `NodeView` tape cursors.  Every per-rule function takes a
//! `NodeView<'a>` and emits text via `::bbnf_ser::Serializer`.

mod serialize;

use bbnf_ir::GrammarIR;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::backend::rust::ir_types::IrCodegenCtx;

pub fn generate_serialize_methods(ir: &GrammarIR, ctx: &IrCodegenCtx) -> TokenStream {
    let mut methods = Vec::new();
    let node_view = &ctx.boxed_enum_type; // <Grammar>NodeView<'a>

    // Per-rule serialize functions — only for rules that produce
    // tape records (non-transparent, non-TransparentElide).
    //
    // Under tape-first, the most robust serialization is to emit
    // span_text() which faithfully reproduces the matched region.
    // The Alt dispatch provides structural branching; within each
    // rule, span_text() gives exact round-trip output.
    for rule in &ir.rules {
        if rule.meta.is_transparent || !serialize::rule_pushes_tape_record(ir, rule) {
            continue;
        }
        let rule_name = ir.get_string(rule.name);
        let fn_ident = format_ident!("serialize_{}", rule_name);

        methods.push(quote! {
            pub fn #fn_ident<'a, __S: ::bbnf_ser::Serializer<'a>>(
                __v: #node_view,
                __ser: &mut __S,
            ) {
                __ser.text(__v.span_text());
            }
        });
    }

    // __dispatch_serialize: variant_idx-based dispatch.
    {
        let arms = serialize::generate_dispatch_arms(ir, ctx);
        methods.push(quote! {
            fn __dispatch_serialize<'a, __S: ::bbnf_ser::Serializer<'a>>(
                __v: #node_view,
                __ser: &mut __S,
            ) {
                match __v.variant_idx() {
                    #(#arms)*
                    _ => {
                        // Fallback: emit span text for unrecognized variants.
                        __ser.text(__v.span_text());
                    }
                }
            }
        });
    }

    // Entry points.
    {
        let entry_rule = &ir.rules[ir.entry as usize];
        let entry_name = ir.get_string(entry_rule.name);
        let emit_fn = format_ident!("serialize_{}", entry_name);

        // Check if the entry rule has a serialize function (non-transparent
        // and non-TransparentElide). If it does, call it directly. Otherwise,
        // use __dispatch_serialize which routes on variant_idx.
        let entry_has_method =
            !entry_rule.meta.is_transparent && serialize::rule_pushes_tape_record(ir, entry_rule);

        if entry_has_method {
            methods.push(quote! {
                pub fn serialize_compact<'a>(__v: #node_view) -> String {
                    let mut __ser = ::bbnf_ser::StringSerializer::new();
                    Self::#emit_fn(__v, &mut __ser);
                    __ser.finish()
                }
                pub fn serialize<'a, __S: ::bbnf_ser::Serializer<'a>>(
                    __v: #node_view, __ser: &mut __S,
                ) {
                    Self::#emit_fn(__v, __ser);
                }
            });
        } else {
            // Entry rule is transparent or TransparentElide — dispatch.
            methods.push(quote! {
                pub fn serialize_compact<'a>(__v: #node_view) -> String {
                    let mut __ser = ::bbnf_ser::StringSerializer::new();
                    Self::__dispatch_serialize(__v, &mut __ser);
                    __ser.finish()
                }
                pub fn serialize<'a, __S: ::bbnf_ser::Serializer<'a>>(
                    __v: #node_view, __ser: &mut __S,
                ) {
                    Self::__dispatch_serialize(__v, __ser);
                }
            });
        }
    }

    quote! { #(#methods)* }
}
