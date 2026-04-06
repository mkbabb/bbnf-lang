//! Emission codegen: generates `EmitSink`-based traversal from grammar IR.
//!
//! Architecture: dual-topology walk. The generated code destructures the TYPED
//! VALUE (follows TypeDesc tree) while interleaving STRUCTURAL CONTENT (from
//! IR tree). Each emit function receives `value: &T` where `T` is the rule's
//! projected type, and `sink: &mut S` where `S: EmitSink`.

pub mod node;
pub mod seq;
pub mod alt;
pub mod repeat;
pub mod map;

use bbnf_ir::GrammarIR;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::generate::ir_types::IrCodegenCtx;

/// Generate emit methods for all rules in the grammar.
///
/// Returns a single `TokenStream` of impl methods to inject into the parser struct.
pub fn generate_emit_methods(ir: &GrammarIR, ctx: &IrCodegenCtx) -> TokenStream {
    let mut methods = Vec::new();

    for rule in &ir.rules {
        // Transparent rules are inlined into their callers — no standalone emit fn.
        if rule.meta.is_transparent {
            continue;
        }

        let rule_name = ir.get_string(rule.name);
        let fn_ident = format_ident!("{}_emit", rule_name);

        // Rule's projected type (what the parse method returns for this rule).
        let rule_type = ctx.rule_types.get(&rule.id)
            .cloned()
            .unwrap_or_else(|| ctx.enum_type.clone());

        let val = quote! { __v };
        let body = node::emit_node(&rule.body, &val, ir, ctx);

        methods.push(quote! {
            pub fn #fn_ident<'__e, __S: ::bbnf_emit::EmitSink<'__e>>(
                #val: &#rule_type,
                __sink: &mut __S,
            ) {
                #body
            }
        });
    }

    // Top-level convenience methods on the parser struct.
    if let Some(entry_rule) = ir.rules.last() {
        if !entry_rule.meta.is_transparent {
            let entry_name = ir.get_string(entry_rule.name);
            let emit_fn = format_ident!("{}_emit", entry_name);
            let entry_type = ctx.rule_types.get(&entry_rule.id)
                .cloned()
                .unwrap_or_else(|| ctx.enum_type.clone());

            methods.push(quote! {
                /// Emit a value as compact text (no formatting).
                pub fn emit_compact(__v: &#entry_type) -> String {
                    let mut __sink = ::bbnf_emit::StringSink::new();
                    Self::#emit_fn(__v, &mut __sink);
                    __sink.finish()
                }

                /// Emit a value through an [`EmitSink`].
                pub fn emit<'__e, __S: ::bbnf_emit::EmitSink<'__e>>(
                    __v: &#entry_type,
                    __sink: &mut __S,
                ) {
                    Self::#emit_fn(__v, __sink);
                }
            });
        }
    }

    quote! { #(#methods)* }
}
