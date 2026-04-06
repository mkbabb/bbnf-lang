//! Emission codegen: generates `EmitSink`-based traversal code from grammar IR.
//!
//! For each rule, generates a method that walks the parsed value and calls
//! `EmitSink` methods to serialize it back to text. This is the inverse of
//! the parse codegen — where parsing reads text into typed values, emission
//! writes typed values back to text.

mod expr;
mod alt;
mod repeat;

use bbnf_ir::{GrammarIR, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::generate::ir_types::IrCodegenCtx;

/// Generate emit methods for all rules in the grammar.
///
/// Returns a single `TokenStream` of impl methods to include in the parser struct.
pub fn generate_emit_methods(ir: &GrammarIR, ctx: &IrCodegenCtx) -> TokenStream {
    let _enum_ident = &ctx.enum_ident;
    let mut methods = Vec::new();

    for rule in &ir.rules {
        let rule_name = ir.get_string(rule.name);
        let fn_ident = format_ident!("{}_emit", rule_name);

        // Get the rule's type from the TypeMap. Fall back to the enum type.
        let rule_type = ctx.rule_types.get(&rule.id)
            .cloned()
            .unwrap_or_else(|| ctx.enum_type.clone());

        let body = expr::emit_node(&rule.body, ir, ctx, rule);

        methods.push(quote! {
            /// Emit this rule's value through an [`EmitSink`].
            pub fn #fn_ident<'__emit, S: ::bbnf_emit::EmitSink<'__emit>>(
                value: &#rule_type,
                sink: &mut S,
            ) {
                #body
            }
        });
    }

    // Generate top-level emit dispatcher for the enum.
    let entry_rule = ir.rules.last().map(|r| ir.get_string(r.name));
    if let Some(entry) = entry_rule {
        let emit_fn = format_ident!("{}_emit", entry);
        methods.push(quote! {
            /// Emit a value as compact text (no formatting).
            pub fn emit_compact(value: &Self) -> String {
                let mut sink = ::bbnf_emit::StringSink::new();
                Self::#emit_fn(value, &mut sink);
                sink.finish()
            }

            /// Emit a value through a generic [`EmitSink`].
            pub fn emit<'a, S: ::bbnf_emit::EmitSink<'a>>(
                value: &Self,
                sink: &mut S,
            ) {
                Self::#emit_fn(value, sink);
            }
        });
    }

    quote! { #(#methods)* }
}
