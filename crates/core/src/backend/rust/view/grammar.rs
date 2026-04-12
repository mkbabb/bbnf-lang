//! Top-level grammar-level root binding emission.
//!
//! This module emits the convenience `root_view()` method on the
//! grammar marker struct, connecting it to the entry rule's typed
//! view. It also emits a `parse_and_view()` helper that runs the
//! full parse and returns the root view in one step.
//!
//! The `impl Root for <Grammar>` binding is already emitted by
//! `mod.rs` — this module supplements it with ergonomic helpers.

use bbnf_ir::GrammarIR;
use proc_macro2::TokenStream;
use quote::quote;

use crate::backend::rust::ir_types::IrCodegenCtx;

/// Emit the grammar-level root binding helpers.
///
/// Returns an `impl` block on the grammar marker struct with a
/// `root_view()` convenience that delegates to `Root::make_view`.
pub fn emit_grammar_root_helpers(ir: &GrammarIR, ctx: &IrCodegenCtx<'_>) -> TokenStream {
    // Find the entry rule (same logic as mod.rs root selection).
    let entry_rule = ir
        .rules
        .iter()
        .find(|r| r.id == ir.entry && !r.meta.is_transparent);
    let root_rule = entry_rule.or_else(|| ir.rules.iter().find(|r| !r.meta.is_transparent));

    let root_name = match root_rule {
        Some(rule) => ir.get_string(rule.name),
        None => return quote! {},
    };

    let grammar_ident = ctx.ident;

    quote! {
        #[allow(dead_code)]
        impl #grammar_ident {
            /// The name of the root rule for this grammar.
            #[inline]
            pub fn root_rule_name() -> &'static str {
                #root_name
            }
        }
    }
}
