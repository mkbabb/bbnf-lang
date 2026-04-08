//! Rust schema emitter — full parity with the v1 ir_visitor codegen.
//!
//! Takes a `CstSchema` (frontend-owned, target-agnostic) and emits the
//! Rust CST helper code that replaces hand-written walkers across the
//! repo. Generates:
//!
//! - `impl<'a> {Enum}<'a>::children(node) -> Vec<&'a {Enum}<'a>>` — debug helper
//! - `impl<'a> {Enum}<'a>::span_text(node) -> &'a str` — terminal text accessor
//! - `impl<'a> {Enum}<'a>::identifier_text(node) -> &'a str` — identifier extractor
//! - `impl<'a> {Enum}<'a>::identifier_span(node) -> Span<'a>` — identifier span
//! - `impl<'a> {Enum}<'a>::walk_children<V>(self, v) -> Vec<V::Output>` —
//!   direct per-variant dispatch
//! - `pub trait {Enum}Visitor<'a>` — namespaced visitor trait
//! - `pub mod cst_directives { ... }` — typed directive value structs
//! - `impl<'a> {Enum}<'a>::as_*_directive(&self) -> Option<...>` — accessors
//!
//! Each concern lives in its own sub-module. This `mod.rs` orchestrates the
//! full bundle.

mod children;
mod directives;
mod identifiers;
mod shared;
mod span_text;
mod visitor;
mod walkers;

use std::collections::HashSet;

use bbnf_ir::RuleId;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::model::CstSchema;

/// Generate the full Rust CST helper bundle from a `CstSchema`.
///
/// `fused_number_rules` is the Rust-backend-specific set of rules whose
/// payload is `(Span, f64)` instead of plain `Span`. Those variants emit
/// no children regardless of their schema-level type.
pub fn generate(schema: &CstSchema, fused_number_rules: &HashSet<RuleId>) -> TokenStream {
    let enum_ident = format_ident!("{}", schema.enum_name);
    let visitor_ident = format_ident!("{}Visitor", schema.enum_name);

    let children_fn = children::generate(schema, &enum_ident, fused_number_rules);
    let walk_children_fn = walkers::generate(schema, &enum_ident, fused_number_rules);
    let span_text_fn = span_text::generate(schema, &enum_ident);
    let identifier_text_fn = identifiers::generate_text(schema, &enum_ident);
    let identifier_span_fn = identifiers::generate_span(schema, &enum_ident);
    let visitor_trait = visitor::generate(&enum_ident, &visitor_ident);
    let directive_module = directives::generate_module(schema, &enum_ident);
    let directive_accessors = directives::generate_accessors(schema, &enum_ident);

    quote! {
        impl<'a> #enum_ident<'a> {
            /// Debug helper: collect references to all enum-typed children.
            ///
            /// Allocates a `Vec`. Walkers should prefer `walk_children`, which
            /// dispatches per variant directly with the visitor in scope.
            pub fn children(node: &'a #enum_ident<'a>) -> ::std::vec::Vec<&'a #enum_ident<'a>> {
                #children_fn
            }

            /// Extract terminal text by recursively unwrapping wrapper variants.
            pub fn span_text(node: &'a #enum_ident<'a>) -> &'a str {
                #span_text_fn
            }

            /// Recursively extract an identifier carrier's text. Returns the
            /// empty string if no identifier is reachable.
            pub fn identifier_text(node: &'a #enum_ident<'a>) -> &'a str {
                #identifier_text_fn
            }

            /// Recursively extract an identifier carrier's `Span`. Returns
            /// `Span::default()` if no identifier is reachable.
            pub fn identifier_span(node: &'a #enum_ident<'a>) -> ::parse_that::Span<'a> {
                #identifier_span_fn
            }

            /// Direct per-variant dispatch: visit each enum-typed child via
            /// the supplied visitor and collect their `Output`s. No intermediate
            /// allocation of a `Vec<&Enum>`.
            pub fn walk_children<__V: #visitor_ident<'a> + ?Sized>(
                node: &'a #enum_ident<'a>,
                v: &mut __V,
            ) -> ::std::vec::Vec<__V::Output> {
                #walk_children_fn
            }

            #directive_accessors
        }

        #visitor_trait

        #directive_module
    }
}
