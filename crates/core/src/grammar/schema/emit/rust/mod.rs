//! Rust schema emitter — tape-backed view helper codegen.
//!
//! Post-Tranche AC.2, the Rust backend emits one
//! `<Rule>View<'p> { cursor: TapeCursor<'p>, input: &'p str }`
//! struct per non-transparent rule via
//! `backend::rust::view::generate_views`, along with a set of
//! universal accessors (`kind`, `span`, `span_text`, `variant_idx`,
//! `children`, `child`, `is_recovered`) on every view. Those
//! accessors subsume the pre-AC.2 `children`, `span_text`,
//! `walk_children`, and visitor-trait helpers, so this emitter
//! focuses on the **schema-specific** additions the view module
//! doesn't know about:
//!
//! - `identifier_text` / `identifier_span` on the `identifierView`
//!   struct (grammar has a rule named `identifier`).
//! - Free helper functions for extracting identifiers from arbitrary
//!   cursors, used by the directive accessors below.
//! - The `cst_directives` module — one `#[derive(Clone, Copy)]`
//!   struct per directive variant, plus `try_as_<rule>` extraction
//!   helpers.
//! - `as_*_directive` convenience methods on the root grammar view.
//!
//! The `children.rs`, `walkers.rs`, `visitor.rs`, and `span_text.rs`
//! siblings remain as empty emitters: their pre-AC.2 responsibilities
//! are now carried by the universal cursor accessor set.
//!
//! `fused_number_rules` is accepted as a parameter for backwards
//! compatibility with the `generate::generate_all` caller but is
//! otherwise unused — the tape model doesn't need special-casing for
//! fused numeric payloads because each rule's view carries a single
//! cursor regardless of its projected payload shape.

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
use quote::quote;

use super::super::model::CstSchema;

/// Generate the full Rust schema helper bundle from a `CstSchema`.
///
/// The emitted `TokenStream` is spliced into the final codegen
/// output by `generate::generate_all` alongside the backend's
/// per-rule view types and parser functions. It defines:
///
/// - `impl<'p> identifierView<'p> { fn identifier_text, fn identifier_span }`
///   (only if the grammar declares an `identifier` rule).
/// - Free helpers `cst_identifier_text` / `cst_identifier_span`
///   that DFS-walk a cursor looking for the first identifier record.
/// - `pub mod cst_directives { ... }` with directive value structs
///   and `try_as_<rule>` extraction helpers.
/// - `impl<'p> <Root>View<'p> { fn as_*_directive }` convenience
///   accessors on the root grammar view.
///
/// Returns an empty `TokenStream` when the schema declares no
/// identifier rule and no directives — defensive for trivial
/// grammars.
pub fn generate(schema: &CstSchema, _fused_number_rules: &HashSet<RuleId>) -> TokenStream {
    // Empty-emitter slots, kept for future expansion.
    let _children = children::generate(schema);
    let _walk_children = walkers::generate(schema);
    let _span_text = span_text::generate(schema);
    let _visitor = visitor::generate();

    let identifier_block = identifiers::generate(schema);
    let directive_module = directives::generate_module(schema);
    let directive_accessors = directives::generate_root_accessors(schema);

    quote! {
        #identifier_block

        #directive_module

        #directive_accessors
    }
}
