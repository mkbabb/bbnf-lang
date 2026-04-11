//! Per-rule tape-view codegen for the Rust backend.
//!
//! Tranche AC.2 landing surface. For every rule in the IR, this
//! module emits a `<Rule>View<'tape>` wrapper struct that holds a
//! [`bbnf_tape::TapeCursor`] and exposes cursor-based accessors:
//!
//! - `.kind()` — the `TapeKind` discriminator of the record.
//! - `.span()` — the source span (`span_lo`, `span_hi`) as a byte
//!   range.
//! - `.variant_idx()` — codegen-assigned discriminator for Alt
//!   rules (which branch was chosen) and for heterogeneous
//!   sub-variant coercion.
//! - `.children()` — iterator of child cursors, for `MustTape`
//!   compound records (Seq/Alt/Repeat/Rule).
//! - `.is_recovered()` — true iff the record was pushed from an
//!   `@recover` arm (delegates to `TapeKind::is_recovered`).
//!
//! The top-level grammar view is a discriminator enum
//! `<Grammar>View<'tape>` whose variants are the per-rule views.
//! Generated parsers bind this against the
//! [`bbnf::runtime::Root`](crate::runtime::Root) trait GAT
//! `type View<'tape>` so `Parsed<Grammar>::view(&self)` can lend
//! the root view with a `&self` lifetime.
//!
//! # Ownership of generation
//!
//! - `generate_views` is the public entry point the backend calls
//!   to emit all per-rule view types and the top-level grammar
//!   view enum as one `TokenStream`.
//! - `leaves.rs`, `seq.rs`, `alt.rs`, `repeat.rs`, `grammar.rs` own
//!   the per-kind emission paths. They dispatch on
//!   `MaterializationClass` for rules whose class collapses to
//!   `TapeSpanOnly` (single-leaf accessor only) vs `MustTape`
//!   (full compound accessors).
//! - `TransparentElide` rules never get a view type — they are
//!   inlined at every call site during parser emission, so the
//!   view layer never sees them.
//!
//! # Why this module is wired in AC.2 (not earlier)
//!
//! The view types are only meaningful once the emitter produces
//! tape records for every rule. Emitting view types against the
//! existing eager-AST path would produce code that compiles but
//! has no callers. The skeleton lands here so the per-kind
//! sibling files have a landing spot; full wiring happens when
//! `generate_views` is called from `emit_type_definitions_impl`
//! inside the atomic AC.2 commit.

use bbnf_ir::GrammarIR;
use proc_macro2::TokenStream;
use quote::quote;

use crate::backend::rust::ir_types::IrCodegenCtx;

mod alt;
mod grammar;
mod leaves;
mod repeat;
mod seq;

/// Generate the full view-type surface for a grammar.
///
/// Returns a `TokenStream` that, when spliced into the generated
/// parser module, defines:
///
/// - One `<Rule>View<'tape>` struct per rule (shape depends on
///   the rule's `MaterializationClass`).
/// - One top-level `<Grammar>View<'tape>` discriminator enum
///   whose variants are the per-rule views.
/// - The `impl ::bbnf::runtime::Root for <Grammar>` binding that
///   ties the grammar marker struct to its root view via the
///   GAT `type View<'tape>`.
///
/// This function is called from `emit_type_definitions_impl`
/// (inside the atomic AC.2 commit) and produces tokens that flow
/// through the standard `emit_grammar_impl` assembly alongside
/// the rule function definitions.
///
/// The current skeleton emits an empty stream. Full implementation
/// lands in AC.2 alongside the emitter rewrite. The sibling files
/// (`leaves.rs`, `seq.rs`, `alt.rs`, `repeat.rs`, `grammar.rs`)
/// host the per-kind emission helpers.
pub fn generate_views(_ir: &GrammarIR, _ctx: &IrCodegenCtx<'_>) -> TokenStream {
    quote! {}
}
