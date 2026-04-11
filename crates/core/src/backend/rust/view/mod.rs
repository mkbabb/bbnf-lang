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
//! - `.child(i)` — the i-th direct child, for indexed access.
//! - `.is_recovered()` — true iff the record was pushed from an
//!   `@recover` arm (delegates to `TapeKind::is_recovered`).
//!
//! The top-level grammar-view binding ties the grammar marker
//! struct's [`bbnf::runtime::Root`](crate::runtime::Root) GAT
//! `type View<'tape>` to the root rule's view type. Generated
//! `Parsed<Grammar>::view(&self)` calls the `Root::make_view`
//! constructor to lend the root view with a `&self` lifetime.
//!
//! # Ownership of generation
//!
//! - `generate_views` is the public entry point the backend calls
//!   to emit all per-rule view types and the top-level `Root`
//!   binding as one `TokenStream`.
//! - The per-kind sibling files (`leaves.rs`, `seq.rs`, `alt.rs`,
//!   `repeat.rs`, `grammar.rs`) host kind-specific accessor
//!   specializations. The AC.2 baseline shape is uniform —
//!   every view exposes the universal cursor accessors — so the
//!   siblings are reserved for the post-AC typed-accessor pass.
//! - `TransparentElide` rules never get a view type: they are
//!   inlined at every call site during parser emission, so the
//!   view layer never sees them.
//!
//! # Why this is additive
//!
//! `generate_views` is not yet called from
//! `emit_type_definitions_impl` — the call site lands during the
//! atomic AC.2 commit alongside the emitter rewrite. The
//! generator is written here so the AC.2 commit only needs to
//! delete the eager-AST emitter and splice this function into
//! the emission pipeline, rather than carrying view-layer design
//! decisions inside the already-large atomic commit.

use bbnf_ir::GrammarIR;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

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
/// - One `<Rule>View<'tape>` struct per non-transparent rule,
///   wrapping a `TapeCursor<'tape>` with universal accessors.
/// - One `impl ::bbnf::runtime::Root for <Grammar>` binding that
///   ties the grammar marker struct's `type View<'tape>` GAT to
///   the root rule's view type.
///
/// Transparent rules are skipped — they are inlined at every
/// call site and never materialize a tape record.
///
/// # Root-rule selection
///
/// The root rule is the first non-transparent rule in the IR's
/// declaration order. BBNF grammars put the entry point first by
/// convention (the bootstrap enforces this), so this matches the
/// user's intent without needing a separate annotation.
///
/// # Downstream dependencies
///
/// The emitted code references:
/// - `::bbnf_tape::{Tape, TapeOffset, TapeCursor, TapeKind}` —
///   already on the hot path for every parser.
/// - `::bbnf::runtime::Root` — re-exported from `bbnf::runtime`.
///
/// Consumer crates that invoke `#[derive(Parser)]` must have both
/// `bbnf-tape` and `bbnf` in their dependency list. AC.3 handles
/// the workspace-wide migration.
pub fn generate_views(ir: &GrammarIR, ctx: &IrCodegenCtx<'_>) -> TokenStream {
    let mut rule_views: Vec<TokenStream> = Vec::new();

    for rule in &ir.rules {
        if rule.meta.is_transparent {
            continue;
        }
        let name = ir.get_string(rule.name);
        let view_ident = format_ident!("{}View", name);

        rule_views.push(quote! {
            /// Generated view over a tape record produced by this rule.
            ///
            /// Wraps a [`::bbnf_tape::TapeCursor`] and exposes the
            /// universal accessor set. The cursor is `Copy`, so the
            /// view is cheap to clone and passes cheaply by value.
            #[derive(Clone, Copy, Debug)]
            #[allow(non_camel_case_types)]
            pub struct #view_ident<'tape> {
                cursor: ::bbnf_tape::TapeCursor<'tape>,
            }

            #[allow(dead_code)]
            impl<'tape> #view_ident<'tape> {
                /// Construct a view pointing at `offset` in `tape`.
                #[inline]
                pub fn new(
                    tape: &'tape ::bbnf_tape::Tape,
                    offset: ::bbnf_tape::TapeOffset,
                ) -> Self {
                    Self { cursor: ::bbnf_tape::TapeCursor::new(tape, offset) }
                }

                /// Borrow the underlying cursor for direct access.
                #[inline]
                pub fn cursor(&self) -> ::bbnf_tape::TapeCursor<'tape> {
                    self.cursor
                }

                /// Classification tag of the wrapped record.
                #[inline]
                pub fn kind(&self) -> ::bbnf_tape::TapeKind {
                    self.cursor.kind()
                }

                /// Source-byte span `(lo, hi)` of the wrapped record.
                #[inline]
                pub fn span(&self) -> (u32, u32) {
                    self.cursor.span()
                }

                /// Variant discriminator for Alt / sub-variant rules.
                #[inline]
                pub fn variant_idx(&self) -> u8 {
                    self.cursor.variant_idx()
                }

                /// Iterator over the direct children of this record.
                #[inline]
                pub fn children(
                    &self,
                ) -> impl ::core::iter::Iterator<Item = ::bbnf_tape::TapeCursor<'tape>> + 'tape {
                    self.cursor.children()
                }

                /// The i-th direct child, if present.
                #[inline]
                pub fn child(
                    &self,
                    i: usize,
                ) -> ::core::option::Option<::bbnf_tape::TapeCursor<'tape>> {
                    self.cursor.child(i)
                }

                /// True iff this record was pushed by an `@recover` arm.
                #[inline]
                pub fn is_recovered(&self) -> bool {
                    self.cursor.kind().is_recovered()
                }
            }
        });
    }

    // Pick the entry-point rule: the first non-transparent rule in
    // declaration order.
    let root_rule_name = ir
        .rules
        .iter()
        .find(|r| !r.meta.is_transparent)
        .map(|r| ir.get_string(r.name));

    let grammar_ident = ctx.ident;
    let root_binding = if let Some(root_name) = root_rule_name {
        let root_view_ident = format_ident!("{}View", root_name);
        quote! {
            impl ::bbnf::runtime::Root for #grammar_ident {
                type View<'tape> = #root_view_ident<'tape>;

                #[inline]
                fn make_view(
                    tape: &::bbnf_tape::Tape,
                    root: ::bbnf_tape::TapeOffset,
                ) -> Self::View<'_> {
                    #root_view_ident::new(tape, root)
                }
            }
        }
    } else {
        // Empty grammar (all-transparent) — no binding emitted.
        // `ir_enums::generate_enum` already panics in this case, so
        // this branch is defensive only.
        quote! {}
    };

    quote! {
        #(#rule_views)*
        #root_binding
    }
}
