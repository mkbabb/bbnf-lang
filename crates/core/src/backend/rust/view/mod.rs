//! Per-rule tape-view codegen for the Rust backend.
//!
//! Tranche AC.2 landing surface. For every rule in the IR, this
//! module emits a `<Rule>View<'p>` wrapper struct that holds a
//! [`::bbnf::runtime::tape::TapeCursor`] plus a borrow of the
//! original input string. The `input` borrow lets view accessors
//! lazily materialize scalar projections (numeric conversion, hex
//! conversion, constant lookups, etc.) from the source bytes —
//! under AC.2 scalar projection runs at view-read time rather
//! than at parse time.
//!
//! Universal accessors exposed on every generated view:
//!
//! - `.kind()` — the `TapeKind` discriminator of the record.
//! - `.span()` — the source span `(lo, hi)` as a byte range.
//! - `.span_text()` — the source substring for the span.
//! - `.input()` — the full input string borrow.
//! - `.variant_idx()` — codegen-assigned discriminator for Alt
//!   rules (which branch was chosen) and for heterogeneous
//!   sub-variant coercion.
//! - `.children()` — iterator of child cursors, for `MustTape`
//!   compound records (Seq/Alt/Repeat/Rule).
//! - `.child(i)` — the i-th direct child, for indexed access.
//! - `.is_recovered()` — true iff the record was pushed from an
//!   `@recover` arm.
//!
//! The top-level grammar-view binding ties the grammar marker
//! struct's [`::bbnf::runtime::Root`] GAT `type View<'p>` to the
//! root rule's view type. Generated `Parsed<Grammar>::view(&self)`
//! calls the `Root::make_view` constructor to lend the root view
//! with a `&self` lifetime.
//!
//! # Ownership of generation
//!
//! - `generate_views` is the public entry point the backend calls
//!   to emit all per-rule view types and the top-level `Root`
//!   binding as one `TokenStream`.
//! - The per-kind sibling files are reserved for the post-AC
//!   typed-accessor pass.
//! - `TransparentElide` rules never get a view type: they are
//!   inlined at every call site during parser emission, so the
//!   view layer never sees them.

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
/// - One `<Rule>View<'p>` struct per non-transparent rule,
///   wrapping a `TapeCursor<'p>` + `input: &'p str` with
///   universal accessors.
/// - One `impl ::bbnf::runtime::Root for <Grammar>` binding that
///   ties the grammar marker struct's `type View<'p>` GAT to the
///   root rule's view type.
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
            /// Wraps a [`::bbnf::runtime::tape::TapeCursor`] plus a
            /// borrow of the original input. The cursor is `Copy`,
            /// so the view is cheap to clone and passes cheaply by
            /// value.
            #[derive(Clone, Copy, Debug)]
            #[allow(non_camel_case_types)]
            pub struct #view_ident<'p> {
                cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
                input: &'p str,
            }

            #[allow(dead_code)]
            impl<'p> #view_ident<'p> {
                /// Construct a view pointing at `offset` in `tape`,
                /// bound to `input` for scalar-projection accessors.
                #[inline]
                pub fn new(
                    tape: &'p ::bbnf::runtime::tape::Tape,
                    input: &'p str,
                    offset: ::bbnf::runtime::tape::TapeOffset,
                ) -> Self {
                    Self {
                        cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                        input,
                    }
                }

                /// Borrow the underlying cursor for direct access.
                #[inline]
                pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
                    self.cursor
                }

                /// Borrow the original input string this view was built from.
                #[inline]
                pub fn input(&self) -> &'p str {
                    self.input
                }

                /// Classification tag of the wrapped record.
                #[inline]
                pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
                    self.cursor.kind()
                }

                /// Source-byte span `(lo, hi)` of the wrapped record.
                #[inline]
                pub fn span(&self) -> (u32, u32) {
                    self.cursor.span()
                }

                /// Source substring covered by this record's span.
                #[inline]
                pub fn span_text(&self) -> &'p str {
                    let (lo, hi) = self.cursor.span();
                    &self.input[lo as usize..hi as usize]
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
                ) -> impl ::core::iter::Iterator<Item = ::bbnf::runtime::tape::TapeCursor<'p>> + 'p {
                    self.cursor.children()
                }

                /// The i-th direct child, if present.
                #[inline]
                pub fn child(
                    &self,
                    i: usize,
                ) -> ::core::option::Option<::bbnf::runtime::tape::TapeCursor<'p>> {
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
                type View<'p> = #root_view_ident<'p>;

                #[inline]
                fn make_view(
                    tape: &::bbnf::runtime::tape::Tape,
                    input: &'p str,
                    root: ::bbnf::runtime::tape::TapeOffset,
                ) -> Self::View<'_> {
                    #root_view_ident::new(tape, input, root)
                }
            }
        }
    } else {
        // Empty grammar (all-transparent) — no binding emitted.
        quote! {}
    };

    quote! {
        #(#rule_views)*
        #root_binding
    }
}
