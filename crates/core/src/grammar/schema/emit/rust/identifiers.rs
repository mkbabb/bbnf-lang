//! `identifier_text` / `identifier_span` helpers on the tape-backed
//! view types.
//!
//! Two layers of emission:
//!
//! 1. A dedicated impl on the `identifierView<'p>` struct — trivial
//!    slice of `self.input[lo..hi]` derived from the cursor's span.
//!    Every call site that already holds an identifier view goes
//!    through this zero-cost accessor. Only emitted when the grammar
//!    declares a rule named `identifier`.
//!
//! 2. Free functions `cst_identifier_text(cursor, input) -> &'p str`
//!    and `cst_identifier_span(cursor, _input) -> (u32, u32)` in the
//!    enclosing module that walk an arbitrary cursor's sub-tree
//!    looking for a record whose `variant_idx` matches the identifier
//!    rule's codegen-assigned discriminator. The walk is depth-first,
//!    left-to-right; the first match wins.
//!
//! The free helpers are always emitted regardless of whether the
//! grammar has an `identifier` rule, because the directive extractor
//! helpers in `cst_directives` reference `super::cst_identifier_text`
//! for `Identifier`-kind slots. Grammars without an identifier rule
//! get a stub that returns `""` / `(0, 0)`; in practice the stub is
//! never reached because such grammars also don't declare directives
//! whose layout references an `Identifier` slot.

use proc_macro2::TokenStream;
use quote::quote;

use super::super::super::model::CstSchema;
use super::shared::{variant_idx_for, view_ident_for};

/// Emit the identifier-view impl block + the free helper functions.
pub(super) fn generate(schema: &CstSchema) -> TokenStream {
    let identifier_variant_idx = variant_idx_for(schema, "identifier");
    let has_identifier_rule = schema
        .variants
        .iter()
        .any(|v| v.name == "identifier" && v.rule_id.is_some());

    let view_impl = if has_identifier_rule {
        let view_ident = view_ident_for("identifier");
        quote! {
            impl<'p> #view_ident<'p> {
                /// Identifier text — slice of the owning `Parsed`'s
                /// input covered by this view's record span.
                #[inline]
                pub fn identifier_text(&self) -> &'p str {
                    let (lo, hi) = self.cursor.span();
                    &self.input[lo as usize..hi as usize]
                }

                // `identifier_span` is emitted on every view by the
                // backend `view::generate_views` shared accessor block,
                // so no identifier-specific override is needed here.
            }
        }
    } else {
        TokenStream::new()
    };

    let helpers = match identifier_variant_idx {
        Some(idx) => emit_find_helpers(idx),
        None => emit_stub_helpers(),
    };

    quote! {
        #view_impl

        #helpers
    }
}

/// Emit the DFS identifier-finding helpers keyed on the grammar's
/// `identifier` rule's variant_idx.
fn emit_find_helpers(identifier_variant_idx: u8) -> TokenStream {
    let idx_lit = identifier_variant_idx;
    quote! {
        /// Walk `cursor`'s sub-tree depth-first and return the text
        /// of the first reachable identifier record. Returns `""`
        /// when no identifier is reachable.
        #[inline]
        pub(crate) fn cst_identifier_text<'p>(
            cursor: crate::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> &'p str {
            match cst_find_identifier_cursor(cursor, #idx_lit) {
                ::core::option::Option::Some(found) => {
                    let (lo, hi) = found.span();
                    &input[lo as usize..hi as usize]
                }
                ::core::option::Option::None => "",
            }
        }

        /// Walk `cursor`'s sub-tree depth-first and return the
        /// `(lo, hi)` span of the first reachable identifier record.
        /// Returns `(0, 0)` when no identifier is reachable.
        #[inline]
        pub(crate) fn cst_identifier_span<'p>(
            cursor: crate::runtime::tape::TapeCursor<'p>,
            _input: &'p str,
        ) -> (u32, u32) {
            cst_find_identifier_cursor(cursor, #idx_lit)
                .map(|c| c.span())
                .unwrap_or((0, 0))
        }

        /// DFS helper shared by `cst_identifier_text` and
        /// `cst_identifier_span`. Returns the first cursor under
        /// `start` whose `variant_idx` matches `target_idx`.
        #[inline]
        fn cst_find_identifier_cursor<'p>(
            start: crate::runtime::tape::TapeCursor<'p>,
            target_idx: u8,
        ) -> ::core::option::Option<crate::runtime::tape::TapeCursor<'p>> {
            if start.variant_idx() == target_idx {
                return ::core::option::Option::Some(start);
            }
            for child in start.children() {
                if let ::core::option::Option::Some(found) =
                    cst_find_identifier_cursor(child, target_idx)
                {
                    return ::core::option::Option::Some(found);
                }
            }
            ::core::option::Option::None
        }
    }
}

/// Emit trivial stub helpers for grammars with no `identifier` rule.
/// Keeps `cst_directives::try_as_*` references to
/// `super::cst_identifier_text` buildable even when no directive
/// ever actually exercises them.
fn emit_stub_helpers() -> TokenStream {
    quote! {
        #[inline]
        pub(crate) fn cst_identifier_text<'p>(
            _cursor: crate::runtime::tape::TapeCursor<'p>,
            _input: &'p str,
        ) -> &'p str {
            ""
        }

        #[inline]
        pub(crate) fn cst_identifier_span<'p>(
            _cursor: crate::runtime::tape::TapeCursor<'p>,
            _input: &'p str,
        ) -> (u32, u32) {
            (0, 0)
        }
    }
}
