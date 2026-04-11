//! `span_text()` helper — obsolete under Tranche AC.2.
//!
//! The pre-AC.2 emitter produced a recursive `span_text` that
//! pattern-matched every enum variant and unwrapped wrapper
//! variants to return the terminal text slice. Under the tape-
//! backed view model, `span_text` is a universal accessor emitted
//! by `backend::rust::view::generate_views` on every view type —
//! it reads `(lo, hi)` from the cursor and slices
//! `self.input[lo..hi]` directly. No recursion needed because
//! every rule record already carries its full source span.
//!
//! This module survives as an empty emitter so the orchestrator in
//! `mod.rs` can continue to call `span_text::generate(...)` without
//! conditional compilation; the returned `TokenStream` is empty and
//! contributes nothing to the final codegen output.

use proc_macro2::TokenStream;

use super::super::super::model::CstSchema;

/// Emit the `span_text()` helper. Returns an empty `TokenStream` —
/// the universal accessor emitted by `generate_views` supersedes
/// the pre-AC.2 variant walk.
pub(super) fn generate(_schema: &CstSchema) -> TokenStream {
    TokenStream::new()
}
