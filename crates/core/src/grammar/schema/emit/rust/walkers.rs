//! `walk_children<V>()` dispatcher — obsolete under Tranche AC.2.
//!
//! The pre-AC.2 emitter produced a direct-per-variant dispatcher
//! that threaded a `Visitor` trait through every enum variant and
//! collected per-child outputs. Under the tape-backed view model,
//! callers iterate `view.cursor.children()` directly and wrap each
//! child cursor in whichever typed view they need; the bounded
//! variant dispatch is no longer necessary because the cursor's
//! child iterator is grammar-agnostic and O(1) per step.
//!
//! This module survives as an empty emitter so the orchestrator in
//! `mod.rs` can continue to call `walkers::generate(...)` without
//! conditional compilation; the returned `TokenStream` is empty and
//! contributes nothing to the final codegen output.

use proc_macro2::TokenStream;

use super::super::super::model::CstSchema;

/// Emit the `walk_children()` dispatcher. Returns an empty
/// `TokenStream` — direct cursor iteration supersedes the pre-AC.2
/// variant walk.
pub(super) fn generate(_schema: &CstSchema) -> TokenStream {
    TokenStream::new()
}
