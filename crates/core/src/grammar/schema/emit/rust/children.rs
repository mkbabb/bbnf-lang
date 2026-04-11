//! `children()` helper — obsolete under Tranche AC.2.
//!
//! The pre-AC.2 emitter produced a debug helper that pattern-matched
//! every enum variant and collected references to enum-typed children
//! into a `Vec<&Enum>`. Under the tape-backed view model, the
//! equivalent accessor (`TapeCursor::children()`) is emitted on every
//! view by `backend::rust::view::generate_views` as a universal
//! method — no schema-driven variant walk is needed because the
//! cursor already knows how to iterate its record's child run.
//!
//! This module survives as an empty emitter so the orchestrator in
//! `mod.rs` can continue to call `children::generate(...)` without
//! conditional compilation; the returned `TokenStream` is empty and
//! contributes nothing to the final codegen output.

use proc_macro2::TokenStream;

use super::super::super::model::CstSchema;

/// Emit the `children()` helper. Returns an empty `TokenStream` —
/// the universal `TapeCursor::children()` accessor emitted by
/// `generate_views` supersedes the pre-AC.2 variant walk.
pub(super) fn generate(_schema: &CstSchema) -> TokenStream {
    TokenStream::new()
}
