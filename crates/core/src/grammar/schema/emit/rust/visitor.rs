//! Visitor trait — obsolete under Tranche AC.2.
//!
//! The pre-AC.2 emitter produced a `<Enum>Visitor<'a>` trait + a
//! default `walk_children`-backed fold. Under the tape-backed view
//! model, callers walk cursors directly; grammars that need a
//! visitor-style API compose it on top of `TapeCursor::children()`
//! without any schema-emitted glue.
//!
//! This module survives as an empty emitter so the orchestrator in
//! `mod.rs` can continue to call `visitor::generate(...)` without
//! conditional compilation; the returned `TokenStream` is empty and
//! contributes nothing to the final codegen output.

use proc_macro2::TokenStream;

/// Emit the visitor trait. Returns an empty `TokenStream` — direct
/// cursor iteration supersedes the pre-AC.2 visitor pattern.
pub(super) fn generate() -> TokenStream {
    TokenStream::new()
}
