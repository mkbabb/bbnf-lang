//! AZ-IV.W3.6 — Cursor-parameter token helpers.
//!
//! Centralises the codegen-side tokens for the `&mut PathCursor<'_, __P>`
//! parameter that every emitted `parse_<shape>_<grammar>_<rule>`
//! function carries through cross-shape recursion. The cursor is the
//! lazy bail-out parser's path-state thread; eager parses pass an
//! always-`ParseFully` cursor and the emitted body remains byte-stable
//! against the pre-W3.6 1582-test corpus.
//!
//! ## Lifetime decoupling
//!
//! The cursor's schema lifetime is independent of the input bytes
//! lifetime `'p`. Eager-mode call sites construct the cursor against a
//! function-local empty `TypedPath`; tying the cursor's internal
//! schema lifetime to `'p` would force the returned document's
//! lifetime back through the function-local cursor, which the borrow
//! checker rejects as escaping a local. By emitting
//! `&mut PathCursor<'_, __P>` (anonymous lifetime, not `'p`), the
//! cursor's borrow stays scoped to the parse-fn call frame while the
//! input-bytes lifetime `'p` flows through the builder and the
//! returned document unchanged.
//!
//! Three tokens are always shaped together:
//!
//! - [`cursor_generic_clause`] — `__P` — the function-level generic
//!   parameter the cursor's schema binds against.
//! - [`cursor_param`] — `cursor: &mut PathCursor<'_, __P>` — the
//!   parameter declaration.
//! - [`cursor_arg`] — `cursor` — the bare ident threaded into nested
//!   call sites.
//!
//! The decision-consult sites at the four hot points (Array / Object /
//! Wrap-Alt / Flat-Seq) invoke `cursor.decide(rule_id) -> Decision`
//! and translate the `ParseFully` / `ParseUntil(idx)` / `Skip`
//! variants into emitted control flow per the AZ-IV.W3 spec.

use proc_macro2::TokenStream;
use quote::quote;

/// Emit the function-level generic parameter list fragment that
/// introduces `__P`. The where-clause binds it to `PathSchema<'_>`
/// with an anonymous lifetime so the cursor's schema lifetime stays
/// decoupled from the input-bytes lifetime `'p`.
///
/// Example expansion:
/// ```ignore
/// pub fn parse_object_JsonParser_object<'p, __P>(...)
/// where
///     __P: for<'__c> crate::path::schema::PathSchema<'__c>,
/// ```
pub fn cursor_generic_clause() -> TokenStream {
    quote! { __P }
}

/// Emit the where-clause fragment binding `__P` to `PathSchema` for
/// any lifetime. The `for<'__c>` HRTB lets the cursor's internal
/// schema lifetime be inferred independently of the input-bytes
/// lifetime `'p`, which is the unblock for eager-mode parses where
/// the cursor is constructed function-local against an empty path.
pub fn cursor_where_clause() -> TokenStream {
    quote! { __P: for<'__c> crate::path::schema::PathSchema<'__c> }
}

/// Emit the cursor parameter declaration fragment used in every
/// emitted `parse_<shape>_<grammar>_<rule>` function signature. The
/// cursor's schema lifetime is anonymous so the borrow checker does
/// not propagate it through the function's return type.
pub fn cursor_param() -> TokenStream {
    quote! { cursor: &mut crate::path::cursor::PathCursor<'_, __P> }
}

/// Emit the bare cursor argument used at every cross-shape call site.
pub fn cursor_arg() -> TokenStream {
    quote! { cursor }
}
