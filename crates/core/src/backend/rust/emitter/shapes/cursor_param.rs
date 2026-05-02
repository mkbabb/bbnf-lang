//! AZ-IV.W3.6 — Cursor-parameter token helpers.
//!
//! Centralises the codegen-side tokens for the `&mut PathCursor<'p, __P>`
//! parameter that every emitted `parse_<shape>_<grammar>_<rule>`
//! function carries through cross-shape recursion. The cursor is the
//! lazy bail-out parser's path-state thread; eager parses pass an
//! always-`ParseFully` cursor and the emitted body remains byte-stable
//! against the pre-W3.6 1582-test corpus.
//!
//! Three tokens are always shaped together:
//!
//! - [`cursor_generic_param`] — `__P: crate::path::schema::PathSchema<'p>`
//!   — the function-level generic parameter the cursor's schema lifetime
//!   binds against.
//! - [`cursor_param`] — `cursor: &mut crate::path::cursor::PathCursor<'p, __P>`
//!   — the parameter declaration itself.
//! - [`cursor_arg`] — `cursor` — the bare ident threaded into nested
//!   call sites under the same lifetime.
//!
//! Splicing these three tokens consistently keeps the cursor's
//! lifetime / schema-generic story uniform across every shape emitter
//! without each emitter open-coding the tokens. The decision-consult
//! sites at the four hot points (Array / Object / Wrap-Alt / Flat-Seq)
//! invoke `cursor.decide(rule_id) -> Decision` and translate the
//! `ParseFully` / `ParseUntil(idx)` / `Skip` variants into emitted
//! control flow per the AZ-IV.W3 spec.

use proc_macro2::TokenStream;
use quote::quote;

/// Emit the function-level generic parameter list fragment that
/// introduces `__P: PathSchema<'p>`. Call sites concatenate this with
/// the `'p` lifetime to form the full generic clause.
///
/// Example expansion:
/// ```ignore
/// pub fn parse_object_JsonParser_object<'p, __P>(...)
/// where
///     __P: crate::path::schema::PathSchema<'p>,
/// ```
pub fn cursor_generic_clause() -> TokenStream {
    quote! { __P }
}

/// Emit the where-clause fragment binding `__P` to `PathSchema<'p>`.
pub fn cursor_where_clause() -> TokenStream {
    quote! { __P: crate::path::schema::PathSchema<'p> }
}

/// Emit the cursor parameter declaration fragment used in every
/// emitted `parse_<shape>_<grammar>_<rule>` function signature.
pub fn cursor_param() -> TokenStream {
    quote! { cursor: &mut crate::path::cursor::PathCursor<'p, __P> }
}

/// Emit the bare cursor argument used at every cross-shape call site.
pub fn cursor_arg() -> TokenStream {
    quote! { cursor }
}
