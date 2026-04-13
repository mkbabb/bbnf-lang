//! Quoted-string kernel emission.
//!
//! Targets `parse_that::scan_string_quoted` (`parsers/scan/quoted.rs`).
//! Used by `RegexClass::QuotedString`, `JsonString`, `CssQuotedString`.

use proc_macro2::TokenStream;
use quote::quote;

/// Emit a call to the standard quoted-string scanner. The kernel
/// handles backslash escapes via `memchr2(quote, '\\')` and an
/// inner skip loop.
pub fn emit_call() -> TokenStream {
    quote! { ::parse_that::scan_string_quoted(state) }
}

/// Emit a call to the strict quoted-string scanner with full
/// escape semantics (RFC 8259 grammar).
pub fn emit_call_strict() -> TokenStream {
    quote! { ::parse_that::scan_quoted_string_strict(state) }
}
