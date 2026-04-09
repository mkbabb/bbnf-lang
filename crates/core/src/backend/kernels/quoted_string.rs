//! Quoted-string kernel emission.
//!
//! Targets `parse_that::scan_string_quoted` (`parsers/scan.rs:552`).
//! Used by `RegexClass::QuotedString`, `JsonString`, `CssQuotedString`.

use proc_macro2::TokenStream;
use quote::quote;

/// Emit a call to the standard quoted-string scanner. The kernel
/// handles backslash escapes via `memchr2(quote, '\\')` and an
/// inner skip loop.
pub fn emit_call() -> TokenStream {
    quote! { ::parse_that::scan_string_quoted(state) }
}

/// Emit a call to the JSON-specific quoted-string scanner with full
/// escape semantics (RFC 8259).
pub fn emit_json_call() -> TokenStream {
    quote! { ::parse_that::quoted_string_scan_full(state) }
}
