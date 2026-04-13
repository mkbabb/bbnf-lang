//! Identifier kernel emission.
//!
//! Targets `parse_that::scan_ident` (`parsers/scan/ident.rs`).
//! Used by `RegexClass::Identifier`, `CssIdent`.

use proc_macro2::TokenStream;
use quote::quote;

/// Emit a call to the default-config identifier scanner. Accepts
/// `[a-zA-Z_][\w-]*` (no vendor prefix, no custom property).
pub fn emit_call() -> TokenStream {
    quote! { ::parse_that::scan_ident(state, &::parse_that::DEFAULT_IDENT_CONFIG) }
}

/// Emit a call to the CSS-flavored identifier scanner. Accepts
/// vendor prefixes (`-foo`) and custom properties (`--foo`).
pub fn emit_call_css() -> TokenStream {
    quote! { ::parse_that::scan_ident(state, &::parse_that::CSS_IDENT_CONFIG) }
}

/// Emit a call to the CSS-flavored identifier scanner with escape support.
/// Accepts vendor prefixes, custom properties, and `\X` escape sequences.
pub fn emit_call_with_escapes() -> TokenStream {
    quote! { ::parse_that::scan_ident(state, &::parse_that::CSS_IDENT_ESCAPE_CONFIG) }
}
