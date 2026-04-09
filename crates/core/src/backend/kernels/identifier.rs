//! Identifier kernel emission.
//!
//! Targets `parse_that::scan_ident` (`parsers/scan.rs:430`).
//! Used by `RegexClass::Identifier`, `CssIdent`.

use proc_macro2::TokenStream;
use quote::quote;

/// Emit a call to the CSS-aware identifier scanner. Handles vendor
/// prefixes (`-foo`, `--foo`) and Unicode escape sequences.
pub fn emit_call() -> TokenStream {
    quote! { ::parse_that::scan_ident(state) }
}
