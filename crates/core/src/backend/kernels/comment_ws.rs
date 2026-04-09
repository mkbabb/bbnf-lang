//! Comment-aware whitespace kernel emission.
//!
//! Targets `parse_that::scan_ws_block_comments` (`parsers/scan.rs:497`).
//! Used by `RegexClass::WsBlockComment`.

use proc_macro2::TokenStream;
use quote::quote;

/// Emit a call to the comment-aware whitespace scanner. Skips
/// `\s` characters AND `/* ... */` block comments in one pass.
pub fn emit_call() -> TokenStream {
    quote! { ::parse_that::scan_ws_block_comments(state) }
}
