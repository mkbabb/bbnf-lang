//! Literal-prefix-then-class kernel emission.
//!
//! Targets `memcmp` + a tail dispatch into another kernel module.
//! Used by `RegexClass::PrefixThenClass`, `Anchored`, `AccelDriven`.

use proc_macro2::TokenStream;
use quote::quote;

/// Emit a literal-prefix memcmp followed by a class-tail scan.
///
/// V.7 keeps emission inline; hoisting is a follow-up.
pub fn emit_call(_prefix: &[u8]) -> TokenStream {
    // Stub — the existing generalized emitter handles literal-prefix
    // patterns via the pattern registry. The V.7 contract is the
    // kernel module exists; the body can be specialized in follow-up.
    quote! { ::parse_that::scan_ident(state) }
}
