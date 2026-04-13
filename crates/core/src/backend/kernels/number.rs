//! Number kernel emission.
//!
//! Targets `parse_that::scan_number_strict_span` and
//! `parse_that::scan_number_strict_fused`. Used by
//! `RegexClass::Numeric`, `JsonNumber`.

use proc_macro2::TokenStream;
use quote::quote;

/// Emit a call to the strict number scanner that returns only the span.
pub fn emit_call_span() -> TokenStream {
    quote! { ::parse_that::scan_number_strict_span(state) }
}

/// Emit a call to the fused strict number scanner that returns
/// `(Span, f64)` via Eisel-Lemire conversion.
pub fn emit_call_fused() -> TokenStream {
    quote! { ::parse_that::scan_number_strict_fused(state) }
}
