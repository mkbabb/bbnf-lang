//! Number kernel emission.
//!
//! Targets `parse_that::number_span_scan_strict` and
//! `parse_that::number_fused_scan_convert`. Used by
//! `RegexClass::Numeric`, `JsonNumber`.

use proc_macro2::TokenStream;
use quote::quote;

/// Emit a call to the number scanner that returns only the span.
pub fn emit_call_span() -> TokenStream {
    quote! { ::parse_that::number_span_scan_strict(state) }
}

/// Emit a call to the fused number scanner that returns
/// `(Span, f64)` via Eisel-Lemire conversion.
pub fn emit_call_fused() -> TokenStream {
    quote! { ::parse_that::number_fused_scan_convert(state) }
}
