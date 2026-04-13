//! Number kernel emission.
//!
//! Targets `parse_that::scan_json_number_span` and
//! `parse_that::scan_json_number_fused`. Used by
//! `RegexClass::Numeric`, `JsonNumber`.

use proc_macro2::TokenStream;
use quote::quote;

/// Emit a call to the number scanner that returns only the span.
pub fn emit_call_span() -> TokenStream {
    quote! { ::parse_that::scan_json_number_span(state) }
}

/// Emit a call to the fused number scanner that returns
/// `(Span, f64)` via Eisel-Lemire conversion.
pub fn emit_call_fused() -> TokenStream {
    quote! { ::parse_that::scan_json_number_fused(state) }
}
