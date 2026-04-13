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

/// Emit a call to the generic (non-strict) number span scanner.
/// Accepts CSS-style numbers: optional sign, leading zero, no
/// leading-zero rejection. Used for `Numeric { reject_leading_zero:
/// false }` patterns.
pub fn emit_call_generic_span() -> TokenStream {
    quote! { ::parse_that::scan_number_span(state) }
}

/// Emit a call to the generic (non-strict) fused number scanner.
/// Returns `Option<f64>` via Eisel-Lemire. Accepts CSS-style numbers.
pub fn emit_call_generic_f64() -> TokenStream {
    quote! { ::parse_that::scan_number_f64(state) }
}
