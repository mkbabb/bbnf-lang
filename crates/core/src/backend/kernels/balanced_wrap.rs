//! Balanced-delimiter kernel emission.
//!
//! Targets `parse_that::scan_balanced` (`parsers/scan.rs:647`).
//! Used by `NodeFacts.recognizer.shape == DelimiterBalanced` and
//! the existing `DelimScanConfig` machinery.
//!
//! V.7 scope: re-exports a TokenStream constructor that the wrap
//! driver can call after migration in V.8. The actual delim-scan
//! emission still flows through `backend/driver/wrap.rs`'s
//! `emit_delim_scan` until V.8 swaps the consumer.

use proc_macro2::TokenStream;
use quote::quote;

/// Emit a stub call indicating the balanced-wrap kernel slot. V.8
/// migrates the wrap driver to consume this in place of the inline
/// `emit_delim_scan` body.
pub fn emit_call(_open: u8, _close: u8) -> TokenStream {
    quote! { compile_error!("backend/kernels/balanced_wrap: V.8 wires this") }
}
