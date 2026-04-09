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

/// Emit a balanced-wrap scanner call.
///
/// Tranche W phase 3d: returns a real `scan_balanced` invocation
/// against `parse_that::parsers::scan::balanced`. The wrap driver
/// reads the open/close bytes from `BalancedScanConfig` at the call
/// site; this kernel produces the inline TokenStream that the
/// emitter can splice into a parser body.
pub fn emit_call(open: u8, close: u8) -> TokenStream {
    quote! {
        ::parse_that::parsers::scan::balanced::scan_balanced(
            state.remaining().as_bytes(),
            &::parse_that::parsers::scan::balanced::BalancedScanConfig {
                open: #open,
                close: #close,
            },
        )
    }
}
