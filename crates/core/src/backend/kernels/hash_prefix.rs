//! CSS hash-prefix hex-tail kernel emission (Tranche X.10b).
//!
//! Emits a fused scanner for `#` + hex-digit tail: memcmp the `#`
//! prefix, delegate the tail to the hoisted `scan_hex_mut` helper,
//! and roll back on tail failure so the caller observes a clean
//! `None`. Covers CSS color literals (`#abc`, `#abcdef`,
//! `#abcdefab`) with a single call site.

use proc_macro2::TokenStream;
use quote::quote;

/// Emit the `#`+hex-tail fused scanner.
///
/// Requires at least one hex digit after the `#`. Returns `Option<Span>`
/// covering the full match on success; `None` with the offset rolled
/// back to `__start` on miss.
pub fn emit_call() -> TokenStream {
    quote! {
        {
            let __start = state.offset;
            if state.src_bytes.get(__start) == Some(&b'#') {
                state.offset = __start + 1;
                match ::parse_that::scan_hex_mut(state) {
                    Some(_) => Some(::parse_that::Span::new(__start, state.offset, state.src)),
                    None => {
                        state.offset = __start;
                        None
                    }
                }
            } else {
                None
            }
        }
    }
}
