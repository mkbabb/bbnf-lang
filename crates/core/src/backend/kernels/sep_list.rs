//! Separator-list kernel emission.
//!
//! Composition: `memchr2` + inlined element callback. Used by
//! `NodeFacts.recognizer.shape == SeparatorList`.

use proc_macro2::TokenStream;
use quote::quote;

/// V.7 stub. The existing `backend/driver/repeat.rs` sep-by emission
/// continues to handle this until V.8 swaps the consumer.
pub fn emit_call(_separator: u8) -> TokenStream {
    quote! { compile_error!("backend/kernels/sep_list: V.8 wires this") }
}
