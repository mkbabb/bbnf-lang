//! Separator-list kernel emission.
//!
//! Composition: `memchr2` + inlined element callback. Used by
//! `NodeFacts.recognizer.shape == SeparatorList`.

use proc_macro2::TokenStream;
use quote::quote;

/// Emit a memchr-driven separator-list scanner call.
///
/// Tranche W phase 3d: returns a real `memchr2` invocation against
/// the separator byte and the (presumed) closing byte. The repeat
/// driver supplies the separator byte at the call site; this kernel
/// produces the inline TokenStream that the emitter splices into
/// the parser body. The full sep-by element loop is still driven by
/// `backend/driver/repeat.rs::emit_sep_by` — this kernel is the
/// shape-classified entry point that the strategy CSP routes to
/// when `RecognizerShape::SeparatorList` is the resolved decision.
pub fn emit_call(separator: u8) -> TokenStream {
    quote! {
        ::memchr::memchr(#separator, state.remaining().as_bytes())
    }
}
