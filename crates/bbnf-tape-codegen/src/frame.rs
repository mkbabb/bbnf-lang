//! Body fragment for
//! `bbnf_tape::driver::FrameStack::nearest_variant_frame`.
//!
//! This helper walks the frame stack top-down to recover the first
//! non-anonymous frame's `variant_idx` — consulted on every
//! payload-bearing leaf emission. Per-shape emission obviates the
//! stack walk for most call sites (the variant_idx is known at
//! codegen time), but the body is still required for the fallback
//! emission path where the splice embeds the walk inline rather than
//! crossing the function-call boundary.
//!
//! The runtime helper is a method on `FrameStack` — the splicer
//! captures `&self` into the generated code's outer scope (or binds
//! `let stack_self = &self;` and substitutes `self` references
//! appropriately) before emitting `quote! { #fragment }`. The body
//! references `self.overflow` and `self.inline` / `self.inline_len`
//! verbatim.

use proc_macro2::TokenStream;
use quote::ToTokens;

/// Verbatim source for the body of
/// `bbnf_tape::driver::FrameStack::nearest_variant_frame` — the text
/// between the outer `fn ... { ... }` braces.
///
/// The runtime helper at `crates/bbnf-tape/src/driver.rs:546`
/// survives unchanged. This constant is the splice source for the
/// per-shape emitter's payload-emit paths; divergence between the
/// two is detected by `tests/parse_fragments.rs`.
pub const SOURCE: &str = r#"{
    // Walk overflow from top down.
    for f in self.overflow.iter().rev() {
        if f.variant_idx != u8::MAX {
            return Some(f);
        }
    }
    // Then walk inline from top down.
    for i in (0..self.inline_len as usize).rev() {
        let f = &self.inline[i];
        if f.variant_idx != u8::MAX {
            return Some(f);
        }
    }
    None
}"#;

/// Parse [`SOURCE`] as a [`syn::Block`] and return the
/// [`TokenStream`] the per-shape emitter splices inline.
///
/// # Panics
///
/// Panics if [`SOURCE`] fails to parse — caught by
/// `tests/parse_fragments.rs`.
pub fn fragment() -> TokenStream {
    syn::parse_str::<syn::Block>(SOURCE)
        .expect(
            "bbnf-tape-codegen: nearest_variant_frame body fragment must \
             parse as syn::Block — the runtime helper at \
             crates/bbnf-tape/src/driver.rs has likely diverged from \
             this crate's SOURCE constant",
        )
        .to_token_stream()
}
