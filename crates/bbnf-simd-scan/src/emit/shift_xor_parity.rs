//! Body source for the 6-op shift-XOR prefix-XOR ladder.
//!
//! # Paired runtime
//!
//! [`crate::parity::shift_xor_prefix`] (sibling module `parity.rs:166`).
//!
//! # Splice-target type
//!
//! [`syn::Block`] — the body is a self-contained sequence of statements
//! operating on a bound `let mut x: u64 = <mask>;` named by the
//! enclosing emitter scope.
//!
//! The fragment assumes the caller has already bound the input u64
//! into a local `x` and computes the prefix-XOR in place. The caller
//! reads `x` after the block to obtain the result.
//!
//! # Shape
//!
//! ```text
//! {
//!     x ^= x << 1;
//!     x ^= x << 2;
//!     x ^= x << 4;
//!     x ^= x << 8;
//!     x ^= x << 16;
//!     x ^= x << 32;
//! }
//! ```
//!
//! Lemire's "fast parsing" §6.1 shape — correct on all architectures,
//! used verbatim as the portable fallback and as the test reference
//! for [`crate::parity::prefix_xor_64`].

use proc_macro2::TokenStream;
use quote::ToTokens;

/// Verbatim source of the shift-XOR prefix-XOR ladder body.
///
/// Requires the caller to have pre-bound `x: u64` (mutable) in the
/// enclosing scope. The block mutates `x` in place.
pub const SOURCE: &str = r#"{
    x ^= x << 1;
    x ^= x << 2;
    x ^= x << 4;
    x ^= x << 8;
    x ^= x << 16;
    x ^= x << 32;
}"#;

/// Parse [`SOURCE`] into a [`TokenStream`] suitable for splicing at the
/// call site of the enclosing emitter-produced fn.
pub fn fragment() -> TokenStream {
    syn::parse_str::<syn::Block>(SOURCE)
        .expect("bbnf-simd-scan::emit::shift_xor_parity: SOURCE must parse as syn::Block")
        .to_token_stream()
}
