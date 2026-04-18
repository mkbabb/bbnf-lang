//! Body source for the trailing-zero bitmap-compaction kernel.
//!
//! # Paired runtime
//!
//! [`crate::compaction::compact_stripe_tzcnt`] (sibling
//! `compaction.rs:27`). Compacts one 64-bit mask into
//! `out_positions: &mut Vec<u32>` + `out_kinds: &mut Vec<u8>`, reading
//! the kind byte from `bytes[stripe_base + bit]`.
//!
//! # Splice-target type
//!
//! [`syn::Block`]. The body is architecture-neutral — `u64::trailing_zeros`
//! lowers to `TZCNT` on x86_64-v3 / `CLZ+NEG` on aarch64 / `CTZ`
//! intrinsic on WASM / hardware-specific on RISC-V. No `unsafe` is
//! required for the compaction itself; the runtime kernel's
//! `get_unchecked` is the only `unsafe` and is present because the
//! caller has already validated `stripe_base + 64 <= bytes.len()`.
//!
//! # Prebound names
//!
//! The fragment expects the enclosing scope to have the following
//! identifiers in scope at splice time:
//!
//! - `mask: u64` — the per-stripe classification bitmap.
//! - `stripe_base: u32` — the byte offset of stripe bit 0.
//! - `bytes: &[u8]` — the full input slice.
//! - `out_positions: &mut Vec<u32>` — compacted positions output.
//! - `out_kinds: &mut Vec<u8>` — compacted kind bytes output.
//!
//! # Shape
//!
//! ```text
//! {
//!     let mut m = mask;
//!     while m != 0 {
//!         let bit = m.trailing_zeros();
//!         let pos = stripe_base + bit;
//!         let kind = unsafe { *bytes.get_unchecked(pos as usize) };
//!         out_positions.push(pos);
//!         out_kinds.push(kind);
//!         m &= m - 1;
//!     }
//! }
//! ```

use proc_macro2::TokenStream;
use quote::ToTokens;

/// Verbatim source of the tzcnt-loop compaction body.
pub const SOURCE: &str = r#"{
    let mut m = mask;
    while m != 0 {
        let bit = m.trailing_zeros();
        let pos = stripe_base + bit;
        let kind = unsafe { *bytes.get_unchecked(pos as usize) };
        out_positions.push(pos);
        out_kinds.push(kind);
        m &= m - 1;
    }
}"#;

/// Parse [`SOURCE`] into a [`TokenStream`].
pub fn fragment() -> TokenStream {
    syn::parse_str::<syn::Block>(SOURCE)
        .expect("bbnf-simd-scan::emit::tzcnt_compact: SOURCE must parse as syn::Block")
        .to_token_stream()
}
