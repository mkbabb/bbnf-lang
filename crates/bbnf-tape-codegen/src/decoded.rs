//! Body fragment for `bbnf_tape::psi::write_decoded`.
//!
//! This helper is the Stage-B payload-decode kernel — it matches on
//! [`bbnf_tape::psi::PayloadKind`] and writes the decoded value into
//! the per-column arena offset. AW-IV left it out-of-line; the
//! per-shape emitter splices the body inline so the scalar payload
//! kinds (F64 / I64 / U8 / Bool / HexU32) never cross a function-call
//! boundary when the emitter has resolved the kind at codegen time.
//!
//! The runtime helper is an `unsafe fn` — the fragment below is the
//! body (already containing `unsafe { ... }` blocks for each
//! pointer write). The splicer wraps the splice in its own `unsafe`
//! block at the call site.

use proc_macro2::TokenStream;
use quote::ToTokens;

/// Verbatim source for the body of `bbnf_tape::psi::write_decoded`
/// — the text between the outer `fn ... { ... }` braces.
///
/// The runtime helper at `crates/bbnf-tape/src/psi.rs:662` survives
/// unchanged; it remains the cold-path `PayloadStream::fill_*`
/// dispatch target. This constant is the splice source for the
/// per-shape emitter's Stage-B inline path; divergence between the
/// two is detected by `tests/parse_fragments.rs`.
pub const SOURCE: &str = r#"{
    let lo = job.input_lo as usize;
    let hi = job.input_hi as usize;
    let slice = &input[lo..hi];
    let dst_off = job.arena_offset as usize;
    match job.kind {
        PayloadKind::F64 => {
            let s = std::str::from_utf8(slice).unwrap_or("0");
            let bits = s.parse::<f64>().unwrap_or(0.0).to_bits();
            let bytes = bits.to_le_bytes();
            debug_assert!(dst_off + 8 <= cells.pay_agg_len);
            unsafe {
                std::ptr::copy_nonoverlapping(bytes.as_ptr(), cells.pay_agg.add(dst_off), 8);
            }
        }
        PayloadKind::I64 => {
            let s = std::str::from_utf8(slice).unwrap_or("0");
            let bits = s.parse::<i64>().unwrap_or(0) as u64;
            let bytes = bits.to_le_bytes();
            debug_assert!(dst_off + 8 <= cells.pay_agg_len);
            unsafe {
                std::ptr::copy_nonoverlapping(bytes.as_ptr(), cells.pay_agg.add(dst_off), 8);
            }
        }
        PayloadKind::U8 => {
            let value = slice.first().copied().unwrap_or(0);
            debug_assert!(dst_off + 1 <= cells.pay_agg_len);
            unsafe {
                *cells.pay_agg.add(dst_off) = value;
            }
        }
        PayloadKind::Bool => {
            let value: u8 = if slice.eq_ignore_ascii_case(b"true") { 1 } else { 0 };
            debug_assert!(dst_off + 1 <= cells.pay_agg_len);
            unsafe {
                *cells.pay_agg.add(dst_off) = value;
            }
        }
        PayloadKind::HexU32 => {
            let value = parse_hex_u32(slice);
            let bytes = value.to_le_bytes();
            debug_assert!(dst_off + 4 <= cells.pay_agg_len);
            unsafe {
                std::ptr::copy_nonoverlapping(bytes.as_ptr(), cells.pay_agg.add(dst_off), 4);
            }
        }
        PayloadKind::String => {
            // AW-III.W1.A: route through the JSON string-escape
            // decoder kernel — `\n`, `\t`, `\"`, `\\`, `\/`, `\b`,
            // `\f`, `\r`, `\uXXXX`, and `\uD8XX\uDCXX` surrogate
            // pairs all decode into the arena frame. The kernel is
            // general per `decoders/json_string`; the dispatch sits
            // here because `PayloadKind::String` is the lifter's
            // canonical "string with escapes" classification.
            unsafe {
                crate::decoders::json_string::decode_into(
                    slice,
                    cells.pay_agg,
                    dst_off,
                    cells.pay_agg_len,
                );
            }
        }
        PayloadKind::AggregateLarge => {
            debug_assert!(dst_off + slice.len() <= cells.pay_agg_len);
            unsafe {
                std::ptr::copy_nonoverlapping(
                    slice.as_ptr(),
                    cells.pay_agg.add(dst_off),
                    slice.len(),
                );
            }
        }
    }
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
            "bbnf-tape-codegen: write_decoded body fragment must \
             parse as syn::Block — the runtime helper at \
             crates/bbnf-tape/src/psi.rs has likely diverged from \
             this crate's SOURCE constant",
        )
        .to_token_stream()
}
