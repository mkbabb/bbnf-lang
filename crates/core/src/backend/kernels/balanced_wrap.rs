//! Balanced-delimiter kernel emission — Tranche W phase 3d.
//!
//! Owns the delim-scan emission body that previously lived inline in
//! `backend/rust/emitter/dispatch.rs::emit_delim_scan_impl`. The emitter
//! now delegates to [`emit_call`] with the per-grammar dynamic dispatch
//! token streams (`block_call`, `pivot_call`, `trail_consume`) spliced
//! in. The kernel module is the canonical home for the emission shape;
//! per-target emitters compose it with their grammar-specific call
//! tokens.

use proc_macro2::TokenStream;
use quote::quote;

/// Emit a delimiter-balanced scanner block.
///
/// `open` and `close` are the literal delimiter bytes; `pivot` is the
/// byte that splits the body alternatives (typically a separator like
/// `;` or `,`). `block_call`, `pivot_call`, and `trail_consume` are
/// per-grammar token streams produced by the emitter — they encode the
/// recursive descent into nested blocks, the pivot-rule call, and any
/// trailing-byte consumption.
///
/// The body matches the previous `emit_delim_scan_impl` exactly. Moving
/// it to the kernel module satisfies the tranche-W §5 hard gate
/// ("backend/kernels/ consumer count ≥ 1 production caller per family
/// module") via the real production wiring in
/// `backend/rust/emitter/dispatch.rs::emit_delim_scan_impl`.
pub fn emit_call(
    open: u8,
    close: u8,
    pivot: u8,
    block_call: &TokenStream,
    pivot_call: &TokenStream,
    trail_consume: &TokenStream,
) -> TokenStream {
    quote! {
        (|| {
            let __ds_start = state.offset;
            if state.offset >= state.src.len()
                || state.src.as_bytes()[state.offset] != #open
            {
                return None;
            }
            state.offset += 1;
            loop {
                if state.offset >= state.src.len() {
                    state.offset = __ds_start;
                    return None;
                }
                let __b = state.src.as_bytes()[state.offset];
                if __b == #close {
                    state.offset += 1;
                    return Some(::parse_that::Span::new(
                        __ds_start,
                        state.offset,
                        state.src,
                    ));
                }
                if __b == #open {
                    match #block_call {
                        Some(_) => continue,
                        None => {
                            state.offset = __ds_start;
                            return None;
                        }
                    }
                }
                // Scan for pivot byte.
                loop {
                    if state.offset >= state.src.len() {
                        break;
                    }
                    let __pb = state.src.as_bytes()[state.offset];
                    if __pb == #pivot {
                        state.offset += 1;
                        #trail_consume
                        match #pivot_call {
                            Some(_) => break, // Back to outer loop.
                            None => {
                                state.offset = __ds_start;
                                return None;
                            }
                        }
                    }
                    if __pb == #close || __pb == #open {
                        break; // Let outer loop handle delimiter.
                    }
                    state.offset += 1;
                }
            }
        })()
    }
}
