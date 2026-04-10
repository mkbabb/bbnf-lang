//! Separator-list kernel emission — Tranche X.9b.
//!
//! Canonical `element (SEP element)*` plus optional trailing separator
//! shape, migrated out of the per-emitter `emit_sep_by_impl` Span case
//! so the kernel module is the single source of truth for the loop
//! scaffolding that recognizer sites share.
//!
//! The kernel accepts pre-composed element / separator token streams
//! (which the driver still builds via `compile_node` because the
//! element emission depends on the per-emitter scratch / slab context)
//! plus a `SepByConfig` describing `lo`, optional terminator, and
//! whether the element is Span-collapsible. It returns the full
//! `Option<Span>` expression the Rust emitter splices into the
//! generated parser.
//!
//! Typed (non-Span) element cases still live in the per-emitter
//! `emit_sep_by_impl` because they need scratch-vec ctx access
//! (`ctx.fresh`, `ctx.ir_ctx().emit_scratch_*`) that the kernel crate
//! cannot depend on without circular module references. The kernel is
//! the canonical entry for the hot Span-collapsed shape (where CSS L4
//! and JSON spend most of their sep-by parse time); the typed case is
//! the enum-allocating path used by typed AST rules.

use proc_macro2::TokenStream;
use quote::quote;

use crate::backend::SepByConfig;

/// Emit the canonical Span-case `element (SEP element)*` element loop
/// for the given pre-composed element / separator token streams.
///
/// The returned expression evaluates to `Option<Span>` and:
///
/// 1. Records the starting offset.
/// 2. Attempts the first element; on miss returns `None` (or `Some(empty)`
///    when `config.lo == 0`).
/// 3. Loops `(separator, element)` pairs until either the separator
///    fails (break), the element fails (rewind to the pre-separator
///    checkpoint + break), or — when `config.terminator_bytes` is a
///    single byte — the terminator is seen at the current offset.
/// 4. Returns the accumulated span iff the matched element count is
///    ≥ `config.lo`.
///
/// `count_var` is a caller-provided unique identifier for the
/// per-iteration counter (generated via `ctx.fresh("count")` by the
/// Rust emitter). Passing it in keeps the kernel backend-agnostic: the
/// kernel never needs to reach into `RustEmitCtx` to generate fresh
/// identifiers.
pub fn emit_span_body(
    element: &TokenStream,
    separator: &TokenStream,
    config: &SepByConfig,
    count_var: &syn::Ident,
) -> TokenStream {
    let lo_lit = config.lo as usize;

    // Terminator byte condition expression (single-byte terminators
    // only — multi-byte terminators drop through to the non-terminator
    // path and let the caller handle the close-check inline).
    let terminator_cond = if let Some(ref tb) = config.terminator_bytes {
        if tb.len() == 1 {
            let byte = tb[0];
            Some(quote! {
                state.offset < state.src.len()
                    && state.src.as_bytes()[state.offset] == #byte
            })
        } else {
            None
        }
    } else {
        None
    };
    let terminator_break = terminator_cond.as_ref().map(|cond| {
        quote! { if #cond { break; } }
    }).unwrap_or_default();

    quote! {
        (|| {
            let __sp_start = state.offset;
            let mut #count_var: usize = 0;

            match #element {
                Some(_) => { #count_var += 1; }
                None => {
                    return if #count_var >= #lo_lit {
                        Some(::parse_that::Span::new(__sp_start, state.offset, state.src))
                    } else {
                        None
                    };
                }
            }

            loop {
                #terminator_break
                let __cp = state.offset;
                match #separator {
                    Some(_) => {}
                    None => break,
                }
                match #element {
                    Some(_) => { #count_var += 1; }
                    None => {
                        state.offset = __cp;
                        break;
                    }
                }
            }

            if #count_var >= #lo_lit {
                Some(::parse_that::Span::new(__sp_start, state.offset, state.src))
            } else {
                None
            }
        })()
    }
}

/// Legacy stable surface retained for the V.7 consumer-invariant test.
/// Callers should prefer `emit_span_body`; this shim wraps the span
/// body in a zero-cost `{ ... }` block expression so older call sites
/// that expected a single-expression form continue to compile.
pub fn emit_call(
    element: &TokenStream,
    separator: &TokenStream,
    config: &SepByConfig,
    count_var: &syn::Ident,
) -> TokenStream {
    emit_span_body(element, separator, config, count_var)
}
