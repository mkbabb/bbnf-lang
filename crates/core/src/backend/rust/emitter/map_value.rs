//! Value-construction emission for the Rust backend.
//!
//! Tranche AC.2 tape-first. Under the new model, scalar projection
//! (number/hex/constant/map-expr) is deferred to the view layer —
//! `view.span_text().parse()` and kin run lazily from the typed
//! accessors. The emitter's job is to preserve the parse side
//! effect (`state.offset` advance) and surface a composable
//! `Option<()>` at the call site.
//!
//! `EnumWrap` and `BoxWrap` become pass-throughs: the rule
//! variant discriminator is carried in the owning rule's
//! epilogue via `push_compound(..., variant_idx)`; there is no
//! `Enum::Variant(inner)` construction anywhere in the emitter.

use bbnf_ir::{FnDescriptor, GrammarIR, MapExpr, TypeDesc};
use proc_macro2::TokenStream;
use quote::quote;

use crate::backend::ValuePlacement;

use super::{RustEmitCtx, RustEmitter};

impl RustEmitter {
    pub(super) fn emit_enum_wrap_impl(
        &mut self,
        inner: TokenStream,
        _variant_name: &str,
        _alloc: ValuePlacement,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // Variant wrapping is a no-op under tape-first: the
        // enclosing rule's epilogue writes the variant
        // discriminator into the compound record's `variant_idx`.
        inner
    }

    pub(super) fn emit_number_convert_impl(&mut self, ctx: &mut RustEmitCtx) -> TokenStream {
        // AN Phase 0: when payload_kind is F64, capture the scanned
        // value into `__payload_f64` so the epilogue can store it
        // via `push_leaf_with_f64`.
        //
        // `json` flag selects `scan_number_strict_f64` (RFC 8259) vs
        // `scan_number_f64` (generic/CSS-compatible).
        if let Some(crate::backend::rust::emitter_types::PayloadKind::F64 { json }) = ctx.payload_kind {
            if json {
                quote! {
                    match ::parse_that::scan_number_strict_f64(state) {
                        Some(__v) => { __payload_f64 = __v; __has_payload = true; Some(()) }
                        None => None,
                    }
                }
            } else {
                quote! {
                    match ::parse_that::scan_number_f64(state) {
                        Some(__v) => { __payload_f64 = __v; __has_payload = true; Some(()) }
                        None => None,
                    }
                }
            }
        } else {
            // No payload — use JSON scanner (NumberConvert is always
            // JSON-class, matching `fused_number_rules` gating).
            quote! {
                (::parse_that::scan_number_strict_f64(state)).map(|_| ())
            }
        }
    }

    pub(super) fn emit_constant_impl(
        &mut self,
        discard_inner: TokenStream,
        _value: &str,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // Constant mapping discards the parsed Span and produces
        // the constant value. Under tape-first the view layer
        // derives the constant from the span + variant_idx, so we
        // only need to preserve the side effect — unless a payload
        // is active, in which case we set the payload variable.
        //
        // Note: `emit_constant` is currently dead code (the driver
        // routes through `emit_map_expr`), but kept consistent for
        // future wiring.
        let _ = ctx;
        quote! {
            { #discard_inner }
        }
    }

    pub(super) fn emit_map_expr_impl(
        &mut self,
        inner: TokenStream,
        expr: &MapExpr,
        _return_type: Option<&TypeDesc>,
        _ir: &GrammarIR,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // AN Phase 0: when a Bool or U8 payload is active and the
        // map expression is a matching constant literal, capture the
        // value into the payload variable so the epilogue stores it
        // via push_leaf_with_bool/u8.
        use crate::backend::rust::emitter_types::PayloadKind;
        match (ctx.payload_kind, expr) {
            (Some(PayloadKind::Bool), MapExpr::BoolLit(val)) => {
                let val_lit = *val;
                return quote! {
                    match ({ #inner }) {
                        Some(_) => { __payload_bool = #val_lit; __has_payload = true; Some(()) }
                        None => None,
                    }
                };
            }
            (Some(PayloadKind::U8), MapExpr::IntLit(val)) => {
                let val_u8 = (*val & 0xFF) as u8;
                return quote! {
                    match ({ #inner }) {
                        Some(_) => { __payload_u8 = #val_u8; __has_payload = true; Some(()) }
                        None => None,
                    }
                };
            }
            _ => {}
        }
        // Map-expression evaluation is deferred to the view
        // layer. Preserve the parse side effect.
        quote! {
            { #inner }
        }
    }

    pub(super) fn emit_span_capture_impl(
        &mut self,
        inner: TokenStream,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // Span capture is also a view-layer concern: the tape
        // record already carries (span_lo, span_hi). We preserve
        // the parse side effect.
        quote! {
            { #inner }
        }
    }

    pub(super) fn emit_hex_convert_impl(
        &mut self,
        inner: TokenStream,
        _fn_path: &str,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        quote! {
            { #inner }
        }
    }

    pub(super) fn emit_fused_map_impl(
        &mut self,
        inner: TokenStream,
        inner_fd: &FnDescriptor,
        _outer_fd: &FnDescriptor,
        _alloc: ValuePlacement,
        _ir: &GrammarIR,
        _ctx: &mut RustEmitCtx,
    ) -> Option<TokenStream> {
        // All fused (inner_fd, outer_fd) pairs collapse to the
        // inner expression's side effect under tape-first. The
        // view layer owns the downstream projection.
        //
        // NumberConvert is always JSON-class (lowered from
        // `-> f64` on a JSON number regex).
        match inner_fd {
            FnDescriptor::NumberConvert => Some(quote! {
                (::parse_that::scan_number_strict_f64(state)).map(|_| ())
            }),
            _ => Some(quote! { { #inner } }),
        }
    }
}
