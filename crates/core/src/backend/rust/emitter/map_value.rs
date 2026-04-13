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
        // AQ.6.A: when payload_type is F64, capture the scanned value
        // into `__payload_f64` so the epilogue can store it via
        // `push_leaf_with_f64`. NumberConvert is always JSON-class
        // (lowered from `-> f64` on a JSON number regex), so we
        // unconditionally use `scan_number_strict_f64`.
        if matches!(ctx.payload_type, Some(TypeDesc::F64)) {
            quote! {
                match ::parse_that::scan_number_strict_f64(state) {
                    Some(__v) => { __payload_f64 = __v; __has_payload = true; Some(()) }
                    None => None,
                }
            }
        } else {
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
        // AQ.6.A: when a scalar payload is active and the map
        // expression is a matching constant literal, capture the
        // value into the typed payload variable so the epilogue
        // stores it via the matching `push_leaf_with_<T>`.
        if let Some(td) = ctx.payload_type.as_ref() {
            if let Some(payload_setter) = scalar_payload_setter(td, expr) {
                return quote! {
                    match ({ #inner }) {
                        Some(_) => { #payload_setter; __has_payload = true; Some(()) }
                        None => None,
                    }
                };
            }
        }
        // Map-expression evaluation is deferred to the view layer.
        // Preserve the parse side effect.
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

/// AQ.6.A: emit the assignment that captures a constant `MapExpr`
/// into the rule's typed payload local. Returns `None` when the
/// `MapExpr` cannot be coerced to the active payload `TypeDesc` —
/// the caller then falls back to the inner side-effect-only
/// emission.
///
/// Bool / U8 are the historically supported pairings; this helper
/// expands the integer suite uniformly so any narrow integer
/// constant lands in the matching `__payload_<T>` local.
fn scalar_payload_setter(td: &TypeDesc, expr: &MapExpr) -> Option<TokenStream> {
    use quote::format_ident;

    let rust_ident = td.rust_ident()?;
    let payload_local = format_ident!("__payload_{}", rust_ident);

    match (td, expr) {
        (TypeDesc::Bool, MapExpr::BoolLit(v)) => {
            let val = *v;
            Some(quote! { #payload_local = #val })
        }
        (TypeDesc::I8, MapExpr::IntLit(v)) => {
            let val = *v as i8;
            Some(quote! { #payload_local = #val })
        }
        (TypeDesc::U8, MapExpr::IntLit(v)) => {
            let val = (*v & 0xFF) as u8;
            Some(quote! { #payload_local = #val })
        }
        (TypeDesc::I16, MapExpr::IntLit(v)) => {
            let val = *v as i16;
            Some(quote! { #payload_local = #val })
        }
        (TypeDesc::U16, MapExpr::IntLit(v)) => {
            let val = *v as u16;
            Some(quote! { #payload_local = #val })
        }
        (TypeDesc::I32, MapExpr::IntLit(v)) => {
            let val = *v as i32;
            Some(quote! { #payload_local = #val })
        }
        (TypeDesc::U32, MapExpr::IntLit(v)) => {
            let val = *v as u32;
            Some(quote! { #payload_local = #val })
        }
        (TypeDesc::I64, MapExpr::IntLit(v)) => {
            let val = *v;
            Some(quote! { #payload_local = #val })
        }
        (TypeDesc::U64, MapExpr::IntLit(v)) => {
            let val = *v as u64;
            Some(quote! { #payload_local = #val })
        }
        (TypeDesc::F64, MapExpr::FloatLit(v)) => {
            let val = *v;
            Some(quote! { #payload_local = #val })
        }
        _ => None,
    }
}
