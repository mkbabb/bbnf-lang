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

    pub(super) fn emit_number_convert_impl(&mut self, _ctx: &mut RustEmitCtx) -> TokenStream {
        // Eisel-Lemire number scan is side-effecting (it advances
        // `state.offset`). The parsed `f64` is not captured here —
        // the view layer reconstructs it from the span bytes.
        quote! {
            (::parse_that::scan_number_f64(state)).map(|_| ())
        }
    }

    pub(super) fn emit_constant_impl(
        &mut self,
        discard_inner: TokenStream,
        _value: &str,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // Constant mapping discards the parsed Span and produces
        // the constant value. Under tape-first the view layer
        // derives the constant from the span + variant_idx, so we
        // only need to preserve the side effect.
        quote! {
            { #discard_inner }
        }
    }

    pub(super) fn emit_map_expr_impl(
        &mut self,
        inner: TokenStream,
        _expr: &MapExpr,
        _return_type: Option<&TypeDesc>,
        _ir: &GrammarIR,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
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
        match inner_fd {
            FnDescriptor::NumberConvert => Some(quote! {
                (::parse_that::scan_number_f64(state)).map(|_| ())
            }),
            _ => Some(quote! { { #inner } }),
        }
    }
}
