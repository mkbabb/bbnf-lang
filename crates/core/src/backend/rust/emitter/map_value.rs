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

use bbnf_ir::passes::PayloadField;
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
        // AQ.6.B: when an aggregate layout is active, advance the
        // field cursor and write the scanned f64 into the buffer at
        // the layout-recorded offset. The aggregate path supersedes
        // the legacy `__payload_f64` capture.
        if ctx.payload_layout.is_some() {
            if let Some(field) = ctx.next_aggregate_field() {
                if matches!(field.ty, TypeDesc::F64) {
                    let offset = field.offset as usize;
                    let end = offset + 8;
                    return quote! {
                        match ::parse_that::scan_number_strict_f64(state) {
                            Some(__v) => {
                                __aggregate_buf[#offset..#end]
                                    .copy_from_slice(&__v.to_le_bytes());
                                __has_payload = true;
                                Some(())
                            }
                            None => None,
                        }
                    };
                }
            }
            // Layout active but the current field is not F64 (or no
            // more fields) — return the scanner's `Option<f64>`
            // directly (AU.6.5 no-value-discard). Callers match via
            // `Some(_)` so the inner f64 composes identically to
            // `Option<()>`; stripping the `.map(|_| ())` wrap shrinks
            // the inner-loop icache footprint.
            return quote! {
                ::parse_that::scan_number_strict_f64(state)
            };
        }

        // AT.1: capture f64 when the payload type set includes F64.
        if ctx.has_payload_type(&TypeDesc::F64) {
            let tag_set = ctx.payload_tag(&TypeDesc::F64).map(|tag| {
                quote! { __payload_tag = #tag; }
            });
            quote! {
                match ::parse_that::scan_number_strict_f64(state) {
                    Some(__v) => { __payload_f64 = __v; #tag_set __has_payload = true; Some(()) }
                    None => None,
                }
            }
        } else {
            // AU.6.5 no-value-discard: bare scanner output. The
            // `Option<f64>` threads upward unchanged — callers see
            // `Some(_)` as truthy regardless of the inner type.
            quote! {
                ::parse_that::scan_number_strict_f64(state)
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
        // AQ.6.B: when an aggregate payload layout is active, the
        // current Seq position determines which field of the buffer
        // receives the constant-mapped scalar. Advance the per-field
        // cursor and write the scalar bytes via `to_le_bytes()` at
        // the layout-recorded offset.
        if ctx.payload_layout.is_some() {
            if let Some(field) = ctx.next_aggregate_field() {
                if let Some(buf_setter) = aggregate_constant_setter(&field, expr) {
                    return quote! {
                        match ({ #inner }) {
                            Some(_) => { #buf_setter; __has_payload = true; Some(()) }
                            None => None,
                        }
                    };
                }
            }
            // Layout active but no matching scalar literal at this
            // position — fall through to the side-effect-only path
            // (the field defaults to its zero-init from the prelude).
            return quote! {
                { #inner }
            };
        }

        // AT.1: check if the map expression produces a constant
        // whose type is in the current rule's payload_types set.
        // If so, capture the value into the typed payload variable
        // and set the tag (for multi-type Alts).
        for td in &ctx.payload_types {
            if let Some(payload_setter) = scalar_payload_setter(td, expr) {
                let tag_set = ctx.payload_tag(td).map(|tag| {
                    quote! { __payload_tag = #tag; }
                });
                return quote! {
                    match ({ #inner }) {
                        Some(_) => { #payload_setter; #tag_set __has_payload = true; Some(()) }
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
        fn_path: &str,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // AU.2.4: route HexConvert through the tape-first emitter so
        // `hex = "#" , /[0-9a-fA-F]{3,8}/ -> parse_hex_color(input) : u32`
        // activates `push_leaf_with_u32` instead of dropping the u32
        // result. The `inner` token stream parses the hex digits and
        // advances the cursor; capturing `state.offset` before the
        // inner parse lets us read the exact matched bytes (just the
        // hex digits, not any leading delimiter the enclosing rule
        // already consumed), pass them through the user-provided
        // converter, and stash the u32 into either the aggregate
        // buffer (layout-active path) or the typed payload local
        // (single-type path).
        let fn_path_tokens: TokenStream = fn_path.parse().unwrap_or_else(|err| {
            panic!("hex_convert: invalid fn_path `{fn_path}`: {err}")
        });

        // Aggregate layout active → write u32 bytes into the buffer.
        if ctx.payload_layout.is_some() {
            if let Some(field) = ctx.next_aggregate_field() {
                if matches!(field.ty, TypeDesc::U32) {
                    let offset = field.offset as usize;
                    let end = offset + 4;
                    return quote! {
                        {
                            let __hex_start = state.offset;
                            match ({ #inner }) {
                                Some(_) => {
                                    let __hex_bytes = &state.src_bytes[__hex_start..state.offset];
                                    let __hex_str = ::core::str::from_utf8(__hex_bytes)
                                        .unwrap_or("");
                                    let __hex_val: u32 = #fn_path_tokens(__hex_str);
                                    __aggregate_buf[#offset..#end]
                                        .copy_from_slice(&__hex_val.to_le_bytes());
                                    __has_payload = true;
                                    Some(())
                                }
                                None => None,
                            }
                        }
                    };
                }
            }
            // Layout active but the current field is not U32 — fall
            // through to side-effect only.
            return quote! { { #inner } };
        }

        // Single-type payload path → capture into `__payload_u32`.
        if ctx.has_payload_type(&TypeDesc::U32) {
            let tag_assign = ctx.payload_tag(&TypeDesc::U32).map(|tag| {
                quote! { __payload_tag = #tag; }
            });
            return quote! {
                {
                    let __hex_start = state.offset;
                    match ({ #inner }) {
                        Some(_) => {
                            let __hex_bytes = &state.src_bytes[__hex_start..state.offset];
                            let __hex_str = ::core::str::from_utf8(__hex_bytes)
                                .unwrap_or("");
                            __payload_u32 = #fn_path_tokens(__hex_str);
                            #tag_assign
                            __has_payload = true;
                            Some(())
                        }
                        None => None,
                    }
                }
            };
        }

        // No payload active (rule not marked as needing u32 capture) —
        // preserve the parse side effect. View layer can still recover
        // the value from the span via `parse_hex_color(view.text())`.
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
        //
        // AU.6.5 no-value-discard: return the scanner's `Option<f64>`
        // directly — callers compose via `Some(_)` so the
        // `.map(|_| ())` wrap has been dropped.
        match inner_fd {
            FnDescriptor::NumberConvert => Some(quote! {
                ::parse_that::scan_number_strict_f64(state)
            }),
            _ => Some(quote! { { #inner } }),
        }
    }
}

/// AQ.6.B: emit the assignment that copies a constant `MapExpr`'s
/// scalar bytes into the active aggregate buffer at the field's
/// layout offset. Returns `None` when the `MapExpr` cannot be
/// coerced to the field's `TypeDesc` (caller falls through to
/// side-effect-only emission).
///
/// Each scalar lowers to a `to_le_bytes()` call followed by a
/// `copy_from_slice` into the buffer slice — LLVM lowers this to a
/// single store of the appropriate width.
fn aggregate_constant_setter(field: &PayloadField, expr: &MapExpr) -> Option<TokenStream> {
    let offset = field.offset as usize;
    let size = field.ty.payload_size_bytes()? as usize;
    let value_expr: TokenStream = match (&field.ty, expr) {
        (TypeDesc::Bool, MapExpr::BoolLit(v)) => {
            let val = *v as u8;
            quote! { [#val] }
        }
        (TypeDesc::I8, MapExpr::IntLit(v)) => {
            let val = *v as i8;
            quote! { (#val as i8).to_le_bytes() }
        }
        (TypeDesc::U8, MapExpr::IntLit(v)) => {
            let val = (*v & 0xFF) as u8;
            quote! { [#val] }
        }
        (TypeDesc::I16, MapExpr::IntLit(v)) => {
            let val = *v as i16;
            quote! { (#val as i16).to_le_bytes() }
        }
        (TypeDesc::U16, MapExpr::IntLit(v)) => {
            let val = *v as u16;
            quote! { (#val as u16).to_le_bytes() }
        }
        (TypeDesc::I32, MapExpr::IntLit(v)) => {
            let val = *v as i32;
            quote! { (#val as i32).to_le_bytes() }
        }
        (TypeDesc::U32, MapExpr::IntLit(v)) => {
            let val = *v as u32;
            quote! { (#val as u32).to_le_bytes() }
        }
        (TypeDesc::I64, MapExpr::IntLit(v)) => {
            let val = *v;
            quote! { (#val as i64).to_le_bytes() }
        }
        (TypeDesc::U64, MapExpr::IntLit(v)) => {
            let val = *v as u64;
            quote! { (#val as u64).to_le_bytes() }
        }
        (TypeDesc::F64, MapExpr::FloatLit(v)) => {
            let val = *v;
            quote! { (#val as f64).to_le_bytes() }
        }
        _ => return None,
    };
    let end = offset + size;
    Some(quote! {
        __aggregate_buf[#offset..#end].copy_from_slice(&#value_expr)
    })
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
