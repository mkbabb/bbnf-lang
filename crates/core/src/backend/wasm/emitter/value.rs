//! Value-manipulation emission for the WASM backend: enum wrap, numeric
//! conversion, constants, map expressions, span capture, hex conversion,
//! and fused map.
//!
//! Each method is `pub(super)` so the trait impl in `mod.rs` can delegate
//! to it via `self.emit_xxx_impl(...)`.

use bbnf_ir::{FnDescriptor, GrammarIR, MapExpr, TypeDesc};

use super::{WasmEmitCtx, WasmEmitter};
use crate::backend::ValuePlacement;

impl WasmEmitter {
    pub(super) fn emit_enum_wrap_impl(
        &mut self,
        inner: String,
        _variant_name: &str,
        _alloc: ValuePlacement,
        _ctx: &mut WasmEmitCtx,
    ) -> String {
        inner
    }

    pub(super) fn emit_number_convert_impl(&mut self, _ctx: &mut WasmEmitCtx) -> String {
        "(call $__number_convert (local.get $off) (local.get $len))".to_string()
    }

    pub(super) fn emit_constant_impl(
        &mut self,
        discard_inner: String,
        _value: &str,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        let result = ctx.fresh("const_r");
        format!(
            "(local.set {result} {discard_inner}) \
             (if (result i32) (i32.ne (local.get {result}) (i32.const -1)) \
               (then (local.get {result})) \
               (else (i32.const -1)))"
        )
    }

    pub(super) fn emit_map_expr_impl(
        &mut self,
        inner: String,
        expr: &MapExpr,
        _return_type: Option<&TypeDesc>,
        _alloc: ValuePlacement,
        _ir: &GrammarIR,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        // WASM map expressions: simple cases only.
        let result = ctx.fresh("map_r");
        match expr {
            MapExpr::IntLit(n) => {
                format!(
                    "(local.set {result} {inner}) \
                     (if (result i32) (i32.ne (local.get {result}) (i32.const -1)) \
                       (then (i32.const {n})) \
                       (else (i32.const -1)))"
                )
            }
            MapExpr::BoolLit(b) => {
                let val = if *b { 1 } else { 0 };
                format!(
                    "(local.set {result} {inner}) \
                     (if (result i32) (i32.ne (local.get {result}) (i32.const -1)) \
                       (then (i32.const {val})) \
                       (else (i32.const -1)))"
                )
            }
            _ => {
                // General case: pass through (WASM doesn't evaluate complex expressions).
                format!(
                    "(local.set {result} {inner}) \
                     (local.get {result})"
                )
            }
        }
    }

    pub(super) fn emit_span_capture_impl(
        &mut self,
        inner: String,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        let result = ctx.fresh("span_r");
        format!(
            "(local.set {result} {inner}) \
             (local.get {result})"
        )
    }

    pub(super) fn emit_hex_convert_impl(
        &mut self,
        inner: String,
        _fn_path: &str,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        let result = ctx.fresh("hex_r");
        format!(
            "(local.set {result} {inner}) \
             (if (result i32) (i32.ne (local.get {result}) (i32.const -1)) \
               (then (call $__hex_convert (local.get {result}))) \
               (else (i32.const -1)))"
        )
    }

    pub(super) fn emit_fused_map_impl(
        &mut self,
        _inner: String,
        _inner_fd: &FnDescriptor,
        _outer_fd: &FnDescriptor,
        _alloc: ValuePlacement,
        _ir: &GrammarIR,
        _ctx: &mut WasmEmitCtx,
    ) -> Option<String> {
        None
    }
}
