//! Value-manipulation emission for the WASM backend: enum wrap, numeric
//! conversion, constants, map expressions, span capture, hex conversion,
//! and fused map.
//!
//! Each method is `pub(super)` so the trait impl in `mod.rs` can delegate
//! to it via `self.emit_xxx_impl(...)`.

use bbnf_ir::{FnDescriptor, GrammarIR, MapExpr, NumericDomain, TypeDesc};

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
        return_type: Option<&TypeDesc>,
        _alloc: ValuePlacement,
        _ir: &GrammarIR,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        // Fold before decoding so representable constant arithmetic uses the
        // exact same scalar gates as a direct literal. Anything still dynamic
        // after folding must be an explicit identity; silently passing through
        // an unsupported semantic expression would miscompile it as `input`.
        let mut folded = expr.clone();
        folded
            .checked_fold(return_type.and_then(NumericDomain::from_type_desc))
            .unwrap_or_else(|error| {
                panic!(
                    "WASM cannot emit invalid numeric map expression: {}",
                    error.message()
                )
            });
        let result = ctx.fresh("map_r");
        match &folded {
            MapExpr::IntLit(n) => {
                let wat_value = wasm_i32_from_signed(*n, return_type);
                format!(
                    "(local.set {result} {inner}) \
                     (if (result i32) (i32.ne (local.get {result}) (i32.const -1)) \
                       (then (i32.const {wat_value})) \
                       (else (i32.const -1)))"
                )
            }
            MapExpr::UIntLit(value) => {
                let wat_value = wasm_i32_from_unsigned(*value, return_type);
                format!(
                    "(local.set {result} {inner}) \
                     (if (result i32) (i32.ne (local.get {result}) (i32.const -1)) \
                       (then (i32.const {wat_value})) \
                       (else (i32.const -1)))"
                )
            }
            MapExpr::BoolLit(b) => {
                assert!(
                    matches!(return_type, None | Some(TypeDesc::Bool)),
                    "boolean WASM map literal has a non-boolean return type"
                );
                let val = if *b { 1 } else { 0 };
                format!(
                    "(local.set {result} {inner}) \
                     (if (result i32) (i32.ne (local.get {result}) (i32.const -1)) \
                       (then (i32.const {val})) \
                       (else (i32.const -1)))"
                )
            }
            MapExpr::Input => {
                format!(
                    "(local.set {result} {inner}) \
                     (local.get {result})"
                )
            }
            _ => panic!(
                "WASM backend cannot emit exact map expression {folded:?}: \
                 the current map-result ABI supports only identity, Bool, \
                 and folded representable integer constants"
            ),
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

fn wasm_i32_from_signed(value: i64, return_type: Option<&TypeDesc>) -> i32 {
    let value = match return_type {
        Some(TypeDesc::I8) => i32::from(
            i8::try_from(value).unwrap_or_else(|_| panic!("WASM i8 map literal is out of range")),
        ),
        Some(TypeDesc::I16) => i32::from(
            i16::try_from(value).unwrap_or_else(|_| panic!("WASM i16 map literal is out of range")),
        ),
        Some(TypeDesc::I32) => {
            i32::try_from(value).unwrap_or_else(|_| panic!("WASM i32 map literal is out of range"))
        }
        Some(TypeDesc::I64) => {
            panic!(
                "WASM backend cannot emit exact i64 map literal {value}: \
                 the current map-result ABI is i32"
            )
        }
        Some(TypeDesc::U8) => i32::from(
            u8::try_from(value).unwrap_or_else(|_| panic!("WASM u8 map literal is out of range")),
        ),
        Some(TypeDesc::U16) => i32::from(
            u16::try_from(value).unwrap_or_else(|_| panic!("WASM u16 map literal is out of range")),
        ),
        Some(TypeDesc::U32) => {
            let value = u32::try_from(value)
                .unwrap_or_else(|_| panic!("WASM u32 map literal is out of range"));
            i32::from_ne_bytes(value.to_ne_bytes())
        }
        Some(TypeDesc::U64) => {
            panic!(
                "WASM backend cannot emit exact u64 map literal {value}: \
                 the current map-result ABI is i32"
            )
        }
        Some(_) => panic!("integer WASM map literal has a non-integer return type"),
        None => i32::try_from(value)
            .unwrap_or_else(|_| panic!("untyped WASM integer map literal is outside i32")),
    };
    assert!(
        value != -1,
        "WASM cannot emit integer -1: its i32 bit pattern is the failure sentinel"
    );
    value
}

fn wasm_i32_from_unsigned(value: u64, return_type: Option<&TypeDesc>) -> i32 {
    let value = match return_type {
        Some(TypeDesc::I8) => i32::from(
            i8::try_from(value).unwrap_or_else(|_| panic!("WASM i8 map literal is out of range")),
        ),
        Some(TypeDesc::I16) => i32::from(
            i16::try_from(value).unwrap_or_else(|_| panic!("WASM i16 map literal is out of range")),
        ),
        Some(TypeDesc::I32) => {
            i32::try_from(value).unwrap_or_else(|_| panic!("WASM i32 map literal is out of range"))
        }
        Some(TypeDesc::I64) => {
            panic!(
                "WASM backend cannot emit exact i64 map literal {value}: \
                 the current map-result ABI is i32"
            )
        }
        Some(TypeDesc::U8) => i32::from(
            u8::try_from(value).unwrap_or_else(|_| panic!("WASM u8 map literal is out of range")),
        ),
        Some(TypeDesc::U16) => i32::from(
            u16::try_from(value).unwrap_or_else(|_| panic!("WASM u16 map literal is out of range")),
        ),
        Some(TypeDesc::U32) => {
            let value = u32::try_from(value)
                .unwrap_or_else(|_| panic!("WASM u32 map literal is out of range"));
            i32::from_ne_bytes(value.to_ne_bytes())
        }
        Some(TypeDesc::U64) => {
            panic!(
                "WASM backend cannot emit exact u64 map literal {value}: \
                 the current map-result ABI is i32"
            )
        }
        Some(_) => panic!("unsigned WASM map literal has a non-integer return type"),
        None => panic!("unsigned WASM map literal requires an explicit representable width"),
    };
    assert!(
        value != -1,
        "WASM cannot emit u32::MAX: its i32 bit pattern is the failure sentinel"
    );
    value
}
