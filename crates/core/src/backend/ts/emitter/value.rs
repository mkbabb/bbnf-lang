//! Value-manipulation emission for the TypeScript backend: enum_wrap /
//! number_convert / constant / map_expr / span_capture / hex_convert /
//! fused_map.
//!
//! Each method is `pub(super)` so the trait impl in `mod.rs` can delegate
//! to it via `self.emit_xxx_impl(...)`.

use bbnf_ir::{FnDescriptor, GrammarIR, MapExpr, TypeDesc};

use crate::backend::ValuePlacement;

use super::{
    TsCode, TsEmitCtx, TsEmitter, compile_map_expr_to_js, translate_rust_constant_to_js,
};

impl TsEmitter {
    pub(super) fn emit_enum_wrap_impl(
        &mut self,
        inner: TsCode,
        variant_name: &str,
        _alloc: ValuePlacement,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        let v = ctx.fresh("ew");
        let mut stmts = String::new();
        let inner_expr = inner.dissolve(&mut stmts);
        stmts.push_str(&format!("const {v} = {inner_expr};\n"));
        TsCode::new(
            stmts,
            format!("{v} !== null ? {{ tag: \"{variant_name}\" as const, value: {v} }} : null"),
        )
    }

    pub(super) fn emit_number_convert_impl(&mut self, ctx: &mut TsEmitCtx) -> TsCode {
        let v = ctx.fresh("num");
        let stmts = format!(
            "const __numRe = /[-+]?(?:[0-9]*\\.)?[0-9]+(?:[eE][-+]?[0-9]+)?/y;\n\
             __numRe.lastIndex = s.offset;\n\
             const __numM = __numRe.exec(s.input);\n\
             let {v} = null;\n\
             if (__numM) {{ {v} = parseFloat(__numM[0]); s.offset = __numRe.lastIndex; }}\n"
        );
        TsCode::new(stmts, v)
    }

    pub(super) fn emit_constant_impl(
        &mut self,
        discard_inner: TsCode,
        value: &str,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        let js_value = translate_rust_constant_to_js(value);
        let v = ctx.fresh("const");
        let mut stmts = String::new();
        let inner_expr = discard_inner.dissolve(&mut stmts);
        stmts.push_str(&format!(
            "const {v} = ({inner_expr}) !== null ? {js_value} : null;\n"
        ));
        TsCode::new(stmts, v)
    }

    pub(super) fn emit_map_expr_impl(
        &mut self,
        inner: TsCode,
        expr: &MapExpr,
        _return_type: Option<&TypeDesc>,
        _alloc: ValuePlacement,
        ir: &GrammarIR,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        let v = ctx.fresh("map");
        let mut stmts = String::new();
        let inner_expr = inner.dissolve(&mut stmts);

        if expr.is_constant() {
            let value_js = compile_map_expr_to_js(expr, ir);
            stmts.push_str(&format!(
                "const {v} = ({inner_expr}) !== null ? {value_js} : null;\n"
            ));
        } else {
            let body_js = compile_map_expr_to_js(expr, ir);
            stmts.push_str(&format!(
                "const __input = {inner_expr};\n\
                 const {v} = __input !== null ? {body_js} : null;\n"
            ));
        }
        TsCode::new(stmts, v)
    }

    pub(super) fn emit_span_capture_impl(
        &mut self,
        inner: TsCode,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        let v = ctx.fresh("span");
        let mut stmts = String::new();
        let inner_expr = inner.dissolve(&mut stmts);
        stmts.push_str(&format!(
            "const __start = s.offset;\n\
             const __inner = {inner_expr};\n\
             const {v} = __inner !== null ? {{ start: __start, end: s.offset }} : null;\n"
        ));
        TsCode::new(stmts, v)
    }

    pub(super) fn emit_hex_convert_impl(
        &mut self,
        inner: TsCode,
        fn_path: &str,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        let v = ctx.fresh("hex");
        let mut stmts = String::new();
        let inner_expr = inner.dissolve(&mut stmts);
        // fn_path is a Rust path — translate to a host function call.
        let js_fn = fn_path.rsplit("::").next().unwrap_or(fn_path);
        stmts.push_str(&format!(
            "const {v} = ({inner_expr}) !== null ? hostFns.{js_fn}({inner_expr}) : null;\n"
        ));
        TsCode::new(stmts, v)
    }

    pub(super) fn emit_fused_map_impl(
        &mut self,
        _inner: TsCode,
        _inner_fd: &FnDescriptor,
        _outer_fd: &FnDescriptor,
        _alloc: ValuePlacement,
        _ir: &GrammarIR,
        _ctx: &mut TsEmitCtx,
    ) -> Option<TsCode> {
        // TS doesn't need fusion — no slab allocation overhead.
        None
    }
}
