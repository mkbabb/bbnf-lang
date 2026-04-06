//! Whitespace trim codegen helpers for the TS emitter.

use super::code::{TsCode, TsEmitCtx, TsEmitter};
use super::helpers::ws_skip_stmts;

impl TsEmitter {
    pub(super) fn ws_trim(
        &mut self,
        ws_pattern: Option<&str>,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        TsCode::new(ws_skip_stmts(ws_pattern, ctx), "{}")
    }

    pub(super) fn with_ws_trim(
        &mut self,
        inner: TsCode,
        ws_pattern: Option<&str>,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        let mut stmts = ws_skip_stmts(ws_pattern, ctx);
        let inner_expr = inner.dissolve(&mut stmts);
        let v = "__ws_inner";
        stmts.push_str(&format!(
            "const {v} = {inner_expr};\n\
             if ({v} === null) return null;\n"
        ));
        stmts.push_str(&ws_skip_stmts(ws_pattern, ctx));
        TsCode::new(stmts, v)
    }
}
