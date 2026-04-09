//! Repetition codegen helpers for the TS emitter.

use bbnf_ir::TypeDesc;

use crate::backend::{ValuePlacement, SepByConfig};

use super::code::{TsCode, TsEmitCtx, TsEmitter};

impl TsEmitter {
    pub(in crate::backend::ts) fn repeat_many(
        &mut self,
        body: TsCode,
        lo: u32,
        _hi: u32,
        _elem_type: &TypeDesc,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        let start = ctx.fresh("start");
        let count = ctx.fresh("count");
        let result = ctx.fresh("rep");
        let body_expr = body.as_expr();
        let stmts = format!(
            "const {start} = s.offset;\n\
             let {count} = 0;\n\
             while (true) {{\n\
               const __prev = s.offset;\n\
               const __r = {body_expr};\n\
               if (__r === null) break;\n\
               {count}++;\n\
               if (s.offset === __prev) break;\n\
             }}\n\
             const {result} = {count} >= {lo} ? span({start}, s.offset) : null;\n"
        );
        TsCode::new(stmts, result)
    }

    pub(in crate::backend::ts) fn repeat_optional(
        &mut self,
        body: TsCode,
        _inner_type: &TypeDesc,
        _alloc: ValuePlacement,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        let cp = ctx.fresh("cp");
        let result = ctx.fresh("opt");
        let mut stmts = format!("const {cp} = s.offset;\n");
        let body_expr = body.dissolve(&mut stmts);
        stmts.push_str(&format!(
            "let {result} = {body_expr};\n\
             if ({result} === null) s.offset = {cp};\n"
        ));
        TsCode::new(stmts, result)
    }

    pub(in crate::backend::ts) fn sep_by(
        &mut self,
        element: TsCode,
        separator: TsCode,
        config: &SepByConfig,
        _elem_type: &TypeDesc,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        let start = ctx.fresh("start");
        let count = ctx.fresh("count");
        let result = ctx.fresh("sep");
        let cp = ctx.fresh("cp");
        let lo = config.lo;
        let elem_expr = element.as_expr();
        let sep_expr = separator.as_expr();

        // Terminator byte early-exit check.
        let terminator_check = if let Some(ref tb) = config.terminator_bytes {
            if tb.len() == 1 {
                format!(
                    "if (s.offset < s.input.length && s.input.charCodeAt(s.offset) === {}) break;\n    ",
                    tb[0]
                )
            } else {
                String::new()
            }
        } else {
            String::new()
        };

        let stmts = format!(
            "const {start} = s.offset;\n\
             let {count} = 0;\n\
             {terminator_check}\
             {{\n  const __r = {elem_expr};\n  \
             if (__r !== null) {count}++;\n}}\n\
             if ({count} > 0) {{\n  \
             while (true) {{\n    \
               {terminator_check}\
               const {cp} = s.offset;\n    \
               const __sep = {sep_expr};\n    \
               if (__sep === null) break;\n    \
               const __r = {elem_expr};\n    \
               if (__r !== null) {{ {count}++; }} else {{ s.offset = {cp}; break; }}\n  \
             }}\n}}\n\
             const {result} = {count} >= {lo} ? span({start}, s.offset) : null;\n"
        );
        TsCode::new(stmts, result)
    }
}
