//! Repetition codegen helpers for the TS emitter.

use bbnf_ir::TypeDesc;

use crate::backend::{SepByConfig, ValuePlacement};

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
               if (__r === null) {{ s.offset = __prev; break; }}\n\
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
        // The body runs as one expression (an IIFE when it carries
        // statements): a fallible sub-sequence early-returns `null`,
        // and that return must fail the optional, not the whole rule.
        // An absent optional succeeds with `undefined` (parse-that's
        // `opt()`), never `null`, which is this emitter's failure value.
        let raw = ctx.fresh("optraw");
        let body_expr = body.as_expr();
        let stmts = format!(
            "const {cp} = s.offset;\n\
             const {raw} = {body_expr};\n\
             const {result} = {raw} === null ? (s.offset = {cp}, undefined) : {raw};\n"
        );
        TsCode::new(stmts, result)
    }

    pub(in crate::backend::ts) fn sep_by(
        &mut self,
        element: TsCode,
        separator: TsCode,
        config: &SepByConfig,
        elem_type: &TypeDesc,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        let start = ctx.fresh("start");
        let count = ctx.fresh("count");
        let result = ctx.fresh("sep");
        // Span elements collapse to the list's span; any other element
        // is collected, so the list's value is the array its
        // `Vec<T>` type declares.
        let items = ctx.fresh("items");
        let collect = *elem_type != TypeDesc::Span;
        let (decl, push, value) = if collect {
            (
                format!("const {items} = [];\n"),
                format!(" {items}.push(__r);"),
                items.clone(),
            )
        } else {
            (
                String::new(),
                String::new(),
                format!("span({start}, s.offset)"),
            )
        };
        let cp = ctx.fresh("cp");
        let lo = config.lo;
        let elem_expr = element.as_expr();
        let sep_expr = separator.as_expr();

        // Terminator byte early-exit check inside the loop body. The
        // head element runs in a plain block (no enclosing loop), so a
        // `break` there would jump out of the surrounding parser
        // function — emit a guarded `if` only inside the inner
        // `while (true)` where it has a target. Terminator-aware bail
        // before the head turns into a bare-byte peek with no
        // transition, so we elide it from the head.
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
             {decl}\
             let {count} = 0;\n\
             {{\n  const __r = {elem_expr};\n  \
             if (__r !== null) {{ {count}++;{push} }} else s.offset = {start};\n}}\n\
             if ({count} > 0) {{\n  \
             while (true) {{\n    \
               {terminator_check}\
               const {cp} = s.offset;\n    \
               const __sep = {sep_expr};\n    \
               if (__sep === null) {{ s.offset = {cp}; break; }}\n    \
               const __r = {elem_expr};\n    \
               if (__r !== null) {{ {count}++;{push} }} else {{ s.offset = {cp}; break; }}\n  \
             }}\n}}\n\
             const {result} = {count} >= {lo} ? {value} : null;\n"
        );
        TsCode::new(stmts, result)
    }
}
