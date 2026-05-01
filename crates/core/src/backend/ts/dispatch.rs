//! Token dispatch and delimiter scan codegen helpers for the TS emitter.

use crate::backend::{DelimScanConfig, TokenDispatchArmCompiled};

use super::code::{TsCode, TsEmitCtx, TsEmitter};
use super::projection::ts_escape;

impl TsEmitter {
    pub(in crate::backend::ts) fn token_dispatch(
        &mut self,
        token: TsCode,
        arms: Vec<TokenDispatchArmCompiled<TsCode>>,
        fallback: TsCode,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        let tok_var = ctx.fresh("tok");
        let result = ctx.fresh("td");
        let mut stmts = String::new();
        let tok_expr = token.dissolve(&mut stmts);
        stmts.push_str(&format!(
            "const {tok_var} = {tok_expr};\nlet {result} = null;\n"
        ));
        stmts.push_str(&format!("if ({tok_var} !== null) {{\n"));
        stmts.push_str(&format!(
            "  const __td_str = s.input.slice({tok_var}.start, {tok_var}.end);\n"
        ));
        for arm in &arms {
            let comparisons: Vec<String> = arm
                .patterns
                .iter()
                .map(|pat| {
                    let s = String::from_utf8_lossy(pat);
                    format!("__td_str === \"{}\"", ts_escape(&s))
                })
                .collect();
            let cond = comparisons.join(" || ");
            let cont_expr = arm.continuation.as_expr();
            if let Some(guard) = arm.guard_byte {
                stmts.push_str(&format!(
                    "  if (({cond}) && s.offset < s.input.length && s.input.charCodeAt(s.offset) === {guard}) {{ {result} = {cont_expr}; }}\n"
                ));
            } else {
                stmts.push_str(&format!("  if ({cond}) {{ {result} = {cont_expr}; }}\n"));
            }
        }
        stmts.push_str("}\n");
        let fallback_expr = fallback.dissolve(&mut stmts);
        stmts.push_str(&format!(
            "if ({result} === null) {{ {result} = {fallback_expr}; }}\n"
        ));
        TsCode::new(stmts, result)
    }

    pub(in crate::backend::ts) fn delim_scan(
        &mut self,
        config: &DelimScanConfig,
        ctx: &mut TsEmitCtx,
    ) -> Option<TsCode> {
        let result = ctx.fresh("ds_result");
        let start = ctx.fresh("ds_start");
        let (open, close, pivot) = (config.open_byte, config.close_byte, config.pivot_byte);
        let block_call = config
            .block_rule
            .as_ref()
            .map(|(_, n)| format!("__{n}(s)"))
            .unwrap_or_else(|| "null".to_string());
        let pivot_call = config
            .pivot_rule
            .as_ref()
            .map(|(_, n)| format!("__{n}(s)"))
            .unwrap_or_else(|| "null".to_string());
        let trail = if let Some(tb) = config.trail_byte {
            format!(
                "if (s.offset < s.input.length && s.input.charCodeAt(s.offset) === {tb}) s.offset++;\n      "
            )
        } else {
            String::new()
        };
        Some(TsCode::new(
            format!(
                "const {start} = s.offset;\n\
                 let {result} = null;\n\
                 if (s.offset < s.input.length && s.input.charCodeAt(s.offset) === {open}) {{\n  \
                   s.offset++;\n  \
                   while (s.offset < s.input.length) {{\n    \
                     const __c = s.input.charCodeAt(s.offset);\n    \
                     if (__c === {close}) {{ s.offset++; {result} = span({start}, s.offset); break; }}\n    \
                     if (__c === {open}) {{\n      \
                       const __br = {block_call};\n      \
                       if (__br === null) break;\n      \
                       continue;\n    \
                     }}\n    \
                     const __pi = s.input.indexOf(String.fromCharCode({pivot}), s.offset);\n    \
                     if (__pi === -1 || __pi >= s.input.length) break;\n    \
                     s.offset = __pi + 1;\n    \
                     {trail}const __pv = {pivot_call};\n    \
                     if (__pv === null) break;\n  \
                   }}\n\
                 }}\n\
                 if ({result} === null) s.offset = {start};\n"
            ),
            result,
        ))
    }
}
