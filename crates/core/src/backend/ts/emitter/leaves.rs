//! Leaf-op emission for the TypeScript backend: literals, regex, epsilon, and
//! sequence forms (all-span fast path + grouped).
//!
//! Each method is `pub(super)` so the trait impl in `mod.rs` can delegate
//! to it via `self.emit_xxx_impl(...)`.

use bbnf_ir::{GrammarIR, TypeDesc};

use crate::backend::{FlattenStrategy, SeqChildGroup};

use super::{TsCode, TsEmitCtx, TsEmitter, ts_escape};

impl TsEmitter {
    pub(super) fn emit_literal_match_impl(
        &mut self,
        value: &str,
        guaranteed_byte: Option<u8>,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        let unescaped = value.to_string();
        let bytes = unescaped.as_bytes();

        if guaranteed_byte.is_some() {
            let v = ctx.fresh("lit");
            let stmts = format!(
                "const {v} = span(s.offset, s.offset + 1);\ns.offset += 1;\n"
            );
            return TsCode::new(stmts, v);
        }

        let v = ctx.fresh("lit");
        if bytes.len() == 1 {
            let byte = bytes[0];
            let stmts = format!(
                "let {v} = null;\n\
                 if (s.offset < s.input.length && s.input.charCodeAt(s.offset) === {byte}) {{\n  \
                   {v} = span(s.offset, s.offset + 1);\n  \
                   s.offset += 1;\n\
                 }}\n"
            );
            TsCode::new(stmts, v)
        } else {
            let escaped = ts_escape(&unescaped);
            let len = unescaped.len();
            let stmts = format!(
                "let {v} = null;\n\
                 if (s.input.startsWith(\"{escaped}\", s.offset)) {{\n  \
                   {v} = span(s.offset, s.offset + {len});\n  \
                   s.offset += {len};\n\
                 }}\n"
            );
            TsCode::new(stmts, v)
        }
    }

    pub(super) fn emit_regex_match_impl(
        &mut self,
        pattern: &str,
        regex_id: usize,
        _ir: &GrammarIR,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        let re_var = format!("__RE{regex_id}");
        if !ctx.hoisted_regexes.iter().any(|s| s.contains(&re_var)) {
            let escaped = ts_escape(pattern);
            ctx.hoisted_regexes
                .push(format!("const {re_var} = new RegExp(\"{escaped}\", \"y\");"));
        }
        let v = ctx.fresh("re");
        let stmts = format!(
            "{re_var}.lastIndex = s.offset;\n\
             const {v}_m = {re_var}.exec(s.input);\n\
             let {v} = null;\n\
             if ({v}_m) {{ {v} = span(s.offset, {re_var}.lastIndex); s.offset = {re_var}.lastIndex; }}\n"
        );
        TsCode::new(stmts, v)
    }

    pub(super) fn emit_epsilon_impl(&mut self, _ctx: &mut TsEmitCtx) -> TsCode {
        TsCode::expr("span(s.offset, s.offset)")
    }

    pub(super) fn emit_seq_all_span_impl(
        &mut self,
        child_outputs: Vec<TsCode>,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        let start = ctx.fresh("start");
        let mut stmts = format!("const {start} = s.offset;\n");
        for child in child_outputs {
            child.bind_checked(&ctx.fresh("_"), &mut stmts);
        }
        TsCode::new(stmts, format!("span({start}, s.offset)"))
    }

    pub(super) fn emit_seq_grouped_impl(
        &mut self,
        groups: Vec<SeqChildGroup<TsCode>>,
        _result_type: &TypeDesc,
        _flatten: Option<FlattenStrategy>,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        let mut stmts = String::new();
        let mut result_vars = Vec::new();

        for group in groups {
            match group {
                SeqChildGroup::Single { output, ty: _ } => {
                    let var = ctx.fresh("v");
                    output.bind_checked(&var, &mut stmts);
                    result_vars.push(var);
                }
                SeqChildGroup::SpanCompressed { outputs } => {
                    let start = ctx.fresh("sp_start");
                    stmts.push_str(&format!("const {start} = s.offset;\n"));
                    for out in outputs {
                        out.bind_checked(&ctx.fresh("_"), &mut stmts);
                    }
                    let var = ctx.fresh("sp");
                    stmts.push_str(&format!("const {var} = span({start}, s.offset);\n"));
                    result_vars.push(var);
                }
            }
        }

        let expr = if result_vars.len() == 1 {
            result_vars[0].clone()
        } else {
            format!("[{}]", result_vars.join(", "))
        };
        TsCode::new(stmts, expr)
    }
}
