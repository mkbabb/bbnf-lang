//! TypeScript Emitter: implements [`Emitter`] to produce self-contained TS source.
//!
//! Uses `TsCode { stmts, expr }` to separate setup statements from result expressions,
//! eliminating IIFE closures. Generated code is flat sequential statements.

use bbnf_ir::{AltDispatch, GrammarIR, IrRule, RuleId, TypeDesc};

use crate::backend::analysis::BackendAnalysis;
use crate::backend::key_dispatch::{KeyClass, KeyDispatchConfig};
use crate::backend::{
    AllocStrategy, AltBranchInfo, DelimScanConfig, Emitter, FlattenStrategy, KeyDispatchBranch,
    SepByConfig, SeqChildGroup, TokenDispatchArmCompiled,
};

pub use super::code::{TsCode, TsEmitCtx, TsEmitter};
pub use super::helpers::{ts_escape, type_desc_to_ts, unescape_literal, translate_rust_constant_to_js, ws_skip_stmts};

// ─── Emitter Impl ───────────────────────────────────────────────────────────

impl Emitter for TsEmitter {
    type Output = TsCode;
    type Ctx = TsEmitCtx;

    // ── Leaves ──────────────────────────────────────────────────────────

    fn emit_literal_match(
        &mut self,
        value: &str,
        guaranteed_byte: Option<u8>,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        let unescaped = unescape_literal(value);
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

    fn emit_regex_match(
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

    fn emit_epsilon(&mut self, _ctx: &mut TsEmitCtx) -> TsCode {
        TsCode::expr("span(s.offset, s.offset)")
    }

    // ── Sequences ───────────────────────────────────────────────────────

    fn emit_seq_all_span(
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

    fn emit_seq_grouped(
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

    // ── Alternations ────────────────────────────────────────────────────

    fn emit_alt_dispatch(
        &mut self,
        table: &AltDispatch,
        branches: Vec<(AltBranchInfo, TsCode)>,
        fallback: Option<(AltBranchInfo, TsCode)>,
        _alloc: AllocStrategy,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        let result = ctx.fresh("dispatch");
        let mut stmts = format!("let {result} = null;\n");
        stmts.push_str("if (s.offset < s.input.length) {\n  switch (s.input.charCodeAt(s.offset)) {\n");

        for (branch_idx, (_info, body)) in branches.iter().enumerate() {
            let byte_patterns: Vec<u8> = table
                .table
                .iter()
                .enumerate()
                .filter(|&(_, &b)| b as usize == branch_idx)
                .map(|(bv, _)| bv as u8)
                .collect();
            for byte in &byte_patterns {
                stmts.push_str(&format!("    case {byte}:\n"));
            }
            if !byte_patterns.is_empty() {
                stmts.push_str(&format!("      {result} = {}; break;\n", body.as_expr()));
            }
        }

        if let Some((_info, fb)) = &fallback {
            stmts.push_str(&format!("    default: {result} = {}; break;\n", fb.as_expr()));
        } else {
            stmts.push_str("    default: break;\n");
        }
        stmts.push_str("  }\n}\n");
        TsCode::new(stmts, result)
    }

    fn emit_alt_checkpoint(
        &mut self,
        branches: Vec<(AltBranchInfo, TsCode)>,
        _alloc: AllocStrategy,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        if branches.len() == 1 {
            return branches.into_iter().next().unwrap().1;
        }

        let result = ctx.fresh("alt");
        let mut stmts = format!("let {result} = null;\n");
        for (_info, branch) in branches {
            let cp = ctx.fresh("cp");
            stmts.push_str(&format!("if ({result} === null) {{\n"));
            stmts.push_str(&format!("  const {cp} = s.offset;\n"));
            let branch_expr = branch.dissolve(&mut stmts);
            stmts.push_str(&format!(
                "  {result} = {branch_expr};\n  if ({result} === null) s.offset = {cp};\n}}\n"
            ));
        }
        TsCode::new(stmts, result)
    }

    fn emit_alt_all_literal(
        &mut self,
        literals: Vec<(String, TsCode)>,
        _alloc: AllocStrategy,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        // Checkpoint-free sequential literal matching.
        let result = ctx.fresh("lit_alt");
        let mut stmts = format!("let {result} = null;\n");
        for (value, body) in literals {
            let unescaped = unescape_literal(&value);
            let escaped = ts_escape(&unescaped);
            let body_expr = body.as_expr();
            stmts.push_str(&format!(
                "if ({result} === null && s.input.startsWith(\"{escaped}\", s.offset)) {{ {result} = {body_expr}; }}\n"
            ));
        }
        TsCode::new(stmts, result)
    }

    fn emit_key_dispatch(
        &mut self,
        config: &KeyDispatchConfig,
        branches: Vec<KeyDispatchBranch<TsCode>>,
        fallback: Option<(AltBranchInfo, TsCode)>,
        _alloc: AllocStrategy,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        let cp = ctx.fresh("kd_cp");
        let result = ctx.fresh("kd_result");
        let mut stmts = format!("const {cp} = s.offset;\n");
        let scanner_code = match &config.key_class {
            KeyClass::Identifier => "const __kd_re = /[a-zA-Z_][\\w-]*/y;\n__kd_re.lastIndex = s.offset;\nconst __kd_m = __kd_re.exec(s.input);\n".to_string(),
            KeyClass::QuotedString { quote_char } => { let q = *quote_char as char; format!("const __kd_re = /{q}[^{q}]*{q}/y;\n__kd_re.lastIndex = s.offset;\nconst __kd_m = __kd_re.exec(s.input);\n") }
        };
        stmts.push_str(&scanner_code);
        stmts.push_str(&format!("let {result} = null;\nif (__kd_m !== null) {{\n"));
        match &config.key_class {
            KeyClass::Identifier => stmts.push_str("  const __kd_key = __kd_m[0];\n"),
            KeyClass::QuotedString { .. } => stmts.push_str("  const __kd_key = __kd_m[0].slice(1, -1);\n"),
        }
        for kd_branch in branches {
            let checks: Vec<String> = kd_branch.key_bytes.iter().map(|key| { let s = String::from_utf8_lossy(key); format!("__kd_key === \"{}\"", ts_escape(&s)) }).collect();
            stmts.push_str(&format!("  if ({result} === null && ({})) {{\n    s.offset = {cp};\n", checks.join(" || ")));
            let expr = kd_branch.body.dissolve(&mut stmts);
            stmts.push_str(&format!("    {result} = {expr};\n  }}\n"));
        }
        stmts.push_str("}}\n");
        stmts.push_str(&format!("if ({result} === null) {{\n  s.offset = {cp};\n"));
        if let Some((_info, fb)) = fallback { let expr = fb.dissolve(&mut stmts); stmts.push_str(&format!("  {result} = {expr};\n}}\n")); }
        else { stmts.push_str("}}\n"); }
        TsCode::new(stmts, result)
    }

    // ── Repetition ──────────────────────────────────────────────────────

    fn emit_repeat_many(
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

    fn emit_repeat_optional(
        &mut self,
        body: TsCode,
        _inner_type: &TypeDesc,
        _alloc: AllocStrategy,
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

    fn emit_sep_by(
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

    // ── References ──────────────────────────────────────────────────────

    fn emit_call(
        &mut self,
        _rule_id: RuleId,
        rule_name: &str,
        _alloc: AllocStrategy,
        _ctx: &mut TsEmitCtx,
    ) -> TsCode {
        TsCode::expr(format!("__{rule_name}(s)"))
    }

    fn emit_inline_wrap(
        &mut self,
        body: TsCode,
        variant_name: Option<&str>,
        _alloc: AllocStrategy,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        if let Some(name) = variant_name {
            let v = ctx.fresh("iw");
            let mut stmts = String::new();
            let body_expr = body.dissolve(&mut stmts);
            stmts.push_str(&format!(
                "const {v} = {body_expr};\n"
            ));
            let expr = format!(
                "{v} !== null ? {{ tag: \"{name}\" as const, value: {v} }} : null"
            );
            TsCode::new(stmts, expr)
        } else {
            body
        }
    }

    // ── Operator chains ─────────────────────────────────────────────────

    fn emit_operator_chain(
        &mut self,
        head: TsCode,
        op: TsCode,
        rhs: TsCode,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        let head_var = ctx.fresh("head");
        let mut stmts = String::new();
        head.bind_checked(&head_var, &mut stmts);
        let op_expr = op.as_expr();
        let rhs_expr = rhs.as_expr();
        stmts.push_str(&format!(
            "while (true) {{\n  \
               const __cp = s.offset;\n  \
               if (({op_expr}) === null) {{ s.offset = __cp; break; }}\n  \
               if (({rhs_expr}) === null) {{ s.offset = __cp; break; }}\n\
             }}\n"
        ));
        TsCode::new(stmts, head_var)
    }

    // ── Binary operators ────────────────────────────────────────────────

    fn emit_skip(
        &mut self,
        kept: TsCode,
        discarded: TsCode,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        let v = ctx.fresh("kept");
        let mut stmts = String::new();
        kept.bind_checked(&v, &mut stmts);
        discarded.bind_checked(&ctx.fresh("_"), &mut stmts);
        TsCode::new(stmts, v)
    }

    fn emit_next(
        &mut self,
        discarded: TsCode,
        kept: TsCode,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        let mut stmts = String::new();
        discarded.bind_checked(&ctx.fresh("_"), &mut stmts);
        let kept_expr = kept.dissolve(&mut stmts);
        TsCode::new(stmts, kept_expr)
    }

    fn emit_minus(
        &mut self,
        lhs: TsCode,
        rhs: TsCode,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        let save = ctx.fresh("save");
        let excl = ctx.fresh("excl");
        let result = ctx.fresh("minus");
        let mut stmts = format!("const {save} = s.offset;\n");
        let rhs_expr = rhs.dissolve(&mut stmts);
        stmts.push_str(&format!(
            "const {excl} = {rhs_expr};\ns.offset = {save};\n\
             let {result} = null;\n\
             if ({excl} === null) {{\n"
        ));
        let lhs_expr = lhs.dissolve(&mut stmts);
        stmts.push_str(&format!("  {result} = {lhs_expr};\n}}\n"));
        TsCode::new(stmts, result)
    }

    fn emit_negate(
        &mut self,
        inner: TsCode,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        let save = ctx.fresh("save");
        let inner_var = ctx.fresh("neg");
        let mut stmts = format!("const {save} = s.offset;\n");
        let inner_expr = inner.dissolve(&mut stmts);
        stmts.push_str(&format!(
            "const {inner_var} = {inner_expr};\ns.offset = {save};\n"
        ));
        TsCode::new(stmts, format!("{inner_var} !== null ? null : {{}}"))
    }

    // ── Value manipulation ──────────────────────────────────────────────

    fn emit_enum_wrap(
        &mut self,
        inner: TsCode,
        variant_name: &str,
        _alloc: AllocStrategy,
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

    fn emit_number_convert(&mut self, ctx: &mut TsEmitCtx) -> TsCode {
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

    fn emit_constant(
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

    fn emit_ws_trim(
        &mut self,
        ws_pattern: Option<&str>,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        TsCode::new(ws_skip_stmts(ws_pattern, ctx), "{}")
    }

    fn emit_with_ws_trim(
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

    // ── Token dispatch ─────────────────────────────────────────────────

    fn emit_token_dispatch(
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
                stmts.push_str(&format!(
                    "  if ({cond}) {{ {result} = {cont_expr}; }}\n"
                ));
            }
        }
        stmts.push_str("}\n");
        let fallback_expr = fallback.dissolve(&mut stmts);
        stmts.push_str(&format!(
            "if ({result} === null) {{ {result} = {fallback_expr}; }}\n"
        ));
        TsCode::new(stmts, result)
    }

    // ── Delimiter scan ─────────────────────────────────────────────────

    fn emit_delim_scan(
        &mut self,
        config: &DelimScanConfig,
        ctx: &mut TsEmitCtx,
    ) -> Option<TsCode> {
        let result = ctx.fresh("ds_result");
        let start = ctx.fresh("ds_start");
        let (open, close, pivot) = (config.open_byte, config.close_byte, config.pivot_byte);
        let block_call = config.block_rule.as_ref().map(|(_, n)| format!("__{n}(s)")).unwrap_or_else(|| "null".to_string());
        let pivot_call = config.pivot_rule.as_ref().map(|(_, n)| format!("__{n}(s)")).unwrap_or_else(|| "null".to_string());
        let trail = if let Some(tb) = config.trail_byte { format!("if (s.offset < s.input.length && s.input.charCodeAt(s.offset) === {tb}) s.offset++;\n      ") } else { String::new() };
        Some(TsCode::new(format!(
            "const {start} = s.offset;\nlet {result} = null;\nif (s.offset < s.input.length && s.input.charCodeAt(s.offset) === {open}) {{\n  s.offset++;\n  while (s.offset < s.input.length) {{\n    const __c = s.input.charCodeAt(s.offset);\n    if (__c === {close}) {{ s.offset++; {result} = span({start}, s.offset); break; }}\n    if (__c === {open}) {{\n      const __br = {block_call};\n      if (__br === null) break;\n      continue;\n    }}\n    const __pi = s.input.indexOf(String.fromCharCode({pivot}), s.offset);\n    if (__pi === -1 || __pi >= s.input.length) break;\n    s.offset = __pi + 1;\n    {trail}const __pv = {pivot_call};\n    if (__pv === null) break;\n  }}\n}}\nif ({result} === null) s.offset = {start};\n"
        ), result))
    }

    // ── Rule-level ──────────────────────────────────────────────────────

    fn emit_rule_function(
        &mut self,
        rule: &IrRule,
        body: TsCode,
        ir: &GrammarIR,
        _ctx: &mut TsEmitCtx,
    ) -> TsCode {
        let name = ir.get_string(rule.name);
        let enum_name = &self.enum_name;

        let return_type = ir
            .types
            .iter()
            .find(|(id, _)| *id == rule.id)
            .map(|(_, td)| type_desc_to_ts(td, enum_name, ir))
            .unwrap_or_else(|| "Span".to_string());

        let fn_body = if body.stmts.is_empty() {
            format!("  return {};\n", body.expr)
        } else {
            format!("  {}\n  return {};\n", body.stmts, body.expr)
        };

        if rule.meta.is_transparent {
            TsCode::expr(format!(
                "function __{name}(s: ParserState): {return_type} | null {{\n{fn_body}}}\n"
            ))
        } else {
            TsCode::expr(format!(
                "function __{name}(s: ParserState): {enum_name} | null {{\n\
                 {fn_body}\
                 }}\n"
            ))
        }
    }

    fn emit_type_definitions(
        &mut self,
        ir: &GrammarIR,
        _analysis: &BackendAnalysis,
        _ctx: &mut TsEmitCtx,
    ) -> TsCode {
        let enum_name = &self.enum_name;

        let variants: Vec<String> = ir
            .rules
            .iter()
            .filter(|r| !r.meta.is_transparent)
            .map(|r| {
                let name = ir.get_string(r.name);
                let value_type = ir
                    .types
                    .iter()
                    .find(|(id, _)| *id == r.id)
                    .map(|(_, td)| type_desc_to_ts(td, enum_name, ir))
                    .unwrap_or_else(|| "Span".to_string());
                format!("  | {{ tag: \"{name}\"; value: {value_type} }}")
            })
            .collect();

        let union_body = variants.join("\n");

        TsCode::expr(format!(
            "// ── Runtime types ────────────────────────────────────────────────\n\n\
             interface ParserState {{\n  input: string;\n  offset: number;\n}}\n\n\
             interface Span {{\n  start: number;\n  end: number;\n}}\n\n\
             function span(start: number, end: number): Span {{\n  return {{ start, end }};\n}}\n\n\
             function createState(input: string): ParserState {{\n  return {{ input, offset: 0 }};\n}}\n\n\
             // ── Grammar types ───────────────────────────────────────────────\n\n\
             type {enum_name} =\n{union_body};\n\n"
        ))
    }

    fn emit_grammar(
        &mut self,
        type_defs: TsCode,
        rule_functions: Vec<TsCode>,
        ir: &GrammarIR,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        let entry_name = ir.get_string(ir.rules[ir.entry as usize].name);

        let mut output = String::new();
        output.push_str("// Generated by BBNF — do not edit.\n\n");
        output.push_str(&type_defs.expr);

        if !ctx.hoisted_regexes.is_empty() {
            output.push_str(
                "// ── Hoisted regex constants ──────────────────────────────────────\n\n",
            );
            for decl in &ctx.hoisted_regexes {
                output.push_str(decl);
                output.push('\n');
            }
            output.push('\n');
        }

        output.push_str(
            "// ── Parser functions ─────────────────────────────────────────────\n\n",
        );
        for func in &rule_functions {
            output.push_str(&func.expr);
            output.push('\n');
        }

        output.push_str(&format!(
            "// ── Public API ──────────────────────────────────────────────────\n\n\
             export function parse(input: string): {{ result: {enum_name} | null; offset: number }} {{\n  \
             const s = createState(input);\n  \
             const result = __{entry_name}(s);\n  \
             return {{ result, offset: s.offset }};\n}}\n",
            enum_name = self.enum_name,
        ));

        TsCode::expr(output)
    }
}
