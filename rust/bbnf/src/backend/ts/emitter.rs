//! TypeScript Emitter: implements [`Emitter`] to produce self-contained TS source.
//!
//! Uses `TsCode { stmts, expr }` to separate setup statements from result expressions,
//! eliminating IIFE closures. Generated code is flat sequential statements.
//!
//! Method bodies are delegated to helper methods in sub-modules:
//! - `alt.rs` — alternation dispatch, checkpoint, all-literal, key dispatch
//! - `repeat.rs` — many, optional, sep-by
//! - `dispatch.rs` — token dispatch, delimiter scan
//! - `ws.rs` — whitespace trim

use bbnf_ir::{AltDispatch, FnDescriptor, GrammarIR, IrRule, MapExpr, RuleId, TypeDesc};

use crate::backend::analysis::BackendAnalysis;
use crate::backend::key_dispatch::KeyDispatchConfig;
use crate::backend::{
    ValuePlacement, AltBranchInfo, DelimScanConfig, Emitter, FlattenStrategy, KeyDispatchBranch,
    SepByConfig, SeqChildGroup, TokenDispatchArmCompiled,
};

pub use super::code::{TsCode, TsEmitCtx, TsEmitter};
pub use super::helpers::{
    compile_map_expr_to_js, translate_rust_constant_to_js, ts_escape, type_desc_to_ts,
    unescape_literal, ws_skip_stmts,
};

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

    // ── Alternations (delegated to alt.rs) ──────────────────────────────

    fn emit_alt_dispatch(
        &mut self,
        table: &AltDispatch,
        branches: Vec<(AltBranchInfo, TsCode)>,
        fallback: Option<(AltBranchInfo, TsCode)>,
        alloc: ValuePlacement,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.alt_dispatch(table, branches, fallback, alloc, ctx)
    }

    fn emit_alt_checkpoint(
        &mut self,
        branches: Vec<(AltBranchInfo, TsCode)>,
        alloc: ValuePlacement,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.alt_checkpoint(branches, alloc, ctx)
    }

    fn emit_alt_all_literal(
        &mut self,
        literals: Vec<(String, TsCode)>,
        alloc: ValuePlacement,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.alt_all_literal(literals, alloc, ctx)
    }

    fn emit_key_dispatch(
        &mut self,
        config: &KeyDispatchConfig,
        branches: Vec<KeyDispatchBranch<TsCode>>,
        fallback: Option<(AltBranchInfo, TsCode)>,
        alloc: ValuePlacement,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.key_dispatch(config, branches, fallback, alloc, ctx)
    }

    // ── Repetition (delegated to repeat.rs) ─────────────────────────────

    fn emit_repeat_many(
        &mut self,
        body: TsCode,
        lo: u32,
        hi: u32,
        elem_type: &TypeDesc,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.repeat_many(body, lo, hi, elem_type, ctx)
    }

    fn emit_repeat_optional(
        &mut self,
        body: TsCode,
        inner_type: &TypeDesc,
        alloc: ValuePlacement,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.repeat_optional(body, inner_type, alloc, ctx)
    }

    fn emit_sep_by(
        &mut self,
        element: TsCode,
        separator: TsCode,
        config: &SepByConfig,
        elem_type: &TypeDesc,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.sep_by(element, separator, config, elem_type, ctx)
    }

    // ── References ──────────────────────────────────────────────────────

    fn emit_call(
        &mut self,
        _rule_id: RuleId,
        rule_name: &str,
        _alloc: ValuePlacement,
        _ctx: &mut TsEmitCtx,
    ) -> TsCode {
        TsCode::expr(format!("__{rule_name}(s)"))
    }

    fn emit_inline_wrap(
        &mut self,
        body: TsCode,
        variant_name: Option<&str>,
        _alloc: ValuePlacement,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        if let Some(name) = variant_name {
            let v = ctx.fresh("iw");
            let mut stmts = String::new();
            let body_expr = body.dissolve(&mut stmts);
            stmts.push_str(&format!("const {v} = {body_expr};\n"));
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
    ) -> Option<TsCode> {
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
        Some(TsCode::new(stmts, head_var))
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

    fn emit_map_expr(
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

    fn emit_span_capture(
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

    fn emit_hex_convert(
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

    fn emit_fused_map(
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

    // ── Whitespace (delegated to ws.rs) ─────────────────────────────────

    fn emit_ws_trim(
        &mut self,
        ws_pattern: Option<&str>,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.ws_trim(ws_pattern, ctx)
    }

    fn emit_with_ws_trim(
        &mut self,
        inner: TsCode,
        ws_pattern: Option<&str>,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.with_ws_trim(inner, ws_pattern, ctx)
    }

    // ── Token dispatch (delegated to dispatch.rs) ───────────────────────

    fn emit_token_dispatch(
        &mut self,
        token: TsCode,
        arms: Vec<TokenDispatchArmCompiled<TsCode>>,
        fallback: TsCode,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.token_dispatch(token, arms, fallback, ctx)
    }

    // ── Delimiter scan (delegated to dispatch.rs) ───────────────────────

    fn emit_delim_scan(
        &mut self,
        config: &DelimScanConfig,
        ctx: &mut TsEmitCtx,
    ) -> Option<TsCode> {
        self.delim_scan(config, ctx)
    }

    // ── Rule-level ──────────────────────────────────────────────────────

    fn emit_rule_function(
        &mut self,
        rule: &IrRule,
        body: TsCode,
        _sync_body: Option<TsCode>,
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
