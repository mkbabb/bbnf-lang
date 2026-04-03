//! TypeScript Emitter: implements [`Emitter`] to produce self-contained TS source.
//!
//! All output is plain `String` — no AST construction, no template engine.
//! Generated code has zero runtime dependencies and uses direct recursive descent.

use bbnf_ir::{AltDispatch, GrammarIR, IrRule, RuleId, TypeDesc};

use crate::backend::analysis::BackendAnalysis;
use crate::backend::{
    AllocStrategy, AltBranchInfo, Emitter, FlattenStrategy, SepByConfig, SeqChildGroup,
};

// ─── TS Emitter ─────────────────────────────────────────────────────────────

/// TypeScript code emitter.
///
/// Produces self-contained `.ts` source with:
/// - Discriminated unions for grammar enums
/// - Direct recursive descent functions (`function __rule(s: S): T | null`)
/// - `switch` dispatch for alternations with dispatch tables
/// - Inline byte checks for literals, `RegExp` sticky for regex
pub struct TsEmitter {
    /// Grammar enum name (e.g., `"JsonValue"`).
    pub enum_name: String,
}

/// Mutable context for TS emission.
pub struct TsEmitCtx {
    /// Counter for unique variable names.
    counter: usize,
}

impl Default for TsEmitCtx {
    fn default() -> Self {
        Self { counter: 0 }
    }
}

impl TsEmitCtx {
    pub fn fresh(&mut self, prefix: &str) -> String {
        let id = self.counter;
        self.counter += 1;
        format!("__{prefix}{id}")
    }
}

// ─── Type Mapping ───────────────────────────────────────────────────────────

fn type_desc_to_ts(td: &TypeDesc, enum_name: &str, ir: &GrammarIR) -> String {
    match td {
        TypeDesc::Span => "Span".to_string(),
        TypeDesc::F64 => "number".to_string(),
        TypeDesc::U32 => "number".to_string(),
        TypeDesc::Option(inner) => format!("{} | null", type_desc_to_ts(inner, enum_name, ir)),
        TypeDesc::Vec(inner) => format!("{}[]", type_desc_to_ts(inner, enum_name, ir)),
        TypeDesc::Tuple(elems) => {
            let parts: Vec<_> = elems.iter().map(|e| type_desc_to_ts(e, enum_name, ir)).collect();
            format!("[{}]", parts.join(", "))
        }
        TypeDesc::Enum | TypeDesc::BoxedEnum => enum_name.to_string(),
        TypeDesc::Named(sid) => ir.get_string(*sid).to_string(),
    }
}

// ─── Helpers ────────────────────────────────────────────────────────────────

/// Escape a string for embedding in a TS string literal.
fn ts_escape(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\0' => out.push_str("\\0"),
            _ => out.push(c),
        }
    }
    out
}

/// Unescape a BBNF literal (same logic as Rust backend).
fn unescape_literal(s: &str) -> String {
    crate::backend::rust::unescape_literal(s)
}

// ─── Emitter Trait Impl ─────────────────────────────────────────────────────

impl Emitter for TsEmitter {
    type Output = String;
    type Ctx = TsEmitCtx;

    // ── Leaves ──────────────────────────────────────────────────────────

    fn emit_literal_match(
        &mut self,
        value: &str,
        guaranteed_byte: Option<u8>,
        _ctx: &mut TsEmitCtx,
    ) -> String {
        let unescaped = unescape_literal(value);
        let bytes = unescaped.as_bytes();

        if let Some(_byte) = guaranteed_byte {
            return format!(
                "((() => {{ const __start = s.offset; s.offset += 1; \
                 return span(__start, s.offset); }})())"
            );
        }

        if bytes.len() == 1 {
            let byte = bytes[0];
            format!(
                "(s.offset < s.input.length && s.input.charCodeAt(s.offset) === {byte} \
                 ? (() => {{ const __start = s.offset; s.offset += 1; \
                 return span(__start, s.offset); }})() : null)"
            )
        } else {
            let escaped = ts_escape(&unescaped);
            let len = unescaped.len();
            format!(
                "(s.input.startsWith(\"{escaped}\", s.offset) \
                 ? (() => {{ const __start = s.offset; s.offset += {len}; \
                 return span(__start, s.offset); }})() : null)"
            )
        }
    }

    fn emit_regex_match(
        &mut self,
        pattern: &str,
        _ir: &GrammarIR,
        ctx: &mut TsEmitCtx,
    ) -> String {
        let var = ctx.fresh("re");
        // Use sticky flag (y) for anchored matching at current offset.
        let escaped_pattern = pattern.replace('\\', "\\\\").replace('/', "\\/");
        format!(
            "((() => {{ const {var} = /{escaped_pattern}/y; \
             {var}.lastIndex = s.offset; \
             const __m = {var}.exec(s.input); \
             if (!__m) return null; \
             const __start = s.offset; \
             s.offset = {var}.lastIndex; \
             return span(__start, s.offset); }})())"
        )
    }

    fn emit_epsilon(&mut self, _ctx: &mut TsEmitCtx) -> String {
        "span(s.offset, s.offset)".to_string()
    }

    // ── Sequences ───────────────────────────────────────────────────────

    fn emit_seq_all_span(
        &mut self,
        child_outputs: Vec<String>,
        _ctx: &mut TsEmitCtx,
    ) -> String {
        let mut body = String::from("const __start = s.offset;\n");
        for child in &child_outputs {
            body.push_str(&format!("  if (({child}) === null) return null;\n"));
        }
        body.push_str("  return span(__start, s.offset);");
        format!("((() => {{ {body} }})())")
    }

    fn emit_seq_grouped(
        &mut self,
        groups: Vec<SeqChildGroup<String>>,
        _result_type: &TypeDesc,
        _flatten: Option<FlattenStrategy>,
        ctx: &mut TsEmitCtx,
    ) -> String {
        let mut stmts = Vec::new();
        let mut result_vars = Vec::new();

        for group in groups {
            match group {
                SeqChildGroup::Single { output, ty: _ } => {
                    let var = ctx.fresh("v");
                    stmts.push(format!("const {var} = {output}; if ({var} === null) return null;"));
                    result_vars.push(var);
                }
                SeqChildGroup::SpanCompressed { outputs } => {
                    let var = ctx.fresh("sp");
                    let mut block = format!("const __sp_start = s.offset;\n");
                    for out in &outputs {
                        block.push_str(&format!("    if (({out}) === null) return null;\n"));
                    }
                    block.push_str(&format!(
                        "    const {var} = span(__sp_start, s.offset);"
                    ));
                    stmts.push(block);
                    result_vars.push(var);
                }
            }
        }

        let result_expr = if result_vars.len() == 1 {
            result_vars[0].clone()
        } else {
            format!("[{}]", result_vars.join(", "))
        };

        let body = stmts.join("\n  ");
        format!("((() => {{ {body}\n  return {result_expr}; }})())")
    }

    // ── Alternations ────────────────────────────────────────────────────

    fn emit_alt_dispatch(
        &mut self,
        table: &AltDispatch,
        branches: Vec<(AltBranchInfo, String)>,
        fallback: Option<(AltBranchInfo, String)>,
        _alloc: AllocStrategy,
        _ctx: &mut TsEmitCtx,
    ) -> String {
        let mut cases = String::new();

        for (branch_idx, (_info, body)) in branches.iter().enumerate() {
            let byte_patterns: Vec<u8> = table
                .table
                .iter()
                .enumerate()
                .filter(|&(_, &b)| b as usize == branch_idx)
                .map(|(byte_val, _)| byte_val as u8)
                .collect();

            for byte in &byte_patterns {
                cases.push_str(&format!("      case {byte}:\n"));
            }
            if !byte_patterns.is_empty() {
                cases.push_str(&format!("        return {body};\n"));
            }
        }

        let default_case = if let Some((_info, fb)) = fallback {
            format!("      default: return {fb};")
        } else {
            "      default: return null;".to_string()
        };

        format!(
            "((() => {{ \
             if (s.offset >= s.input.length) return null; \
             switch (s.input.charCodeAt(s.offset)) {{\n{cases}{default_case}\n    }} }})())"
        )
    }

    fn emit_alt_checkpoint(
        &mut self,
        branches: Vec<(AltBranchInfo, String)>,
        _alloc: AllocStrategy,
        ctx: &mut TsEmitCtx,
    ) -> String {
        if branches.len() == 1 {
            return branches.into_iter().next().unwrap().1;
        }

        let mut body = String::new();
        for (_info, branch_body) in &branches {
            let cp = ctx.fresh("cp");
            body.push_str(&format!(
                "  const {cp} = s.offset; \
                 {{ const __r = {branch_body}; if (__r !== null) return __r; }} \
                 s.offset = {cp};\n"
            ));
        }
        body.push_str("  return null;");
        format!("((() => {{\n{body}\n}})())")
    }

    fn emit_alt_all_literal(
        &mut self,
        literals: Vec<(String, String)>,
        _alloc: AllocStrategy,
        ctx: &mut TsEmitCtx,
    ) -> String {
        // Delegate to checkpoint chain since each literal body already handles matching.
        self.emit_alt_checkpoint(
            literals
                .into_iter()
                .map(|(_, body)| {
                    (
                        AltBranchInfo {
                            ty: TypeDesc::Span,
                            coercion_variant: None,
                        },
                        body,
                    )
                })
                .collect(),
            AllocStrategy::Elide,
            ctx,
        )
    }

    // ── Repetition ──────────────────────────────────────────────────────

    fn emit_repeat_many(
        &mut self,
        body: String,
        lo: u32,
        _hi: u32,
        _elem_type: &TypeDesc,
        ctx: &mut TsEmitCtx,
    ) -> String {
        let count = ctx.fresh("count");
        format!(
            "((() => {{ \
             const __start = s.offset; \
             let {count} = 0; \
             while (true) {{ \
               const __prev = s.offset; \
               const __r = {body}; \
               if (__r === null) break; \
               {count}++; \
               if (s.offset === __prev) break; \
             }} \
             return {count} >= {lo} ? span(__start, s.offset) : null; \
             }})())"
        )
    }

    fn emit_repeat_optional(
        &mut self,
        body: String,
        _inner_type: &TypeDesc,
        _alloc: AllocStrategy,
        ctx: &mut TsEmitCtx,
    ) -> String {
        let cp = ctx.fresh("cp");
        format!(
            "((() => {{ \
             const {cp} = s.offset; \
             const __r = {body}; \
             if (__r !== null) return __r; \
             s.offset = {cp}; \
             return null; \
             }})())"
        )
    }

    fn emit_sep_by(
        &mut self,
        element: String,
        separator: String,
        config: &SepByConfig,
        _elem_type: &TypeDesc,
        ctx: &mut TsEmitCtx,
    ) -> String {
        let count = ctx.fresh("count");
        let cp = ctx.fresh("cp");
        let lo = config.lo;
        format!(
            "((() => {{ \
             const __start = s.offset; \
             let {count} = 0; \
             {{ const __r = {element}; if (__r !== null) {count}++; else return {count} >= {lo} ? span(__start, s.offset) : null; }} \
             while (true) {{ \
               const {cp} = s.offset; \
               {{ const __sep = {separator}; if (__sep === null) break; }} \
               {{ const __r = {element}; if (__r !== null) {{ {count}++; }} else {{ s.offset = {cp}; break; }} }} \
             }} \
             return {count} >= {lo} ? span(__start, s.offset) : null; \
             }})())"
        )
    }

    // ── References ──────────────────────────────────────────────────────

    fn emit_call(
        &mut self,
        _rule_id: RuleId,
        rule_name: &str,
        _alloc: AllocStrategy,
        _ctx: &mut TsEmitCtx,
    ) -> String {
        format!("__{rule_name}(s)")
    }

    fn emit_inline_wrap(
        &mut self,
        body: String,
        variant_name: Option<&str>,
        _alloc: AllocStrategy,
        _ctx: &mut TsEmitCtx,
    ) -> String {
        if let Some(name) = variant_name {
            format!(
                "((() => {{ const __r = {body}; \
                 return __r !== null ? {{ tag: \"{name}\" as const, value: __r }} : null; }})())"
            )
        } else {
            body
        }
    }

    // ── Binary operators ────────────────────────────────────────────────

    fn emit_skip(
        &mut self,
        kept: String,
        discarded: String,
        _ctx: &mut TsEmitCtx,
    ) -> String {
        format!(
            "((() => {{ \
             const __kept = {kept}; \
             if (__kept === null) return null; \
             if (({discarded}) === null) return null; \
             return __kept; \
             }})())"
        )
    }

    fn emit_next(
        &mut self,
        discarded: String,
        kept: String,
        _ctx: &mut TsEmitCtx,
    ) -> String {
        format!(
            "((() => {{ \
             if (({discarded}) === null) return null; \
             return {kept}; \
             }})())"
        )
    }

    fn emit_minus(
        &mut self,
        lhs: String,
        rhs: String,
        _ctx: &mut TsEmitCtx,
    ) -> String {
        format!(
            "((() => {{ \
             const __save = s.offset; \
             const __excluded = {rhs}; \
             s.offset = __save; \
             return __excluded !== null ? null : {lhs}; \
             }})())"
        )
    }

    fn emit_negate(
        &mut self,
        inner: String,
        _ctx: &mut TsEmitCtx,
    ) -> String {
        format!(
            "((() => {{ \
             const __save = s.offset; \
             const __inner = {inner}; \
             s.offset = __save; \
             return __inner !== null ? null : {{}}; \
             }})())"
        )
    }

    // ── Value manipulation ──────────────────────────────────────────────

    fn emit_enum_wrap(
        &mut self,
        inner: String,
        variant_name: &str,
        _alloc: AllocStrategy,
        _ctx: &mut TsEmitCtx,
    ) -> String {
        format!(
            "((() => {{ const __r = {inner}; \
             return __r !== null ? {{ tag: \"{variant_name}\" as const, value: __r }} : null; }})())"
        )
    }

    fn emit_number_convert(&mut self, ctx: &mut TsEmitCtx) -> String {
        // Emit inline number parsing — CSS-compatible (sign, fraction, exponent).
        let re = ctx.fresh("re");
        format!(
            "((() => {{ \
             const {re} = /[-+]?(?:[0-9]*\\.)?[0-9]+(?:[eE][-+]?[0-9]+)?/y; \
             {re}.lastIndex = s.offset; \
             const __m = {re}.exec(s.input); \
             if (!__m) return null; \
             s.offset = {re}.lastIndex; \
             return parseFloat(__m[0]); \
             }})())"
        )
    }

    fn emit_constant(
        &mut self,
        discard_inner: String,
        value: &str,
        _ctx: &mut TsEmitCtx,
    ) -> String {
        format!(
            "((() => {{ \
             if (({discard_inner}) === null) return null; \
             return {value}; \
             }})())"
        )
    }

    fn emit_ws_trim(
        &mut self,
        _ws_pattern: Option<&str>,
        _ctx: &mut TsEmitCtx,
    ) -> String {
        // Default: skip ASCII whitespace.
        "((() => { while (s.offset < s.input.length && \" \\t\\n\\r\".includes(s.input[s.offset])) s.offset++; return {}; })())".to_string()
    }

    // ── Rule-level emission ─────────────────────────────────────────────

    fn emit_rule_function(
        &mut self,
        rule: &IrRule,
        body: String,
        ir: &GrammarIR,
        _ctx: &mut TsEmitCtx,
    ) -> String {
        let name = ir.get_string(rule.name);
        let enum_name = &self.enum_name;

        let return_type = ir
            .types
            .iter()
            .find(|(id, _)| *id == rule.id)
            .map(|(_, td)| type_desc_to_ts(td, enum_name, ir))
            .unwrap_or_else(|| "Span".to_string());

        if rule.meta.is_transparent {
            format!(
                "function __{name}(s: ParserState): {return_type} | null {{\n  return {body};\n}}\n"
            )
        } else {
            format!(
                "function __{name}(s: ParserState): {enum_name} | null {{\n  \
                 const __r = {body};\n  \
                 return __r !== null ? {{ tag: \"{name}\" as const, value: __r }} : null;\n}}\n"
            )
        }
    }

    fn emit_type_definitions(
        &mut self,
        ir: &GrammarIR,
        _analysis: &BackendAnalysis,
        _ctx: &mut TsEmitCtx,
    ) -> String {
        let enum_name = &self.enum_name;

        // Generate discriminated union from non-transparent rules.
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

        format!(
            "// ── Runtime types ────────────────────────────────────────────────\n\n\
             interface ParserState {{\n  input: string;\n  offset: number;\n}}\n\n\
             interface Span {{\n  start: number;\n  end: number;\n}}\n\n\
             function span(start: number, end: number): Span {{\n  return {{ start, end }};\n}}\n\n\
             function createState(input: string): ParserState {{\n  return {{ input, offset: 0 }};\n}}\n\n\
             // ── Grammar types ───────────────────────────────────────────────\n\n\
             type {enum_name} =\n{union_body};\n\n"
        )
    }

    fn emit_grammar(
        &mut self,
        type_defs: String,
        rule_functions: Vec<String>,
        ir: &GrammarIR,
        _ctx: &mut TsEmitCtx,
    ) -> String {
        let entry_name = ir.get_string(ir.rules[ir.entry as usize].name);

        let mut output = String::new();
        output.push_str("// Generated by BBNF — do not edit.\n\n");
        output.push_str(&type_defs);
        output.push_str("// ── Parser functions ─────────────────────────────────────────────\n\n");
        for func in &rule_functions {
            output.push_str(func);
            output.push('\n');
        }

        // Public entry point.
        output.push_str(&format!(
            "// ── Public API ──────────────────────────────────────────────────\n\n\
             export function parse(input: string): {{ result: {enum_name} | null; offset: number }} {{\n  \
             const s = createState(input);\n  \
             const result = __{entry_name}(s);\n  \
             return {{ result, offset: s.offset }};\n}}\n",
            enum_name = self.enum_name,
        ));

        output
    }
}
