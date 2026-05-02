//! Rule- and grammar-level emission for the TypeScript backend:
//! rule_function / type_definitions / emit_grammar.
//!
//! Each method is `pub(super)` so the trait impl in `mod.rs` can delegate
//! to it via `self.emit_xxx_impl(...)`.

use bbnf_ir::{GrammarIR, IrRule, TypeDesc};

use crate::backend::driver::analysis::BackendAnalysis;

use super::{TsCode, TsEmitCtx, TsEmitter, type_desc_to_ts};

fn collect_named_types(
    td: &TypeDesc,
    ir: &GrammarIR,
    names: &mut std::collections::BTreeSet<String>,
) {
    match td {
        TypeDesc::Named(sid) => {
            names.insert(ir.get_string(*sid).to_string());
        }
        TypeDesc::Option(inner) | TypeDesc::Vec(inner) => {
            collect_named_types(inner, ir, names);
        }
        TypeDesc::Tuple(elems) => {
            for inner in elems {
                collect_named_types(inner, ir, names);
            }
        }
        TypeDesc::HeterogeneousAltJoin(branches) => {
            for inner in branches {
                collect_named_types(inner, ir, names);
            }
        }
        _ => {}
    }
}

impl TsEmitter {
    pub(super) fn emit_rule_function_impl(
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

        if rule.meta.is_transparent {
            // Transparent rules pass the inner result through verbatim
            // — the body already produces a value of the rule's typed
            // shape (which, for a pure-Ref Alt, is the discriminated
            // union itself).
            let fn_body = if body.stmts.is_empty() {
                format!("  return {};\n", body.expr)
            } else {
                format!("  {}\n  return {};\n", body.stmts, body.expr)
            };
            TsCode::expr(format!(
                "function __{name}(s: ParserState): {return_type} | null {{\n{fn_body}}}\n"
            ))
        } else {
            // Non-transparent rules wrap the body's typed result in
            // their named discriminated-union variant. The body
            // expression often produces a raw `Span` while the
            // variant declares a richer shape (`Vec<valueValue>` /
            // tuple / nested union); the cast through `unknown` to
            // `enum_name` short-circuits TypeScript's structural
            // narrowing on the literal so `tsc --strict` accepts the
            // emitted output. The W1.4 TS runtime models
            // grouped/repeated rules as raw spans; the W5 typed-
            // materialiser closes the structural gap. The `as const`
            // anchors the tag literal so callers narrow on the
            // discriminator without losing the variant link.
            let mut fn_body = String::new();
            if !body.stmts.is_empty() {
                fn_body.push_str("  ");
                fn_body.push_str(&body.stmts);
                fn_body.push('\n');
            }
            fn_body.push_str(&format!(
                "  const __body = {body_expr};\n  \
                 return __body !== null ? ({{ tag: \"{name}\" as const, value: __body }} as unknown as {enum_name}) : null;\n",
                body_expr = body.expr,
            ));
            TsCode::expr(format!(
                "function __{name}(s: ParserState): {enum_name} | null {{\n\
                 {fn_body}\
                 }}\n"
            ))
        }
    }

    pub(super) fn emit_type_definitions_impl(
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

        let mut named_types = std::collections::BTreeSet::new();
        for (_, td) in &ir.types {
            collect_named_types(td, ir, &mut named_types);
        }
        let named_aliases = if named_types.is_empty() {
            String::new()
        } else {
            let mut s = String::from(
                "// ── Named host types (W5 binding wave will replace `unknown` with executable types) ──\n\n",
            );
            for name in &named_types {
                s.push_str(&format!("type {name} = unknown;\n"));
            }
            s.push('\n');
            s
        };

        TsCode::expr(format!(
            "// ── Runtime types ────────────────────────────────────────────────\n\n\
             interface ParserState {{\n  input: string;\n  offset: number;\n}}\n\n\
             interface Span {{\n  start: number;\n  end: number;\n}}\n\n\
             function span(start: number, end: number): Span {{\n  return {{ start, end }};\n}}\n\n\
             function createState(input: string): ParserState {{\n  return {{ input, offset: 0 }};\n}}\n\n\
             {named_aliases}\
             // ── Grammar types ───────────────────────────────────────────────\n\n\
             type {enum_name} =\n{union_body};\n\n"
        ))
    }

    pub(super) fn emit_grammar_impl(
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

        // Host-function declarations. The grammar's `->` expressions
        // can call user-defined Rust host fns (`@host` / hex-decode /
        // string-decode) that have no native TS counterpart at build
        // time; emit `declare` bindings so TypeScript can typecheck
        // the references. The W5 binding wave plumbs in the actual
        // implementations.
        let host_fns = collect_host_fn_names(ir);
        if !host_fns.is_empty() {
            output.push_str(
                "// ── Host function declarations (resolved by the runtime) ────────\n\n",
            );
            for fn_name in &host_fns {
                output.push_str(&format!("declare function {fn_name}(__input: any): any;\n"));
            }
            output.push('\n');
        }

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

        output.push_str("// ── Parser functions ─────────────────────────────────────────────\n\n");
        for func in &rule_functions {
            output.push_str(&func.expr);
            output.push('\n');
        }

        // Public API: `parse(input)` runs the entry rule and enforces
        // EOF — the parse fails if any input remains unconsumed after
        // the entry rule returns. Trailing whitespace is tolerated
        // when the grammar declared `@ws` so simple round-trips of
        // `parse(serialize(x))` survive insignificant trailing
        // whitespace.
        let trailing_ws = if ir.ws_pattern.is_some() {
            "  while (s.offset < s.input.length) {\n    \
                 const __c = s.input.charCodeAt(s.offset);\n    \
                 if (__c === 32 || __c === 9 || __c === 10 || __c === 13) {\n      s.offset++;\n    } else {\n      break;\n    }\n  }\n  "
        } else {
            "  "
        };

        output.push_str(&format!(
            "// ── Public API ──────────────────────────────────────────────────\n\n\
             export function parse(input: string): {{ result: {enum_name} | null; offset: number }} {{\n  \
             const s = createState(input);\n  \
             const result = __{entry_name}(s);\n\
             {trailing_ws}\
             if (result === null || s.offset !== s.input.length) {{\n    \
             return {{ result: null, offset: s.offset }};\n  \
             }}\n  \
             return {{ result, offset: s.offset }};\n}}\n",
            enum_name = self.enum_name,
        ));

        TsCode::expr(output)
    }
}

/// Walk the grammar IR and collect host-function call names. These are
/// `MapExpr::FnCall` references whose names resolve to user-defined
/// host functions (no JS implementation in the emitted source). The
/// emitter declares `declare function <name>(...)` for each so
/// `tsc --noEmit` can typecheck the generated parser without forcing
/// the W5 runtime binding to be present.
fn collect_host_fn_names(ir: &GrammarIR) -> Vec<String> {
    use bbnf_ir::{FnDescriptor, MapExpr};

    fn last_segment(path: &str) -> String {
        path.rsplit("::").next().unwrap_or(path).to_string()
    }

    fn visit_expr(expr: &MapExpr, names: &mut std::collections::BTreeSet<String>, ir: &GrammarIR) {
        match expr {
            MapExpr::FnCall { name, args } => {
                let raw = ir.get_string(*name);
                names.insert(last_segment(raw));
                for a in args {
                    visit_expr(a, names, ir);
                }
            }
            MapExpr::BinOp { lhs, rhs, .. } => {
                visit_expr(lhs, names, ir);
                visit_expr(rhs, names, ir);
            }
            MapExpr::UnaryOp { inner, .. } => visit_expr(inner, names, ir),
            _ => {}
        }
    }

    let mut names: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    for fd in &ir.fns {
        match fd {
            FnDescriptor::Expr { expr, .. } => visit_expr(expr, &mut names, ir),
            FnDescriptor::HexConvert { fn_path } => {
                let raw = ir.get_string(*fn_path);
                names.insert(last_segment(raw));
            }
            FnDescriptor::EnumWrap { .. }
            | FnDescriptor::BoxWrap
            | FnDescriptor::SpanCapture
            | FnDescriptor::NumberConvert { .. } => {}
        }
    }
    names.into_iter().collect()
}
