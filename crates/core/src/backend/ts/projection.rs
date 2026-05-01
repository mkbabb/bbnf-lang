//! TypeScript projection — `TypeDesc → TS source`, literal escape,
//! Rust-suffix stripping, MapExpr → JS expression compilation.
//!
//! Every emitted-TS body routes its type and value translation through
//! these helpers so the per-rule emitters remain projection-agnostic.

use bbnf_ir::{GrammarIR, MapExpr, TypeDesc};

use super::code::TsEmitCtx;

pub fn type_desc_to_ts(td: &TypeDesc, enum_name: &str, ir: &GrammarIR) -> String {
    match td {
        TypeDesc::Span => "Span".to_string(),
        TypeDesc::F64 => "number".to_string(),
        TypeDesc::Bool => "boolean".to_string(),
        // TS has no native fixed-width integer types — all numeric
        // scalars project to the unified `number`. Range / signedness
        // distinctions matter only on the Rust side where the typed
        // payload is consumed.
        TypeDesc::I8
        | TypeDesc::U8
        | TypeDesc::I16
        | TypeDesc::U16
        | TypeDesc::I32
        | TypeDesc::U32
        | TypeDesc::I64
        | TypeDesc::U64 => "number".to_string(),
        TypeDesc::Option(inner) => format!("{} | null", type_desc_to_ts(inner, enum_name, ir)),
        TypeDesc::Vec(inner) => format!("{}[]", type_desc_to_ts(inner, enum_name, ir)),
        TypeDesc::Tuple(elems) => {
            let parts: Vec<_> = elems
                .iter()
                .map(|e| type_desc_to_ts(e, enum_name, ir))
                .collect();
            format!("[{}]", parts.join(", "))
        }
        TypeDesc::Enum | TypeDesc::BoxedEnum | TypeDesc::HeterogeneousAltJoin(_) => {
            // AZ-III.W3a.3 — `HeterogeneousAltJoin` lowers identically to
            // `BoxedEnum` for the TS surface; the obligation evidence is
            // surfaced in `ir.type_obligations`, not in the projected
            // type string.
            enum_name.to_string()
        }
        TypeDesc::Named(sid) => ir.get_string(*sid).to_string(),
    }
}

pub fn ts_escape(s: &str) -> String {
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

pub fn unescape_literal(s: &str) -> String {
    crate::backend::unescape_literal(s)
}

pub fn translate_rust_constant_to_js(value: &str) -> String {
    let trimmed = value.trim();
    trimmed
        .strip_suffix("u8")
        .or_else(|| trimmed.strip_suffix("u16"))
        .or_else(|| trimmed.strip_suffix("u32"))
        .or_else(|| trimmed.strip_suffix("u64"))
        .or_else(|| trimmed.strip_suffix("i8"))
        .or_else(|| trimmed.strip_suffix("i16"))
        .or_else(|| trimmed.strip_suffix("i32"))
        .or_else(|| trimmed.strip_suffix("i64"))
        .or_else(|| trimmed.strip_suffix("f32"))
        .or_else(|| trimmed.strip_suffix("f64"))
        .or_else(|| trimmed.strip_suffix("usize"))
        .or_else(|| trimmed.strip_suffix("isize"))
        .unwrap_or(trimmed)
        .to_string()
}

/// Compile a MapExpr tree to a JavaScript expression string.
/// `__input` is in scope as the parse result variable.
pub fn compile_map_expr_to_js(expr: &MapExpr, ir: &GrammarIR) -> String {
    match expr {
        MapExpr::IntLit(n) => format!("{n}"),
        MapExpr::FloatLit(f) => format!("{f}"),
        MapExpr::BoolLit(b) => {
            if *b {
                "true".to_string()
            } else {
                "false".to_string()
            }
        }
        MapExpr::StringLit(sid) => {
            let s = ir.get_string(*sid);
            format!("\"{}\"", ts_escape(s))
        }
        MapExpr::Input => "__input".to_string(),
        MapExpr::InputProp { prop } => {
            let prop_name = ir.get_string(*prop);
            format!("__input.{prop_name}")
        }
        MapExpr::FnCall { name, args } => {
            let fn_name = ir.get_string(*name);
            let compiled_args: Vec<String> =
                args.iter().map(|a| compile_map_expr_to_js(a, ir)).collect();
            if compiled_args.is_empty() {
                format!("{fn_name}(__input)")
            } else {
                format!("{fn_name}({})", compiled_args.join(", "))
            }
        }
        MapExpr::BinOp { op, lhs, rhs } => {
            let l = compile_map_expr_to_js(lhs, ir);
            let r = compile_map_expr_to_js(rhs, ir);
            let op_str = match op {
                bbnf_ir::MapBinOp::Add => "+",
                bbnf_ir::MapBinOp::Sub => "-",
                bbnf_ir::MapBinOp::Mul => "*",
                bbnf_ir::MapBinOp::Div => "/",
                bbnf_ir::MapBinOp::Mod => "%",
                bbnf_ir::MapBinOp::Eq => "===",
                bbnf_ir::MapBinOp::Ne => "!==",
                bbnf_ir::MapBinOp::Lt => "<",
                bbnf_ir::MapBinOp::Gt => ">",
                bbnf_ir::MapBinOp::Le => "<=",
                bbnf_ir::MapBinOp::Ge => ">=",
                bbnf_ir::MapBinOp::And => "&&",
                bbnf_ir::MapBinOp::Or => "||",
                bbnf_ir::MapBinOp::BitAnd => "&",
                bbnf_ir::MapBinOp::BitOr => "|",
                bbnf_ir::MapBinOp::Shl => "<<",
                bbnf_ir::MapBinOp::Shr => ">>",
            };
            format!("({l} {op_str} {r})")
        }
        MapExpr::UnaryOp { op, inner } => {
            let i = compile_map_expr_to_js(inner, ir);
            match op {
                bbnf_ir::MapUnaryOp::Neg => format!("(-{i})"),
                bbnf_ir::MapUnaryOp::Not => format!("(!{i})"),
            }
        }
    }
}

/// Inline whitespace-skip statements.
pub fn ws_skip_stmts(ws_pattern: Option<&str>, _ctx: &mut TsEmitCtx) -> String {
    if let Some(pattern) = ws_pattern {
        if pattern.contains("/*") || pattern.contains(r"\/\*") {
            // Block-comment-aware whitespace: inline state machine.
            "while (s.offset < s.input.length) {\n  \
               const __c = s.input.charCodeAt(s.offset);\n  \
               if (__c === 32 || __c === 9 || __c === 10 || __c === 13) { s.offset++; continue; }\n  \
               if (__c === 47 && s.offset + 1 < s.input.length && s.input.charCodeAt(s.offset + 1) === 42) {\n    \
                 const __end = s.input.indexOf(\"*/\", s.offset + 2);\n    \
                 if (__end === -1) break;\n    \
                 s.offset = __end + 2;\n    \
                 continue;\n  \
               }\n  \
               break;\n\
             }\n".to_string()
        } else {
            format!(
                "{{ const __wsRe = new RegExp(\"{}\", \"y\");\n  \
                 __wsRe.lastIndex = s.offset;\n  \
                 const __wsm = __wsRe.exec(s.input);\n  \
                 if (__wsm) s.offset = __wsRe.lastIndex; }}\n",
                ts_escape(pattern)
            )
        }
    } else {
        "while (s.offset < s.input.length) { const __c = s.input.charCodeAt(s.offset); if (__c === 32 || __c === 9 || __c === 10 || __c === 13) s.offset++; else break; }\n".to_string()
    }
}
