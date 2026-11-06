//! TypeScript projection — `TypeDesc → TS source`, literal escape,
//! Rust-suffix stripping, MapExpr → JS expression compilation.
//!
//! Every emitted-TS body routes its type and value translation through
//! these helpers so the per-rule emitters remain projection-agnostic.

use std::fmt::Write as _;

use bbnf_ir::{GrammarIR, MapExpr, NumericDomain, TypeDesc};

use super::code::TsEmitCtx;

pub fn type_desc_to_ts(td: &TypeDesc, enum_name: &str, ir: &GrammarIR) -> String {
    match td {
        TypeDesc::Span => "Span".to_string(),
        TypeDesc::F64 => "number".to_string(),
        TypeDesc::Bool => "boolean".to_string(),
        TypeDesc::I64 | TypeDesc::U64 => "bigint".to_string(),
        // TS has no native fixed-width integer types — all numeric
        // scalars except full-domain `i64` / `u64` project to `number`.
        // JavaScript's number cannot represent every 64-bit integer
        // exactly, so those domains project to bigint.
        TypeDesc::I8
        | TypeDesc::U8
        | TypeDesc::I16
        | TypeDesc::U16
        | TypeDesc::I32
        | TypeDesc::U32 => "number".to_string(),
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
            '\0' => out.push_str("\\u0000"),
            '\u{2028}' => out.push_str("\\u2028"),
            '\u{2029}' => out.push_str("\\u2029"),
            c if c.is_control() => {
                write!(out, "\\u{:04X}", c as u32).expect("writing to String cannot fail");
            }
            c if c as u32 > 0xFFFF => {
                for unit in c.encode_utf16(&mut [0; 2]) {
                    write!(out, "\\u{unit:04X}").expect("writing to String cannot fail");
                }
            }
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
    if let Some(value) = trimmed.strip_suffix("u64") {
        return format!("{value}n");
    }
    if let Some(value) = trimmed.strip_suffix("i64") {
        return format!("{value}n");
    }
    trimmed
        .strip_suffix("u8")
        .or_else(|| trimmed.strip_suffix("u16"))
        .or_else(|| trimmed.strip_suffix("u32"))
        .or_else(|| trimmed.strip_suffix("i8"))
        .or_else(|| trimmed.strip_suffix("i16"))
        .or_else(|| trimmed.strip_suffix("i32"))
        .or_else(|| trimmed.strip_suffix("f32"))
        .or_else(|| trimmed.strip_suffix("f64"))
        .or_else(|| trimmed.strip_suffix("usize"))
        .or_else(|| trimmed.strip_suffix("isize"))
        .unwrap_or(trimmed)
        .to_string()
}

/// Compile a MapExpr tree to a JavaScript expression string.
/// `__input` is in scope as the parse result variable.
pub fn compile_map_expr_to_js(
    expr: &MapExpr,
    return_type: Option<&TypeDesc>,
    ir: &GrammarIR,
) -> String {
    let mut folded = expr.clone();
    folded
        .checked_fold(return_type.and_then(NumericDomain::from_type_desc))
        .unwrap_or_else(|error| {
            panic!(
                "TypeScript cannot emit invalid numeric map expression: {}",
                error.message()
            )
        });
    let expr = &folded;
    assert!(
        !(expr.is_constant() && matches!(expr, MapExpr::BinOp { .. } | MapExpr::UnaryOp { .. })),
        "TypeScript cannot emit a constant map expression that failed checked folding"
    );
    let integer_projection = Some(match return_type {
        Some(TypeDesc::I64 | TypeDesc::U64) => IntegerProjection::BigInt,
        Some(
            TypeDesc::I8
            | TypeDesc::U8
            | TypeDesc::I16
            | TypeDesc::U16
            | TypeDesc::I32
            | TypeDesc::U32,
        ) => IntegerProjection::Number,
        _ if contains_wide_integer(expr) => {
            assert!(
                !contains_dynamic_value(expr),
                "TypeScript cannot infer one exact integer domain for an untyped \
                 expression containing both a wide literal and dynamic input"
            );
            IntegerProjection::BigInt
        }
        _ => IntegerProjection::Number,
    });
    compile_map_expr_to_js_with_integer_projection(expr, return_type, integer_projection, ir)
}

#[derive(Clone, Copy)]
enum IntegerProjection {
    Number,
    BigInt,
}

const JS_MAX_SAFE_INTEGER: u64 = 9_007_199_254_740_991;

fn contains_wide_integer(expr: &MapExpr) -> bool {
    match expr {
        MapExpr::IntLit(value) => value.unsigned_abs() > JS_MAX_SAFE_INTEGER,
        MapExpr::UIntLit(value) => *value > JS_MAX_SAFE_INTEGER,
        _ => {
            let mut found = false;
            expr.for_each_child(&mut |child| found |= contains_wide_integer(child));
            found
        }
    }
}

fn contains_dynamic_value(expr: &MapExpr) -> bool {
    match expr {
        MapExpr::Input | MapExpr::InputProp { .. } | MapExpr::FnCall { .. } => true,
        _ => {
            let mut found = false;
            expr.for_each_child(&mut |child| found |= contains_dynamic_value(child));
            found
        }
    }
}

fn compile_map_expr_to_js_with_integer_projection(
    expr: &MapExpr,
    return_type: Option<&TypeDesc>,
    integer_projection: Option<IntegerProjection>,
    ir: &GrammarIR,
) -> String {
    match expr {
        MapExpr::IntLit(n) => {
            validate_signed_literal(*n, return_type);
            match integer_projection.unwrap_or_else(|| {
                if n.unsigned_abs() > JS_MAX_SAFE_INTEGER {
                    IntegerProjection::BigInt
                } else {
                    IntegerProjection::Number
                }
            }) {
                IntegerProjection::Number => {
                    assert!(
                        n.unsigned_abs() <= JS_MAX_SAFE_INTEGER,
                        "wide signed integer cannot project to a TypeScript number"
                    );
                    format!("{n}")
                }
                IntegerProjection::BigInt => format!("{n}n"),
            }
        }
        MapExpr::UIntLit(n) => {
            validate_unsigned_literal(*n, return_type);
            match integer_projection.unwrap_or_else(|| {
                if *n > JS_MAX_SAFE_INTEGER {
                    IntegerProjection::BigInt
                } else {
                    IntegerProjection::Number
                }
            }) {
                IntegerProjection::Number => {
                    assert!(
                        *n <= JS_MAX_SAFE_INTEGER,
                        "wide unsigned integer cannot project to a TypeScript number"
                    );
                    format!("{n}")
                }
                IntegerProjection::BigInt => format!("{n}n"),
            }
        }
        MapExpr::FloatLit(f) => {
            assert!(f.is_finite(), "TypeScript float map constant is not finite");
            assert!(
                !matches!(integer_projection, Some(IntegerProjection::BigInt)),
                "float cannot participate in a TypeScript bigint expression"
            );
            assert!(
                matches!(return_type, None | Some(TypeDesc::F64)),
                "TypeScript float map constant has a non-float return type"
            );
            format!("{f}")
        }
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
            // Strip Rust-style path qualifiers (`crate::module::fn`) — TS
            // identifiers cannot contain `::`. The TS runtime resolves
            // host functions via the bare function name from the
            // `declare function …` shim emitted at the top of the
            // generated module (see `emitter/grammar.rs`'s host-fn
            // declaration block).
            let raw_name = ir.get_string(*name);
            let fn_name = raw_name.rsplit("::").next().unwrap_or(raw_name);
            let compiled_args: Vec<String> = args
                .iter()
                .map(|a| compile_map_expr_to_js_with_integer_projection(a, None, None, ir))
                .collect();
            if compiled_args.is_empty() {
                // Empty arg list — pass the captured `__re*` Span variable
                // as the implicit input. This branch fires when the IR
                // strips the explicit `input` arg during typed-leaf
                // collapse; the surrounding emitter produces the
                // `(captured) !== null ? fn(captured) : null` shape, but
                // the captured variable name is the inner expr, not
                // `__input` (the closure declares no `__input` in the
                // constant-branch emission path).
                format!("{fn_name}(__input)")
            } else {
                format!("{fn_name}({})", compiled_args.join(", "))
            }
        }
        MapExpr::BinOp {
            op,
            operand_domain,
            lhs,
            rhs,
        } => {
            let owned_operand_type = operand_domain.map(NumericDomain::type_desc);
            let operand_type = owned_operand_type.as_ref().or(match op {
                bbnf_ir::MapBinOp::Add
                | bbnf_ir::MapBinOp::Sub
                | bbnf_ir::MapBinOp::Mul
                | bbnf_ir::MapBinOp::Div
                | bbnf_ir::MapBinOp::Mod
                | bbnf_ir::MapBinOp::BitAnd
                | bbnf_ir::MapBinOp::BitOr
                | bbnf_ir::MapBinOp::Shl
                | bbnf_ir::MapBinOp::Shr => return_type,
                _ => None,
            });
            let local_projection = operand_domain
                .map(|domain| match domain {
                    NumericDomain::I64 | NumericDomain::U64 => IntegerProjection::BigInt,
                    _ => IntegerProjection::Number,
                })
                .or(integer_projection);
            let child_projection = if matches!(op, bbnf_ir::MapBinOp::And | bbnf_ir::MapBinOp::Or) {
                None
            } else {
                local_projection
            };
            let l = compile_map_expr_to_js_with_integer_projection(
                lhs,
                operand_type,
                child_projection,
                ir,
            );
            let r = compile_map_expr_to_js_with_integer_projection(
                rhs,
                operand_type,
                child_projection,
                ir,
            );
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
        MapExpr::UnaryOp {
            op,
            operand_domain,
            inner,
        } => {
            let owned_operand_type = operand_domain.map(NumericDomain::type_desc);
            let operand_type = match op {
                bbnf_ir::MapUnaryOp::Neg => owned_operand_type.as_ref().or(return_type),
                bbnf_ir::MapUnaryOp::Not => None,
            };
            let local_projection = operand_domain
                .map(|domain| match domain {
                    NumericDomain::I64 | NumericDomain::U64 => IntegerProjection::BigInt,
                    _ => IntegerProjection::Number,
                })
                .or(integer_projection);
            let i = compile_map_expr_to_js_with_integer_projection(
                inner,
                operand_type,
                local_projection,
                ir,
            );
            match op {
                bbnf_ir::MapUnaryOp::Neg => format!("(-{i})"),
                bbnf_ir::MapUnaryOp::Not => format!("(!{i})"),
            }
        }
    }
}

fn validate_signed_literal(value: i64, return_type: Option<&TypeDesc>) {
    let valid = match return_type {
        None => true,
        Some(TypeDesc::I8) => i8::try_from(value).is_ok(),
        Some(TypeDesc::I16) => i16::try_from(value).is_ok(),
        Some(TypeDesc::I32) => i32::try_from(value).is_ok(),
        Some(TypeDesc::I64) => true,
        Some(TypeDesc::U8) => u8::try_from(value).is_ok(),
        Some(TypeDesc::U16) => u16::try_from(value).is_ok(),
        Some(TypeDesc::U32) => u32::try_from(value).is_ok(),
        Some(TypeDesc::U64) => u64::try_from(value).is_ok(),
        Some(_) => false,
    };
    assert!(
        valid,
        "signed integer literal {value} is incompatible with its TypeScript return type"
    );
}

fn validate_unsigned_literal(value: u64, return_type: Option<&TypeDesc>) {
    let valid = match return_type {
        None => true,
        Some(TypeDesc::I8) => i8::try_from(value).is_ok(),
        Some(TypeDesc::I16) => i16::try_from(value).is_ok(),
        Some(TypeDesc::I32) => i32::try_from(value).is_ok(),
        Some(TypeDesc::I64) => i64::try_from(value).is_ok(),
        Some(TypeDesc::U8) => u8::try_from(value).is_ok(),
        Some(TypeDesc::U16) => u16::try_from(value).is_ok(),
        Some(TypeDesc::U32) => u32::try_from(value).is_ok(),
        Some(TypeDesc::U64) => true,
        Some(_) => false,
    };
    assert!(
        valid,
        "unsigned integer literal {value} is incompatible with its TypeScript return type"
    );
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
