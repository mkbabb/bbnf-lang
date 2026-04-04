//! TS codegen helper functions.

use bbnf_ir::{GrammarIR, TypeDesc};

use super::code::TsEmitCtx;

pub fn type_desc_to_ts(td: &TypeDesc, enum_name: &str, ir: &GrammarIR) -> String {
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
