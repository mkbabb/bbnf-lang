//! Alternation codegen helpers for the TS emitter.

use bbnf_ir::AltDispatch;

use crate::backend::{
    AltBranchInfo, KeyClass, KeyDispatchBranch, KeyDispatchConfig, ValuePlacement,
};

use super::code::{TsCode, TsEmitCtx, TsEmitter};
use super::projection::ts_escape;

impl TsEmitter {
    pub(in crate::backend::ts) fn alt_dispatch(
        &mut self,
        table: &AltDispatch,
        branches: Vec<(AltBranchInfo, TsCode)>,
        fallback: Option<(AltBranchInfo, TsCode)>,
        _alloc: ValuePlacement,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        let result = ctx.fresh("dispatch");
        let mut stmts = format!("let {result} = null;\n");
        stmts.push_str(
            "if (s.offset < s.input.length) {\n  switch (s.input.charCodeAt(s.offset)) {\n",
        );

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
            stmts.push_str(&format!(
                "    default: {result} = {}; break;\n",
                fb.as_expr()
            ));
        } else {
            stmts.push_str("    default: break;\n");
        }
        stmts.push_str("  }\n}\n");
        TsCode::new(stmts, result)
    }

    pub(in crate::backend::ts) fn alt_checkpoint(
        &mut self,
        branches: Vec<(AltBranchInfo, TsCode)>,
        _alloc: ValuePlacement,
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

    pub(in crate::backend::ts) fn alt_all_literal(
        &mut self,
        literals: Vec<(String, TsCode)>,
        _alloc: ValuePlacement,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        let result = ctx.fresh("lit_alt");
        let mut stmts = format!("let {result} = null;\n");
        for (value, body) in literals {
            let unescaped = value.to_string();
            let escaped = ts_escape(&unescaped);
            let body_expr = body.as_expr();
            stmts.push_str(&format!(
                "if ({result} === null && s.input.startsWith(\"{escaped}\", s.offset)) {{ {result} = {body_expr}; }}\n"
            ));
        }
        TsCode::new(stmts, result)
    }

    pub(in crate::backend::ts) fn key_dispatch(
        &mut self,
        config: &KeyDispatchConfig,
        branches: Vec<KeyDispatchBranch<TsCode>>,
        fallback: Option<(AltBranchInfo, TsCode)>,
        _alloc: ValuePlacement,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        let cp = ctx.fresh("kd_cp");
        let result = ctx.fresh("kd_result");
        let mut stmts = format!("const {cp} = s.offset;\n");
        let scanner_code = match &config.key_class {
            KeyClass::Identifier => "const __kd_re = /[a-zA-Z_][\\w-]*/y;\n__kd_re.lastIndex = s.offset;\nconst __kd_m = __kd_re.exec(s.input);\n".to_string(),
            KeyClass::QuotedString { quote_char } => {
                let q = *quote_char as char;
                format!("const __kd_re = /{q}[^{q}]*{q}/y;\n__kd_re.lastIndex = s.offset;\nconst __kd_m = __kd_re.exec(s.input);\n")
            }
        };
        stmts.push_str(&scanner_code);
        stmts.push_str(&format!("let {result} = null;\nif (__kd_m !== null) {{\n"));
        match &config.key_class {
            KeyClass::Identifier => stmts.push_str("  const __kd_key = __kd_m[0];\n"),
            KeyClass::QuotedString { .. } => {
                stmts.push_str("  const __kd_key = __kd_m[0].slice(1, -1);\n")
            }
        }
        for kd_branch in branches {
            let checks: Vec<String> = kd_branch
                .key_bytes
                .iter()
                .map(|key| {
                    let s = String::from_utf8_lossy(key);
                    format!("__kd_key === \"{}\"", ts_escape(&s))
                })
                .collect();
            stmts.push_str(&format!(
                "  if ({result} === null && ({})) {{\n    s.offset = {cp};\n",
                checks.join(" || ")
            ));
            let expr = kd_branch.body.dissolve(&mut stmts);
            stmts.push_str(&format!("    {result} = {expr};\n  }}\n"));
        }
        stmts.push_str("}}\n");
        stmts.push_str(&format!("if ({result} === null) {{\n  s.offset = {cp};\n"));
        if let Some((_info, fb)) = fallback {
            let expr = fb.dissolve(&mut stmts);
            stmts.push_str(&format!("  {result} = {expr};\n}}\n"));
        } else {
            stmts.push_str("}}\n");
        }
        TsCode::new(stmts, result)
    }
}
