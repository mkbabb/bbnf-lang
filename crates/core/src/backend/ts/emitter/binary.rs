//! Reference, operator-chain, and binary-operator emission for the TypeScript
//! backend: call / inline_wrap / operator_chain / skip / next / minus / negate.
//!
//! Each method is `pub(super)` so the trait impl in `mod.rs` can delegate
//! to it via `self.emit_xxx_impl(...)`.

use bbnf_ir::{GrammarIR, RuleId, TypeDesc};

use crate::backend::ValuePlacement;

use super::{TsCode, TsEmitCtx, TsEmitter};

impl TsEmitter {
    pub(super) fn emit_call_impl(
        &mut self,
        _rule_id: RuleId,
        rule_name: &str,
        _alloc: ValuePlacement,
        _ctx: &mut TsEmitCtx,
    ) -> TsCode {
        TsCode::expr(format!("__{rule_name}(s)"))
    }

    pub(super) fn emit_inline_wrap_impl(
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
            // Cast the literal to the union via `unknown` — TypeScript
            // would otherwise narrow the inferred shape on the inline
            // wrap site (`{ tag: name; value: <body-shape> }`) and
            // reject mismatches against the variant declared in the
            // type definitions. The W1.4 TS runtime models grouped /
            // repeated rules as raw spans; the W5 typed-materialiser
            // closes the structural gap.
            let enum_name = &self.enum_name;
            let expr = format!(
                "{v} !== null ? ({{ tag: \"{name}\" as const, value: {v} }} as unknown as {enum_name}) : null"
            );
            TsCode::new(stmts, expr)
        } else {
            body
        }
    }

    pub(super) fn emit_operator_chain_impl(
        &mut self,
        head: TsCode,
        op: TsCode,
        rhs: TsCode,
        _head_type: &TypeDesc,
        _link_elem_type: &TypeDesc,
        _ir: &GrammarIR,
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

    pub(super) fn emit_skip_impl(
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

    pub(super) fn emit_next_impl(
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

    pub(super) fn emit_minus_impl(
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

    pub(super) fn emit_negate_impl(&mut self, inner: TsCode, ctx: &mut TsEmitCtx) -> TsCode {
        let save = ctx.fresh("save");
        let inner_var = ctx.fresh("neg");
        let mut stmts = format!("const {save} = s.offset;\n");
        let inner_expr = inner.dissolve(&mut stmts);
        stmts.push_str(&format!(
            "const {inner_var} = {inner_expr};\ns.offset = {save};\n"
        ));
        TsCode::new(stmts, format!("{inner_var} !== null ? null : {{}}"))
    }
}
