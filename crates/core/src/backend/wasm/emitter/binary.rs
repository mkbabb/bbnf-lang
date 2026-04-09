//! Binary / call / operator-chain emission for the WASM backend.
//!
//! Each method is `pub(super)` so the trait impl in `mod.rs` can delegate
//! to it via `self.emit_xxx_impl(...)`.

use bbnf_ir::{GrammarIR, RuleId, TypeDesc};

use super::{WasmEmitCtx, WasmEmitter};
use crate::backend::ValuePlacement;

impl WasmEmitter {
    pub(super) fn emit_call_impl(
        &mut self,
        _rule_id: RuleId,
        rule_name: &str,
        _alloc: ValuePlacement,
        _ctx: &mut WasmEmitCtx,
    ) -> String {
        format!("(call $__{rule_name} (local.get $off) (local.get $len))")
    }

    pub(super) fn emit_inline_wrap_impl(
        &mut self,
        body: String,
        _variant_name: Option<&str>,
        _alloc: ValuePlacement,
        _ctx: &mut WasmEmitCtx,
    ) -> String {
        body
    }

    pub(super) fn emit_operator_chain_impl(
        &mut self,
        head: String,
        op: String,
        rhs: String,
        _head_type: &TypeDesc,
        _link_elem_type: &TypeDesc,
        _ir: &GrammarIR,
        ctx: &mut WasmEmitCtx,
    ) -> Option<String> {
        let head_var = ctx.fresh("oc_head");
        let cp = ctx.fresh("oc_cp");
        let op_var = ctx.fresh("oc_op");
        let rhs_var = ctx.fresh("oc_rhs");
        let exit = ctx.fresh_label("oc_exit");
        let lp = ctx.fresh_label("oc_loop");
        Some(format!(
            "(local.set {head_var} {head}) \
             (if (result i32) (i32.eq (local.get {head_var}) (i32.const -1)) (then (i32.const -1)) (else \
             (local.set $off (local.get {head_var})) \
             (block {exit} (loop {lp} \
               (local.set {cp} (local.get $off)) \
               (local.set {op_var} {op}) \
               (br_if {exit} (i32.eq (local.get {op_var}) (i32.const -1))) \
               (local.set $off (local.get {op_var})) \
               (local.set {rhs_var} {rhs}) \
               (if (i32.eq (local.get {rhs_var}) (i32.const -1)) \
                 (then (local.set $off (local.get {cp})) (br {exit}))) \
               (local.set $off (local.get {rhs_var})) \
               (br {lp}) \
             )) \
             (local.get $off) ))"
        ))
    }

    pub(super) fn emit_skip_impl(
        &mut self,
        kept: String,
        discarded: String,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        let kept_var = ctx.fresh("skip_kept");
        let disc_var = ctx.fresh("skip_disc");
        format!(
            "(local.set {kept_var} {kept}) \
             (if (result i32) (i32.ne (local.get {kept_var}) (i32.const -1)) \
               (then \
                 (local.set $off (local.get {kept_var})) \
                 (local.set {disc_var} {discarded}) \
                 (if (result i32) (i32.ne (local.get {disc_var}) (i32.const -1)) \
                   (then (local.get {disc_var})) \
                   (else (i32.const -1)))) \
               (else (i32.const -1)))"
        )
    }

    pub(super) fn emit_next_impl(
        &mut self,
        discarded: String,
        kept: String,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        let disc_var = ctx.fresh("next_disc");
        format!(
            "(local.set {disc_var} {discarded}) \
             (if (result i32) (i32.ne (local.get {disc_var}) (i32.const -1)) \
               (then (local.set $off (local.get {disc_var})) {kept}) \
               (else (i32.const -1)))"
        )
    }

    pub(super) fn emit_minus_impl(
        &mut self,
        lhs: String,
        rhs: String,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        let save = ctx.fresh("minus_save");
        let excluded = ctx.fresh("minus_excl");
        format!(
            "(local.set {save} (local.get $off)) \
             (local.set {excluded} {rhs}) \
             (local.set $off (local.get {save})) \
             (if (result i32) (i32.ne (local.get {excluded}) (i32.const -1)) \
               (then (i32.const -1)) \
               (else {lhs}))"
        )
    }

    pub(super) fn emit_negate_impl(
        &mut self,
        inner: String,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        let save = ctx.fresh("neg_save");
        let result = ctx.fresh("neg_result");
        format!(
            "(local.set {save} (local.get $off)) \
             (local.set {result} {inner}) \
             (local.set $off (local.get {save})) \
             (if (result i32) (i32.ne (local.get {result}) (i32.const -1)) \
               (then (i32.const -1)) \
               (else (local.get $off)))"
        )
    }
}
