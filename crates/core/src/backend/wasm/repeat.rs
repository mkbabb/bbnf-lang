//! Repetition emission helpers for the WASM backend.

use bbnf_ir::TypeDesc;

use crate::backend::{ValuePlacement, SepByConfig};

use super::code::{WasmEmitCtx, WasmEmitter};

impl WasmEmitter {
    /// Emit a many/plus repetition loop.
    pub(super) fn repeat_many(
        &mut self,
        body: String,
        lo: u32,
        _hi: u32,
        _elem_type: &TypeDesc,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        let count = ctx.fresh("rep_count");
        let prev = ctx.fresh("rep_prev");
        let result = ctx.fresh("rep_result");
        let start = ctx.fresh("rep_start");
        let exit = ctx.fresh_label("rep_exit");
        let lp = ctx.fresh_label("rep_loop");
        format!(
            "(local.set {start} (local.get $off)) \
             (local.set {count} (i32.const 0)) \
             (block {exit} (loop {lp} \
               (local.set {prev} (local.get $off)) \
               (local.set {result} {body}) \
               (br_if {exit} (i32.eq (local.get {result}) (i32.const -1))) \
               (local.set $off (local.get {result})) \
               (local.set {count} (i32.add (local.get {count}) (i32.const 1))) \
               (br_if {exit} (i32.eq (local.get $off) (local.get {prev}))) \
               (br {lp}) \
             )) \
             (if (result i32) (i32.ge_u (local.get {count}) (i32.const {lo})) \
               (then (local.get $off)) \
               (else (i32.const -1)))"
        )
    }

    /// Emit an optional (?) wrapper.
    pub(super) fn repeat_optional(
        &mut self,
        body: String,
        _inner_type: &TypeDesc,
        _alloc: ValuePlacement,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        let save = ctx.fresh("opt_save");
        let result = ctx.fresh("opt_result");
        format!(
            "(local.set {save} (local.get $off)) \
             (local.set {result} {body}) \
             (if (result i32) (i32.ne (local.get {result}) (i32.const -1)) \
               (then (local.get {result})) \
               (else (local.get {save})))"
        )
    }

    /// Emit a separated-by loop.
    pub(super) fn sep_by(
        &mut self,
        element: String,
        separator: String,
        config: &SepByConfig,
        _elem_type: &TypeDesc,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        let count = ctx.fresh("sep_count");
        let save = ctx.fresh("sep_save");
        let result = ctx.fresh("sep_result");
        let lo = config.lo;
        let exit = ctx.fresh_label("sep_exit");
        let lp = ctx.fresh_label("sep_loop");

        // Terminator byte early-exit check.
        let terminator_check = if let Some(ref tb) = config.terminator_bytes {
            if tb.len() == 1 {
                let byte = tb[0];
                format!(
                    "(br_if {exit} (i32.and \
                       (i32.lt_u (local.get $off) (local.get $len)) \
                       (i32.eq (i32.load8_u (local.get $off)) (i32.const {byte})))) "
                )
            } else {
                String::new()
            }
        } else {
            String::new()
        };

        format!(
            "(local.set {count} (i32.const 0)) \
             (local.set {result} {element}) \
             (if (i32.ne (local.get {result}) (i32.const -1)) \
               (then \
                 (local.set $off (local.get {result})) \
                 (local.set {count} (i32.const 1)) \
                 (block {exit} (loop {lp} \
                   {terminator_check}\
                   (local.set {save} (local.get $off)) \
                   (local.set {result} {separator}) \
                   (br_if {exit} (i32.eq (local.get {result}) (i32.const -1))) \
                   (local.set $off (local.get {result})) \
                   (local.set {result} {element}) \
                   (if (i32.eq (local.get {result}) (i32.const -1)) \
                     (then (local.set $off (local.get {save})) (br {exit}))) \
                   (local.set $off (local.get {result})) \
                   (local.set {count} (i32.add (local.get {count}) (i32.const 1))) \
                   (br {lp}) \
                 )) \
               )) \
             (if (result i32) (i32.ge_u (local.get {count}) (i32.const {lo})) \
               (then (local.get $off)) \
               (else (i32.const -1)))"
        )
    }
}
