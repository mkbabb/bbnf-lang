//! Whitespace trimming emission for the WASM backend.

use super::code::{WasmEmitCtx, WasmEmitter};

impl WasmEmitter {
    /// Emit standalone whitespace trim (returns new offset).
    pub(super) fn ws_trim(&mut self, ws_pattern: Option<&str>, ctx: &mut WasmEmitCtx) -> String {
        if let Some(pattern) = ws_pattern {
            if pattern.contains("/*") || pattern.contains(r"\/\*") {
                let ws_done = ctx.fresh_label("ws_done");
                let ws_loop = ctx.fresh_label("ws_loop");
                let c_done = ctx.fresh_label("comment_done");
                let c_loop = ctx.fresh_label("comment_loop");
                format!(
                    "(block {ws_done} (loop {ws_loop} \
                       (br_if {ws_done} (i32.ge_u (local.get $off) (local.get $len))) \
                       (if (i32.or \
                         (i32.or \
                           (i32.eq (i32.load8_u (local.get $off)) (i32.const 32)) \
                           (i32.eq (i32.load8_u (local.get $off)) (i32.const 9))) \
                         (i32.or \
                           (i32.eq (i32.load8_u (local.get $off)) (i32.const 10)) \
                           (i32.eq (i32.load8_u (local.get $off)) (i32.const 13)))) \
                         (then (local.set $off (i32.add (local.get $off) (i32.const 1))) (br {ws_loop}))) \
                       (if (i32.and \
                         (i32.lt_u (i32.add (local.get $off) (i32.const 1)) (local.get $len)) \
                         (i32.and \
                           (i32.eq (i32.load8_u (local.get $off)) (i32.const 47)) \
                           (i32.eq (i32.load8_u (i32.add (local.get $off) (i32.const 1))) (i32.const 42)))) \
                         (then \
                           (local.set $off (i32.add (local.get $off) (i32.const 2))) \
                           (block {c_done} (loop {c_loop} \
                             (br_if {c_done} (i32.ge_u (i32.add (local.get $off) (i32.const 1)) (local.get $len))) \
                             (if (i32.and \
                               (i32.eq (i32.load8_u (local.get $off)) (i32.const 42)) \
                               (i32.eq (i32.load8_u (i32.add (local.get $off) (i32.const 1))) (i32.const 47))) \
                               (then (local.set $off (i32.add (local.get $off) (i32.const 2))) (br {ws_loop}))) \
                             (local.set $off (i32.add (local.get $off) (i32.const 1))) \
                             (br {c_loop}) \
                           )) \
                         )) \
                     )) \
                     (local.get $off)"
                )
            } else if let Some(ws_id) = self.ws_regex_id {
                format!(
                    "(call $__match_regex (i32.const {ws_id}) (local.get $off) (local.get $len))"
                )
            } else {
                self.ws_trim(None, ctx)
            }
        } else {
            let ws_done = ctx.fresh_label("ws_done");
            let ws_loop = ctx.fresh_label("ws_loop");
            format!(
                "(block {ws_done} (loop {ws_loop} \
                   (br_if {ws_done} (i32.ge_u (local.get $off) (local.get $len))) \
                   (br_if {ws_done} (i32.and \
                     (i32.ne (i32.load8_u (local.get $off)) (i32.const 32)) \
                     (i32.and \
                       (i32.ne (i32.load8_u (local.get $off)) (i32.const 9)) \
                       (i32.and \
                         (i32.ne (i32.load8_u (local.get $off)) (i32.const 10)) \
                         (i32.ne (i32.load8_u (local.get $off)) (i32.const 13)))))) \
                   (local.set $off (i32.add (local.get $off) (i32.const 1))) \
                   (br {ws_loop}) \
                 )) \
                 (local.get $off)"
            )
        }
    }

    /// Emit ws-trimmed inner expression (ws before, inner, ws after).
    pub(super) fn with_ws_trim(
        &mut self,
        inner: String,
        ws_pattern: Option<&str>,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        let result = ctx.fresh("ws_inner");
        let ws_before = self.emit_ws_side_effect(ws_pattern, ctx);
        let ws_after = self.emit_ws_side_effect(ws_pattern, ctx);
        format!(
            "{ws_before}\
             (local.set {result} {inner}) \
             (if (result i32) (i32.eq (local.get {result}) (i32.const -1)) \
               (then (i32.const -1)) \
               (else \
                 (local.set $off (local.get {result})) \
                 {ws_after}\
                 (local.get $off)))"
        )
    }
}
