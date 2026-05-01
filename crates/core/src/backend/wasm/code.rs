//! WasmEmitter struct, WasmEmitCtx, and helper functions.

/// WASM emitter producing WAT text.
pub struct WasmEmitter {
    pub module_name: String,
    pub ws_regex_id: Option<usize>,
}

impl WasmEmitter {
    /// Build balanced OR tree testing if `byte_var` equals any of the given bytes.
    pub fn byte_match_condition(byte_var: &str, bytes: &[u8]) -> String {
        assert!(!bytes.is_empty());
        let mut parts: Vec<String> = bytes
            .iter()
            .map(|&b| format!("(i32.eq (local.get {byte_var}) (i32.const {b}))"))
            .collect();
        while parts.len() > 1 {
            let mut next = Vec::with_capacity((parts.len() + 1) / 2);
            let mut i = 0;
            while i + 1 < parts.len() {
                next.push(format!("(i32.or {} {})", parts[i], parts[i + 1]));
                i += 2;
            }
            if i < parts.len() {
                next.push(parts[i].clone());
            }
            parts = next;
        }
        parts.into_iter().next().unwrap()
    }

    /// ASCII whitespace skip as side-effect only.
    pub fn ascii_ws_side_effect(ctx: &mut WasmEmitCtx) -> String {
        let done = ctx.fresh_label("ws_done");
        let lp = ctx.fresh_label("ws_loop");
        format!(
            "(block {done} (loop {lp} \
               (br_if {done} (i32.ge_u (local.get $off) (local.get $len))) \
               (br_if {done} (i32.and \
                 (i32.ne (i32.load8_u (local.get $off)) (i32.const 32)) \
                 (i32.and (i32.ne (i32.load8_u (local.get $off)) (i32.const 9)) \
                   (i32.and (i32.ne (i32.load8_u (local.get $off)) (i32.const 10)) \
                     (i32.ne (i32.load8_u (local.get $off)) (i32.const 13)))))) \
               (local.set $off (i32.add (local.get $off) (i32.const 1))) \
               (br {lp}) \
             )) "
        )
    }

    /// Emit ws-skip WAT as side-effect. Handles comment-aware, regex, or plain ASCII.
    /// Each call generates unique block/loop labels.
    pub fn emit_ws_side_effect(&self, ws_pattern: Option<&str>, ctx: &mut WasmEmitCtx) -> String {
        if let Some(pattern) = ws_pattern {
            if pattern.contains("/*") || pattern.contains(r"\/\*") {
                let ws_done = ctx.fresh_label("ws_done");
                let ws_loop = ctx.fresh_label("ws_loop");
                let c_done = ctx.fresh_label("c_done");
                let c_loop = ctx.fresh_label("c_loop");
                format!(
                    "(block {ws_done} (loop {ws_loop} \
                       (br_if {ws_done} (i32.ge_u (local.get $off) (local.get $len))) \
                       (if (i32.or \
                         (i32.or (i32.eq (i32.load8_u (local.get $off)) (i32.const 32)) (i32.eq (i32.load8_u (local.get $off)) (i32.const 9))) \
                         (i32.or (i32.eq (i32.load8_u (local.get $off)) (i32.const 10)) (i32.eq (i32.load8_u (local.get $off)) (i32.const 13)))) \
                         (then (local.set $off (i32.add (local.get $off) (i32.const 1))) (br {ws_loop}))) \
                       (if (i32.and (i32.lt_u (i32.add (local.get $off) (i32.const 1)) (local.get $len)) \
                         (i32.and (i32.eq (i32.load8_u (local.get $off)) (i32.const 47)) (i32.eq (i32.load8_u (i32.add (local.get $off) (i32.const 1))) (i32.const 42)))) \
                         (then (local.set $off (i32.add (local.get $off) (i32.const 2))) \
                           (block {c_done} (loop {c_loop} \
                             (br_if {c_done} (i32.ge_u (i32.add (local.get $off) (i32.const 1)) (local.get $len))) \
                             (if (i32.and (i32.eq (i32.load8_u (local.get $off)) (i32.const 42)) (i32.eq (i32.load8_u (i32.add (local.get $off) (i32.const 1))) (i32.const 47))) \
                               (then (local.set $off (i32.add (local.get $off) (i32.const 2))) (br {ws_loop}))) \
                             (local.set $off (i32.add (local.get $off) (i32.const 1))) (br {c_loop}) \
                           )) )) \
                     )) "
                )
            } else if let Some(ws_id) = self.ws_regex_id {
                format!(
                    "(local.set $off (call $__match_regex (i32.const {ws_id}) (local.get $off) (local.get $len))) "
                )
            } else {
                Self::ascii_ws_side_effect(ctx)
            }
        } else {
            Self::ascii_ws_side_effect(ctx)
        }
    }
}

/// Mutable context for WASM emission.
#[derive(Default)]
pub struct WasmEmitCtx {
    counter: usize,
    locals: Vec<String>,
}

impl WasmEmitCtx {
    pub fn fresh(&mut self, prefix: &str) -> String {
        let id = self.counter;
        self.counter += 1;
        let name = format!("${prefix}{id}");
        self.locals.push(format!("(local {name} i32)"));
        name
    }

    /// Generate a unique label for block/loop control flow (no local declaration).
    pub fn fresh_label(&mut self, prefix: &str) -> String {
        let id = self.counter;
        self.counter += 1;
        format!("${prefix}{id}")
    }

    pub fn drain_locals(&mut self) -> String {
        let locals = self.locals.join(" ");
        self.locals.clear();
        locals
    }
}
