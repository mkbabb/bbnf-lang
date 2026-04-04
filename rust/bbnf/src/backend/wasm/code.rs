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
    pub fn ascii_ws_side_effect() -> String {
        "(block $ws_done (loop $ws_loop \
           (br_if $ws_done (i32.ge_u (local.get $off) (local.get $len))) \
           (br_if $ws_done (i32.and \
             (i32.ne (i32.load8_u (local.get $off)) (i32.const 32)) \
             (i32.and (i32.ne (i32.load8_u (local.get $off)) (i32.const 9)) \
               (i32.and (i32.ne (i32.load8_u (local.get $off)) (i32.const 10)) \
                 (i32.ne (i32.load8_u (local.get $off)) (i32.const 13)))))) \
           (local.set $off (i32.add (local.get $off) (i32.const 1))) \
           (br $ws_loop) \
         )) ".to_string()
    }
}

/// Mutable context for WASM emission.
pub struct WasmEmitCtx {
    counter: usize,
    locals: Vec<String>,
}

impl Default for WasmEmitCtx {
    fn default() -> Self {
        Self { counter: 0, locals: Vec::new() }
    }
}

impl WasmEmitCtx {
    pub fn fresh(&mut self, prefix: &str) -> String {
        let id = self.counter;
        self.counter += 1;
        let name = format!("${prefix}{id}");
        self.locals.push(format!("(local {name} i32)"));
        name
    }

    pub fn drain_locals(&mut self) -> String {
        let locals = self.locals.join(" ");
        self.locals.clear();
        locals
    }
}
