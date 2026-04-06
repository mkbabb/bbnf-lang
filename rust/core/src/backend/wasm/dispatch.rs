//! Token dispatch and delimiter scan emission for the WASM backend.

use crate::backend::{DelimScanConfig, TokenDispatchArmCompiled};

use super::code::{WasmEmitCtx, WasmEmitter};

impl WasmEmitter {
    /// Emit a token dispatch: match a token span against known byte patterns.
    pub(super) fn token_dispatch(
        &mut self,
        token: String,
        arms: Vec<TokenDispatchArmCompiled<String>>,
        fallback: String,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        let save = ctx.fresh("td_save");
        let tok = ctx.fresh("td_tok");
        let result = ctx.fresh("td_result");
        let td_len = ctx.fresh("td_len");
        let mut body = format!(
            "(local.set {save} (local.get $off)) \
             (local.set {tok} {token}) \
             (local.set {result} (i32.const -1)) "
        );
        body.push_str(&format!(
            "(if (i32.ne (local.get {tok}) (i32.const -1)) (then \
             (local.set {td_len} (i32.sub (local.get {tok}) (local.get {save}))) \
             (local.set $off (local.get {tok})) "
        ));
        for arm in &arms {
            for pat in &arm.patterns {
                let len = pat.len();
                let mut cond = format!("(i32.eq (local.get {td_len}) (i32.const {len}))");
                for (i, &b) in pat.iter().enumerate() {
                    cond = format!(
                        "(i32.and {cond} (i32.eq (i32.load8_u (i32.add (local.get {save}) \
                         (i32.const {i}))) (i32.const {b})))"
                    );
                }
                if let Some(guard) = arm.guard_byte {
                    cond = format!(
                        "(i32.and {cond} (i32.and \
                           (i32.lt_u (local.get $off) (local.get $len)) \
                           (i32.eq (i32.load8_u (local.get $off)) (i32.const {guard}))))"
                    );
                }
                let cont = &arm.continuation;
                body.push_str(&format!(
                    "(if (i32.and (i32.eq (local.get {result}) (i32.const -1)) {cond}) \
                       (then (local.set {result} {cont}))) "
                ));
            }
        }
        body.push_str(")) ");
        body.push_str(&format!(
            "(if (i32.eq (local.get {result}) (i32.const -1)) (then \
               (local.set $off (local.get {save})) \
               (local.set {result} {fallback}))) \
             (local.get {result})"
        ));
        body
    }

    /// Emit a delimiter-driven flat scan loop.
    pub(super) fn delim_scan(
        &mut self,
        config: &DelimScanConfig,
        ctx: &mut WasmEmitCtx,
    ) -> Option<String> {
        let start = ctx.fresh("ds_start");
        let result = ctx.fresh("ds_result");
        let byte_var = ctx.fresh("ds_byte");
        let open = config.open_byte;
        let close = config.close_byte;
        let pivot = config.pivot_byte;
        let block_call = config
            .block_rule
            .as_ref()
            .map(|(_, name)| format!("(call $__{name} (local.get $off) (local.get $len))"))
            .unwrap_or_else(|| "(i32.const -1)".to_string());
        let pivot_call = config
            .pivot_rule
            .as_ref()
            .map(|(_, name)| format!("(call $__{name} (local.get $off) (local.get $len))"))
            .unwrap_or_else(|| "(i32.const -1)".to_string());
        let trail_consume = if let Some(tb) = config.trail_byte {
            format!(
                "(if (i32.and (i32.lt_u (local.get $off) (local.get $len)) \
                    (i32.eq (i32.load8_u (local.get $off)) (i32.const {tb}))) \
                   (then (local.set $off (i32.add (local.get $off) (i32.const 1))))) "
            )
        } else {
            String::new()
        };
        let ds_exit = ctx.fresh_label("ds_exit");
        let ds_loop = ctx.fresh_label("ds_loop");
        let piv_scan = ctx.fresh_label("piv_scan");
        let piv_loop = ctx.fresh_label("piv_loop");
        Some(format!(
            "(local.set {start} (local.get $off)) \
             (local.set {result} (i32.const -1)) \
             (if (i32.and (i32.lt_u (local.get $off) (local.get $len)) \
                  (i32.eq (i32.load8_u (local.get $off)) (i32.const {open}))) \
               (then \
                 (local.set $off (i32.add (local.get $off) (i32.const 1))) \
                 (block {ds_exit} (loop {ds_loop} \
                   (br_if {ds_exit} (i32.ge_u (local.get $off) (local.get $len))) \
                   (local.set {byte_var} (i32.load8_u (local.get $off))) \
                   (if (i32.eq (local.get {byte_var}) (i32.const {close})) (then \
                     (local.set $off (i32.add (local.get $off) (i32.const 1))) \
                     (local.set {result} (local.get $off)) \
                     (br {ds_exit}))) \
                   (if (i32.eq (local.get {byte_var}) (i32.const {open})) (then \
                     (local.set {result} {block_call}) \
                     (br_if {ds_exit} (i32.eq (local.get {result}) (i32.const -1))) \
                     (local.set $off (local.get {result})) \
                     (local.set {result} (i32.const -1)) \
                     (br {ds_loop}))) \
                   (block {piv_scan} (loop {piv_loop} \
                     (br_if {piv_scan} (i32.ge_u (local.get $off) (local.get $len))) \
                     (if (i32.eq (i32.load8_u (local.get $off)) (i32.const {pivot})) (then \
                       (local.set $off (i32.add (local.get $off) (i32.const 1))) \
                       {trail_consume}\
                       (local.set {result} {pivot_call}) \
                       (br_if {ds_exit} (i32.eq (local.get {result}) (i32.const -1))) \
                       (local.set $off (local.get {result})) \
                       (local.set {result} (i32.const -1)) \
                       (br {ds_loop}))) \
                     (if (i32.or (i32.eq (i32.load8_u (local.get $off)) (i32.const {close})) \
                                 (i32.eq (i32.load8_u (local.get $off)) (i32.const {open}))) \
                       (then (br {piv_scan}))) \
                     (local.set $off (i32.add (local.get $off) (i32.const 1))) \
                     (br {piv_loop}) \
                   )) \
                   (br {ds_exit}) \
                 )) \
               )) \
             (if (i32.eq (local.get {result}) (i32.const -1)) (then \
               (local.set $off (local.get {start})))) \
             (local.get {result})"
        ))
    }
}
