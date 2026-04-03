//! WASM Emitter: implements [`Emitter`] to produce WAT (WebAssembly Text) source.
//!
//! Each grammar rule becomes a WASM function:
//! ```wat
//! (func $__ruleName (param $off i32) (param $len i32) (result i32)
//!   ;; body — returns new offset on success, -1 on failure
//! )
//! ```
//!
//! Linear memory layout: `[input bytes...]` starting at address 0.
//! The caller writes input bytes to memory before invoking the parser.

use bbnf_ir::{AltDispatch, GrammarIR, IrRule, RuleId, TypeDesc};

use crate::backend::analysis::BackendAnalysis;
use crate::backend::{
    AllocStrategy, AltBranchInfo, Emitter, FlattenStrategy, SepByConfig, SeqChildGroup,
};

// ─── WASM Emitter ───────────────────────────────────────────────────────────

/// WASM emitter producing WAT text.
///
/// Generated functions operate on linear memory:
/// - Input bytes at memory offset 0
/// - Functions take `(off: i32, len: i32)` and return `i32` (new offset or -1)
/// - All values are Span-like: `(start: i32, end: i32)` packed as `(end << 16) | start`
///   or just the end offset for simple span tracking
pub struct WasmEmitter {
    /// Module name for the WASM output.
    pub module_name: String,
    /// Regex ID for the custom @ws pattern (set on first emit_ws_trim call).
    pub ws_regex_id: Option<usize>,
}

/// Mutable context for WASM emission.
pub struct WasmEmitCtx {
    /// Local variable counter for unique names.
    counter: usize,
    /// Accumulated local declarations for the current function.
    locals: Vec<String>,
}

impl Default for WasmEmitCtx {
    fn default() -> Self {
        Self {
            counter: 0,
            locals: Vec::new(),
        }
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

    /// Reset locals for a new function body.
    pub fn reset_locals(&mut self) {
        self.locals.clear();
        self.counter = 0;
    }

    /// Drain accumulated locals as WAT declarations.
    pub fn drain_locals(&mut self) -> String {
        let locals = self.locals.join(" ");
        self.locals.clear();
        locals
    }
}

// ─── Helpers ────────────────────────────────────────────────────────────────

fn unescape_literal(s: &str) -> String {
    crate::backend::rust::unescape_literal(s)
}

// ─── Emitter Trait Impl ─────────────────────────────────────────────────────

impl Emitter for WasmEmitter {
    type Output = String;
    type Ctx = WasmEmitCtx;

    // ── Leaves ──────────────────────────────────────────────────────────

    fn emit_literal_match(
        &mut self,
        value: &str,
        guaranteed_byte: Option<u8>,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        let unescaped = unescape_literal(value);
        let bytes = unescaped.as_bytes();

        if let Some(_byte) = guaranteed_byte {
            // Guaranteed match — just advance offset by 1.
            return "(i32.add (local.get $off) (i32.const 1))".to_string();
        }

        if bytes.len() == 1 {
            let byte = bytes[0];
            let fail = ctx.fresh("lit_fail");
            format!(
                "(if (result i32) (i32.and \
                   (i32.lt_u (local.get $off) (local.get $len)) \
                   (i32.eq (i32.load8_u (local.get $off)) (i32.const {byte}))) \
                 (then (i32.add (local.get $off) (i32.const 1))) \
                 (else (i32.const -1)))"
            )
        } else {
            // Multi-byte: check each byte sequentially.
            let len = bytes.len();
            let checks: Vec<String> = bytes
                .iter()
                .enumerate()
                .map(|(i, &b)| {
                    format!(
                        "(i32.eq (i32.load8_u (i32.add (local.get $off) (i32.const {i}))) (i32.const {b}))"
                    )
                })
                .collect();

            // AND all checks together.
            let mut condition = format!(
                "(i32.le_u (i32.add (local.get $off) (i32.const {len})) (local.get $len))"
            );
            for check in checks {
                condition = format!("(i32.and {condition} {check})");
            }

            format!(
                "(if (result i32) {condition} \
                 (then (i32.add (local.get $off) (i32.const {len}))) \
                 (else (i32.const -1)))"
            )
        }
    }

    fn emit_regex_match(
        &mut self,
        _pattern: &str,
        regex_id: usize,
        _ir: &GrammarIR,
        _ctx: &mut WasmEmitCtx,
    ) -> String {
        // Use driver-assigned regex_id as host function argument.
        format!("(call $__match_regex (i32.const {regex_id}) (local.get $off) (local.get $len))")
    }

    fn emit_epsilon(&mut self, _ctx: &mut WasmEmitCtx) -> String {
        "(local.get $off)".to_string()
    }

    // ── Sequences ───────────────────────────────────────────────────────

    fn emit_seq_all_span(
        &mut self,
        child_outputs: Vec<String>,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        // Sequential: each child advances offset, fail if any returns -1.
        let mut body = String::new();
        for child in &child_outputs {
            let tmp = ctx.fresh("seq");
            body.push_str(&format!(
                "(local.set {tmp} {child}) \
                 (if (i32.eq (local.get {tmp}) (i32.const -1)) (then (return (i32.const -1)))) \
                 (local.set $off (local.get {tmp})) "
            ));
        }
        body.push_str("(local.get $off)");
        body
    }

    fn emit_seq_grouped(
        &mut self,
        groups: Vec<SeqChildGroup<String>>,
        _result_type: &TypeDesc,
        _flatten: Option<FlattenStrategy>,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        // In WASM span mode, all values are offsets. Group as sequential.
        let mut all_outputs = Vec::new();
        for group in groups {
            match group {
                SeqChildGroup::Single { output, .. } => all_outputs.push(output),
                SeqChildGroup::SpanCompressed { outputs } => all_outputs.extend(outputs),
            }
        }
        self.emit_seq_all_span(all_outputs, ctx)
    }

    // ── Alternations ────────────────────────────────────────────────────

    fn emit_alt_dispatch(
        &mut self,
        table: &AltDispatch,
        branches: Vec<(AltBranchInfo, String)>,
        fallback: Option<(AltBranchInfo, String)>,
        _alloc: AllocStrategy,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        // Build a 128-entry br_table: byte → branch block index.
        // WASM blocks close innermost-first, so the first opened block
        // is the OUTERMOST and gets the HIGHEST branch index in br_table.
        //
        // Layout:
        //   (block $exit         ;; outermost — final result
        //     (block $fb         ;; fallback target
        //       (block $b{n-1}   ;; last branch
        //         ...
        //           (block $b0   ;; first branch (innermost)
        //             br_table [mapping] $fb
        //           ) ;; end $b0
        //           <branch 0 body>
        //         ) ;; end $b1
        //         <branch 1 body>
        //       ...
        //     ) ;; end $fb
        //     <fallback body>
        //   ) ;; end $exit

        let save = ctx.fresh("dt_save");
        let result = ctx.fresh("dt_result");
        let n = branches.len();

        // Build br_table entries: table[byte] → block depth to break to.
        // Block $b0 is innermost (depth 0), $b{n-1} is depth n-1, $fb is depth n.
        let mut br_entries = vec![n as u32; 128]; // default → fallback
        for (branch_idx, _) in branches.iter().enumerate() {
            for (byte_val, &mapped) in table.table.iter().enumerate() {
                if mapped as usize == branch_idx {
                    br_entries[byte_val] = branch_idx as u32;
                }
            }
        }

        // Format br_table target list.
        let br_targets: String = br_entries.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(" ");

        // Open blocks: outermost $exit, then $fb, then $b{n-1} .. $b0 (innermost).
        let mut body = format!("(local.set {save} (local.get $off)) ");
        body.push_str("(block $dt_exit ");
        body.push_str("(block $dt_fb ");
        for i in (0..n).rev() {
            body.push_str(&format!("(block $dt_b{i} "));
        }

        // br_table instruction inside innermost block.
        body.push_str(&format!(
            "(br_table {br_targets} $dt_fb (i32.load8_u (local.get {save})))"
        ));

        // Close blocks and emit branch bodies (innermost first = branch 0 first).
        for (i, (_info, branch_body)) in branches.iter().enumerate() {
            body.push_str(&format!(
                ") \
                 (local.set $off (local.get {save})) \
                 (local.set {result} {branch_body}) \
                 (br_if $dt_exit (i32.ne (local.get {result}) (i32.const -1))) "
            ));
        }

        // Fallback block.
        let fb_body = fallback
            .as_ref()
            .map(|(_, b)| b.as_str())
            .unwrap_or("(i32.const -1)");
        body.push_str(&format!(
            ") \
             (local.set $off (local.get {save})) \
             (local.set {result} {fb_body}) "
        ));

        // Exit block — return result.
        body.push_str(&format!(") (local.get {result})"));
        body
    }

    fn emit_alt_checkpoint(
        &mut self,
        branches: Vec<(AltBranchInfo, String)>,
        _alloc: AllocStrategy,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        if branches.len() == 1 {
            return branches.into_iter().next().unwrap().1;
        }

        let save = ctx.fresh("alt_save");
        let result = ctx.fresh("alt_result");
        let mut body = format!("(local.set {save} (local.get $off)) ");

        for (_info, branch) in &branches {
            body.push_str(&format!(
                "(local.set $off (local.get {save})) \
                 (local.set {result} {branch}) \
                 (if (i32.ne (local.get {result}) (i32.const -1)) \
                   (then (return (local.get {result})))) "
            ));
        }

        body.push_str("(i32.const -1)");
        body
    }

    fn emit_alt_all_literal(
        &mut self,
        literals: Vec<(String, String)>,
        alloc: AllocStrategy,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        self.emit_alt_checkpoint(
            literals
                .into_iter()
                .map(|(_, body)| {
                    (
                        AltBranchInfo {
                            ty: TypeDesc::Span,
                            coercion_variant: None,
                        },
                        body,
                    )
                })
                .collect(),
            alloc,
            ctx,
        )
    }

    // ── Repetition ──────────────────────────────────────────────────────

    fn emit_repeat_many(
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
        format!(
            "(local.set {start} (local.get $off)) \
             (local.set {count} (i32.const 0)) \
             (block $rep_exit (loop $rep_loop \
               (local.set {prev} (local.get $off)) \
               (local.set {result} {body}) \
               (br_if $rep_exit (i32.eq (local.get {result}) (i32.const -1))) \
               (local.set $off (local.get {result})) \
               (local.set {count} (i32.add (local.get {count}) (i32.const 1))) \
               (br_if $rep_exit (i32.eq (local.get $off) (local.get {prev}))) \
               (br $rep_loop) \
             )) \
             (if (result i32) (i32.ge_u (local.get {count}) (i32.const {lo})) \
               (then (local.get $off)) \
               (else (i32.const -1)))"
        )
    }

    fn emit_repeat_optional(
        &mut self,
        body: String,
        _inner_type: &TypeDesc,
        _alloc: AllocStrategy,
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

    fn emit_sep_by(
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
        format!(
            "(local.set {count} (i32.const 0)) \
             (local.set {result} {element}) \
             (if (i32.ne (local.get {result}) (i32.const -1)) \
               (then \
                 (local.set $off (local.get {result})) \
                 (local.set {count} (i32.const 1)) \
                 (block $sep_exit (loop $sep_loop \
                   (local.set {save} (local.get $off)) \
                   (local.set {result} {separator}) \
                   (br_if $sep_exit (i32.eq (local.get {result}) (i32.const -1))) \
                   (local.set $off (local.get {result})) \
                   (local.set {result} {element}) \
                   (if (i32.eq (local.get {result}) (i32.const -1)) \
                     (then (local.set $off (local.get {save})) (br $sep_exit))) \
                   (local.set $off (local.get {result})) \
                   (local.set {count} (i32.add (local.get {count}) (i32.const 1))) \
                   (br $sep_loop) \
                 )) \
               )) \
             (if (result i32) (i32.ge_u (local.get {count}) (i32.const {lo})) \
               (then (local.get $off)) \
               (else (i32.const -1)))"
        )
    }

    // ── References ──────────────────────────────────────────────────────

    fn emit_call(
        &mut self,
        _rule_id: RuleId,
        rule_name: &str,
        _alloc: AllocStrategy,
        _ctx: &mut WasmEmitCtx,
    ) -> String {
        format!("(call $__{rule_name} (local.get $off) (local.get $len))")
    }

    fn emit_inline_wrap(
        &mut self,
        body: String,
        _variant_name: Option<&str>,
        _alloc: AllocStrategy,
        _ctx: &mut WasmEmitCtx,
    ) -> String {
        // WASM uses offset-based values — no variant wrapping needed.
        body
    }

    // ── Operator chains ──────────────────────────────────────────────────

    fn emit_operator_chain(
        &mut self,
        head: String,
        op: String,
        rhs: String,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        let head_var = ctx.fresh("oc_head");
        let cp = ctx.fresh("oc_cp");
        let op_var = ctx.fresh("oc_op");
        let rhs_var = ctx.fresh("oc_rhs");
        format!(
            "(local.set {head_var} {head}) \
             (if (i32.eq (local.get {head_var}) (i32.const -1)) (then (return (i32.const -1)))) \
             (local.set $off (local.get {head_var})) \
             (block $oc_exit (loop $oc_loop \
               (local.set {cp} (local.get $off)) \
               (local.set {op_var} {op}) \
               (br_if $oc_exit (i32.eq (local.get {op_var}) (i32.const -1))) \
               (local.set $off (local.get {op_var})) \
               (local.set {rhs_var} {rhs}) \
               (if (i32.eq (local.get {rhs_var}) (i32.const -1)) \
                 (then (local.set $off (local.get {cp})) (br $oc_exit))) \
               (local.set $off (local.get {rhs_var})) \
               (br $oc_loop) \
             )) \
             (local.get $off)"
        )
    }

    // ── Binary operators ────────────────────────────────────────────────

    fn emit_skip(
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

    fn emit_next(
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

    fn emit_minus(
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

    fn emit_negate(
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

    // ── Value manipulation ──────────────────────────────────────────────

    fn emit_enum_wrap(
        &mut self,
        inner: String,
        _variant_name: &str,
        _alloc: AllocStrategy,
        _ctx: &mut WasmEmitCtx,
    ) -> String {
        // WASM: no enum wrapping — offset-based values.
        inner
    }

    fn emit_number_convert(&mut self, _ctx: &mut WasmEmitCtx) -> String {
        // Delegate to imported host function.
        "(call $__number_convert (local.get $off) (local.get $len))".to_string()
    }

    fn emit_constant(
        &mut self,
        discard_inner: String,
        _value: &str,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        let result = ctx.fresh("const_r");
        format!(
            "(local.set {result} {discard_inner}) \
             (if (result i32) (i32.ne (local.get {result}) (i32.const -1)) \
               (then (local.get {result})) \
               (else (i32.const -1)))"
        )
    }

    fn emit_ws_trim(
        &mut self,
        ws_pattern: Option<&str>,
        _ctx: &mut WasmEmitCtx,
    ) -> String {
        if let Some(ws_id) = self.ws_regex_id {
            // Custom @ws: call host regex with the registered pattern ID.
            format!("(call $__match_regex (i32.const {ws_id}) (local.get $off) (local.get $len))")
        } else {
            "(block $ws_done (loop $ws_loop \
               (br_if $ws_done (i32.ge_u (local.get $off) (local.get $len))) \
               (br_if $ws_done (i32.and \
                 (i32.ne (i32.load8_u (local.get $off)) (i32.const 32)) \
                 (i32.and \
                   (i32.ne (i32.load8_u (local.get $off)) (i32.const 9)) \
                   (i32.and \
                     (i32.ne (i32.load8_u (local.get $off)) (i32.const 10)) \
                     (i32.ne (i32.load8_u (local.get $off)) (i32.const 13)))))) \
               (local.set $off (i32.add (local.get $off) (i32.const 1))) \
               (br $ws_loop) \
             )) \
             (local.get $off)"
                .to_string()
        }
    }

    fn emit_with_ws_trim(
        &mut self,
        inner: String,
        ws_pattern: Option<&str>,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        let result = ctx.fresh("ws_inner");
        let ws_call = self.emit_ws_trim(ws_pattern, ctx);
        // For custom ws: the host regex returns new offset; update $off.
        let ws_block = if ws_pattern.is_some() {
            format!("(local.set $off {ws_call}) ")
        } else {
            // ASCII ws loop already updates $off in-place.
            format!("{ws_call} ")
        };
        let ws_loop = ws_block.clone();
        format!(
            "{ws_loop} \
             (local.set {result} {inner}) \
             (if (i32.eq (local.get {result}) (i32.const -1)) (then (return (i32.const -1)))) \
             (local.set $off (local.get {result})) \
             {ws_loop} \
             (local.get $off)"
        )
    }

    // ── Rule-level emission ─────────────────────────────────────────────

    fn emit_rule_function(
        &mut self,
        rule: &IrRule,
        body: String,
        ir: &GrammarIR,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        let name = ir.get_string(rule.name);
        let locals = ctx.drain_locals();
        format!(
            "  (func $__{name} (param $off i32) (param $len i32) (result i32)\n    \
             {locals}\n    \
             {body}\n  )\n"
        )
    }

    fn emit_type_definitions(
        &mut self,
        _ir: &GrammarIR,
        _analysis: &BackendAnalysis,
        _ctx: &mut WasmEmitCtx,
    ) -> String {
        // WASM module header: imports MUST come before all other definitions.
        // match_regex takes (pattern_id: i32, offset: i32, input_len: i32) → new_offset or -1.
        "  ;; Host imports: our DFA regex engine + number scanner\n  \
         (import \"host\" \"match_regex\" (func $__match_regex (param i32 i32 i32) (result i32)))\n  \
         (import \"host\" \"number_convert\" (func $__number_convert (param i32 i32) (result i32)))\n  \
         (memory (export \"memory\") 1)\n"
            .to_string()
    }

    fn emit_grammar(
        &mut self,
        type_defs: String,
        rule_functions: Vec<String>,
        ir: &GrammarIR,
        _ctx: &mut WasmEmitCtx,
    ) -> String {
        let entry_name = ir.get_string(ir.rules[ir.entry as usize].name);
        let module_name = &self.module_name;

        let mut output = format!(
            ";; Generated by BBNF — do not edit.\n(module ${module_name}\n"
        );
        output.push_str(&type_defs);
        output.push('\n');
        for func in &rule_functions {
            output.push_str(func);
        }
        // Export entry rule.
        output.push_str(&format!(
            "\n  (export \"parse\" (func $__{entry_name}))\n"
        ));
        output.push_str(")\n");
        output
    }
}
