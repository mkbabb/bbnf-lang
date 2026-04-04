//! Emitter trait implementation for the WASM backend.

use bbnf_ir::{AltDispatch, GrammarIR, IrRule, RuleId, TypeDesc};

use crate::backend::analysis::BackendAnalysis;
use crate::backend::key_dispatch::KeyDispatchConfig;
use crate::backend::{
    AllocStrategy, AltBranchInfo, DelimScanConfig, Emitter, FlattenStrategy, KeyDispatchBranch,
    SepByConfig, SeqChildGroup, TokenDispatchArmCompiled,
};

pub use super::code::{WasmEmitCtx, WasmEmitter};
pub use super::helpers::unescape_literal;

// ─── Emitter Trait Impl ─────────────────────────────────────────────────────

impl Emitter for WasmEmitter {
    type Output = String;
    type Ctx = WasmEmitCtx;

    // ── Leaves ──────────────────────────────────────────────────────────

    fn emit_literal_match(
        &mut self,
        value: &str,
        guaranteed_byte: Option<u8>,
        _ctx: &mut WasmEmitCtx,
    ) -> String {
        let unescaped = unescape_literal(value);
        let bytes = unescaped.as_bytes();

        if let Some(_byte) = guaranteed_byte {
            return "(i32.add (local.get $off) (i32.const 1))".to_string();
        }

        if bytes.len() == 1 {
            let byte = bytes[0];
            format!(
                "(if (result i32) (i32.and \
                   (i32.lt_u (local.get $off) (local.get $len)) \
                   (i32.eq (i32.load8_u (local.get $off)) (i32.const {byte}))) \
                 (then (i32.add (local.get $off) (i32.const 1))) \
                 (else (i32.const -1)))"
            )
        } else {
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
        if child_outputs.is_empty() {
            return "(local.get $off)".to_string();
        }
        let result = ctx.fresh("seq_ok");
        let mut body = format!("(local.set {result} (i32.const 1)) ");
        for child in &child_outputs {
            let v = ctx.fresh("seq");
            body.push_str(&format!(
                "(if (local.get {result}) (then \
                   (local.set {v} {child}) \
                   (if (i32.eq (local.get {v}) (i32.const -1)) \
                     (then (local.set {result} (i32.const 0))) \
                     (else (local.set $off (local.get {v})))) \
                 )) "
            ));
        }
        body.push_str(&format!(
            "(if (result i32) (local.get {result}) \
               (then (local.get $off)) \
               (else (i32.const -1)))"
        ));
        body
    }

    fn emit_seq_grouped(
        &mut self,
        groups: Vec<SeqChildGroup<String>>,
        _result_type: &TypeDesc,
        _flatten: Option<FlattenStrategy>,
        ctx: &mut WasmEmitCtx,
    ) -> String {
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
        // Byte-based dispatch: load one byte, try only the matching branch.
        let byte_var = ctx.fresh("d_byte");
        let result = ctx.fresh("d_result");
        let save = ctx.fresh("d_save");

        let mut body = format!(
            "(local.set {save} (local.get $off)) \
             (local.set {result} (i32.const -1)) "
        );
        body.push_str(&format!(
            "(if (i32.lt_u (local.get $off) (local.get $len)) (then \
               (local.set {byte_var} (i32.load8_u (local.get $off))) "
        ));
        for (branch_idx, (_info, branch_body)) in branches.iter().enumerate() {
            let byte_patterns: Vec<u8> = table
                .table
                .iter()
                .enumerate()
                .filter(|&(_, &b)| b as usize == branch_idx)
                .map(|(bv, _)| bv as u8)
                .collect();
            if byte_patterns.is_empty() {
                continue;
            }
            let byte_cond = WasmEmitter::byte_match_condition(&byte_var, &byte_patterns);
            body.push_str(&format!(
                "(if (i32.and (i32.eq (local.get {result}) (i32.const -1)) {byte_cond}) (then \
                   (local.set $off (local.get {save})) \
                   (local.set {result} {branch_body}) \
                 )) "
            ));
        }
        body.push_str(")) ");
        if let Some((_info, fb_body)) = &fallback {
            body.push_str(&format!(
                "(if (i32.eq (local.get {result}) (i32.const -1)) (then \
                   (local.set $off (local.get {save})) \
                   (local.set {result} {fb_body}) \
                 )) "
            ));
        }
        body.push_str(&format!("(local.get {result})"));
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
        let mut body = format!(
            "(local.set {save} (local.get $off)) \
             (local.set {result} (i32.const -1)) "
        );

        for (_info, branch) in &branches {
            body.push_str(&format!(
                "(if (i32.eq (local.get {result}) (i32.const -1)) (then \
                   (local.set $off (local.get {save})) \
                   (local.set {result} {branch}) \
                 )) "
            ));
        }

        body.push_str(&format!("(local.get {result})"));
        body
    }

    fn emit_alt_all_literal(
        &mut self,
        literals: Vec<(String, String)>,
        _alloc: AllocStrategy,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        if literals.len() == 1 {
            return literals.into_iter().next().unwrap().1;
        }
        // Literal matching is non-destructive — no checkpoint needed.
        let result = ctx.fresh("alt_result");
        let mut body = String::new();
        let mut iter = literals.into_iter();
        let (_, first) = iter.next().unwrap();
        body.push_str(&format!("(local.set {result} {first}) "));
        for (_, branch) in iter {
            body.push_str(&format!(
                "(if (i32.eq (local.get {result}) (i32.const -1)) (then \
                   (local.set {result} {branch}) \
                 )) "
            ));
        }
        body.push_str(&format!("(local.get {result})"));
        body
    }

    fn emit_key_dispatch(
        &mut self,
        config: &KeyDispatchConfig,
        branches: Vec<KeyDispatchBranch<String>>,
        fallback: Option<(AltBranchInfo, String)>,
        _alloc: AllocStrategy,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        let save = ctx.fresh("kd_save");
        let result = ctx.fresh("kd_result");
        let key_end = ctx.fresh("kd_end");
        let key_len = ctx.fresh("kd_len");
        let regex_id = config
            .key_scanner_regex_id
            .expect("key_scanner_regex_id must be set by driver");

        let mut body = format!(
            "(local.set {save} (local.get $off)) \
             (local.set {result} (i32.const -1)) \
             (local.set {key_end} (call $__match_regex (i32.const {regex_id}) \
             (local.get $off) (local.get $len))) "
        );
        body.push_str(&format!(
            "(if (i32.ne (local.get {key_end}) (i32.const -1)) (then \
             (local.set {key_len} (i32.sub (local.get {key_end}) (local.get {save}))) "
        ));
        for kd_branch in &branches {
            for key in &kd_branch.key_bytes {
                let len = key.len();
                let mut cond = format!("(i32.eq (local.get {key_len}) (i32.const {len}))");
                for (i, &b) in key.iter().enumerate() {
                    cond = format!(
                        "(i32.and {cond} (i32.eq (i32.load8_u (i32.add (local.get {save}) \
                         (i32.const {i}))) (i32.const {b})))"
                    );
                }
                let branch_body = &kd_branch.body;
                body.push_str(&format!(
                    "(if (i32.and (i32.eq (local.get {result}) (i32.const -1)) {cond}) \
                       (then (local.set $off (local.get {save})) \
                             (local.set {result} {branch_body}))) "
                ));
            }
        }
        body.push_str(")) ");
        if let Some((_info, fb_body)) = &fallback {
            body.push_str(&format!(
                "(if (i32.eq (local.get {result}) (i32.const -1)) (then \
                   (local.set $off (local.get {save})) \
                   (local.set {result} {fb_body}))) "
            ));
        }
        body.push_str(&format!("(local.get {result})"));
        body
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

        // Terminator byte early-exit check.
        let terminator_check = if let Some(ref tb) = config.terminator_bytes {
            if tb.len() == 1 {
                let byte = tb[0];
                format!(
                    "(br_if $sep_exit (i32.and \
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
                 (block $sep_exit (loop $sep_loop \
                   {terminator_check}\
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
             (if (result i32) (i32.eq (local.get {head_var}) (i32.const -1)) (then (i32.const -1)) (else \
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
             (local.get $off) ))"
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
        inner
    }

    fn emit_number_convert(&mut self, _ctx: &mut WasmEmitCtx) -> String {
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
        if let Some(pattern) = ws_pattern {
            if pattern.contains("/*") || pattern.contains(r"\/\*") {
                "(block $ws_done (loop $ws_loop \
                   (br_if $ws_done (i32.ge_u (local.get $off) (local.get $len))) \
                   (if (i32.or \
                     (i32.or \
                       (i32.eq (i32.load8_u (local.get $off)) (i32.const 32)) \
                       (i32.eq (i32.load8_u (local.get $off)) (i32.const 9))) \
                     (i32.or \
                       (i32.eq (i32.load8_u (local.get $off)) (i32.const 10)) \
                       (i32.eq (i32.load8_u (local.get $off)) (i32.const 13)))) \
                     (then (local.set $off (i32.add (local.get $off) (i32.const 1))) (br $ws_loop))) \
                   (if (i32.and \
                     (i32.lt_u (i32.add (local.get $off) (i32.const 1)) (local.get $len)) \
                     (i32.and \
                       (i32.eq (i32.load8_u (local.get $off)) (i32.const 47)) \
                       (i32.eq (i32.load8_u (i32.add (local.get $off) (i32.const 1))) (i32.const 42)))) \
                     (then \
                       (local.set $off (i32.add (local.get $off) (i32.const 2))) \
                       (block $comment_done (loop $comment_loop \
                         (br_if $comment_done (i32.ge_u (i32.add (local.get $off) (i32.const 1)) (local.get $len))) \
                         (if (i32.and \
                           (i32.eq (i32.load8_u (local.get $off)) (i32.const 42)) \
                           (i32.eq (i32.load8_u (i32.add (local.get $off) (i32.const 1))) (i32.const 47))) \
                           (then (local.set $off (i32.add (local.get $off) (i32.const 2))) (br $ws_loop))) \
                         (local.set $off (i32.add (local.get $off) (i32.const 1))) \
                         (br $comment_loop) \
                       )) \
                     )) \
                 )) \
                 (local.get $off)"
                    .to_string()
            } else if let Some(ws_id) = self.ws_regex_id {
                format!("(call $__match_regex (i32.const {ws_id}) (local.get $off) (local.get $len))")
            } else {
                self.emit_ws_trim(None, _ctx)
            }
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
        let ws_side_effect = if let Some(pattern) = ws_pattern {
            if pattern.contains("/*") || pattern.contains(r"\/\*") {
                "(block $ws_done (loop $ws_loop \
                   (br_if $ws_done (i32.ge_u (local.get $off) (local.get $len))) \
                   (if (i32.or \
                     (i32.or (i32.eq (i32.load8_u (local.get $off)) (i32.const 32)) (i32.eq (i32.load8_u (local.get $off)) (i32.const 9))) \
                     (i32.or (i32.eq (i32.load8_u (local.get $off)) (i32.const 10)) (i32.eq (i32.load8_u (local.get $off)) (i32.const 13)))) \
                     (then (local.set $off (i32.add (local.get $off) (i32.const 1))) (br $ws_loop))) \
                   (if (i32.and (i32.lt_u (i32.add (local.get $off) (i32.const 1)) (local.get $len)) \
                     (i32.and (i32.eq (i32.load8_u (local.get $off)) (i32.const 47)) (i32.eq (i32.load8_u (i32.add (local.get $off) (i32.const 1))) (i32.const 42)))) \
                     (then (local.set $off (i32.add (local.get $off) (i32.const 2))) \
                       (block $c_done (loop $c_loop \
                         (br_if $c_done (i32.ge_u (i32.add (local.get $off) (i32.const 1)) (local.get $len))) \
                         (if (i32.and (i32.eq (i32.load8_u (local.get $off)) (i32.const 42)) (i32.eq (i32.load8_u (i32.add (local.get $off) (i32.const 1))) (i32.const 47))) \
                           (then (local.set $off (i32.add (local.get $off) (i32.const 2))) (br $ws_loop))) \
                         (local.set $off (i32.add (local.get $off) (i32.const 1))) (br $c_loop) \
                       )) )) \
                 )) ".to_string()
            } else if let Some(ws_id) = self.ws_regex_id {
                format!("(local.set $off (call $__match_regex (i32.const {ws_id}) (local.get $off) (local.get $len))) ")
            } else {
                WasmEmitter::ascii_ws_side_effect()
            }
        } else {
            WasmEmitter::ascii_ws_side_effect()
        };
        format!(
            "{ws_side_effect}\
             (local.set {result} {inner}) \
             (if (result i32) (i32.eq (local.get {result}) (i32.const -1)) \
               (then (i32.const -1)) \
               (else \
                 (local.set $off (local.get {result})) \
                 {ws_side_effect}\
                 (local.get $off)))"
        )
    }

    // ── Token dispatch ─────────────────────────────────────────────────

    fn emit_token_dispatch(
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

    // ── Delimiter scan ─────────────────────────────────────────────────

    fn emit_delim_scan(
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
        let block_call = config.block_rule.as_ref()
            .map(|(_, name)| format!("(call $__{name} (local.get $off) (local.get $len))"))
            .unwrap_or_else(|| "(i32.const -1)".to_string());
        let pivot_call = config.pivot_rule.as_ref()
            .map(|(_, name)| format!("(call $__{name} (local.get $off) (local.get $len))"))
            .unwrap_or_else(|| "(i32.const -1)".to_string());
        let trail_consume = if let Some(tb) = config.trail_byte {
            format!(
                "(if (i32.and (i32.lt_u (local.get $off) (local.get $len)) \
                    (i32.eq (i32.load8_u (local.get $off)) (i32.const {tb}))) \
                   (then (local.set $off (i32.add (local.get $off) (i32.const 1))))) "
            )
        } else { String::new() };
        Some(format!(
            "(local.set {start} (local.get $off)) \
             (local.set {result} (i32.const -1)) \
             (if (i32.and (i32.lt_u (local.get $off) (local.get $len)) \
                  (i32.eq (i32.load8_u (local.get $off)) (i32.const {open}))) \
               (then \
                 (local.set $off (i32.add (local.get $off) (i32.const 1))) \
                 (block $ds_exit (loop $ds_loop \
                   (br_if $ds_exit (i32.ge_u (local.get $off) (local.get $len))) \
                   (local.set {byte_var} (i32.load8_u (local.get $off))) \
                   (if (i32.eq (local.get {byte_var}) (i32.const {close})) (then \
                     (local.set $off (i32.add (local.get $off) (i32.const 1))) \
                     (local.set {result} (local.get $off)) \
                     (br $ds_exit))) \
                   (if (i32.eq (local.get {byte_var}) (i32.const {open})) (then \
                     (local.set {result} {block_call}) \
                     (br_if $ds_exit (i32.eq (local.get {result}) (i32.const -1))) \
                     (local.set $off (local.get {result})) \
                     (local.set {result} (i32.const -1)) \
                     (br $ds_loop))) \
                   (block $piv_scan (loop $piv_loop \
                     (br_if $piv_scan (i32.ge_u (local.get $off) (local.get $len))) \
                     (if (i32.eq (i32.load8_u (local.get $off)) (i32.const {pivot})) (then \
                       (local.set $off (i32.add (local.get $off) (i32.const 1))) \
                       {trail_consume}\
                       (local.set {result} {pivot_call}) \
                       (br_if $ds_exit (i32.eq (local.get {result}) (i32.const -1))) \
                       (local.set $off (local.get {result})) \
                       (local.set {result} (i32.const -1)) \
                       (br $ds_loop))) \
                     (if (i32.or (i32.eq (i32.load8_u (local.get $off)) (i32.const {close})) \
                                 (i32.eq (i32.load8_u (local.get $off)) (i32.const {open}))) \
                       (then (br $piv_scan))) \
                     (local.set $off (i32.add (local.get $off) (i32.const 1))) \
                     (br $piv_loop) \
                   )) \
                   (br $ds_exit) \
                 )) \
               )) \
             (if (i32.eq (local.get {result}) (i32.const -1)) (then \
               (local.set $off (local.get {start})))) \
             (local.get {result})"
        ))
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
        output.push_str(&format!(
            "\n  (export \"parse\" (func $__{entry_name}))\n"
        ));
        output.push_str(")\n");
        output
    }
}
