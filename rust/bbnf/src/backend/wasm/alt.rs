//! Alternation emission helpers for the WASM backend.

use bbnf_ir::AltDispatch;

use crate::backend::key_dispatch::KeyDispatchConfig;
use crate::backend::{AllocStrategy, AltBranchInfo, KeyDispatchBranch};

use super::code::{WasmEmitCtx, WasmEmitter};

impl WasmEmitter {
    /// Byte-based dispatch: load one byte, try only the matching branch.
    pub(super) fn alt_dispatch(
        &mut self,
        table: &AltDispatch,
        branches: Vec<(AltBranchInfo, String)>,
        fallback: Option<(AltBranchInfo, String)>,
        _alloc: AllocStrategy,
        ctx: &mut WasmEmitCtx,
    ) -> String {
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

    /// Sequential checkpoint alternation — try each branch, backtrack on failure.
    pub(super) fn alt_checkpoint(
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

    /// All-literal alternation — no checkpoint needed since literal matching is non-destructive.
    pub(super) fn alt_all_literal(
        &mut self,
        literals: Vec<(String, String)>,
        _alloc: AllocStrategy,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        if literals.len() == 1 {
            return literals.into_iter().next().unwrap().1;
        }
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

    /// Key-based dispatch — scan a key via regex, then match against known key byte patterns.
    pub(super) fn key_dispatch(
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
}
