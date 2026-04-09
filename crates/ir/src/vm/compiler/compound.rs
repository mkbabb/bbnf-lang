//! Compound-node compilers — sequence, alternation (linear + dispatch),
//! token dispatch, repetition, Skip/Next, Minus, Negate.

use crate::vm::bytecode::{
    DispatchData, Op, TokenDispatchArmData, TokenDispatchData,
};
use crate::{AltBranch, AltDispatch, GrammarIR, IrNode, TokenDispatchArm};

use super::Compiler;

/// Common scratch state for lowering backtracking dispatches.
pub(super) struct DispatchPatchPlan {
    branch_offsets: Vec<u32>,
    fail_jumps: Vec<usize>,
    success_jumps: Vec<usize>,
}

impl DispatchPatchPlan {
    fn with_capacity(capacity: usize) -> Self {
        Self {
            branch_offsets: Vec::with_capacity(capacity),
            fail_jumps: Vec::with_capacity(capacity),
            success_jumps: Vec::with_capacity(capacity),
        }
    }

    fn record_branch(&mut self, offset: u32) {
        self.branch_offsets.push(offset);
    }

    fn record_fail(&mut self, idx: usize) {
        self.fail_jumps.push(idx);
    }

    fn record_success(&mut self, idx: usize) {
        self.success_jumps.push(idx);
    }
}

impl Compiler {
    /// Compile a sequence: SaveState → child₁ → JumpIfFail → child₂ → ... → DropState.
    ///
    /// Uses a proper fixup list to avoid the global `JumpIfFail(0)` patching bug.
    pub(super) fn compile_seq(&mut self, children: &[IrNode], ir: &GrammarIR) {
        if children.len() <= 1 {
            for child in children {
                self.compile_node(child, ir);
            }
            return;
        }

        self.emit(Op::SaveState);
        let mut fail_jumps: Vec<usize> = Vec::new();

        for (i, child) in children.iter().enumerate() {
            self.compile_node(child, ir);
            if i < children.len() - 1 {
                fail_jumps.push(self.emit(Op::JumpIfFail(0)));
            }
        }

        // Success path: drop checkpoint, jump past restore.
        self.emit(Op::DropState);
        let end_jump = self.emit(Op::Jump(0));

        // Failure path: restore checkpoint.
        let fail_offset = self.current_offset();
        self.emit(Op::RestoreState);
        let after_fail = self.current_offset();

        self.patch(end_jump, Op::Jump(after_fail));
        self.patch_fail_jumps(&fail_jumps, fail_offset);
    }

    /// Compile alternation: try each branch with SaveState/RestoreState.
    ///
    /// If the dispatch pass has pre-computed a dispatch table (`AltDispatch`),
    /// emits an O(1) `Dispatch` instruction. Otherwise falls back to linear backtracking.
    pub(super) fn compile_alt(
        &mut self,
        branches: &[AltBranch],
        dispatch: Option<&AltDispatch>,
        ir: &GrammarIR,
    ) {
        if branches.is_empty() {
            return;
        }
        if branches.len() == 1 {
            self.compile_node(&branches[0].node, ir);
            return;
        }

        // Use pre-computed dispatch table when available, including fallback-aware
        // dispatch tables from the shared IR dispatch pass.
        if let Some(alt_dispatch) = dispatch {
            self.compile_dispatch(branches, alt_dispatch, ir);
            return;
        }

        // Fallback: linear backtracking.
        let mut jump_to_end: Vec<usize> = Vec::new();

        for (i, branch) in branches.iter().enumerate() {
            let is_last = i == branches.len() - 1;

            if !is_last {
                self.emit(Op::SaveState);
            }

            self.compile_node(&branch.node, ir);

            if !is_last {
                let ok_jump = self.emit(Op::JumpIfOk(0));
                self.emit(Op::RestoreState);
                jump_to_end.push(ok_jump);
            }
        }

        // The last branch has no corresponding SaveState on the checkpoint stack
        // (it was consumed by RestoreState from the previous branch). Both its
        // success and failure paths must skip DropState.
        let skip_drop_fail = self.emit(Op::JumpIfFail(0));
        let skip_drop_ok = self.emit(Op::Jump(0));

        // Success path for non-last branches: drop their SaveState checkpoint.
        // Only reachable via JumpIfOk from non-last branches.
        let success_offset = self.current_offset();
        self.emit(Op::DropState);

        let after_drop = self.current_offset();
        for &idx in &jump_to_end {
            self.patch(idx, Op::JumpIfOk(success_offset));
        }

        // Last branch paths: skip DropState entirely.
        self.patch(skip_drop_ok, Op::Jump(after_drop));
        self.patch(skip_drop_fail, Op::JumpIfFail(self.current_offset()));
    }

    fn finalize_dispatch_backtracking(
        &mut self,
        plan: DispatchPatchPlan,
        restore_offset: u32,
        drop_offset: u32,
        fallback_jump: usize,
        end_jump: usize,
        after_offset: u32,
        patch_table: impl FnOnce(&mut Self, Vec<u32>, u32),
    ) {
        patch_table(self, plan.branch_offsets, restore_offset);

        for idx in plan.fail_jumps {
            self.patch(idx, Op::JumpIfFail(restore_offset));
        }
        for idx in plan.success_jumps {
            self.patch(idx, Op::Jump(drop_offset));
        }
        self.patch(fallback_jump, Op::Jump(after_offset));
        self.patch(end_jump, Op::Jump(after_offset));
    }

    /// Emit a Dispatch instruction using a pre-computed table from the dispatch pass.
    ///
    /// Wraps the entire dispatch in SaveState/RestoreState so that when a
    /// dispatch-selected branch partially matches then fails, the offset is
    /// correctly restored (mirroring the linear Alt pattern).
    fn compile_dispatch(
        &mut self,
        branches: &[AltBranch],
        dispatch: &AltDispatch,
        ir: &GrammarIR,
    ) {
        // Save state before dispatch so we can restore on branch failure.
        self.emit(Op::SaveState);

        // Reserve a dispatch table index and emit a placeholder Dispatch op.
        let table_idx = self.dispatch_tables.len() as u16;
        self.dispatch_tables.push(DispatchData {
            table: dispatch.table.clone(),
            offsets: vec![0; branches.len()],
            fallback: 0,
        });
        let _dispatch_idx = self.emit(Op::Dispatch(table_idx));

        let mut plan = DispatchPatchPlan::with_capacity(branches.len());
        let fallback_idx = dispatch.fallback_idx.map(|idx| idx as usize);

        for branch in branches.iter() {
            plan.record_branch(self.current_offset());
            self.compile_node(&branch.node, ir);
            // Check if branch failed → jump to restore.
            plan.record_fail(self.emit(Op::JumpIfFail(0)));
            // Branch succeeded → jump to drop state.
            plan.record_success(self.emit(Op::Jump(0)));
        }

        let restore_offset = self.current_offset();
        self.emit(Op::RestoreState);
        let fallback_jump = if let Some(idx) = fallback_idx {
            self.compile_node(&branches[idx].node, ir);
            self.emit(Op::Jump(0))
        } else {
            self.emit(Op::Jump(0))
        };

        // Success path: drop the checkpoint.
        let drop_offset = self.current_offset();
        self.emit(Op::DropState);
        let end_jump = self.emit(Op::Jump(0));

        let after = self.current_offset();

        self.finalize_dispatch_backtracking(
            plan,
            restore_offset,
            drop_offset,
            fallback_jump,
            end_jump,
            after,
            |this, branch_offsets, fallback| {
                this.dispatch_tables[table_idx as usize] = DispatchData {
                    table: dispatch.table.clone(),
                    offsets: branch_offsets,
                    fallback,
                };
            },
        );
    }

    pub(super) fn compile_token_dispatch(
        &mut self,
        token: &IrNode,
        arms: &[TokenDispatchArm],
        fallback: &IrNode,
        ir: &GrammarIR,
    ) {
        if arms.is_empty() {
            self.compile_node(fallback, ir);
            return;
        }

        self.emit(Op::SaveState);
        self.compile_node(token, ir);
        let token_fail = self.emit(Op::JumpIfFail(0));

        let table_idx = self.token_dispatch_tables.len() as u16;
        self.token_dispatch_tables.push(TokenDispatchData {
            arms: Vec::new(),
            fallback: 0,
        });
        self.emit(Op::DispatchToken(table_idx));

        let mut plan = DispatchPatchPlan::with_capacity(arms.len());

        for arm in arms {
            plan.record_branch(self.current_offset());
            self.compile_node(&arm.continuation, ir);
            plan.record_fail(self.emit(Op::JumpIfFail(0)));
            plan.record_success(self.emit(Op::Jump(0)));
        }

        let restore_offset = self.current_offset();
        self.emit(Op::RestoreState);
        self.compile_node(fallback, ir);
        let fallback_jump = self.emit(Op::Jump(0));

        let drop_offset = self.current_offset();
        self.emit(Op::DropState);
        let end_jump = self.emit(Op::Jump(0));
        let after = self.current_offset();

        self.patch(token_fail, Op::JumpIfFail(restore_offset));
        self.finalize_dispatch_backtracking(
            plan,
            restore_offset,
            drop_offset,
            fallback_jump,
            end_jump,
            after,
            |this, arm_offsets, fallback| {
                this.token_dispatch_tables[table_idx as usize] = TokenDispatchData {
                    arms: arms
                        .iter()
                        .zip(arm_offsets)
                        .map(|(arm, offset)| TokenDispatchArmData {
                            patterns: arm.patterns.clone(),
                            guard_byte: arm.guard_byte,
                            offset,
                        })
                        .collect(),
                    fallback,
                };
            },
        );
    }

    /// Compile repetition: RepeatBegin → body → RepeatEnd.
    pub(super) fn compile_repeat(&mut self, inner: &IrNode, lo: u32, hi: u32, ir: &GrammarIR) {
        let begin_idx = self.emit(Op::RepeatBegin {
            lo,
            hi,
            body_end: 0,
        });

        self.compile_node(inner, ir);
        self.emit(Op::RepeatEnd);

        let after_end = self.current_offset();
        if let Op::RepeatBegin { body_end, .. } = &mut self.code[begin_idx] {
            *body_end = after_end;
        }
    }

    /// Compile Skip (`a << b`) and Next (`a >> b`) — shared backtracking structure.
    ///
    /// `is_skip`: true for Skip (`<<`, keep left), false for Next (`>>`, keep right).
    pub(super) fn compile_binary_backtrack(
        &mut self,
        left: &IrNode,
        right: &IrNode,
        is_skip: bool,
        ir: &GrammarIR,
    ) {
        self.emit(Op::SaveState);

        // Save value stack depth before left — the boundary between context and left's values.
        self.emit(Op::SaveValueDepth);

        self.compile_node(left, ir);
        let fail1 = self.emit(Op::JumpIfFail(0));

        // Save depth again between left and right.
        self.emit(Op::SaveValueDepth);

        self.compile_node(right, ir);
        let fail2 = self.emit(Op::JumpIfFail(0));

        // Discard the unwanted side's values.
        if is_skip {
            self.emit(Op::DiscardRight); // Pop right/left boundary, truncate to it (drops right)
        } else {
            self.emit(Op::DiscardLeft); // Pop right/left boundary + left/context boundary, splice out left
        }

        self.emit(Op::DropState);
        let success_jump = self.emit(Op::Jump(0));

        let fail_offset = self.current_offset();
        self.emit(Op::RestoreState);
        let after = self.current_offset();

        self.patch(fail1, Op::JumpIfFail(fail_offset));
        self.patch(fail2, Op::JumpIfFail(fail_offset));
        self.patch(success_jump, Op::Jump(after));
    }

    /// Compile set-difference: match left only if right does NOT match.
    pub(super) fn compile_minus(&mut self, left: &IrNode, right: &IrNode, ir: &GrammarIR) {
        self.emit(Op::SaveState);

        // Try right first (to check exclusion).
        self.compile_node(right, ir);
        let right_succeeded = self.emit(Op::JumpIfOk(0));

        // Right didn't match — restore and try left.
        self.emit(Op::RestoreState);
        self.emit(Op::SaveState);
        self.compile_node(left, ir);
        let left_failed = self.emit(Op::JumpIfFail(0));
        self.emit(Op::DropState);
        let success_jump = self.emit(Op::Jump(0));

        // Right matched → overall fail.
        let right_match_fail = self.current_offset();
        self.emit(Op::RestoreState);

        let after = self.current_offset();
        self.patch(right_succeeded, Op::JumpIfOk(right_match_fail));
        self.patch(left_failed, Op::JumpIfFail(after));
        self.patch(success_jump, Op::Jump(after));
    }

    /// Compile zero-width negative assertion.
    pub(super) fn compile_negate(&mut self, inner: &IrNode, ir: &GrammarIR) {
        self.emit(Op::SaveState);
        self.compile_node(inner, ir);
        let inner_ok = self.emit(Op::JumpIfOk(0));
        self.emit(Op::RestoreState);
        self.emit(Op::Epsilon);
        let success_jump = self.emit(Op::Jump(0));

        let negate_fail = self.current_offset();
        self.emit(Op::RestoreState);

        let after = self.current_offset();
        self.patch(inner_ok, Op::JumpIfOk(negate_fail));
        self.patch(success_jump, Op::Jump(after));
    }
}
