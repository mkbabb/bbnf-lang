//! Low-level instruction-buffer primitives: emit, patch, current_offset.

use crate::vm::bytecode::Op;

use super::Compiler;

impl Compiler {
    pub(super) fn emit(&mut self, op: Op) -> usize {
        let idx = self.code.len();
        self.code.push(op);
        idx
    }

    pub(super) fn current_offset(&self) -> u32 {
        self.code.len() as u32
    }

    /// Patch a previously emitted instruction.
    pub(super) fn patch(&mut self, idx: usize, op: Op) {
        self.code[idx] = op;
    }

    /// Patch a list of `JumpIfFail` placeholders to the given target.
    pub(super) fn patch_fail_jumps(&mut self, indices: &[usize], target: u32) {
        for &idx in indices {
            self.code[idx] = Op::JumpIfFail(target);
        }
    }
}
