//! Control-flow execution: call/return, checkpoint save/restore/drop,
//! whitespace trimming.

use super::{CallFrame, Checkpoint, Interpreter};

// ── Call / return ───────────────────────────────────────────────────────────

impl<'prog> Interpreter<'prog> {
    #[inline(always)]
    pub(super) fn exec_call(&mut self, rule_id: u32) {
        self.call_stack.push(CallFrame {
            return_pc: self.pc + 1,
            start_offset: self.offset,
            value_depth: self.values.len(),
        });
        self.rule_stack.push(rule_id);
        self.pc = self.program.rule_entry(rule_id);
    }

    /// Returns false if top-level return (halt).
    #[inline(always)]
    pub(super) fn exec_return(&mut self) -> bool {
        self.rule_stack.pop();
        if let Some(frame) = self.call_stack.pop() {
            self.pc = frame.return_pc;
            true
        } else {
            false
        }
    }
}

// ── State management ────────────────────────────────────────────────────────

impl<'prog> Interpreter<'prog> {
    #[inline(always)]
    pub(super) fn exec_save_state(&mut self) {
        self.checkpoints.push(Checkpoint {
            offset: self.offset,
            value_depth: self.values.len(),
            value_depth_stack_depth: self.value_depth_stack.len(),
        });
        self.pc += 1;
    }

    #[inline(always)]
    pub(super) fn exec_restore_state(&mut self) {
        if let Some(cp) = self.checkpoints.pop() {
            self.offset = cp.offset;
            self.values.truncate(cp.value_depth);
            self.value_depth_stack.truncate(cp.value_depth_stack_depth);
            self.is_error = true;
        }
        self.pc += 1;
    }

    #[inline(always)]
    pub(super) fn exec_drop_state(&mut self) {
        self.checkpoints.pop();
        self.pc += 1;
    }

    #[inline(always)]
    pub(super) fn exec_trim_ws(&mut self) {
        let mut pos = self.offset as usize;
        while pos < self.input_bytes.len() {
            match self.input_bytes[pos] {
                b' ' | b'\t' | b'\n' | b'\r' => pos += 1,
                _ => break,
            }
        }
        self.offset = pos as u32;
        self.pc += 1;
    }

    /// Trim whitespace using a custom `@ws` DFA pattern.
    /// Advances offset without pushing a value (like TrimWs). Always succeeds.
    #[inline(always)]
    pub(super) fn exec_trim_ws_pattern(&mut self, sid: u32) {
        let start = self.offset as usize;
        let dfa = self.program.compiled_regexes[sid as usize]
            .as_ref()
            .expect("TrimWsPattern references non-regex StringId");
        if let Some(end) = dfa.find_at(self.input.as_bytes(), start) {
            self.offset = end as u32;
        }
        self.pc += 1;
    }
}
