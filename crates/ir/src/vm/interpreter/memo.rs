//! Memoization: `MemoCheck` and `MemoStore`.

use super::Interpreter;
use super::value::Value;

impl<'prog> Interpreter<'prog> {
    #[inline(always)]
    pub(super) fn exec_memo_check(&mut self, rule_id: u32, hit_offset: u32) {
        if !self.program.memo_enabled {
            self.pc += 1;
            return;
        }
        let start_offset = self.offset;
        let key = (rule_id, start_offset);
        if let Some((result_offset, value, was_error)) = self.memo.get(&key) {
            self.offset = *result_offset;
            self.values.push(value.clone()); // Deep clone on cache hit (rare)
            self.is_error = *was_error;
            self.pc = hit_offset;
        } else {
            // Cache miss — record start offset for MemoStore to use.
            self.memo_starts.push((rule_id, start_offset));
            self.pc += 1;
        }
    }

    #[inline(always)]
    pub(super) fn exec_memo_store(&mut self, _rule_id: u32) {
        if !self.program.memo_enabled {
            self.pc += 1;
            return;
        }
        let value = self.values.last().cloned().unwrap_or(Value::Nil);
        // Pop the start offset saved by MemoCheck.
        let (rule_id, start_offset) = self
            .memo_starts
            .pop()
            .expect("MemoStore without matching MemoCheck");
        let key = (rule_id, start_offset);
        self.memo.insert(key, (self.offset, value, self.is_error));
        self.pc += 1;
    }
}
