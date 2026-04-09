//! Repetition execution: `RepeatBegin`, `RepeatEnd`, and the finalizer.

use super::value::Value;
use super::{Interpreter, RepeatState};

impl<'prog> Interpreter<'prog> {
    #[inline(always)]
    pub(super) fn exec_repeat_begin(&mut self, lo: u32, hi: u32, body_end: u32) {
        self.repeats.push(RepeatState {
            count: 0,
            lo,
            hi,
            body_start: self.pc + 1,
            body_end,
            value_depth: self.values.len(),
            iter_start_offset: self.offset,
        });
        self.pc += 1;
    }

    #[inline(always)]
    pub(super) fn exec_repeat_end(&mut self) {
        let repeat = self
            .repeats
            .last_mut()
            .expect("repeat stack should not be empty at RepeatEnd");

        if self.is_error || repeat.iter_start_offset == self.offset {
            // Body failed or zero-length match — finalize.
            self.is_error = false;
            self.finalize_repeat();
        } else {
            // Body succeeded — increment and maybe loop.
            repeat.count += 1;
            repeat.iter_start_offset = self.offset;

            if repeat.count >= repeat.hi {
                self.finalize_repeat();
            } else {
                let body_start = repeat.body_start;
                self.pc = body_start;
            }
        }
    }

    /// Pop the repeat state and collect values into an Array.
    /// Succeeds if `count >= lo`, fails otherwise.
    ///
    /// The depth is clamped to the current values stack length because
    /// backtracking (`RestoreState`) and discard ops (`>>`, `<<`) within the
    /// repeat body can legitimately truncate the values stack below the level
    /// recorded at `RepeatBegin`.
    #[inline(always)]
    fn finalize_repeat(&mut self) {
        let repeat = self
            .repeats
            .pop()
            .expect("repeat stack should not be empty during finalize");
        let depth = repeat.value_depth.min(self.values.len());

        if repeat.count >= repeat.lo {
            let collected = self.collect_values_from(depth);
            self.values.push(Value::Array(collected));
            self.is_error = false;
        } else {
            self.values.truncate(depth);
            self.is_error = true;
        }
        self.pc = repeat.body_end;
    }
}
