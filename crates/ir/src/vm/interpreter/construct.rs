//! Value construction: `MakeArray` and `MakeTagged`.

use super::Interpreter;
use super::value::Value;

impl<'prog> Interpreter<'prog> {
    #[inline(always)]
    pub(super) fn exec_make_array(&mut self, count: u32) {
        let n = count as usize;
        let start = self.values.len().saturating_sub(n);
        let collected = self.collect_values_from(start);
        self.values.push(Value::Array(collected));
        self.pc += 1;
    }

    #[inline(always)]
    pub(super) fn exec_make_tagged(&mut self, tag: u32) {
        // Collect ALL values pushed since the call frame was entered,
        // so sequences produce multiple children instead of losing all but the last.
        let (start, depth) = self
            .call_stack
            .last()
            .map(|f| (f.start_offset, f.value_depth))
            .unwrap_or((0, 0));
        let depth = depth.min(self.values.len());
        let children = self.collect_values_from(depth);
        if !children.is_empty() {
            self.values.push(Value::Tagged {
                tag,
                span: (start, self.offset),
                children,
            });
        }
        self.pc += 1;
    }
}
