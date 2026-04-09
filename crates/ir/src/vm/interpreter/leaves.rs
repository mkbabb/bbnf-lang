//! Leaf op execution: literal/regex matching, epsilon, token dispatch.

use super::Interpreter;
use super::value::Value;

impl<'prog> Interpreter<'prog> {
    #[inline(always)]
    pub(super) fn exec_match_string(&mut self, sid: u32) {
        let s = &self.program.strings[sid as usize];
        let start = self.offset as usize;
        let end = start + s.len();

        if end <= self.input.len() && &self.input_bytes[start..end] == s.as_bytes() {
            self.values.push(Value::Span(self.offset, end as u32));
            self.offset = end as u32;
            self.is_error = false;
            self.track_furthest();
        } else {
            self.is_error = true;
            self.track_furthest();
        }
        self.pc += 1;
    }

    #[inline(always)]
    pub(super) fn exec_match_regex(&mut self, sid: u32) {
        let start = self.offset as usize;

        let dfa = self.program.compiled_regexes[sid as usize]
            .as_ref()
            .expect("MatchRegex references non-regex StringId");

        if let Some(end) = dfa.find_at(self.input.as_bytes(), start) {
            self.values.push(Value::Span(self.offset, end as u32));
            self.offset = end as u32;
            self.is_error = false;
            self.track_furthest();
        } else {
            self.is_error = true;
            self.track_furthest();
        }
        self.pc += 1;
    }

    #[inline(always)]
    pub(super) fn exec_epsilon(&mut self) {
        self.values.push(Value::Nil);
        self.is_error = false;
        self.pc += 1;
    }

    #[inline(always)]
    pub(super) fn exec_dispatch_token(&mut self, table_idx: u16) {
        let data = &self.program.token_dispatch_tables[table_idx as usize];
        let Some(Value::Span(start, end)) = self.values.last().cloned() else {
            self.is_error = true;
            self.pc = data.fallback;
            return;
        };

        let token_bytes = &self.input_bytes[start as usize..end as usize];
        for arm in &data.arms {
            let matched = arm
                .patterns
                .iter()
                .any(|sid| self.program.strings[*sid as usize].as_bytes() == token_bytes);
            if !matched {
                continue;
            }
            if let Some(guard) = arm.guard_byte {
                if self.input_bytes.get(self.offset as usize).copied() != Some(guard) {
                    continue;
                }
            }

            self.values.pop();
            self.is_error = false;
            self.pc = arm.offset;
            return;
        }

        self.is_error = true;
        self.pc = data.fallback;
    }
}
