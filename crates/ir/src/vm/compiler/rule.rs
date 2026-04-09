//! Rule compilation: emit entry point, source map entry, debug breakpoints,
//! memo wrapping, and the rule-tag construction (`MakeTagged` for non-
//! transparent, non-alias rules).

use crate::vm::bytecode::{Op, SourceMapEntry};
use crate::{GrammarIR, IrRule, MemoStrategy};

use super::Compiler;

impl Compiler {
    pub(super) fn compile_rule(&mut self, rule: &IrRule, ir: &GrammarIR) {
        let entry = self.code.len() as u32;
        self.entries[rule.id as usize] = entry;

        // Emit source map entry when debug info is requested.
        if self.debug {
            if let Some(ref span) = rule.source_span {
                self.source_map.push(SourceMapEntry {
                    pc: entry,
                    rule_id: rule.id,
                    span: span.clone(),
                });
            }
        }

        // Emit debug breakpoint at rule entry for @debug-annotated rules.
        let rule_debug = self.debug_all || rule.meta.directives.debug;
        if rule_debug {
            self.emit(Op::DebugBreak {
                rule_id: rule.id,
                is_entry: true,
            });
        }

        // Emit memo check for memoized rules.
        let memo_check_idx = if self.memo_enabled && rule.meta.memo != MemoStrategy::None {
            Some(self.emit(Op::MemoCheck {
                rule_id: rule.id,
                hit_offset: 0,
            }))
        } else {
            None
        };

        self.compile_node(&rule.body, ir);

        // Tag the result with the rule name for non-transparent, non-alias rules.
        // Transparent rules and aliases pass through their inner value unwrapped.
        let should_tag = !rule.meta.is_transparent && rule.meta.is_alias.is_none();
        if should_tag {
            self.emit(Op::MakeTagged(rule.name));
        }

        if self.memo_enabled && rule.meta.memo != MemoStrategy::None {
            self.emit(Op::MemoStore(rule.id));
        }

        // Emit debug breakpoint at rule exit.
        if rule_debug {
            self.emit(Op::DebugBreak {
                rule_id: rule.id,
                is_entry: false,
            });
        }

        let return_idx = self.emit(Op::Return);

        // Patch memo check to jump to the Return instruction on cache hit.
        // The Return is needed to pop the call frame when we entered via Call.
        if let Some(idx) = memo_check_idx {
            if let Op::MemoCheck { hit_offset, .. } = &mut self.code[idx] {
                *hit_offset = return_idx as u32;
            }
        }
    }
}
