//! Bytecode opcodes for the grammar VM interpreter.
//!
//! Designed for a tight interpreter loop with excellent branch prediction.
//! Bytecode is compact (avg ~3 bytes/instruction) and fast to emit from IR.

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::{CharSet128, GrammarSpan, RuleId, StringId};

/// Dispatch table data stored in a side table, referenced by index.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct DispatchData {
    /// 128-entry table: `table[byte]` = branch index, or 255 for no match.
    pub table: Vec<u8>,
    /// Bytecode offsets for each branch.
    pub offsets: Vec<u32>,
    /// Bytecode offset for the fallback (no match) path.
    pub fallback: u32,
}

/// A single bytecode instruction.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub enum Op {
    // ── Leaves ──────────────────────────────────────────────────────────

    /// Match a literal string. Fails if the input at current offset doesn't match.
    MatchString(StringId),

    /// Match a regex. Fails if the regex doesn't match at current offset.
    MatchRegex(StringId),

    /// Always succeeds, consumes nothing.
    Epsilon,

    // ── Control Flow ────────────────────────────────────────────────────

    /// Jump to an absolute bytecode offset.
    Jump(u32),

    /// Jump to an absolute bytecode offset if the last operation failed.
    JumpIfFail(u32),

    /// Jump to an absolute bytecode offset if the last operation succeeded.
    JumpIfOk(u32),

    /// Call a rule: push a call frame, jump to the rule's entry point.
    Call(RuleId),

    /// Return from the current rule.
    Return,

    // ── State Management ────────────────────────────────────────────────

    /// Push current offset + error state onto the checkpoint stack.
    SaveState,

    /// Pop checkpoint and restore offset + error state (backtrack).
    RestoreState,

    /// Pop checkpoint without restoring (commit).
    DropState,

    /// Trim optional whitespace at current offset (basic ASCII whitespace).
    TrimWs,

    /// Trim optional whitespace using a custom `@ws` regex pattern.
    /// Advances offset without pushing a value (like TrimWs).
    TrimWsPattern(StringId),

    // ── Combinators ─────────────────────────────────────────────────────

    /// Set-difference: execute next two instructions, fail if both succeed.
    /// Layout: Minus → [lhs instruction(s)] → [rhs instruction(s)]
    /// Uses SaveState/RestoreState internally in the interpreter.
    Minus,

    /// Zero-width negative assertion: fail if next instruction succeeds.
    Negate,

    // ── Dispatch ────────────────────────────────────────────────────────

    /// O(1) byte dispatch table: index into `BytecodeProgram::dispatch_tables`.
    Dispatch(u16),

    // ── Repetition ──────────────────────────────────────────────────────

    /// Begin a repeat loop. Pushes repeat state (count = 0).
    /// `body_end` is the bytecode offset of the instruction after the body.
    RepeatBegin {
        lo: u32,
        hi: u32,
        body_end: u32,
    },

    /// End of repeat body. Increments count, checks bounds, loops or exits.
    RepeatEnd,

    // ── Value Construction ──────────────────────────────────────────────

    /// Push a span (start_offset, end_offset) for the just-matched input.
    CaptureSpan,

    /// Save the current value stack depth for later truncation (used by Skip/Next).
    SaveValueDepth,

    /// For Skip (`<<`): discard all values pushed since `SaveValueDepth`, keeping left's values.
    DiscardRight,

    /// For Next (`>>`): keep only values pushed since `SaveValueDepth`, discarding left's values.
    DiscardLeft,

    /// Collect the top `count` values from the value stack into an array.
    MakeArray(u32),

    /// Tag the top value with a rule name (for AST construction).
    MakeTagged(StringId),

    // ── Memoization ─────────────────────────────────────────────────────

    /// Check memo table for this rule at current offset.
    /// If cached: restore state from cache, jump to `hit_offset`.
    /// If not cached: continue to next instruction.
    MemoCheck {
        rule_id: RuleId,
        hit_offset: u32,
    },

    /// Store current result in memo table for this rule.
    MemoStore(RuleId),

    // ── Debug ───────────────────────────────────────────────────────────

    /// Debug breakpoint. No-op when interpreter has no debug state.
    /// Emitted at rule entry/exit for `@debug`-annotated rules.
    DebugBreak {
        rule_id: RuleId,
        is_entry: bool,
    },

    /// No-op. Used for padding/alignment.
    Nop,

    /// Halt execution (success or failure based on current state).
    Halt,
}

/// A compiled bytecode program for a grammar.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct BytecodeProgram {
    /// The bytecode instructions.
    pub code: Vec<Op>,

    /// Entry points for each rule: `entry[rule_id] = bytecode_offset`.
    pub entries: Vec<u32>,

    /// Interned string table (shared with GrammarIR).
    pub strings: Vec<String>,

    /// Entry rule ID.
    pub entry: RuleId,

    /// Dispatch tables referenced by `Op::Dispatch(idx)`.
    #[serde(default)]
    pub dispatch_tables: Vec<DispatchData>,

    /// Pre-compiled regex patterns, indexed by StringId.
    /// `None` for string IDs that are not regex patterns.
    #[serde(skip)]
    pub compiled_regexes: Vec<Option<regex::Regex>>,

    /// FOLLOW sets per rule, for error recovery and expected-token reporting.
    /// Populated from `GrammarIR::follow_sets` during compilation.
    #[serde(default)]
    pub follow_sets: HashMap<RuleId, CharSet128>,

    /// Rule names indexed by RuleId (for error messages).
    #[serde(default)]
    pub rule_names: Vec<StringId>,

    /// Source map: bytecode PC → grammar source span.
    /// Only populated when compiled with debug info.
    #[serde(default)]
    pub source_map: Vec<SourceMapEntry>,
}

/// Maps a bytecode program counter to a grammar source location.
#[derive(Serialize, Deserialize, Clone, Debug, Default)]
pub struct SourceMapEntry {
    /// Bytecode offset (program counter).
    pub pc: u32,
    /// Rule that this PC belongs to.
    pub rule_id: RuleId,
    /// Byte range in the original grammar source.
    pub span: GrammarSpan,
}

impl BytecodeProgram {
    /// Get the entry point for a rule.
    pub fn rule_entry(&self, rule_id: RuleId) -> u32 {
        self.entries[rule_id as usize]
    }

    /// Pre-compile all regex patterns referenced by `MatchRegex` opcodes.
    /// Called automatically by `compile()`, and should be called after deserialization.
    pub fn prepare_regexes(&mut self) {
        // Collect all StringIds used by regex-matching opcodes.
        let mut regex_sids = std::collections::HashSet::new();
        for op in &self.code {
            match op {
                Op::MatchRegex(sid) | Op::TrimWsPattern(sid) => {
                    regex_sids.insert(*sid);
                }
                _ => {}
            }
        }

        // Build the compiled_regexes vec, sized to cover all referenced SIDs.
        let max_sid = regex_sids.iter().copied().max().unwrap_or(0) as usize;
        self.compiled_regexes = vec![None; max_sid + 1];

        for sid in regex_sids {
            let pattern = &self.strings[sid as usize];
            let anchored = format!("^(?:{})", pattern);
            let re = regex::Regex::new(&anchored)
                .unwrap_or_else(|e| panic!("Invalid regex in grammar: {}: {}", pattern, e));
            self.compiled_regexes[sid as usize] = Some(re);
        }
    }
}
