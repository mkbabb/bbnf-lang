//! Bytecode interpreter for grammar VMs.
//!
//! Executes a `BytecodeProgram` against an input string, producing a parse tree.
//! The interpreter uses a tight loop with explicit stacks for call frames,
//! checkpoints, and values.
//!
//! Sub-modules group inherent methods on `Interpreter` by responsibility:
//!
//! - [`value`] — public types: `Value`, `ValueSlice`, `ParseDiagnostic`,
//!   `ParseResult`.
//! - [`leaves`] — leaf-op exec fns (`exec_match_string`, `exec_match_regex`,
//!   `exec_epsilon`, `exec_dispatch_token`).
//! - [`control`] — call/return + state-save/restore/drop + whitespace trim.
//! - [`repeat`] — repetition begin/end + finalize.
//! - [`construct`] — value construction (`exec_make_array`, `exec_make_tagged`).
//! - [`memo`] — memoization check + store.
//!
//! `mod.rs` owns the `Interpreter` struct, the private stack-frame types,
//! the `new` constructor, the main `run` dispatch loop, the per-iteration
//! helpers (`collect_values_from`, `track_furthest`,
//! `emit_furthest_diagnostic`), and the public `parse_with_ir` convenience.

mod construct;
mod control;
mod leaves;
mod memo;
mod repeat;
mod value;

use std::fmt;

use rustc_hash::FxHashMap;

use super::bytecode::{BytecodeProgram, Op};
pub use super::debug::*;
use crate::RuleId;

pub use self::value::{ParseDiagnostic, ParseResult, Value, ValueSlice};

// ── Internal stack frames ───────────────────────────────────────────────────

#[derive(Clone, Debug)]
pub(super) struct CallFrame {
    pub(super) return_pc: u32,
    /// Input offset when the call started (for accurate span computation in MakeTagged).
    pub(super) start_offset: u32,
    /// Values stack depth when the call started (for collecting all children in MakeTagged).
    pub(super) value_depth: usize,
}

#[derive(Clone, Debug)]
pub(super) struct Checkpoint {
    pub(super) offset: u32,
    pub(super) value_depth: usize,
    pub(super) value_depth_stack_depth: usize,
}

#[derive(Clone, Debug)]
pub(super) struct RepeatState {
    pub(super) count: u32,
    pub(super) lo: u32,
    pub(super) hi: u32,
    pub(super) body_start: u32,
    pub(super) body_end: u32,
    pub(super) value_depth: usize,
    pub(super) iter_start_offset: u32,
}

// ── Interpreter ─────────────────────────────────────────────────────────────

pub struct Interpreter<'prog> {
    pub(super) program: &'prog BytecodeProgram,
    pub(super) input: &'prog str,
    pub(super) input_bytes: &'prog [u8],

    pub(super) pc: u32,
    pub(super) offset: u32,
    pub(super) is_error: bool,

    pub(super) values: Vec<Value>,
    pub(super) call_stack: Vec<CallFrame>,
    pub(super) checkpoints: Vec<Checkpoint>,
    pub(super) repeats: Vec<RepeatState>,
    pub(super) memo: FxHashMap<(RuleId, u32), (u32, Value, bool)>,

    /// Stack of (rule_id, start_offset) pushed by MemoCheck on cache miss,
    /// popped by MemoStore to key the cache entry at the correct pre-parse offset.
    pub(super) memo_starts: Vec<(RuleId, u32)>,

    /// Stack of value stack depths for Skip/Next value selection.
    pub(super) value_depth_stack: Vec<usize>,

    /// Stack of rule IDs tracking the current parse context (for error reporting).
    pub(super) rule_stack: Vec<RuleId>,

    /// Diagnostics collected during parsing.
    pub(super) diagnostics: Vec<ParseDiagnostic>,

    /// Furthest offset reached (for best-effort error location).
    pub(super) furthest_offset: u32,

    /// Rule active at the furthest offset (for error context).
    pub(super) furthest_rule: Option<RuleId>,

    /// Enable step-by-step trace output to stderr (for debugging).
    pub trace: bool,

    /// Interactive debug state. When `None`, `DebugBreak` is a no-op.
    pub debug_state: Option<DebugState>,
}

impl<'prog> Interpreter<'prog> {
    #[inline(always)]
    pub fn new(program: &'prog BytecodeProgram, input: &'prog str) -> Self {
        Self {
            program,
            input,
            input_bytes: input.as_bytes(),
            pc: 0,
            offset: 0,
            is_error: false,
            values: Vec::with_capacity(64),
            call_stack: Vec::with_capacity(32),
            checkpoints: Vec::with_capacity(32),
            repeats: Vec::with_capacity(16),
            memo: FxHashMap::default(),
            memo_starts: Vec::with_capacity(8),
            value_depth_stack: Vec::with_capacity(8),
            rule_stack: Vec::with_capacity(16),
            diagnostics: Vec::new(),
            furthest_offset: 0,
            furthest_rule: None,
            trace: false,
            debug_state: None,
        }
    }

    /// Snapshot of the current rule stack (for debug adapters).
    pub fn rule_stack_snapshot(&self) -> &[RuleId] {
        &self.rule_stack
    }

    /// Execute the program and return the parse result.
    pub fn run(&mut self) -> ParseResult {
        self.pc = self.program.rule_entry(self.program.entry);

        loop {
            let pc = self.pc as usize;
            if pc >= self.program.code.len() {
                break;
            }

            if self.trace {
                eprintln!(
                    "  pc={:3} off={:3} err={} vs={} cs={} cp={} | {:?}",
                    self.pc,
                    self.offset,
                    self.is_error as u8,
                    self.values.len(),
                    self.call_stack.len(),
                    self.checkpoints.len(),
                    self.program.code[pc]
                );
            }

            // Borrow the op by reference — extract small payloads by copy.
            // This avoids cloning Arc/Vec for Dispatch ops.
            match self.program.code[pc] {
                Op::MatchString(sid) => self.exec_match_string(sid),
                Op::MatchRegex(sid) => self.exec_match_regex(sid),
                Op::Epsilon => self.exec_epsilon(),
                Op::Jump(t) => {
                    self.pc = t;
                }
                Op::JumpIfFail(t) => {
                    self.pc = if self.is_error { t } else { self.pc + 1 };
                }
                Op::JumpIfOk(t) => {
                    self.pc = if !self.is_error { t } else { self.pc + 1 };
                }
                Op::Call(rule_id) => self.exec_call(rule_id),
                Op::Return => {
                    if !self.exec_return() {
                        break;
                    }
                }
                Op::SaveState => self.exec_save_state(),
                Op::RestoreState => self.exec_restore_state(),
                Op::DropState => self.exec_drop_state(),
                Op::TrimWs => self.exec_trim_ws(),
                Op::TrimWsPattern(sid) => self.exec_trim_ws_pattern(sid),
                Op::RepeatBegin { lo, hi, body_end } => {
                    self.exec_repeat_begin(lo, hi, body_end);
                }
                Op::RepeatEnd => self.exec_repeat_end(),
                Op::CaptureSpan => {
                    self.pc += 1;
                }
                Op::SaveValueDepth => {
                    self.value_depth_stack.push(self.values.len());
                    self.pc += 1;
                }
                Op::DiscardRight => {
                    if let Some(left_right_boundary) = self.value_depth_stack.pop() {
                        self.values.truncate(left_right_boundary);
                    }
                    self.value_depth_stack.pop();
                    self.pc += 1;
                }
                Op::DiscardLeft => {
                    let left_right_boundary = self
                        .value_depth_stack
                        .pop()
                        .expect("DiscardLeft: value_depth_stack missing left_right_boundary");
                    let context_depth = self
                        .value_depth_stack
                        .pop()
                        .expect("DiscardLeft: value_depth_stack missing context_depth");
                    if left_right_boundary > context_depth
                        && left_right_boundary <= self.values.len()
                    {
                        self.values.drain(context_depth..left_right_boundary);
                    }
                    self.pc += 1;
                }
                Op::MakeArray(count) => self.exec_make_array(count),
                Op::MakeTagged(tag) => self.exec_make_tagged(tag),
                Op::MemoCheck {
                    rule_id,
                    hit_offset,
                } => self.exec_memo_check(rule_id, hit_offset),
                Op::MemoStore(rule_id) => self.exec_memo_store(rule_id),
                Op::Dispatch(table_idx) => {
                    let data = &self.program.dispatch_tables[table_idx as usize];
                    // Copy the small values we need before the mutable dispatch call.
                    let fallback = data.fallback;
                    let byte = if (self.offset as usize) < self.input_bytes.len() {
                        self.input_bytes[self.offset as usize]
                    } else {
                        255
                    };
                    let branch_idx = if byte < 128 {
                        data.table[byte as usize]
                    } else {
                        255
                    };
                    if (branch_idx as usize) < data.offsets.len() {
                        self.pc = data.offsets[branch_idx as usize];
                    } else {
                        self.is_error = true;
                        self.pc = fallback;
                    }
                }
                Op::DispatchToken(table_idx) => self.exec_dispatch_token(table_idx),
                Op::DebugBreak { rule_id, is_entry } => {
                    if let Some(ref mut dbg) = self.debug_state {
                        dbg.trace.push(TraceEntry {
                            pc: self.pc,
                            offset: self.offset,
                            rule_id,
                            is_entry,
                        });
                        let should_break = match dbg.step_mode {
                            StepMode::Continue => dbg.breakpoints.contains(&rule_id),
                            StepMode::StepRule | StepMode::StepNode => true,
                            StepMode::StepInstruction => true,
                        };
                        if should_break {
                            let snapshot = DebugSnapshot {
                                pc: self.pc,
                                offset: self.offset,
                                rule_stack: self.rule_stack.clone(),
                                rule_id,
                                is_entry,
                                is_error: self.is_error,
                                values_depth: self.values.len(),
                            };
                            let action = (dbg.on_break)(&snapshot);
                            match action {
                                DebugAction::Continue => dbg.step_mode = StepMode::Continue,
                                DebugAction::StepRule => dbg.step_mode = StepMode::StepRule,
                                DebugAction::StepNode => dbg.step_mode = StepMode::StepNode,
                                DebugAction::StepInstruction => {
                                    dbg.step_mode = StepMode::StepInstruction;
                                }
                                DebugAction::Stop => break,
                            }
                        }
                    }
                    self.pc += 1;
                }
                Op::Minus | Op::Negate | Op::Nop => {
                    self.pc += 1;
                }
                Op::Halt => break,
            }
        }

        let value = self.values.pop();
        let success = !self.is_error && value.is_some();

        // On failure, generate a diagnostic from FOLLOW sets at the furthest offset.
        if !success && self.diagnostics.is_empty() {
            self.emit_furthest_diagnostic();
        }

        ParseResult {
            success,
            value,
            offset: self.offset,
            diagnostics: std::mem::take(&mut self.diagnostics),
        }
    }

    /// Allocate a slice of the value stack into the bump slab and truncate
    /// the live stack down to `start`. Used by `MakeArray`, `MakeTagged`,
    /// and the repeat finalizer.
    #[inline(always)]
    pub(super) fn collect_values_from(&mut self, start: usize) -> ValueSlice {
        let collected = ValueSlice::from_vec(self.values[start..].to_vec());
        self.values.truncate(start);
        collected
    }

    /// Track the furthest offset reached for best-effort error reporting.
    #[inline(always)]
    pub(super) fn track_furthest(&mut self) {
        if self.offset > self.furthest_offset {
            self.furthest_offset = self.offset;
            self.furthest_rule = self.rule_stack.last().copied();
        }
    }

    /// Emit a diagnostic at the furthest offset using FOLLOW sets.
    fn emit_furthest_diagnostic(&mut self) {
        let rule_name = self.furthest_rule.and_then(|rid| {
            self.program
                .rule_names
                .get(rid as usize)
                .and_then(|&sid| self.program.strings.get(sid as usize).cloned())
        });

        let expected: Vec<u8> = self
            .furthest_rule
            .and_then(|rid| self.program.follow_sets.get(&rid))
            .map(|cs| cs.iter().collect())
            .unwrap_or_default();

        if rule_name.is_some() || !expected.is_empty() {
            self.diagnostics.push(ParseDiagnostic {
                offset: self.furthest_offset,
                rule_name,
                expected,
            });
        }
    }
}

impl<'prog> fmt::Debug for ParseResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ParseResult")
            .field("value", &self.value)
            .field("offset", &self.offset)
            .field("success", &self.success)
            .field("diagnostics", &self.diagnostics)
            .finish()
    }
}

// ── Public API ──────────────────────────────────────────────────────────────

/// Convenience function: compile IR and parse input.
pub fn parse_with_ir(ir: &crate::GrammarIR, input: &str) -> ParseResult {
    let program = super::compiler::compile(ir);
    let mut interp = Interpreter::new(&program, input);
    interp.run()
}
