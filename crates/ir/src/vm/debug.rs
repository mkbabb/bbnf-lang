//! Debug types for the bytecode VM: stepping modes, breakpoint state,
//! parse snapshots, and trace entries.

use std::collections::HashSet;

use crate::RuleId;

/// Step mode for interactive debugging.
#[derive(Clone, Debug, PartialEq)]
pub enum StepMode {
    /// Run until a breakpoint is hit.
    Continue,
    /// Stop at the next rule entry/exit.
    StepRule,
    /// Stop at the next `DebugBreak` opcode.
    StepNode,
    /// Stop at every opcode.
    StepInstruction,
}

/// Action returned by the debug callback to control execution.
#[derive(Clone, Debug, PartialEq)]
pub enum DebugAction {
    Continue,
    StepRule,
    StepNode,
    StepInstruction,
    Stop,
}

/// Snapshot of interpreter state at a debug break.
#[derive(Clone, Debug)]
pub struct DebugSnapshot {
    pub pc: u32,
    pub offset: u32,
    pub rule_stack: Vec<RuleId>,
    pub rule_id: RuleId,
    pub is_entry: bool,
    pub is_error: bool,
    pub values_depth: usize,
}

/// A recorded trace entry for deterministic replay.
#[derive(Clone, Debug)]
pub struct TraceEntry {
    pub pc: u32,
    pub offset: u32,
    pub rule_id: RuleId,
    pub is_entry: bool,
}

/// Interactive debug state attached to the interpreter.
///
/// When `None`, `DebugBreak` opcodes are a single branch (`self.pc += 1`) —
/// negligible overhead. When `Some`, enables breakpoints, stepping, and replay.
pub struct DebugState {
    /// Rules with active breakpoints.
    pub breakpoints: HashSet<RuleId>,
    /// Current step mode.
    pub step_mode: StepMode,
    /// Trace log for deterministic replay (`stepBack`).
    pub trace: Vec<TraceEntry>,
    /// Callback invoked when the interpreter hits a debug point.
    pub on_break: Box<dyn FnMut(&DebugSnapshot) -> DebugAction>,
}
