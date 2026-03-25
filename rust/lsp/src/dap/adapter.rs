//! DAP adapter — grammar compilation, interpreter lifecycle, request handling.
//!
//! `DapAdapter` holds the compiled grammar + interpreter and translates DAP
//! requests into interpreter actions.

use std::collections::HashSet;

use bbnf::pipeline::{compile_grammar, PipelineOptions};
use bbnf_ir::bytecode::BytecodeProgram;
use bbnf_ir::compiler::compile_with_debug;
use bbnf_ir::interpreter::{
    DebugAction, DebugSnapshot, DebugState, Interpreter, ParseResult, StepMode,
};
use bbnf_ir::{GrammarIR, RuleId};

use super::mapping::{self, LineIndex};
use super::protocol::*;

/// Session state for one debug run.
pub struct DapAdapter {
    /// Compiled grammar IR (with source_span + debug metadata).
    pub ir: GrammarIR,
    /// Compiled bytecode program (with source map + DebugBreak opcodes).
    pub program: BytecodeProgram,
    /// Grammar source text (for line↔offset mapping).
    pub grammar_source: String,
    /// Input text to parse.
    pub input: String,
    /// Pre-computed line index for the grammar source.
    pub line_index: LineIndex,
    /// Active breakpoints (rule IDs).
    pub breakpoints: HashSet<RuleId>,
    /// Whether to stop on entry.
    pub stop_on_entry: bool,
    /// Entry rule override.
    pub entry_rule: Option<String>,
}

/// Result of a single execution step.
pub enum StepResult {
    /// Interpreter hit a breakpoint or step boundary.
    Stopped(DebugSnapshot),
    /// Interpreter completed (success or failure).
    Completed(ParseResult),
}

impl DapAdapter {
    /// Compile a grammar and create the adapter.
    pub fn launch(args: &LaunchArgs) -> Result<Self, String> {
        let grammar_source = std::fs::read_to_string(&args.grammar)
            .map_err(|e| format!("Failed to read grammar: {}", e))?;

        let input = if let Some(ref path) = args.input {
            std::fs::read_to_string(path)
                .map_err(|e| format!("Failed to read input file: {}", e))?
        } else if let Some(ref text) = args.input_text {
            text.clone()
        } else {
            String::new()
        };

        let options = PipelineOptions {
            entry_rule: args.entry_rule.clone(),
            ..PipelineOptions::default()
        };

        let ir = compile_grammar(&grammar_source, &options)?;
        let program = compile_with_debug(&ir, true);
        let line_index = LineIndex::new(&grammar_source);

        Ok(Self {
            ir,
            program,
            grammar_source,
            input,
            line_index,
            breakpoints: HashSet::new(),
            stop_on_entry: args.stop_on_entry.unwrap_or(true),
            entry_rule: args.entry_rule.clone(),
        })
    }

    /// Set breakpoints from DAP line numbers. Returns verified breakpoints.
    pub fn set_breakpoints(&mut self, bp_args: &SetBreakpointsArgs) -> Vec<Breakpoint> {
        self.breakpoints.clear();
        let mut verified = Vec::new();

        for (i, sbp) in bp_args.breakpoints.iter().enumerate() {
            if let Some((rule_id, snapped_line)) =
                mapping::resolve_breakpoint(&self.ir, &self.line_index, sbp.line)
            {
                self.breakpoints.insert(rule_id);
                verified.push(Breakpoint {
                    id: Some(i as i64),
                    verified: true,
                    line: Some(snapped_line),
                    message: None,
                });
            } else {
                verified.push(Breakpoint {
                    id: Some(i as i64),
                    verified: false,
                    line: Some(sbp.line),
                    message: Some("No rule at this line".into()),
                });
            }
        }

        verified
    }

    /// Set function breakpoints (by rule name).
    pub fn set_function_breakpoints(&mut self, args: &SetFunctionBreakpointsArgs) -> Vec<Breakpoint> {
        let mut verified = Vec::new();
        for (i, fbp) in args.breakpoints.iter().enumerate() {
            if let Some(rule) = self.ir.find_rule(&fbp.name) {
                self.breakpoints.insert(rule.id);
                verified.push(Breakpoint {
                    id: Some(i as i64),
                    verified: true,
                    line: rule.source_span.as_ref().map(|s| self.line_index.offset_to_line(s.start)),
                    message: None,
                });
            } else {
                verified.push(Breakpoint {
                    id: Some(i as i64),
                    verified: false,
                    line: None,
                    message: Some(format!("Unknown rule: {}", fbp.name)),
                });
            }
        }
        verified
    }

    /// Run the interpreter until it hits a breakpoint or completes.
    ///
    /// Returns snapshots via the provided callback. The interpreter runs
    /// synchronously (parsing is single-threaded).
    pub fn run(&self, step_mode: StepMode) -> (ParseResult, Vec<DebugSnapshot>) {
        let mut snapshots = Vec::new();
        let breakpoints = self.breakpoints.clone();

        let mut interp = Interpreter::new(&self.program, &self.input);
        interp.debug_state = Some(DebugState {
            breakpoints,
            step_mode,
            trace: Vec::new(),
            on_break: Box::new(|snapshot| {
                // In the synchronous model, we just record the first stop
                // and halt. The DAP server will re-run from the trace for
                // subsequent steps.
                DebugAction::Stop
            }),
        });

        // For a richer model, we'd use channels. For now, run to first stop.
        let result = interp.run();

        // Extract trace for replay support.
        if let Some(ref dbg) = interp.debug_state {
            // The last trace entry before Stop is our snapshot.
            if let Some(last) = dbg.trace.last() {
                snapshots.push(DebugSnapshot {
                    pc: last.pc,
                    offset: last.offset,
                    rule_id: last.rule_id,
                    is_entry: last.is_entry,
                    is_error: !result.success,
                    rule_stack: Vec::new(), // Not available after run completes.
                    values_depth: 0,
                });
            }
        }

        (result, snapshots)
    }

    /// Build stack frames from a debug snapshot.
    pub fn build_stack_frames(
        &self,
        snapshot: &DebugSnapshot,
        grammar_path: &str,
    ) -> Vec<StackFrame> {
        let mut frames = Vec::new();

        // Current rule frame.
        let rule = &self.ir.rules[snapshot.rule_id as usize];
        let name = self.ir.get_string(rule.name);
        let (line, col) = rule
            .source_span
            .as_ref()
            .map(|s| (self.line_index.offset_to_line(s.start), 1))
            .unwrap_or((1, 1));

        frames.push(StackFrame {
            id: 0,
            name: name.to_string(),
            source: Some(SourceRef {
                name: Some(grammar_path.to_string()),
                path: Some(grammar_path.to_string()),
            }),
            line,
            column: col,
        });

        // Frames from rule_stack (if available).
        for (i, &rule_id) in snapshot.rule_stack.iter().rev().enumerate() {
            let r = &self.ir.rules[rule_id as usize];
            let rname = self.ir.get_string(r.name);
            let (rline, _) = r
                .source_span
                .as_ref()
                .map(|s| (self.line_index.offset_to_line(s.start), 1))
                .unwrap_or((1, 1));
            frames.push(StackFrame {
                id: (i + 1) as i64,
                name: rname.to_string(),
                source: Some(SourceRef {
                    name: Some(grammar_path.to_string()),
                    path: Some(grammar_path.to_string()),
                }),
                line: rline,
                column: 1,
            });
        }

        frames
    }

    /// Build variables for the parse state scope.
    pub fn build_state_variables(&self, snapshot: &DebugSnapshot) -> Vec<Variable> {
        let preview_start = snapshot.offset.saturating_sub(10) as usize;
        let preview_end = (snapshot.offset as usize + 40).min(self.input.len());
        let preview = if preview_start < self.input.len() {
            &self.input[preview_start..preview_end]
        } else {
            ""
        };

        vec![
            Variable {
                name: "offset".into(),
                value: format!("{}", snapshot.offset),
                ty: Some("u32".into()),
                variables_reference: 0,
            },
            Variable {
                name: "isError".into(),
                value: format!("{}", snapshot.is_error),
                ty: Some("bool".into()),
                variables_reference: 0,
            },
            Variable {
                name: "currentRule".into(),
                value: mapping::rule_name(&self.ir, snapshot.rule_id).to_string(),
                ty: Some("String".into()),
                variables_reference: 0,
            },
            Variable {
                name: "isEntry".into(),
                value: format!("{}", snapshot.is_entry),
                ty: Some("bool".into()),
                variables_reference: 0,
            },
            Variable {
                name: "inputPreview".into(),
                value: format!("{:?}", preview),
                ty: Some("String".into()),
                variables_reference: 0,
            },
        ]
    }
}
