//! Bytecode interpreter for grammar VMs.
//!
//! Executes a `BytecodeProgram` against an input string, producing a parse tree.
//! The interpreter uses a tight loop with explicit stacks for call frames,
//! checkpoints, and values.

use std::collections::HashMap;
use std::rc::Rc;

use crate::bytecode::{BytecodeProgram, Op};
use crate::RuleId;

// ── Value types ─────────────────────────────────────────────────────────────

/// A generic parse tree node produced by the interpreter.
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    /// A matched span of input: (start, end) byte offsets.
    Span(u32, u32),
    /// A tagged node (rule match).
    Tagged {
        tag: RuleId,
        span: (u32, u32),
        children: Vec<Value>,
    },
    /// An array of values (from repeat/collect).
    Array(Vec<Value>),
    /// Nothing matched (Epsilon).
    Nil,
}

/// Extract the start offset from a Value (for span computation in MakeTagged).
fn value_start(val: &Value) -> u32 {
    match val {
        Value::Span(s, _) => *s,
        Value::Tagged { span, .. } => span.0,
        Value::Array(items) => items.first().map(value_start).unwrap_or(0),
        Value::Nil => 0,
    }
}

// ── Internal stack frames ───────────────────────────────────────────────────

#[derive(Clone, Debug)]
struct CallFrame {
    return_pc: u32,
    /// Input offset when the call started (for accurate span computation in MakeTagged).
    start_offset: u32,
    /// Values stack depth when the call started (for collecting all children in MakeTagged).
    value_depth: usize,
}

#[derive(Clone, Debug)]
struct Checkpoint {
    offset: u32,
    value_depth: usize,
}

#[derive(Clone, Debug)]
struct RepeatState {
    count: u32,
    lo: u32,
    hi: u32,
    body_start: u32,
    body_end: u32,
    value_depth: usize,
    iter_start_offset: u32,
}

// ── Parse result ────────────────────────────────────────────────────────────

/// A diagnostic message from the interpreter (parse error with context).
#[derive(Clone, Debug)]
pub struct ParseDiagnostic {
    /// Byte offset where the error occurred.
    pub offset: u32,
    /// Rule that was being parsed when the error occurred.
    pub rule_name: Option<String>,
    /// Characters that were expected at this position (from FOLLOW set).
    pub expected: Vec<u8>,
}

#[derive(Clone, Debug)]
pub struct ParseResult {
    pub value: Option<Value>,
    pub offset: u32,
    pub success: bool,
    /// Diagnostics collected during parsing (populated when FOLLOW sets are available).
    pub diagnostics: Vec<ParseDiagnostic>,
}

// ── Interpreter ─────────────────────────────────────────────────────────────

pub struct Interpreter<'a> {
    program: &'a BytecodeProgram,
    input: &'a str,
    input_bytes: &'a [u8],

    pc: u32,
    offset: u32,
    is_error: bool,

    values: Vec<Rc<Value>>,
    call_stack: Vec<CallFrame>,
    checkpoints: Vec<Checkpoint>,
    repeats: Vec<RepeatState>,
    memo: HashMap<(RuleId, u32), (u32, Rc<Value>, bool)>,
    regex_cache: HashMap<u32, regex::Regex>,

    /// Stack of (rule_id, start_offset) pushed by MemoCheck on cache miss,
    /// popped by MemoStore to key the cache entry at the correct pre-parse offset.
    memo_starts: Vec<(RuleId, u32)>,

    /// Stack of value stack depths for Skip/Next value selection.
    value_depth_stack: Vec<usize>,

    /// Stack of rule IDs tracking the current parse context (for error reporting).
    rule_stack: Vec<RuleId>,

    /// Diagnostics collected during parsing.
    diagnostics: Vec<ParseDiagnostic>,

    /// Furthest offset reached (for best-effort error location).
    furthest_offset: u32,

    /// Rule active at the furthest offset (for error context).
    furthest_rule: Option<RuleId>,

    /// Enable step-by-step trace output to stderr (for debugging).
    pub trace: bool,
}

impl<'a> Interpreter<'a> {
    pub fn new(program: &'a BytecodeProgram, input: &'a str) -> Self {
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
            memo: HashMap::new(),
            regex_cache: HashMap::new(),
            memo_starts: Vec::with_capacity(8),
            value_depth_stack: Vec::with_capacity(8),
            rule_stack: Vec::with_capacity(16),
            diagnostics: Vec::new(),
            furthest_offset: 0,
            furthest_rule: None,
            trace: false,
        }
    }

    /// Execute the program and return the parse result.
    pub fn run(&mut self) -> ParseResult {
        self.pc = self.program.rule_entry(self.program.entry);

        loop {
            if self.pc as usize >= self.program.code.len() {
                break;
            }

            // Clone the op to avoid borrow conflict with &mut self dispatch.
            let op = self.program.code[self.pc as usize].clone();
            if self.trace {
                eprintln!(
                    "  pc={:3} off={:3} err={} vs={} cs={} cp={} | {:?}",
                    self.pc, self.offset, self.is_error as u8,
                    self.values.len(), self.call_stack.len(), self.checkpoints.len(),
                    op
                );
            }
            match op {
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
                    // Pop the left/right boundary depth. Truncate to it (drops right's values).
                    if let Some(left_right_boundary) = self.value_depth_stack.pop() {
                        self.values.truncate(left_right_boundary);
                    }
                    // Also pop the context/left boundary (not used for Skip).
                    self.value_depth_stack.pop();
                    self.pc += 1;
                }
                Op::DiscardLeft => {
                    // Pop the left/right boundary and context/left boundary.
                    // Stack: [context...] [left...] [right...]
                    //         ^context_depth  ^left_right_boundary
                    let left_right_boundary = self.value_depth_stack.pop().unwrap_or(0);
                    let context_depth = self.value_depth_stack.pop().unwrap_or(0);
                    // Drain left's values (between context_depth and left_right_boundary).
                    if left_right_boundary > context_depth && left_right_boundary <= self.values.len() {
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
                Op::Dispatch(ref data) => self.exec_dispatch(&data.table, &data.offsets, data.fallback),
                Op::Minus | Op::Negate | Op::Nop => {
                    self.pc += 1;
                }
                Op::Halt => break,
            }
        }

        // Clear memo to release shared references before unwrapping.
        self.memo.clear();

        let value = self.values.pop().map(|rc| {
            Rc::try_unwrap(rc).unwrap_or_else(|rc| (*rc).clone())
        });
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

    /// Track the furthest offset reached for best-effort error reporting.
    fn track_furthest(&mut self) {
        if self.offset > self.furthest_offset {
            self.furthest_offset = self.offset;
            self.furthest_rule = self.rule_stack.last().copied();
        }
    }

    /// Emit a diagnostic at the furthest offset using FOLLOW sets.
    fn emit_furthest_diagnostic(&mut self) {
        let rule_name = self.furthest_rule.and_then(|rid| {
            self.program.rule_names.get(rid as usize).and_then(|&sid| {
                self.program.strings.get(sid as usize).cloned()
            })
        });

        let expected: Vec<u8> = self.furthest_rule
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

// ── Leaf ops ────────────────────────────────────────────────────────────────

impl<'a> Interpreter<'a> {
    fn exec_match_string(&mut self, sid: u32) {
        let s = &self.program.strings[sid as usize];
        let start = self.offset as usize;
        let end = start + s.len();

        if end <= self.input.len() && &self.input_bytes[start..end] == s.as_bytes() {
            self.values.push(Rc::new(Value::Span(self.offset, end as u32)));
            self.offset = end as u32;
            self.is_error = false;
            self.track_furthest();
        } else {
            self.is_error = true;
            self.track_furthest();
        }
        self.pc += 1;
    }

    fn exec_match_regex(&mut self, sid: u32) {
        let pattern = &self.program.strings[sid as usize];
        let start = self.offset as usize;
        let remaining = &self.input[start..];

        let re = self.regex_cache.entry(sid).or_insert_with(|| {
            let anchored = format!("^(?:{})", pattern);
            regex::Regex::new(&anchored)
                .unwrap_or_else(|e| panic!("Invalid regex in grammar: {}: {}", pattern, e))
        });

        if let Some(m) = re.find(remaining) {
            let end = start + m.end();
            self.values.push(Rc::new(Value::Span(self.offset, end as u32)));
            self.offset = end as u32;
            self.is_error = false;
            self.track_furthest();
        } else {
            self.is_error = true;
            self.track_furthest();
        }
        self.pc += 1;
    }

    fn exec_epsilon(&mut self) {
        self.values.push(Rc::new(Value::Nil));
        self.is_error = false;
        self.pc += 1;
    }
}

// ── Call/Return ─────────────────────────────────────────────────────────────

impl<'a> Interpreter<'a> {
    fn exec_call(&mut self, rule_id: u32) {
        self.call_stack.push(CallFrame {
            return_pc: self.pc + 1,
            start_offset: self.offset,
            value_depth: self.values.len(),
        });
        self.rule_stack.push(rule_id);
        self.pc = self.program.rule_entry(rule_id);
    }

    /// Returns false if top-level return (halt).
    fn exec_return(&mut self) -> bool {
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

impl<'a> Interpreter<'a> {
    fn exec_save_state(&mut self) {
        self.checkpoints.push(Checkpoint {
            offset: self.offset,
            value_depth: self.values.len(),
        });
        self.pc += 1;
    }

    fn exec_restore_state(&mut self) {
        if let Some(cp) = self.checkpoints.pop() {
            self.offset = cp.offset;
            self.values.truncate(cp.value_depth);
            self.is_error = true;
        }
        self.pc += 1;
    }

    fn exec_drop_state(&mut self) {
        self.checkpoints.pop();
        self.pc += 1;
    }

    fn exec_trim_ws(&mut self) {
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
}

// ── Repetition ──────────────────────────────────────────────────────────────

impl<'a> Interpreter<'a> {
    fn exec_repeat_begin(&mut self, lo: u32, hi: u32, body_end: u32) {
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

    fn exec_repeat_end(&mut self) {
        let repeat = self.repeats.last_mut().unwrap();

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
    fn finalize_repeat(&mut self) {
        let repeat = self.repeats.pop().unwrap();
        let depth = repeat.value_depth.min(self.values.len());

        if repeat.count >= repeat.lo {
            let collected: Vec<Value> = self.values.drain(depth..)
                .map(|rc| Rc::try_unwrap(rc).unwrap_or_else(|rc| (*rc).clone()))
                .collect();
            self.values.push(Rc::new(Value::Array(collected)));
            self.is_error = false;
        } else {
            self.values.truncate(depth);
            self.is_error = true;
        }
        self.pc = repeat.body_end;
    }
}

// ── Value construction ──────────────────────────────────────────────────────

impl<'a> Interpreter<'a> {
    fn exec_make_array(&mut self, count: u32) {
        let n = count as usize;
        let start = self.values.len().saturating_sub(n);
        let collected: Vec<Value> = self.values.drain(start..)
            .map(|rc| Rc::try_unwrap(rc).unwrap_or_else(|rc| (*rc).clone()))
            .collect();
        self.values.push(Rc::new(Value::Array(collected)));
        self.pc += 1;
    }

    fn exec_make_tagged(&mut self, tag: u32) {
        // Collect ALL values pushed since the call frame was entered,
        // so sequences produce multiple children instead of losing all but the last.
        let (start, depth) = self.call_stack.last()
            .map(|f| (f.start_offset, f.value_depth))
            .unwrap_or((0, 0));
        let depth = depth.min(self.values.len());
        let children: Vec<Value> = self.values.drain(depth..)
            .map(|rc| Rc::try_unwrap(rc).unwrap_or_else(|rc| (*rc).clone()))
            .collect();
        if !children.is_empty() {
            self.values.push(Rc::new(Value::Tagged {
                tag,
                span: (start, self.offset),
                children,
            }));
        }
        self.pc += 1;
    }
}

// ── Memoization ─────────────────────────────────────────────────────────────

impl<'a> Interpreter<'a> {
    fn exec_memo_check(&mut self, rule_id: u32, hit_offset: u32) {
        let start_offset = self.offset;
        let key = (rule_id, start_offset);
        if let Some((result_offset, value, was_error)) = self.memo.get(&key) {
            self.offset = *result_offset;
            self.values.push(Rc::clone(value)); // O(1) instead of deep clone
            self.is_error = *was_error;
            self.pc = hit_offset;
        } else {
            // Cache miss — record start offset for MemoStore to use.
            self.memo_starts.push((rule_id, start_offset));
            self.pc += 1;
        }
    }

    fn exec_memo_store(&mut self, _rule_id: u32) {
        let value = self.values.last()
            .map(Rc::clone)
            .unwrap_or_else(|| Rc::new(Value::Nil));
        // Pop the start offset saved by MemoCheck.
        let (rule_id, start_offset) = self.memo_starts.pop()
            .expect("MemoStore without matching MemoCheck");
        let key = (rule_id, start_offset);
        self.memo.insert(key, (self.offset, value, self.is_error));
        self.pc += 1;
    }
}

// ── Dispatch ────────────────────────────────────────────────────────────────

impl<'a> Interpreter<'a> {
    fn exec_dispatch(&mut self, table: &[u8], offsets: &[u32], fallback: u32) {
        let byte = if (self.offset as usize) < self.input_bytes.len() {
            self.input_bytes[self.offset as usize]
        } else {
            255
        };

        let branch_idx = if byte < 128 { table[byte as usize] } else { 255 };

        if branch_idx < offsets.len() as u8 {
            self.pc = offsets[branch_idx as usize];
        } else {
            // No matching byte — dispatch fails.
            self.is_error = true;
            self.pc = fallback;
        }
    }
}

// ── Public API ──────────────────────────────────────────────────────────────

/// Convenience function: compile IR and parse input.
pub fn parse_with_ir(ir: &crate::GrammarIR, input: &str) -> ParseResult {
    let program = crate::compiler::compile(ir);
    let mut interp = Interpreter::new(&program, input);
    interp.run()
}
