//! DTA runtime driver — Tranche AW Phase 1 (AW.1.1).
//!
//! # Architectural role
//!
//! The driver consumes a compiled [`DtaTable`] and a byte input, walks
//! the grammar's flat state machine forward in one pass, and emits
//! structural records into [`Columns`] plus a parallel `frame_depth`
//! stream and a [`PayloadStream`] of stage-B decode jobs. It is the
//! runtime half of the V3 DTA substrate the emitter lowered as `const
//! DTA_TABLE` in each grammar's `generated.rs`.
//!
//! Post-AW the driver replaces the legacy `fn __<rule>` recursive
//! descent entirely — one parse path, one emission shape.
//!
//! ```text
//!          input bytes
//!               │
//!               ▼
//!    ┌────────────────────┐
//!    │      dta_run       │
//!    │  ─────────────────  │
//!    │   FrameStack walk   │   ─► Columns (structural skeleton)
//!    │   Forward byte scan │   ─► Vec<u8> frame_depth (per-row stamp)
//!    │   PSI job enqueues  │   ─► PayloadStream (typed leaves)
//!    └────────────────────┘
//! ```
//!
//! # Pre-order tape layout (R01 §7 recommendation, orchestrator
//! accept)
//!
//! The forward walk yields pre-order records naturally — each Seq
//! reserves its parent row first, then pushes children into the gap.
//! Post-order was the AV.2 inheritance; pre-order is the W1 adoption
//! so `cursor::child(0)` degrades to the O(1) `idx + 1` lookup the
//! AW.1.10 hard gate requires.
//!
//! # FrameStack
//!
//! A 64-entry inline array ([`STACK_INLINE_DEPTH`]) covers every
//! grammar in the target corpus (max observed nesting: CSS L4 = 12,
//! JSON = 8, Sheets = 6, BBNF = 10). Grammars that exceed this bound
//! spill to the `overflow: Vec<Frame>` tail. The 64-byte cache-line
//! fits two frames at 32 B each; inline-array reads stay in L1 for
//! every realistic depth.
//!
//! Parallel `counters: SmallVec<[u32; 16]>` column — iteration counts
//! for Repeat frames, selected branch indices for Alt frames,
//! shunting-yard precedence cells. Each frame names its slot via
//! `counter_idx`; nested frames reuse memory after their enclosing
//! frames pop. Isomorphic to `DtaSnapshot::counter_regs` so the
//! replay substrate reuses the type.
//!
//! # `frame_depth` inline emission (Stage-C elision — AW.0.1 closure)
//!
//! Every row push stamps `frame_depth[i] = stack.depth()`. One 1 B
//! store per push, in the cache line the stack counter already
//! occupies (L1 hit). Post-W1, [`TapeBuilder::finish`] consumes this
//! stream directly; `derive_frame_depth`'s backward walk (currently
//! gated behind `has_inline_frame_depth`) deletes.
//!
//! # Regex + byte-dispatch hooks
//!
//! Regex matching is delegated to a caller-supplied scanner — the
//! DTA describes the state machine but the regex engine lives in
//! `parse-that`'s scanner suite. The driver accepts a `&dyn
//! RegexScanner` so the tape crate carries no regex dependency edge.
//! Literal matches are pure byte compares.

use crate::columns::Columns;
use crate::dta::{
    DtaFrameKind, DtaRuleId, DtaState, DtaStateId, DtaTable,
};
use crate::kind::TapeKind;
use crate::psi::{PayloadJob, PayloadKind, PayloadStream};
use crate::tape::TapeOffset;

/// Inline frame-stack depth budget.
///
/// Mirrors `STACK_DEPTH_HINT` in the finaliser; rules whose runtime
/// nesting exceeds 64 spill to heap. Empirical corpus maxima:
/// CSS L4 = 12, BBNF self = 10, JSON = 8, Sheets = 6.
pub const STACK_INLINE_DEPTH: usize = 64;

/// Inline counter-register budget. Mirrors `DtaSnapshot::counter_regs`.
pub const COUNTER_INLINE_SLOTS: usize = 16;

// ── FrameStack ──────────────────────────────────────────────────────

/// One live compound frame in the DTA walker.
///
/// Pushed when the driver enters a `Seq` / `Repeat` / `ShuntingYard`
/// state; popped when the child list is exhausted (or the Repeat body
/// fails). Size kept at 32 B so two frames share one cache line.
#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct Frame {
    /// Frame class — drives stack-advance semantics.
    pub kind: DtaFrameKind,
    /// Counter slot index in the parallel [`FrameStack::counters`]
    /// column. `u8::MAX` marks "no counter slot allocated" for frames
    /// whose kind doesn't carry runtime state.
    pub counter_idx: u8,
    /// Cursor into the frame's child list. Semantics per kind:
    ///   - `Seq`: index of the next child state to visit.
    ///   - `Alt`: selected branch index.
    ///   - `Repeat`: current iteration count.
    pub cursor: u16,
    /// Children list for the Seq / Alt variant. Empty for Repeat /
    /// ShuntingYard — those frames read [`Self::repeat_inner`]
    /// instead.
    pub children: &'static [DtaStateId],
    /// Inner body state for Repeat / ShuntingYard frames. For Seq /
    /// Alt the field is unused ([`DtaStateId::NONE`]).
    pub repeat_inner: DtaStateId,
    /// Index of the reserved parent row in [`Columns`]. The Seq /
    /// Repeat / ShuntingYard pushes a structural-leaf placeholder at
    /// frame-enter time; `span_hi` and `child_off` stamp on pop.
    pub parent_rec: u32,
    /// `columns.len()` at push time — the AV `mark_children` offset.
    /// When the frame pops, an empty child run is detected by
    /// `columns.len() == child_mark`.
    pub child_mark: u32,
    /// The frame's kind tag as a tape TapeKind discriminant. Computed
    /// once at push to avoid re-deriving at pop.
    pub tape_kind: TapeKind,
}

/// Byte-aligned stack of live [`Frame`]s.
///
/// `inline` holds the hot 64-frame budget; overflow spills to
/// `overflow` (Vec on heap). `counters` is the parallel u32 column
/// keyed by `Frame::counter_idx`; allocation is monotonic per-frame,
/// memory reuses when the owning frame pops.
pub struct FrameStack {
    inline: [Frame; STACK_INLINE_DEPTH],
    overflow: Vec<Frame>,
    /// Live depth = `inline_len + overflow.len()`.
    inline_len: u8,
    /// Parallel counter register column.
    pub counters: Vec<u32>,
}

impl FrameStack {
    /// Construct an empty frame stack.
    ///
    /// The inline array is uninitialised-equivalent via a dummy
    /// template; live frames overwrite in place as they are pushed.
    #[inline]
    pub fn new() -> Self {
        let template = Frame {
            kind: DtaFrameKind::Seq,
            counter_idx: u8::MAX,
            cursor: 0,
            children: &[],
            repeat_inner: DtaStateId::NONE,
            parent_rec: 0,
            child_mark: 0,
            tape_kind: TapeKind::Rule,
        };
        Self {
            inline: [template; STACK_INLINE_DEPTH],
            overflow: Vec::new(),
            inline_len: 0,
            counters: Vec::with_capacity(COUNTER_INLINE_SLOTS),
        }
    }

    /// Current live depth.
    #[inline]
    pub fn depth(&self) -> u8 {
        self.inline_len
            .saturating_add(self.overflow.len() as u8)
    }

    /// True when every slot is drained.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.inline_len == 0 && self.overflow.is_empty()
    }

    /// Push a frame onto the stack.
    #[inline]
    pub fn push(&mut self, frame: Frame) {
        if (self.inline_len as usize) < STACK_INLINE_DEPTH {
            self.inline[self.inline_len as usize] = frame;
            self.inline_len += 1;
        } else {
            self.overflow.push(frame);
        }
    }

    /// Pop the topmost frame.
    #[inline]
    pub fn pop(&mut self) -> Option<Frame> {
        if let Some(frame) = self.overflow.pop() {
            return Some(frame);
        }
        if self.inline_len == 0 {
            return None;
        }
        self.inline_len -= 1;
        Some(self.inline[self.inline_len as usize])
    }

    /// Mutable reference to the topmost frame.
    #[inline]
    pub fn top_mut(&mut self) -> Option<&mut Frame> {
        if let Some(last) = self.overflow.last_mut() {
            return Some(last);
        }
        if self.inline_len == 0 {
            return None;
        }
        let idx = (self.inline_len - 1) as usize;
        Some(&mut self.inline[idx])
    }
}

impl Default for FrameStack {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

// ── Driver errors ───────────────────────────────────────────────────

/// Error surface for [`dta_run`]. Kept flat — the generated `parse()`
/// converts to its own `ParseErr` shape at the crate boundary.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DtaError {
    /// The driver could not match the current state against the input
    /// at `offset`. `failing_state` carries the `DtaStateId` that
    /// dispatched to no successful branch (or `DtaStateId::NONE` when
    /// the error is a byte-dispatch miss).
    Syntax {
        offset: u32,
        failing_state: DtaStateId,
        failing_rule: DtaRuleId,
    },
    /// The driver exhausted the DTA table without reaching a valid
    /// terminal state — either the table is malformed or the input
    /// contains trailing bytes beyond the entry rule's match.
    UnexpectedEnd { offset: u32 },
    /// The DTA state table references a state id outside its bounds.
    InvalidState { state: DtaStateId },
}

// ── Regex scanner trait ─────────────────────────────────────────────

/// Scanner hook supplied by the caller for [`DtaState::Regex`] arms.
///
/// The tape crate cannot depend on a regex engine directly (leaf-crate
/// invariant: zero inter-crate dependencies beyond std). Callers pass
/// a scanner implementation that converts `(pattern, input[offset..])`
/// into `Some(match_len)` on success, `None` on no match. The bbnf-
/// generated `parse()` wires the scanner to `parse-that`'s regex
/// suite.
pub trait RegexScanner {
    /// Match `pattern` at `input[offset..]`, returning the match
    /// length in bytes on success.
    fn scan(&self, pattern: &str, input: &[u8], offset: usize) -> Option<u32>;
}

// ── Driver entry point ──────────────────────────────────────────────

/// Run the DTA against `input`, populating `columns`, `psi`, and
/// `frame_depth`.
///
/// Returns the tape offset of the root record (index 0 in a
/// well-formed parse) on success. On syntax failure every output
/// buffer holds partial state up to the failure point; the caller is
/// expected to discard the partial tape.
///
/// # Panics
///
/// Debug builds panic when the DTA table references state ids outside
/// its bounds; release builds surface an [`DtaError::InvalidState`].
pub fn dta_run(
    table: &DtaTable,
    input: &[u8],
    scanner: &dyn RegexScanner,
    columns: &mut Columns,
    psi: &mut PayloadStream,
    frame_depth: &mut Vec<u8>,
) -> Result<TapeOffset, DtaError> {
    dta_run_inner(table, input, scanner, columns, psi, frame_depth)
}

/// Replay-enabled variant — feature-gated behind `dta-replay`. When
/// the feature is off, only [`dta_run`] is emitted so LLVM has no
/// `Option` to hoist on the hot path (R01 §5).
#[cfg(feature = "dta-replay")]
pub fn dta_run_with_replay(
    table: &DtaTable,
    input: &[u8],
    scanner: &dyn RegexScanner,
    columns: &mut Columns,
    psi: &mut PayloadStream,
    frame_depth: &mut Vec<u8>,
    decision_log: Option<&mut Vec<u8>>,
) -> Result<TapeOffset, DtaError> {
    dta_run_with_log(table, input, scanner, columns, psi, frame_depth, decision_log)
}

// ── Core walker ─────────────────────────────────────────────────────

fn dta_run_inner(
    table: &DtaTable,
    input: &[u8],
    scanner: &dyn RegexScanner,
    columns: &mut Columns,
    psi: &mut PayloadStream,
    frame_depth: &mut Vec<u8>,
) -> Result<TapeOffset, DtaError> {
    #[cfg(feature = "dta-replay")]
    {
        dta_run_with_log(table, input, scanner, columns, psi, frame_depth, None)
    }
    #[cfg(not(feature = "dta-replay"))]
    {
        dta_run_core(table, input, scanner, columns, psi, frame_depth)
    }
}

#[cfg(feature = "dta-replay")]
fn dta_run_with_log(
    table: &DtaTable,
    input: &[u8],
    scanner: &dyn RegexScanner,
    columns: &mut Columns,
    psi: &mut PayloadStream,
    frame_depth: &mut Vec<u8>,
    mut decision_log: Option<&mut Vec<u8>>,
) -> Result<TapeOffset, DtaError> {
    let mut stack = FrameStack::new();
    let mut pos: u32 = 0;
    let root_rec = columns.len() as u32;

    // Entry state: the first rule entry is the grammar entry.
    let mut state = match table.rule_entries.first() {
        Some(e) => e.state,
        None => return Err(DtaError::InvalidState { state: DtaStateId::NONE }),
    };

    loop {
        if let Some(ref mut log) = decision_log {
            log.push(state.0 as u8);
        }
        match dispatch_one(
            table, input, scanner, columns, psi, frame_depth, &mut stack,
            state, &mut pos,
        )? {
            StepResult::Next(next) => state = next,
            StepResult::Done => break,
        }
    }
    if (pos as usize) < input.len() {
        return Err(DtaError::UnexpectedEnd { offset: pos });
    }
    Ok(TapeOffset(root_rec))
}

#[cfg(not(feature = "dta-replay"))]
fn dta_run_core(
    table: &DtaTable,
    input: &[u8],
    scanner: &dyn RegexScanner,
    columns: &mut Columns,
    psi: &mut PayloadStream,
    frame_depth: &mut Vec<u8>,
) -> Result<TapeOffset, DtaError> {
    let mut stack = FrameStack::new();
    let mut pos: u32 = 0;
    let root_rec = columns.len() as u32;

    let mut state = match table.rule_entries.first() {
        Some(e) => e.state,
        None => return Err(DtaError::InvalidState { state: DtaStateId::NONE }),
    };

    loop {
        match dispatch_one(
            table, input, scanner, columns, psi, frame_depth, &mut stack,
            state, &mut pos,
        )? {
            StepResult::Next(next) => state = next,
            StepResult::Done => break,
        }
    }
    if (pos as usize) < input.len() {
        return Err(DtaError::UnexpectedEnd { offset: pos });
    }
    Ok(TapeOffset(root_rec))
}

/// One step of the walker's main loop.
enum StepResult {
    Next(DtaStateId),
    Done,
}

fn dispatch_one(
    table: &DtaTable,
    input: &[u8],
    scanner: &dyn RegexScanner,
    columns: &mut Columns,
    psi: &mut PayloadStream,
    frame_depth: &mut Vec<u8>,
    stack: &mut FrameStack,
    state: DtaStateId,
    pos: &mut u32,
) -> Result<StepResult, DtaError> {
    let state_idx = state.0 as usize;
    if state_idx >= table.states.len() {
        return Err(DtaError::InvalidState { state });
    }
    match table.states[state_idx] {
        DtaState::Epsilon => {
            // No column emission, no byte advance — step to the
            // parent's next child (or terminate).
            advance_or_pop(columns, frame_depth, stack, *pos)
        }
        DtaState::Literal { text } => {
            let bytes = text.as_bytes();
            let start = *pos as usize;
            let end = start.saturating_add(bytes.len());
            if end > input.len() || &input[start..end] != bytes {
                return Err(DtaError::Syntax {
                    offset: *pos,
                    failing_state: state,
                    failing_rule: DtaRuleId(u32::MAX),
                });
            }
            let lo = *pos;
            *pos = end as u32;
            emit_leaf(columns, frame_depth, stack, TapeKind::Literal, lo, *pos);
            advance_or_pop(columns, frame_depth, stack, *pos)
        }
        DtaState::Regex { pattern } => {
            let match_len = scanner
                .scan(pattern, input, *pos as usize)
                .ok_or(DtaError::Syntax {
                    offset: *pos,
                    failing_state: state,
                    failing_rule: DtaRuleId(u32::MAX),
                })?;
            let lo = *pos;
            *pos = lo + match_len;
            let rec_idx = columns.len() as u32;
            emit_leaf(columns, frame_depth, stack, TapeKind::Span, lo, *pos);
            // Enqueue a PSI job — the PayloadKind classification is
            // the emitter's responsibility; without a per-state kind
            // annotation on the table we default to F64 as the most
            // common numeric payload. The emitter-driven lowering in
            // AW.1.2 threads the right kind through.
            psi.push(PayloadJob::new(rec_idx, lo, *pos, PayloadKind::F64, 0));
            advance_or_pop(columns, frame_depth, stack, *pos)
        }
        DtaState::Seq { children, frame } => {
            // Reserve the parent row — pre-order: parent sits at the
            // lowest index in its subtree, children flow in after.
            // `child_mark` is the column length AFTER the parent row
            // has been reserved, i.e. `parent_rec + 1`. Under
            // pre-order this is where the first child will land; the
            // cursor's AW.1.10 fast path recognises this layout as
            // `child_off == parent + 1` and degrades `child(0)` to
            // an O(1) lookup.
            let parent_rec = columns.len() as u32;
            let tape_kind = frame_to_tape_kind(frame);
            reserve_compound(columns, frame_depth, stack.depth(), tape_kind, *pos);
            let child_mark = columns.len() as u32;
            stack.push(Frame {
                kind: frame,
                counter_idx: u8::MAX,
                cursor: 0,
                children,
                repeat_inner: DtaStateId::NONE,
                parent_rec,
                child_mark,
                tape_kind,
            });
            if children.is_empty() {
                // Degenerate Seq — close immediately.
                close_compound(columns, stack, *pos);
                return advance_or_pop(columns, frame_depth, stack, *pos);
            }
            Ok(StepResult::Next(children[0]))
        }
        DtaState::Ref { target, .. } => {
            if target == DtaStateId::NONE {
                return Err(DtaError::Syntax {
                    offset: *pos,
                    failing_state: state,
                    failing_rule: DtaRuleId(u32::MAX),
                });
            }
            Ok(StepResult::Next(target))
        }
        DtaState::ByteDispatch { table: disp, fallback } => {
            let b = input.get(*pos as usize).copied().unwrap_or(0);
            let next = disp[b as usize];
            let chosen = if next == DtaStateId::NONE { fallback } else { next };
            if chosen == DtaStateId::NONE {
                return Err(DtaError::Syntax {
                    offset: *pos,
                    failing_state: state,
                    failing_rule: DtaRuleId(u32::MAX),
                });
            }
            // Record the branch selection onto the enclosing Alt frame
            // if any — the variant_idx stamp.
            if let Some(top) = stack.top_mut() {
                if matches!(top.kind, DtaFrameKind::Alt) {
                    top.cursor = chosen.0;
                }
            }
            Ok(StepResult::Next(chosen))
        }
        DtaState::AltLinear { branches } => {
            // Linear Alt branches are attempted in order. This arm is
            // reached when FIRST sets did not admit a byte dispatch;
            // the driver commits to each branch in turn and backtracks
            // on failure. W1 implements a single-probe commit — the
            // first branch wins. The AX backtracking substrate turns
            // this into a real backtracking Alt with savepoints.
            if branches.is_empty() {
                return Err(DtaError::Syntax {
                    offset: *pos,
                    failing_state: state,
                    failing_rule: DtaRuleId(u32::MAX),
                });
            }
            if let Some(top) = stack.top_mut() {
                if matches!(top.kind, DtaFrameKind::Alt) {
                    top.cursor = branches[0].0;
                }
            }
            Ok(StepResult::Next(branches[0]))
        }
        DtaState::Repeat { inner, lo: _, hi: _, .. } => {
            // Repeat opens a frame whose counter tracks iteration
            // count; the AV.3.2 counter-optional marker extends this
            // to admit empty bodies. W1 implements the greedy
            // baseline — enter the body, advance-or-pop consults the
            // Repeat frame on each body completion to decide whether
            // to re-enter or emit the compound close.
            //
            // `child_mark = parent + 1` under pre-order — see the
            // AW.1.10 note on `DtaState::Seq` above.
            let parent_rec = columns.len() as u32;
            reserve_compound(columns, frame_depth, stack.depth(), TapeKind::Rule, *pos);
            let child_mark = columns.len() as u32;
            stack.push(Frame {
                kind: DtaFrameKind::Repeat,
                counter_idx: u8::MAX,
                cursor: 0,
                children: &[],
                repeat_inner: inner,
                parent_rec,
                child_mark,
                tape_kind: TapeKind::Rule,
            });
            Ok(StepResult::Next(inner))
        }
        DtaState::ShuntingYard { head, .. } => {
            // The shunting-yard loop collapses an operator-precedence
            // chain into one state. W1 enters the head operand; full
            // operator-precedence dispatch with the precedence table
            // lands in W4.6 (Pratt lowering). For now, treat as a
            // forward ref to the head state — the chain semantics
            // follow the normal Seq / Ref path until the Pratt
            // frontend takes over.
            Ok(StepResult::Next(head))
        }
    }
}

// ── Column emission helpers ─────────────────────────────────────────

/// Emit a structural leaf record. `frame_depth` stamp happens inline
/// (the AW.1.4 Stage-C elision: `derive_frame_depth` no longer runs
/// because this store is the authoritative depth column).
#[inline]
fn emit_leaf(
    columns: &mut Columns,
    frame_depth: &mut Vec<u8>,
    stack: &FrameStack,
    kind: TapeKind,
    span_lo: u32,
    span_hi: u32,
) -> u32 {
    let idx = columns.len() as u32;
    let kind_meta = kind as u8 & 0x0F;
    columns.kinds.push(kind_meta);
    columns.flags.push(0);
    columns.extra.push(0);
    columns.span_lo.push(span_lo);
    columns.span_hi.push(span_hi);
    columns.sib_skip.push(0);
    columns.child_off.push(TapeOffset::NONE);
    frame_depth.push(stack.depth());
    idx
}

/// Reserve a compound row with `span_lo` only; `span_hi` / `child_off`
/// are stamped at frame-pop time.
#[inline]
fn reserve_compound(
    columns: &mut Columns,
    frame_depth: &mut Vec<u8>,
    depth: u8,
    kind: TapeKind,
    span_lo: u32,
) -> u32 {
    let idx = columns.len() as u32;
    let kind_meta = kind as u8 & 0x0F;
    columns.kinds.push(kind_meta);
    columns.flags.push(0);
    columns.extra.push(0);
    columns.span_lo.push(span_lo);
    columns.span_hi.push(span_lo); // provisional — overwritten on close
    columns.sib_skip.push(0);
    columns.child_off.push(TapeOffset::NONE);
    frame_depth.push(depth);
    idx
}

/// Close the topmost compound frame — stamp `span_hi`, `child_off`,
/// and `has_children` on the reserved parent row.
#[inline]
fn close_compound(columns: &mut Columns, stack: &FrameStack, pos: u32) {
    if let Some(frame) = stack_top(stack) {
        let parent = frame.parent_rec as usize;
        let has_children = (columns.len() as u32) > frame.child_mark;
        columns.span_hi[parent] = pos;
        if has_children {
            // Pre-order: first child sits at `parent + 1` — the
            // O(1) `idx + 1` lookup AW.1.10 relies on.
            columns.child_off[parent] = TapeOffset(frame.child_mark);
            columns.flags[parent] |= 0x40;
        }
    }
}

/// Peek at the topmost frame without popping. `FrameStack::top_mut`
/// requires `&mut`; for read-only probing we inline the logic.
#[inline]
fn stack_top(stack: &FrameStack) -> Option<&Frame> {
    if let Some(f) = stack.overflow.last() {
        return Some(f);
    }
    if stack.inline_len == 0 {
        return None;
    }
    Some(&stack.inline[(stack.inline_len - 1) as usize])
}

/// Advance the topmost frame's cursor; pop and propagate when the
/// frame's child list is exhausted. Returns the next state to
/// dispatch, or [`StepResult::Done`] when the stack drains.
fn advance_or_pop(
    columns: &mut Columns,
    _frame_depth: &mut Vec<u8>,
    stack: &mut FrameStack,
    pos: u32,
) -> Result<StepResult, DtaError> {
    loop {
        let Some(top) = stack.top_mut() else {
            return Ok(StepResult::Done);
        };
        match top.kind {
            DtaFrameKind::Seq => {
                top.cursor += 1;
                if (top.cursor as usize) < top.children.len() {
                    return Ok(StepResult::Next(top.children[top.cursor as usize]));
                }
            }
            DtaFrameKind::Alt => {
                // Alt frames resolve on the first successful branch —
                // the ByteDispatch / AltLinear stamp is already
                // recorded in `cursor`. Close and pop.
            }
            DtaFrameKind::Repeat => {
                // Greedy Repeat: re-enter the body. W1 exits after
                // the first iteration — the backtracking substrate
                // in AX turns this into a proper lo..=hi loop.
            }
            DtaFrameKind::ShuntingYard => {
                // Same shape as Alt — the precedence-driven reducer
                // lands in W4.6.
            }
        }
        // Close the compound and pop.
        close_compound(columns, stack, pos);
        stack.pop();
    }
}

// ── DtaSnapshot (AW.1.7) ────────────────────────────────────────────

/// Resumable snapshot of the DTA driver's runtime state.
///
/// Captured at any byte offset so the replay substrate can pause /
/// resume a parse. Feature-gated — the on-by-default build omits the
/// snapshot surface so the cost is zero when the feature is off.
#[cfg(feature = "dta-replay")]
#[derive(Clone, Debug)]
pub struct DtaSnapshot {
    /// The live frame stack at snapshot time. Sized to the inline
    /// budget; overflow frames copy out to the overflow vec.
    pub frame_stack: Vec<Frame>,
    /// Live depth — `frame_stack.len()` modulo the overflow split.
    pub depth: u8,
    /// Counter register column — isomorphic to
    /// [`FrameStack::counters`].
    pub counter_regs: Vec<u32>,
    /// Byte offset the walker had reached when the snapshot was
    /// taken.
    pub byte_offset: u32,
}

// ── Kind resolution helper ─────────────────────────────────────────

#[inline]
fn frame_to_tape_kind(frame: DtaFrameKind) -> TapeKind {
    match frame {
        DtaFrameKind::Seq => TapeKind::Seq,
        DtaFrameKind::Alt => TapeKind::Alt,
        DtaFrameKind::Repeat => TapeKind::Rule,
        DtaFrameKind::ShuntingYard => TapeKind::Rule,
    }
}
