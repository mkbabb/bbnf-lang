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
    DtaFrameKind, DtaRuleId, DtaState, DtaStateId, DtaTable, LiteralPayload, SeqPromote,
};
use crate::kind::TapeKind;
use crate::psi::{PayloadJob, PayloadStream};
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
/// fails). Size widened to 40 B in W2.1 to carry Repeat iteration
/// bookkeeping (`last_pos`, `lo`/`hi`, `counter_optional_flag`) without
/// a second allocation.
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
    /// Byte position at the start of the current iteration (Repeat)
    /// or at frame entry (all other kinds). Used by the Repeat arm to
    /// detect position stagnation for the unbounded `*` case.
    pub last_pos: u32,
    /// Lower bound for Repeat frames (inclusive); unused for other
    /// kinds. Saturated to `u16::MAX`.
    pub lo: u16,
    /// Upper bound for Repeat frames (inclusive); unused for other
    /// kinds. Saturated to `u16::MAX` which the arm treats as
    /// unbounded.
    pub hi: u16,
    /// Repeat counter-optional flag (AV.3.2). Non-zero when the
    /// owning rule appears in `table.counter_optional_rules` — in
    /// that case, position-stagnant iterations count toward `lo`.
    pub counter_optional_flag: u8,
    /// AW-I.W4ζ — rule-entry variant_idx stamp.
    ///
    /// When a `DtaState::Ref { rule, .. }` dispatches to this frame's
    /// state, the driver captures `rule.0 as u8` here so `close_compound`
    /// can stamp the tape record's `variant_idx` field with the owning
    /// rule's discriminant. Without this, every compound would stamp
    /// `variant_idx = 0` (or the Alt branch index), causing
    /// `rule_kind()` to return the first-indexed rule for every record
    /// — the W4 self-host round-trip failure.
    ///
    /// `u8::MAX` indicates "no rule context" — the compound uses the
    /// existing Alt-cursor stamping (for non-rule-entry Alt frames).
    pub variant_idx: u8,
    /// AW-III.W1.6 — Seq → KvPair promotion classification.
    ///
    /// Set from the lifted `DtaState::Seq.promote`. When `KvPair`,
    /// `close_compound` collapses the parent compound + children
    /// into a single flat `TapeKind::KvPair` leaf whose `child_off`
    /// points at the scalar payload's arena offset (mined from the
    /// last child with `payload_in_arena()` set).
    pub promote: SeqPromote,
}

/// One entry on the ShuntingYard reducer's auxiliary operator stack.
///
/// The reducer consults this stack to decide when to emit an operator
/// compound. Each entry records enough to re-emit the binary compound
/// correctly: `op_rule` + `op_discriminant` + the LHS tape offset the
/// compound will point at via `child_off`.
#[derive(Clone, Copy, Debug)]
pub struct OpStackEntry {
    /// Rule id whose variant_idx the runtime threads into the pushed
    /// compound.
    pub op_rule: DtaRuleId,
    /// Which Alt branch index within `op_rule` — the typed payload's
    /// u8 discriminant.
    pub op_discriminant: u8,
    /// Precedence bucket (higher = tighter binding).
    pub precedence: u8,
    /// Left vs right associativity.
    pub associativity: crate::dta::DtaAssociativity,
    /// Tape index of the LHS operand's root — used as `child_off` for
    /// the eventual emitted op compound.
    pub lhs_idx: u32,
    /// Byte offset where the LHS span began — the emitted compound's
    /// `span_lo`.
    pub lhs_span_lo: u32,
}

/// Snapshot of the output buffers + stack state captured at a Repeat
/// frame's iteration boundary.
///
/// The Repeat arm's body-failure logic restores the walker to this
/// snapshot when the body fails with `counter >= lo`, allowing the
/// Repeat to close at the current count rather than propagate the
/// failure. One snapshot per live Repeat frame; slot indexed by the
/// frame's `counter_idx` (which doubles as the iter-savepoint slot).
#[derive(Clone, Copy, Debug)]
pub struct IterSavepoint {
    /// `columns.len()` at iteration start.
    pub cols_len: u32,
    /// `frame_depth.len()` at iteration start.
    pub fd_len: u32,
    /// `psi.len()` at iteration start.
    pub psi_len: u32,
    /// AW-III.W1 — `columns.pay_agg.len()` at iteration start.
    /// Failed iterations that staged typed-leaf constants into the
    /// arena must roll back so subsequent iterations' arena offsets
    /// stay aligned with the surviving record stream.
    pub pay_agg_len: u32,
    /// Byte position at iteration start.
    pub pos: u32,
    /// `FrameStack` length state at iteration start — restored with
    /// [`FrameStack::restore`].
    pub stack: FrameStackSavepoint,
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
    /// Auxiliary operator stack for the ShuntingYard reducer. Kept
    /// alongside the frame stack so savepoints can snapshot it
    /// atomically with the other structural columns. Realistic chain
    /// depths are ≤ 6 (research/03 §2); inline 8 avoids the heap edge
    /// in the hot path while remaining `Vec`-backed for spill safety.
    pub op_stack: Vec<OpStackEntry>,
    /// Parallel iteration-savepoint column for live Repeat frames.
    /// Indexed by the Repeat frame's `counter_idx`. An iteration
    /// savepoint is written at each iteration start so body-failure
    /// handling can restore and close the compound at `counter >=
    /// lo`.
    pub iter_savepoints: Vec<IterSavepoint>,
    /// AW-I.W4ζ — pending rule-entry variant_idx stamp.
    ///
    /// Set by the `DtaState::Ref { rule, .. }` arm to `rule.0 as u8`
    /// before dispatching to the rule's entry state. The next compound
    /// frame push (Seq/Alt/Repeat/ShuntingYard) consumes this value
    /// into `frame.variant_idx`, then clears it back to `u8::MAX`. On
    /// close_compound, the captured variant_idx is stamped into the
    /// tape record's `variant_idx` bits — the rule identity the view
    /// layer's `rule_kind()` dispatch decodes.
    ///
    /// `u8::MAX` indicates "no pending rule context".
    pub pending_variant_idx: u8,
}

/// Savepoint captured by [`FrameStack::savepoint`] — used by the
/// `AltLinear` arm for branch backtracking and by Repeat iteration
/// bookkeeping.
///
/// Records the `inline_len`, `overflow.len()`, `counters.len()`,
/// `op_stack.len()`, and `iter_savepoints.len()` at capture time. On
/// restore, each buffer truncates back to the captured length; inline
/// frames are truncated via `inline_len` alone (the underlying array
/// is overwritten in place by future pushes).
#[derive(Clone, Copy, Debug)]
pub struct FrameStackSavepoint {
    /// `inline_len` at capture time.
    pub inline_len: u8,
    /// `overflow.len()` at capture time.
    pub overflow_len: u32,
    /// `counters.len()` at capture time.
    pub counters_len: u32,
    /// `op_stack.len()` at capture time.
    pub op_stack_len: u32,
    /// `iter_savepoints.len()` at capture time.
    pub iter_savepoints_len: u32,
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
            last_pos: 0,
            lo: 0,
            hi: 0,
            counter_optional_flag: 0,
            variant_idx: u8::MAX,
            promote: SeqPromote::Default,
        };
        Self {
            inline: [template; STACK_INLINE_DEPTH],
            overflow: Vec::new(),
            inline_len: 0,
            counters: Vec::with_capacity(COUNTER_INLINE_SLOTS),
            op_stack: Vec::with_capacity(8),
            iter_savepoints: Vec::with_capacity(COUNTER_INLINE_SLOTS),
            pending_variant_idx: u8::MAX,
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

    /// Capture a savepoint — the full stack's length state so a
    /// caller can restore it after a failed branch probe.
    ///
    /// Infrastructure for the `AltLinear` arm. Not a separate module
    /// because the savepoint is intimately tied to `FrameStack`'s
    /// internal lengths and the two are never used independently.
    #[inline]
    pub fn savepoint(&self) -> FrameStackSavepoint {
        FrameStackSavepoint {
            inline_len: self.inline_len,
            overflow_len: self.overflow.len() as u32,
            counters_len: self.counters.len() as u32,
            op_stack_len: self.op_stack.len() as u32,
            iter_savepoints_len: self.iter_savepoints.len() as u32,
        }
    }

    /// Restore the stack to a prior savepoint. Truncates each backing
    /// buffer to the captured length; no cloning, no deep restore —
    /// only the lengths matter because every push either overwrites
    /// an inline slot or appends to a `Vec`, so future pushes paper
    /// over any stale data beyond the truncation boundary.
    #[inline]
    pub fn restore(&mut self, sp: FrameStackSavepoint) {
        self.inline_len = sp.inline_len;
        self.overflow.truncate(sp.overflow_len as usize);
        self.counters.truncate(sp.counters_len as usize);
        self.op_stack.truncate(sp.op_stack_len as usize);
        self.iter_savepoints.truncate(sp.iter_savepoints_len as usize);
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
        /// Byte offset where the match attempt failed.
        offset: u32,
        /// The DTA state id that dispatched to no successful child.
        failing_state: DtaStateId,
        /// The rule that was active when the failure occurred;
        /// `DtaRuleId(u32::MAX)` when the driver cannot attribute
        /// the failure to a specific rule.
        failing_rule: DtaRuleId,
    },
    /// The driver exhausted the DTA table without reaching a valid
    /// terminal state — either the table is malformed or the input
    /// contains trailing bytes beyond the entry rule's match.
    UnexpectedEnd {
        /// Byte offset where the driver terminated.
        offset: u32,
    },
    /// The DTA state table references a state id outside its bounds.
    InvalidState {
        /// The out-of-range state id the driver encountered.
        state: DtaStateId,
    },
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

    // Entry state: AW-I.W4γ — look up the grammar's authoritative
    // entry rule. Pre-W4γ the walker read `rule_entries.first()`,
    // which surfaced the first-indexed rule — incorrect for grammars
    // whose entry rule is lifted after other rules (e.g. bbnf's
    // `grammar` rule is the last one lifted).
    let mut state = {
        let s = table.rule_entry_for(table.entry);
        if s == DtaStateId::NONE {
            return Err(DtaError::InvalidState { state: DtaStateId::NONE });
        }
        s
    };

    loop {
        if let Some(ref mut log) = decision_log {
            log.push(state.0 as u8);
        }
        match dispatch_one(
            table, input, scanner, columns, psi, frame_depth, &mut stack,
            state, &mut pos,
        ) {
            Ok(StepResult::Next(next)) => state = next,
            Ok(StepResult::Done) => break,
            Err(e @ DtaError::Syntax { .. }) => {
                match handle_repeat_failure(
                    table, input, columns, psi, frame_depth, &mut stack, &mut pos,
                )? {
                    RepeatAbsorbResult::Continue(next) => state = next,
                    RepeatAbsorbResult::Done => break,
                    RepeatAbsorbResult::NotAbsorbed => return Err(e),
                }
            }
            Err(e) => return Err(e),
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

    // AW-I.W4γ: dispatch the grammar's authoritative entry rule, not
    // `rule_entries.first()`.
    let mut state = {
        let s = table.rule_entry_for(table.entry);
        if s == DtaStateId::NONE {
            return Err(DtaError::InvalidState { state: DtaStateId::NONE });
        }
        s
    };

    // AW-I.W4ε: bootstrap ws-trim fallback.
    //
    // The W3.2 DTA lifter silently stripped `IrNode::OptionalWhitespace`
    // from the IR — the W4γ.2 lifter fixed this by emitting explicit
    // `DtaState::WsTrim` states, but the checked-in `generated.rs` was
    // emitted under the pre-W4γ.2 lifter. Until the next regen produces
    // explicit WsTrim states the driver implicitly trims ASCII
    // whitespace at rule-entry dispatch boundaries (grammar-semantic
    // token gaps). Post-regen generated.rs that has explicit WsTrim
    // states is unaffected — the implicit trim is idempotent at
    // positions where ws has already been consumed.
    let ws_fallback = table.states.iter().all(|s| !matches!(s, DtaState::WsTrim { .. }));

    if ws_fallback {
        trim_ascii_ws(input, &mut pos);
    }

    loop {
        if ws_fallback {
            // Idempotent fallback: trim ASCII ws before every
            // dispatch so the stale generated.rs (without explicit
            // `WsTrim` states) still handles the bbnf grammar's
            // `?w` gaps between atoms. Safe for post-regen
            // generated.rs — `WsTrim` semantics are idempotent,
            // so dispatching `WsTrim` at an already-trimmed
            // position is a no-op.
            trim_ascii_ws(input, &mut pos);
        }
        match dispatch_one(
            table, input, scanner, columns, psi, frame_depth, &mut stack,
            state, &mut pos,
        ) {
            Ok(StepResult::Next(next)) => state = next,
            Ok(StepResult::Done) => break,
            Err(e @ DtaError::Syntax { .. }) => {
                match handle_repeat_failure(
                    table, input, columns, psi, frame_depth, &mut stack, &mut pos,
                )? {
                    RepeatAbsorbResult::Continue(next) => state = next,
                    RepeatAbsorbResult::Done => break,
                    RepeatAbsorbResult::NotAbsorbed => return Err(e),
                }
            }
            Err(e) => return Err(e),
        }
    }
    if ws_fallback {
        trim_ascii_ws(input, &mut pos);
    }
    if (pos as usize) < input.len() {
        return Err(DtaError::UnexpectedEnd { offset: pos });
    }
    Ok(TapeOffset(root_rec))
}

/// AW-I.W4ε bootstrap fallback: trim ASCII whitespace in-place at
/// `*pos`. Mirrors `DtaState::WsTrim { pattern: None }` semantics.
#[inline]
fn trim_ascii_ws(input: &[u8], pos: &mut u32) {
    let mut p = *pos as usize;
    while let Some(&b) = input.get(p) {
        match b {
            b' ' | b'\t' | b'\n' | b'\r' => p += 1,
            _ => break,
        }
    }
    *pos = p as u32;
}

/// One step of the walker's main loop.
enum StepResult {
    Next(DtaStateId),
    Done,
}

/// Result of a Repeat-failure absorption attempt.
enum RepeatAbsorbResult {
    /// Absorption fired; the walker should continue with this state.
    Continue(DtaStateId),
    /// Absorption fired AND the stack fully drained — parse is done.
    Done,
    /// No absorbing Repeat in scope — caller propagates the original
    /// error.
    NotAbsorbed,
}

/// Handle a body-failure that occurred inside a live Repeat frame.
///
/// Walks down the frame stack looking for the innermost Repeat frame
/// whose counter (iterations completed) is at least `lo`. If found,
/// restores the walker state to that frame's iteration savepoint,
/// closes the Repeat compound at the current counter, and returns
/// [`RepeatAbsorbResult::Continue`] with the next state the main loop
/// should dispatch, or [`RepeatAbsorbResult::Done`] if the absorbing
/// close drained the stack.
fn handle_repeat_failure(
    table: &DtaTable,
    input: &[u8],
    columns: &mut Columns,
    psi: &mut PayloadStream,
    frame_depth: &mut Vec<u8>,
    stack: &mut FrameStack,
    pos: &mut u32,
) -> Result<RepeatAbsorbResult, DtaError> {
    let mut scan_depth = stack.depth();
    while scan_depth > 0 {
        let idx = (scan_depth - 1) as usize;
        let frame = frame_at(stack, idx);
        if let DtaFrameKind::Repeat = frame.kind {
            let counter_idx = frame.counter_idx as usize;
            let counter_val = stack.counters[counter_idx];
            if counter_val as u32 >= frame.lo as u32 {
                let sp = stack.iter_savepoints[counter_idx];
                columns.truncate(sp.cols_len as usize);
                frame_depth.truncate(sp.fd_len as usize);
                psi.truncate(sp.psi_len as usize);
                // AW-III.W1: roll back staged arena bytes alongside.
                columns.pay_agg.truncate(sp.pay_agg_len as usize);
                stack.restore(sp.stack);
                // AW-I.W4ζ — clear pending rule-entry stamp from the
                // failed iteration body's Refs so it doesn't leak to
                // the next sibling's compound push.
                stack.pending_variant_idx = u8::MAX;
                *pos = sp.pos;
                close_compound(columns, frame_depth, stack, *pos);
                pop_and_release(stack);
                let res = advance_or_pop_with(
                    Some(table), Some(input), columns, frame_depth, psi, stack, pos,
                )?;
                return Ok(match res {
                    StepResult::Next(n) => RepeatAbsorbResult::Continue(n),
                    StepResult::Done => RepeatAbsorbResult::Done,
                });
            }
        }
        scan_depth -= 1;
    }
    Ok(RepeatAbsorbResult::NotAbsorbed)
}

/// Repeat-failure handler constrained to frames above `bound_depth`
/// — used by `try_branch` so a Repeat inside the branch can absorb
/// its own failures without leaking into the Alt's outer context.
fn handle_repeat_failure_bounded(
    table: &DtaTable,
    input: &[u8],
    columns: &mut Columns,
    psi: &mut PayloadStream,
    frame_depth: &mut Vec<u8>,
    stack: &mut FrameStack,
    pos: &mut u32,
    bound_depth: u8,
) -> Result<RepeatAbsorbResult, DtaError> {
    let mut scan_depth = stack.depth();
    while scan_depth > bound_depth {
        let idx = (scan_depth - 1) as usize;
        let frame = frame_at(stack, idx);
        if let DtaFrameKind::Repeat = frame.kind {
            let counter_idx = frame.counter_idx as usize;
            let counter_val = stack.counters[counter_idx];
            if counter_val as u32 >= frame.lo as u32 {
                let sp = stack.iter_savepoints[counter_idx];
                columns.truncate(sp.cols_len as usize);
                frame_depth.truncate(sp.fd_len as usize);
                psi.truncate(sp.psi_len as usize);
                // AW-III.W1: roll back staged arena bytes alongside.
                columns.pay_agg.truncate(sp.pay_agg_len as usize);
                stack.restore(sp.stack);
                // AW-I.W4ζ — clear pending rule-entry stamp from the
                // failed iteration body's Refs so it doesn't leak to
                // the next sibling's compound push.
                stack.pending_variant_idx = u8::MAX;
                *pos = sp.pos;
                close_compound(columns, frame_depth, stack, *pos);
                pop_and_release(stack);
                let res = advance_or_pop_with(
                    Some(table), Some(input), columns, frame_depth, psi, stack, pos,
                )?;
                return Ok(match res {
                    StepResult::Next(n) => RepeatAbsorbResult::Continue(n),
                    StepResult::Done => RepeatAbsorbResult::Done,
                });
            }
        }
        scan_depth -= 1;
    }
    Ok(RepeatAbsorbResult::NotAbsorbed)
}

/// Pop the topmost frame and release its counter slot if it is a
/// Repeat. Centralises the AW-I.W4δ counter-release invariant so
/// every pop site drops the counter/iter_savepoint entry when the
/// popped frame is a Repeat — absent the release, long parses
/// (bbnf.bbnf's ~250-Repeat traversal) exhaust the `u8` counter
/// index space.
#[inline]
fn pop_and_release(stack: &mut FrameStack) -> Option<Frame> {
    let popped = stack.pop();
    if let Some(f) = popped {
        if matches!(f.kind, DtaFrameKind::Repeat) {
            let idx = f.counter_idx as usize;
            if idx < stack.counters.len() {
                stack.counters.truncate(idx);
                stack.iter_savepoints.truncate(idx);
            }
        }
    }
    popped
}

/// Peek at the frame at the given stack depth index (0 = bottom).
#[inline]
fn frame_at(stack: &FrameStack, idx: usize) -> Frame {
    let inline_len = stack.inline_len as usize;
    if idx < inline_len {
        stack.inline[idx]
    } else {
        stack.overflow[idx - inline_len]
    }
}

/// Nested walker that drives dispatch from `entry_state` until the
/// stack depth returns to `stop_depth` (the Alt-frame boundary). On
/// success, returns the next state the outer walker should dispatch,
/// or [`StepResult::Done`] when the stack fully drains. On syntax
/// failure, returns the error so the caller can restore the savepoint
/// and try the next branch.
///
/// Repeat-body failures inside a branch are absorbed by
/// `handle_repeat_failure_bounded` if an enclosing Repeat has
/// `counter >= lo` — the branch continues past the Repeat's
/// successful close. Failures that cannot be absorbed propagate to
/// the AltLinear caller.
fn try_branch(
    table: &DtaTable,
    input: &[u8],
    scanner: &dyn RegexScanner,
    columns: &mut Columns,
    psi: &mut PayloadStream,
    frame_depth: &mut Vec<u8>,
    stack: &mut FrameStack,
    entry_state: DtaStateId,
    pos: &mut u32,
    stop_depth: u8,
) -> Result<StepResult, DtaError> {
    let mut state = entry_state;
    loop {
        match dispatch_one(
            table, input, scanner, columns, psi, frame_depth, stack, state, pos,
        ) {
            Ok(StepResult::Next(next)) => {
                state = next;
                if stack.depth() <= stop_depth {
                    return Ok(StepResult::Next(next));
                }
            }
            Ok(StepResult::Done) => return Ok(StepResult::Done),
            Err(e @ DtaError::Syntax { .. }) => {
                match handle_repeat_failure_bounded(
                    table, input, columns, psi, frame_depth, stack, pos, stop_depth,
                )? {
                    RepeatAbsorbResult::Continue(next) => {
                        state = next;
                        if stack.depth() <= stop_depth {
                            return Ok(StepResult::Next(next));
                        }
                    }
                    RepeatAbsorbResult::Done => return Ok(StepResult::Done),
                    RepeatAbsorbResult::NotAbsorbed => return Err(e),
                }
            }
            Err(e) => return Err(e),
        }
    }
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
            // parent's next child (or terminate). Drop any pending
            // rule-entry stamp since the rule produced no record.
            stack.pending_variant_idx = u8::MAX;
            advance_or_pop_with(Some(table), Some(input), columns, frame_depth, psi, stack, pos)
        }
        DtaState::Literal { text, payload } => {
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
            // AW-III.W1: when the lifter resolved an enclosing `Map {
            // Literal, IntLit/BoolLit/FloatLit }`, the const-folded
            // value lives in `payload`. The leaf is emitted as a
            // payload-bearing `TapeKind::Span` (the matched bytes are
            // the canonical span; the constant rides in the arena via
            // `child_off`). Compound Seq promotion to `TapeKind::
            // KvPair` happens at the enclosing rule's level via
            // `frame_to_tape_kind` when the rule's `payload_layout`
            // matches the KvPair shape. Without payload the legacy
            // structural-only emission survives.
            if payload.is_some() {
                let arena_off = stage_literal_payload_in_arena(columns, payload);
                emit_leaf_with_payload(
                    columns,
                    frame_depth,
                    stack,
                    TapeKind::Span,
                    lo,
                    *pos,
                    arena_off,
                );
            } else {
                emit_leaf(columns, frame_depth, stack, TapeKind::Literal, lo, *pos);
            }
            // AW-I.W4ζ — consume the pending rule-entry stamp.
            stack.pending_variant_idx = u8::MAX;
            advance_or_pop_with(Some(table), Some(input), columns, frame_depth, psi, stack, pos)
        }
        DtaState::Regex { pattern, payload } => {
            let match_len = scanner
                .scan(pattern, input, *pos as usize)
                .ok_or(DtaError::Syntax {
                    offset: *pos,
                    failing_state: state,
                    failing_rule: DtaRuleId(u32::MAX),
                })?;
            let lo = *pos;
            *pos = lo + match_len;
            // AW-III.W1: when the lifter attached a decoder selector,
            // pre-allocate the arena slot here so the leaf's
            // `child_off` carries the offset Stage B will write into.
            // The arena reserves the worst-case width up front so the
            // PSI worker performs only the decode + store, no growth.
            // Fixed-width payloads use [`PayloadKind::arena_byte_width`];
            // variable-width payloads (`String`/`AggregateLarge`) reserve
            // the matched byte run.
            let (rec_idx, child_off) = match payload {
                None => {
                    let rec_idx = columns.len() as u32;
                    emit_leaf(columns, frame_depth, stack, TapeKind::Span, lo, *pos);
                    (rec_idx, TapeOffset::NONE)
                }
                Some(kind) => {
                    let width = match kind.arena_byte_width() {
                        0 => match_len as usize,
                        w => w,
                    };
                    let arena_off = columns.pay_agg.len() as u32;
                    columns.pay_agg.resize(arena_off as usize + width, 0);
                    let rec_idx = columns.len() as u32;
                    emit_leaf_with_payload(
                        columns,
                        frame_depth,
                        stack,
                        TapeKind::Span,
                        lo,
                        *pos,
                        TapeOffset(arena_off),
                    );
                    psi.push(PayloadJob::new(rec_idx, lo, *pos, kind, arena_off));
                    (rec_idx, TapeOffset(arena_off))
                }
            };
            let _ = (rec_idx, child_off);
            // AW-I.W4ζ — consume the pending rule-entry stamp.
            stack.pending_variant_idx = u8::MAX;
            advance_or_pop_with(Some(table), Some(input), columns, frame_depth, psi, stack, pos)
        }
        DtaState::Seq { children, frame, promote } => {
            // Reserve the parent row — pre-order: parent sits at the
            // lowest index in its subtree, children flow in after.
            // `child_mark` is the column length AFTER the parent row
            // has been reserved, i.e. `parent_rec + 1`. Under
            // pre-order this is where the first child will land; the
            // cursor's AW.1.10 fast path recognises this layout as
            // `child_off == parent + 1` and degrades `child(0)` to
            // an O(1) lookup.
            //
            // AW-III.W1.6 — when `promote == KvPair`, the parent row
            // is provisionally pushed as a KvPair leaf (the same
            // structural slot the Seq compound would have used) and
            // `close_compound` collapses the children into a flat
            // record at frame-pop time. The provisional kind is the
            // same `Seq` placeholder pre-W1 used so `child_mark`
            // arithmetic stays uniform; the `promote` field on the
            // frame is what triggers the close-time collapse.
            let parent_rec = columns.len() as u32;
            let tape_kind = frame_to_tape_kind(frame);
            reserve_compound(columns, frame_depth, stack.depth(), tape_kind, *pos);
            let child_mark = columns.len() as u32;
            // AW-I.W4ζ — consume pending rule-entry stamp. The Ref
            // arm set this to `rule.0 as u8` before dispatching here;
            // capturing it onto the frame lets close_compound stamp
            // the correct variant_idx so `rule_kind()` returns the
            // owning rule's enum variant.
            let variant_idx = stack.pending_variant_idx;
            stack.pending_variant_idx = u8::MAX;
            stack.push(Frame {
                kind: frame,
                counter_idx: u8::MAX,
                cursor: 0,
                children,
                repeat_inner: DtaStateId::NONE,
                parent_rec,
                child_mark,
                tape_kind,
                last_pos: *pos,
                lo: 0,
                hi: 0,
                counter_optional_flag: 0,
                variant_idx,
                promote,
            });
            if children.is_empty() {
                // Degenerate Seq — close immediately.
                close_compound(columns, frame_depth, stack, *pos);
                return advance_or_pop_with(
                    Some(table), Some(input), columns, frame_depth, psi, stack, pos,
                );
            }
            Ok(StepResult::Next(children[0]))
        }
        DtaState::Ref { rule, target } => {
            // AW-I.W4δ — resolve unresolved Refs via `rule_entry_for`.
            // The lifter marks a Ref's `target` with `DtaStateId::NONE`
            // when the referenced rule is lifted later than the
            // dispatching Ref's state — forward references in the
            // rule graph. The driver finishes the resolution at parse
            // time by looking the rule id up in
            // [`DtaTable::rule_entries`] (a sorted binary-search
            // table, log₂(rule_count) ≈ 6 comparisons for the BBNF
            // grammar's 53 rules). Refs whose target IS pre-resolved
            // are dispatched directly, skipping the lookup.
            //
            // Pre-W4δ the arm errored on `target == NONE`, which
            // surfaced as a Syntax failure at every forward-reference
            // site. BBNF self-hosting exercised 11 such sites (Ref to
            // rule `rhs` from inside `term`, among others) and every
            // paren-expression rule body traversed at least one —
            // hence `a = ( "x" ) ;` failed where `a = "x" ;` succeeded.
            let chosen = if target == DtaStateId::NONE {
                table.rule_entry_for(rule)
            } else {
                target
            };
            if chosen == DtaStateId::NONE {
                return Err(DtaError::Syntax {
                    offset: *pos,
                    failing_state: state,
                    failing_rule: rule,
                });
            }
            // AW-I.W4ζ — stamp the rule's discriminant so the next
            // compound push captures it as the tape record's
            // variant_idx. Without this, rule_kind() would decode
            // every record as the first-indexed rule. See Frame's
            // `variant_idx` field + close_compound's stamping logic.
            stack.pending_variant_idx = (rule.0 & 0x3F) as u8;
            Ok(StepResult::Next(chosen))
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
            // Linear Alt: attempt each branch in order with savepoint
            // backtracking. On entry we push an Alt frame that will
            // wrap the chosen branch in a compound; the Alt frame's
            // `cursor` records the selected branch index for the
            // emitter's variant_idx stamp.
            //
            // The savepoint restore truncates `psi` alongside columns
            // and frame_depth — this is the AV.0.1 Bug-1 carry-forward:
            // a branch's payload writes land on the correct side of
            // the savepoint, so subsequent branches see a clean PSI.
            if branches.is_empty() {
                return Err(DtaError::Syntax {
                    offset: *pos,
                    failing_state: state,
                    failing_rule: DtaRuleId(u32::MAX),
                });
            }

            let start_depth = stack.depth();
            let start_pos = *pos;

            // Reserve an Alt compound frame. The branch's subtree is
            // emitted into the compound's child run; on successful
            // branch close, the Alt frame's cursor carries the branch
            // index (the variant_idx).
            let parent_rec = columns.len() as u32;
            reserve_compound(columns, frame_depth, start_depth, TapeKind::Alt, *pos);
            let child_mark = columns.len() as u32;
            // AW-I.W4ζ — consume pending rule-entry stamp. For an Alt
            // reached directly through a Ref (rule body IS an Alt),
            // the rule's variant_idx wins over the branch index; the
            // sub-variant (branch) is carried separately in meta_idx
            // downstream, mirroring pre-W3 fn-per-rule semantics.
            let variant_idx = stack.pending_variant_idx;
            stack.pending_variant_idx = u8::MAX;
            stack.push(Frame {
                kind: DtaFrameKind::Alt,
                counter_idx: u8::MAX,
                cursor: 0,
                children: &[],
                repeat_inner: DtaStateId::NONE,
                parent_rec,
                child_mark,
                tape_kind: TapeKind::Alt,
                last_pos: *pos,
                lo: 0,
                hi: 0,
                counter_optional_flag: 0,
                variant_idx,
                promote: SeqPromote::Default,
            });

            // Savepoint AFTER pushing the Alt frame so a failed branch
            // restores the stack to exactly "Alt frame pushed, no body
            // yet".
            let sp_after_push = stack.savepoint();
            let cols_len_after_push = columns.len();
            let fd_len_after_push = frame_depth.len();
            let psi_len_after_push = psi.len();
            // AW-III.W1: arena snapshot. Failed branches that wrote
            // typed-leaf constants into `pay_agg` (via
            // `stage_literal_payload_in_arena`) must not leak those
            // bytes forward — subsequent successful branches' arena
            // offsets would shift past the orphaned bytes and the
            // record `child_off` ↔ arena byte alignment would
            // desynchronise. Truncating `pay_agg` to the pre-attempt
            // length restores the arena cursor.
            let pay_agg_len_after_push = columns.pay_agg.len();
            // AW-I.W4ζ — snapshot pending_variant_idx so a failed
            // branch's Ref dispatch (which sets pending) does not
            // leak into the next branch. The Alt frame has already
            // consumed its own pending stamp above into
            // `frame.variant_idx`; subsequent Refs inside each branch
            // must start from a clean slate on every attempt.
            let pending_after_push = stack.pending_variant_idx;

            let mut last_err: Option<DtaError> = None;
            for (branch_idx, &branch) in branches.iter().enumerate() {
                *pos = start_pos;
                // Stamp the branch index onto the Alt frame. Writable
                // even after a failed attempt because the cursor is
                // refreshed below; only the successful branch's index
                // persists to close_compound.
                if let Some(top) = stack.top_mut() {
                    top.cursor = branch_idx as u16;
                }
                match try_branch(
                    table,
                    input,
                    scanner,
                    columns,
                    psi,
                    frame_depth,
                    stack,
                    branch,
                    pos,
                    start_depth,
                ) {
                    Ok(next) => return Ok(next),
                    Err(e @ DtaError::Syntax { .. }) => {
                        // Restore columns + frame_depth + psi + stack
                        // back to the post-push savepoint. The psi
                        // truncation closes Bug-1 (AV.0.1) by making
                        // sure the failed branch's payload writes do
                        // not leak into the next branch's view.
                        columns.truncate(cols_len_after_push);
                        frame_depth.truncate(fd_len_after_push);
                        psi.truncate(psi_len_after_push);
                        // AW-III.W1: drop arena bytes the failed
                        // branch staged. Restoring `pay_agg` to its
                        // pre-attempt length keeps arena offsets in
                        // sync with the surviving record stream.
                        columns.pay_agg.truncate(pay_agg_len_after_push);
                        stack.restore(sp_after_push);
                        stack.pending_variant_idx = pending_after_push;
                        last_err = Some(e);
                    }
                    Err(e) => return Err(e),
                }
            }

            // All branches exhausted — propagate the last syntax error.
            // Pop the Alt frame (it never successfully closed) and
            // restore columns/psi past its parent-row reservation.
            // AW-III.W1: also drop staged arena bytes so the next
            // outer-Alt branch sees a clean arena cursor.
            columns.truncate(parent_rec as usize);
            frame_depth.truncate(parent_rec as usize);
            columns.pay_agg.truncate(pay_agg_len_after_push);
            // Stack already restored to post-push; pop the Alt frame.
            pop_and_release(stack);
            Err(last_err.unwrap_or(DtaError::Syntax {
                offset: start_pos,
                failing_state: state,
                failing_rule: DtaRuleId(u32::MAX),
            }))
        }
        DtaState::Repeat { inner, lo, hi, counter_optional } => {
            // Repeat opens a Repeat frame whose counter tracks
            // iteration count. Allocates a counter slot + an
            // iteration savepoint slot; the first iteration begins
            // immediately by dispatching to `inner`.
            //
            // The AV.3.2 `counter_optional` marker is a rule-set
            // membership check — when the body has nested optionals
            // whose empties should count toward `lo`, stagnant
            // iterations do not terminate the loop.
            //
            // Body failure with `counter >= lo` is caught by
            // `handle_repeat_failure` at the walker-loop boundary;
            // the iteration savepoint captured below is the restore
            // target.
            let parent_rec = columns.len() as u32;
            reserve_compound(columns, frame_depth, stack.depth(), TapeKind::Rule, *pos);
            let child_mark = columns.len() as u32;

            // Allocate a counter slot + matching iter-savepoint slot.
            let counter_idx = stack.counters.len();
            if counter_idx >= u8::MAX as usize {
                return Err(DtaError::InvalidState { state });
            }
            stack.counters.push(0);

            let counter_optional_flag = match counter_optional {
                Some(_) => 1u8,
                None => 0u8,
            };

            // Pre-reserve the iter-savepoint slot; the stack field is
            // filled in place AFTER the Repeat frame is pushed.
            stack.iter_savepoints.push(IterSavepoint {
                cols_len: columns.len() as u32,
                fd_len: frame_depth.len() as u32,
                psi_len: psi.len() as u32,
                pay_agg_len: columns.pay_agg.len() as u32,
                pos: *pos,
                stack: FrameStackSavepoint {
                    inline_len: 0,
                    overflow_len: 0,
                    counters_len: 0,
                    op_stack_len: 0,
                    iter_savepoints_len: 0,
                },
            });

            // AW-I.W4ζ — consume pending rule-entry stamp.
            let variant_idx = stack.pending_variant_idx;
            stack.pending_variant_idx = u8::MAX;
            stack.push(Frame {
                kind: DtaFrameKind::Repeat,
                counter_idx: counter_idx as u8,
                cursor: 0,
                children: &[],
                repeat_inner: inner,
                parent_rec,
                child_mark,
                tape_kind: TapeKind::Rule,
                last_pos: *pos,
                lo: saturating_u16(lo),
                hi: saturating_u16(hi),
                counter_optional_flag,
                variant_idx,
                promote: SeqPromote::Default,
            });

            // Fill in the stack savepoint AFTER the push so body
            // failure restores exactly to "Repeat frame present, no
            // body state yet".
            stack.iter_savepoints[counter_idx].stack = stack.savepoint();

            // Handle degenerate `hi == 0` — close immediately.
            if hi == 0 {
                close_compound(columns, frame_depth, stack, *pos);
                return advance_or_pop_with(
                    Some(table), Some(input), columns, frame_depth, psi, stack, pos,
                );
            }

            Ok(StepResult::Next(inner))
        }
        DtaState::WsTrim { pattern } => {
            // AW-I.W4γ: consume whitespace via the grammar's `@ws`
            // regex when set; otherwise fall back to the default
            // ASCII whitespace class (space / tab / newline / CR)
            // matching `bbnf_ir::vm::interpreter::control::exec_trim_ws`.
            // Zero-byte matches are admitted — `?w` is optional,
            // not required.
            //
            // WsTrim never emits a record; any pending rule-entry
            // stamp survives to the next emitting state so a rule
            // whose body is `?w <body>` still tags correctly.
            if let Some(pat) = pattern {
                if let Some(len) = scanner.scan(pat, input, *pos as usize) {
                    *pos += len;
                }
            } else {
                let mut p = *pos as usize;
                while let Some(&b) = input.get(p) {
                    match b {
                        b' ' | b'\t' | b'\n' | b'\r' => p += 1,
                        _ => break,
                    }
                }
                *pos = p as u32;
            }
            advance_or_pop_with(Some(table), Some(input), columns, frame_depth, psi, stack, pos)
        }
        DtaState::Minus { primary, excluded } => {
            // AW-II.W5b — Set-difference: match `primary` only if
            // `excluded` does NOT match at the same start offset.
            // Mirrors the VM compiler's `compile_minus` semantic.
            //
            // Implementation: savepoint at current position, probe
            // `excluded` via `try_branch`; if it succeeds, restore
            // the savepoint and raise a Syntax error (a match that
            // should have been excluded). If it fails, restore the
            // savepoint (so any partial side effects the probe left
            // are discarded) and dispatch `primary` as the next state.
            let start_depth = stack.depth();
            let start_pos = *pos;
            let sp = stack.savepoint();
            let cols_len = columns.len();
            let fd_len = frame_depth.len();
            let psi_len = psi.len();
            let pay_agg_len = columns.pay_agg.len();
            let pending_before = stack.pending_variant_idx;

            let probe = try_branch(
                table,
                input,
                scanner,
                columns,
                psi,
                frame_depth,
                stack,
                excluded,
                pos,
                start_depth,
            );

            // Restore state unconditionally — the probe's work is a
            // lookahead, never consumed into the tape. AW-III.W1
            // adds arena restoration alongside the structural
            // truncation to prevent staged constants from a probe-
            // matched sub-Literal leaking into the post-restore
            // record stream.
            columns.truncate(cols_len);
            frame_depth.truncate(fd_len);
            psi.truncate(psi_len);
            columns.pay_agg.truncate(pay_agg_len);
            stack.restore(sp);
            stack.pending_variant_idx = pending_before;
            *pos = start_pos;

            match probe {
                Ok(_) => {
                    // `excluded` matched → overall Minus fails.
                    Err(DtaError::Syntax {
                        offset: start_pos,
                        failing_state: state,
                        failing_rule: DtaRuleId(u32::MAX),
                    })
                }
                Err(DtaError::Syntax { .. }) => {
                    // `excluded` did not match → dispatch `primary`.
                    Ok(StepResult::Next(primary))
                }
                Err(e) => Err(e),
            }
        }
        DtaState::ShuntingYard { head, .. } => {
            // Shunting-yard entry: reserve the outer compound, push a
            // ShuntingYard frame, and dispatch into `head` to parse
            // the first operand. The operator-precedence reducer
            // lives in `advance_or_pop_with`'s SY arm — after each
            // operand completes, the reducer peeks the next byte,
            // consults the precedence table, and either pushes a
            // new operator onto the op stack (possibly emitting
            // reduced compounds first) or closes the frame if no
            // further operator fires.
            //
            // Operator compounds emitted by the reducer are laid out
            // post-order: the compound's record follows the RHS
            // operand, with `child_off` pointing back at the LHS
            // operand's tape row. The cursor's bounded backward-walk
            // fallback handles this layout; the outer SY compound's
            // `child_off` still satisfies the pre-order fast path
            // (it points at `parent + 1`).
            let parent_rec = columns.len() as u32;
            reserve_compound(columns, frame_depth, stack.depth(), TapeKind::Rule, *pos);
            let child_mark = columns.len() as u32;
            // `repeat_inner` on a ShuntingYard frame stores the SY
            // state id itself, so `advance_or_pop_with`'s reducer can
            // look up both `head` and `precedence` from the table on
            // each operand-complete tick.
            // AW-I.W4ζ — consume pending rule-entry stamp.
            let variant_idx = stack.pending_variant_idx;
            stack.pending_variant_idx = u8::MAX;
            stack.push(Frame {
                kind: DtaFrameKind::ShuntingYard,
                counter_idx: u8::MAX,
                cursor: 0,
                children: &[],
                repeat_inner: state,
                parent_rec,
                child_mark,
                tape_kind: TapeKind::Rule,
                last_pos: *pos,
                lo: 0,
                hi: 0,
                counter_optional_flag: 0,
                variant_idx,
                promote: SeqPromote::Default,
            });
            let _ = head; // head is retrieved from the state at dispatch time
            Ok(StepResult::Next(head))
        }
    }
}

#[inline]
fn saturating_u16(v: u32) -> u16 {
    if v >= u16::MAX as u32 {
        u16::MAX
    } else {
        v as u16
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
    emit_leaf_with_payload(
        columns,
        frame_depth,
        stack,
        kind,
        span_lo,
        span_hi,
        TapeOffset::NONE,
    )
}

/// AW-III.W1 — emit a leaf record with `child_off` stamped to a
/// payload-bearing arena offset.
///
/// Same column-write shape as [`emit_leaf`] except the polymorphic
/// `child_off` slot carries the arena byte offset where the typed
/// payload bytes live (written by [`stage_literal_payload_in_arena`]
/// or the PSI stage-B drain). The record's
/// [`TapeRec::PAYLOAD_IN_ARENA_BIT`] is set when `child_off !=
/// NONE`, signalling to scalar readers (`payload_u8`,
/// `payload_scalar::<T>`) that they should slice `pay_agg` directly
/// instead of indirecting through `pay_narrow` / `pay_wide`.
#[inline]
fn emit_leaf_with_payload(
    columns: &mut Columns,
    frame_depth: &mut Vec<u8>,
    stack: &FrameStack,
    kind: TapeKind,
    span_lo: u32,
    span_hi: u32,
    child_off: TapeOffset,
) -> u32 {
    let idx = columns.len() as u32;
    let kind_meta = kind as u8 & 0x0F;
    columns.kinds.push(kind_meta);
    // AW-I.W4ζ — consume the pending rule-entry variant_idx stamp
    // into this leaf's flags. Leaf rules (`identifier = /regex/`)
    // reach here via `Ref → Regex/Literal`; the rule's discriminant
    // must survive onto the tape so `rule_kind()` can decode it.
    let variant = if stack.pending_variant_idx != u8::MAX {
        stack.pending_variant_idx & 0x3F
    } else {
        0
    };
    columns.flags.push(variant);
    let extra: u16 = if child_off.is_none() {
        0
    } else {
        crate::tape::TapeRec::PAYLOAD_IN_ARENA_BIT
    };
    columns.extra.push(extra);
    columns.span_lo.push(span_lo);
    columns.span_hi.push(span_hi);
    columns.sib_skip.push(0);
    columns.child_off.push(child_off);
    frame_depth.push(stack.depth());
    idx
}

/// AW-III.W1 — write a `LiteralPayload` constant into the tape's
/// arena (`pay_agg`) and return its byte offset.
///
/// Constants from `Map { Literal, MapExpr::IntLit/BoolLit/FloatLit }`
/// are grammar-fixed; the lifter computes the value at compile time
/// (see [`crate::dta::LiteralPayload`]). The walker stages it into
/// the arena post-match so downstream readers find a single source
/// of truth at `arena[child_off..child_off + width]`. PSI is reserved
/// for value-from-input decoders (regex-driven scalar conversion).
#[inline]
fn stage_literal_payload_in_arena(columns: &mut Columns, payload: LiteralPayload) -> TapeOffset {
    let mut buf = [0u8; 8];
    let width = payload.write_le(&mut buf);
    if width == 0 {
        return TapeOffset::NONE;
    }
    let arena = &mut columns.pay_agg;
    let offset = arena.len() as u32;
    arena.extend_from_slice(&buf[..width]);
    TapeOffset(offset)
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

/// Emit a binary-operator compound for the ShuntingYard reducer.
///
/// The compound sits AFTER the RHS operand with `child_off` pointing
/// back at the LHS operand's tape row. Post-order layout for
/// reducer-produced compounds; the cursor's bounded backward-walk
/// fallback resolves the first-child lookup at read time.
#[inline]
fn emit_reducer_compound(
    columns: &mut Columns,
    frame_depth: &mut Vec<u8>,
    depth: u8,
    lhs_idx: u32,
    op_discriminant: u8,
    span_lo: u32,
    span_hi: u32,
) -> u32 {
    let idx = columns.len() as u32;
    let kind_meta = TapeKind::Rule as u8 & 0x0F;
    columns.kinds.push(kind_meta);
    // Pack the op_discriminant into the variant_idx bits (low 6) +
    // stamp the has_children bit (0x40) eagerly — the reducer always
    // has two operand subtrees.
    columns.flags.push((op_discriminant & 0x3F) | 0x40);
    columns.extra.push(0);
    columns.span_lo.push(span_lo);
    columns.span_hi.push(span_hi);
    columns.sib_skip.push(0);
    columns.child_off.push(TapeOffset(lhs_idx));
    frame_depth.push(depth);
    idx
}

/// Lookup the precedence entry for the operator starting at byte `b`
/// (with optional second byte `b2` for two-byte operators).
///
/// Two-byte operators (e.g. `<=`, `>=`) require both bytes to match;
/// a single-byte op entry matches regardless of the second byte.
/// Prefers two-byte matches when available; falls back to single-
/// byte.
#[inline]
fn lookup_precedence(
    precedence: &'static [crate::dta::DtaPrecedenceEntry],
    b: u8,
    b2: Option<u8>,
) -> Option<&'static crate::dta::DtaPrecedenceEntry> {
    if let Some(b2v) = b2 {
        for entry in precedence {
            if entry.byte == b && entry.second_byte == Some(b2v) {
                return Some(entry);
            }
        }
    }
    for entry in precedence {
        if entry.byte == b && entry.second_byte.is_none() {
            return Some(entry);
        }
    }
    None
}

/// Close the topmost compound frame — stamp `span_hi`, `child_off`,
/// and `has_children` on the reserved parent row.
///
/// AW-III.W1.6 — when the frame's `promote == KvPair`, the children
/// (already emitted by the inner state walks) collapse into a single
/// flat `TapeKind::KvPair` leaf at the parent's slot. The parent's
/// `child_off` is rewritten to the scalar payload's arena offset
/// (mined from the last child whose
/// [`TapeRec::PAYLOAD_IN_ARENA_BIT`] is set), the kind byte is
/// repacked from `Seq` → `KvPair`, the `has_children` bit is left
/// clear, and the trailing child records are truncated. Tape size
/// shrinks by `(children_count - 0)` records per KvPair-promoted
/// rule body — the structural saving the layout pass paid for.
#[inline]
fn close_compound(
    columns: &mut Columns,
    frame_depth: &mut Vec<u8>,
    stack: &FrameStack,
    pos: u32,
) {
    if let Some(frame) = stack_top(stack) {
        let parent = frame.parent_rec as usize;
        let has_children = (columns.len() as u32) > frame.child_mark;
        columns.span_hi[parent] = pos;
        // AW-III.W1.6 — KvPair promotion path. Mine the most recent
        // child whose `PAYLOAD_IN_ARENA_BIT` is set (the scalar value
        // of the (key, scalar) pair), flatten into a KvPair leaf,
        // truncate the children. When no payload-bearing child exists
        // (e.g. the body's typed leaf was elided by an upstream
        // optimisation), the promotion is a no-op fallback to the
        // structural Seq close — the layout-pass invariant should
        // make this branch unreachable but failing soft beats panic.
        if matches!(frame.promote, SeqPromote::KvPair) && has_children {
            let mut scalar_arena_off: Option<TapeOffset> = None;
            let parent_extra: u16 = crate::tape::TapeRec::PAYLOAD_IN_ARENA_BIT;
            for i in (frame.child_mark as usize)..columns.len() {
                if (columns.extra[i] & crate::tape::TapeRec::PAYLOAD_IN_ARENA_BIT) != 0 {
                    scalar_arena_off = Some(columns.child_off[i]);
                }
            }
            if let Some(off) = scalar_arena_off {
                // Rewrite parent's kind byte to KvPair (preserve high
                // 4 bits = meta_idx low nibble).
                let meta_hi = columns.kinds[parent] & 0xF0;
                columns.kinds[parent] = meta_hi | (TapeKind::KvPair as u8 & 0x0F);
                columns.child_off[parent] = off;
                columns.extra[parent] = parent_extra;
                // Variant_idx stamping for the KvPair flat record:
                // the rule's variant_idx wins (frame.variant_idx);
                // fall through to the legacy Alt-cursor stamp when
                // no rule context.
                if frame.variant_idx != u8::MAX {
                    let variant = frame.variant_idx & 0x3F;
                    columns.flags[parent] = (columns.flags[parent] & 0xC0) | variant;
                } else if matches!(frame.kind, DtaFrameKind::Alt) {
                    let variant = (frame.cursor as u8) & 0x3F;
                    columns.flags[parent] = (columns.flags[parent] & 0xC0) | variant;
                }
                // Drop has_children — KvPair is a leaf.
                columns.flags[parent] &= !0x40;
                // Truncate the children — the parent is now a self-
                // contained leaf record. Both the structural columns
                // and the parallel `frame_depth` stream shrink to the
                // pre-children mark.
                columns.truncate(frame.child_mark as usize);
                frame_depth.truncate(frame.child_mark as usize);
                return;
            }
            // Fall through to legacy compound close when no
            // payload-bearing child surfaced.
        }
        if has_children {
            // Pre-order: first child sits at `parent + 1` — the
            // O(1) `idx + 1` lookup AW.1.10 relies on.
            columns.child_off[parent] = TapeOffset(frame.child_mark);
            columns.flags[parent] |= 0x40;
        }
        // Stamp variant_idx (low 6 bits). Precedence:
        //   1. `frame.variant_idx` — rule-entry stamp captured from
        //      `FrameStack::pending_variant_idx` at push time. This
        //      encodes the OWNING rule's discriminant so the view
        //      layer's `rule_kind()` decodes correctly.
        //   2. Alt frame's `cursor` — branch index, when no rule
        //      context is set (non-rule-entry Alt compounds).
        // The u8::MAX sentinel indicates "no rule stamp"; fall through
        // to the Alt-cursor path to preserve sub-variant dispatch.
        if frame.variant_idx != u8::MAX {
            let variant = frame.variant_idx & 0x3F;
            columns.flags[parent] = (columns.flags[parent] & 0xC0) | variant;
        } else if matches!(frame.kind, DtaFrameKind::Alt) {
            let variant = (frame.cursor as u8) & 0x3F;
            columns.flags[parent] = (columns.flags[parent] & 0xC0) | variant;
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
/// frame's child list is exhausted.
///
/// Takes optional `table` / `input` references so the Repeat +
/// ShuntingYard arms can consult compile-time metadata and peek
/// operator bytes.
///
/// The Repeat arm reads the frame's counter, consults `lo`/`hi`, and
/// either re-enters `repeat_inner` (capturing a fresh iteration
/// savepoint) or closes at `counter >= lo`. The ShuntingYard arm
/// consults the SY state's precedence table: on each operand
/// completion it peeks the next byte, pushes a new operator
/// (emitting reduced compounds for higher-precedence top-of-stack
/// ops first), or reduces the remaining op stack to completion and
/// closes.
fn advance_or_pop_with(
    _table: Option<&DtaTable>,
    _input: Option<&[u8]>,
    columns: &mut Columns,
    frame_depth: &mut Vec<u8>,
    psi: &mut PayloadStream,
    stack: &mut FrameStack,
    pos: &mut u32,
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
                // the AltLinear arm already stamped `cursor` with the
                // branch index, and the branch's subtree sits inside
                // the Alt compound's child run. Close and pop.
            }
            DtaFrameKind::Repeat => {
                // One iteration completed. Consult lo/hi + position-
                // stagnation to decide whether to re-enter or close.
                // Copy-out the `top` fields first to release the
                // mutable borrow on `stack`.
                let counter_idx = top.counter_idx as usize;
                let iter_start_pos = top.last_pos;
                let counter_optional_flag = top.counter_optional_flag;
                let hi = top.hi;
                let inner = top.repeat_inner;
                let _ = top;

                let counter_val = stack.counters[counter_idx] + 1;
                stack.counters[counter_idx] = counter_val;

                let stagnant = *pos == iter_start_pos;
                let should_close = counter_val as u32 >= hi as u32
                    || (stagnant && counter_optional_flag == 0);

                if should_close {
                    // Fall through to close+pop.
                } else {
                    // Re-enter the body. Refresh the iteration
                    // savepoint + `last_pos` for the next round.
                    //
                    // AW-I.W4δ: refresh psi_len alongside cols/fd.
                    // Pre-W4δ the code preserved the iter-1 psi_len
                    // ("prior_psi_len"), which meant a later-
                    // iteration body-failure would truncate psi back
                    // past already-committed iterations' payload
                    // writes. Use the CURRENT psi.len() so absorbed
                    // failures restore to "end of successful
                    // iterations", not "before the loop".
                    let new_sp_cols = columns.len() as u32;
                    let new_sp_fd = frame_depth.len() as u32;
                    let new_sp_psi = psi.len() as u32;
                    let new_sp_pay_agg = columns.pay_agg.len() as u32;
                    let pos_val = *pos;
                    let new_stack_sp = stack.savepoint();
                    stack.iter_savepoints[counter_idx] = IterSavepoint {
                        cols_len: new_sp_cols,
                        fd_len: new_sp_fd,
                        psi_len: new_sp_psi,
                        pay_agg_len: new_sp_pay_agg,
                        pos: pos_val,
                        stack: new_stack_sp,
                    };
                    if let Some(top2) = stack.top_mut() {
                        top2.last_pos = pos_val;
                    }
                    return Ok(StepResult::Next(inner));
                }
            }
            DtaFrameKind::ShuntingYard => {
                // Operand complete. Consult the precedence table to
                // decide: reduce-and-pop (no more ops), or push a new
                // operator (emitting a reduced compound first if the
                // stack top's precedence demands it).
                let sy_state_id = top.repeat_inner;
                let sy_parent_depth_marker = top.child_mark;
                let sy_parent_rec = top.parent_rec;
                // The operand just finished at span [top.last_pos ..
                // pos]. Track the operand's tape root — the record at
                // `sy_parent_depth_marker` is the first operand; each
                // subsequent operand starts at the length-marker
                // from the prior op-push.
                let mut this_operand_root = top.cursor as u32;
                if this_operand_root == 0 {
                    // First operand: its root sits at the child_mark
                    // (the first record after the outer SY parent).
                    this_operand_root = sy_parent_depth_marker;
                }

                let (head_state, precedence_slice, input_ref) = match (_table, _input) {
                    (Some(t), Some(i)) => {
                        let st = t.states[sy_state_id.0 as usize];
                        match st {
                            DtaState::ShuntingYard { head, precedence } => {
                                (head, precedence, i)
                            }
                            _ => return Err(DtaError::InvalidState { state: sy_state_id }),
                        }
                    }
                    _ => {
                        // Context unavailable — the walker always
                        // supplies table + input for ShuntingYard
                        // dispatch. This arm is unreachable in
                        // practice; failing loud beats silent
                        // misbehaviour.
                        return Err(DtaError::InvalidState { state: sy_state_id });
                    }
                };

                let b = input_ref.get(*pos as usize).copied().unwrap_or(0);
                let b2 = input_ref.get(*pos as usize + 1).copied();
                let entry_opt = lookup_precedence(precedence_slice, b, b2);

                // Reduce top-of-op-stack entries whose precedence
                // exceeds (or ties with, for left-assoc) the new
                // op's precedence; reducing emits a binary compound.
                // If no new op, reduce all pending ops.
                let new_prec = entry_opt.map(|e| e.precedence);
                while let Some(top_op) = stack.op_stack.last().copied() {
                    let should_reduce = match new_prec {
                        None => true, // no new op — reduce all remaining
                        Some(p) => {
                            top_op.precedence > p
                                || (top_op.precedence == p
                                    && matches!(
                                        top_op.associativity,
                                        crate::dta::DtaAssociativity::Left
                                    ))
                        }
                    };
                    if !should_reduce {
                        break;
                    }
                    stack.op_stack.pop();
                    let compound_idx = emit_reducer_compound(
                        columns,
                        frame_depth,
                        stack.depth(),
                        top_op.lhs_idx,
                        top_op.op_discriminant,
                        top_op.lhs_span_lo,
                        *pos,
                    );
                    this_operand_root = compound_idx;
                    let _ = top_op.op_rule;
                }

                if let Some(entry) = entry_opt {
                    // Push the new op onto the stack. Advance past
                    // its bytes (1 or 2). Re-enter `head` to parse
                    // the RHS operand.
                    let op_width = if entry.second_byte.is_some() { 2 } else { 1 };
                    *pos = pos.saturating_add(op_width);
                    let lhs_span_lo = columns
                        .span_lo
                        .get(this_operand_root as usize)
                        .copied()
                        .unwrap_or(*pos);
                    stack.op_stack.push(OpStackEntry {
                        op_rule: entry.op_rule,
                        op_discriminant: entry.op_discriminant,
                        precedence: entry.precedence,
                        associativity: entry.associativity,
                        lhs_idx: this_operand_root,
                        lhs_span_lo,
                    });
                    let pos_val = *pos;
                    if let Some(top) = stack.top_mut() {
                        top.cursor = 0;
                        top.last_pos = pos_val;
                    }
                    return Ok(StepResult::Next(head_state));
                } else {
                    // No operator — the outermost SY frame closes.
                    // The parent compound's child_off points at the
                    // final reduced operand (this_operand_root).
                    // Patch it instead of letting close_compound
                    // default to the frame's `child_mark`.
                    let sy_parent = sy_parent_rec as usize;
                    columns.child_off[sy_parent] = TapeOffset(this_operand_root);
                    columns.flags[sy_parent] |= 0x40;
                    columns.span_hi[sy_parent] = *pos;
                    // Suppress the default close_compound path for
                    // this frame by popping manually and continuing
                    // the outer loop.
                    pop_and_release(stack);
                    continue;
                }
            }
        }
        // Close the compound and pop.
        close_compound(columns, frame_depth, stack, *pos);
        pop_and_release(stack);
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
