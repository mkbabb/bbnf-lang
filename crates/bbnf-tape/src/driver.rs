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
//!    ┌─────────────────────────┐
//!    │  dta_run_<grammar>      │   per-grammar specialised walker
//!    │  ─────────────────────  │   (emitted by W4.b — labels per
//!    │   FrameStack walk       │    DtaState, no enum match in the
//!    │   Forward byte scan     │    output)
//!    │   PSI job enqueues      │   ─► Columns (structural skeleton)
//!    └─────────────────────────┘   ─► Vec<u8> frame_depth (per-row stamp)
//!               ▲                   ─► PayloadStream (typed leaves)
//!               │
//!    ┌─────────────────────────┐
//!    │  dta_run_cold           │   replay-only dispatch loop
//!    │  ─────────────────────  │   (cold path; consults dispatch_one
//!    │   loop { dispatch_one } │    over the DtaState enum match)
//!    └─────────────────────────┘
//! ```
//!
//! # AW-III.W4 — hot/cold split
//!
//! The hot path is the W4.b-emitted per-grammar walker. The cold path
//! is [`dta_run_cold`] — preserved verbatim for the AX replay
//! subsystem, which re-derives a parse's decision trace by single-
//! stepping with the same dispatch loop the original parse would have
//! taken pre-W4. [`dispatch_one`] is the canonical state-machine
//! semantic; the emitted walker is its mechanical lowering.
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
use crate::stage1::StructuralIndex;
use crate::tape::TapeOffset;

// ── Dual cursor (AW-III.W5.c) ───────────────────────────────────────

/// Dual-cursor carrying both the byte position and the structural-index
/// slot the driver consults at each dispatch.
///
/// Pre-W5.c the driver advanced byte-by-byte through `pos: u32`; every
/// `ByteDispatch` arm read `input[*pos]` and every `Regex` arm scanned
/// open-ended into the input tail. With the stage-1 SIMD structural
/// scanner (W5.b) producing a `StructuralIndex` of `(positions, kinds)`
/// pairs at parse-prologue time, the driver's hot path collapses to
/// slot-indexed lookups:
///
/// - `ByteDispatch` reads `idx.kinds[slot]` and advances `slot`.
/// - `Regex` scans bounded by `[pos, idx.positions[slot])`.
/// - `WsTrim` jumps `pos = idx.positions[slot]`.
/// - `ConsumeToNextStructural` collapses to the same one-step jump.
///
/// The driver consults `idx` opportunistically — when the
/// `StructuralIndex` is empty (grammar without stage-1 enrichment, or
/// pre-W5.b scanner integration), the cursor degrades to byte-stepping
/// and the scanner-based fallback. The dual-cursor's invariant: when
/// `idx` is non-empty, `slot` is always `idx.positions.partition_point(|p| *p < pos)`
/// or one past it — `pos` and `slot` are consistent, and any failed
/// branch / iter / probe rewind restores both atomically.
///
/// `src` is the input byte slice; the structure is `'a`-bounded by it
/// + `idx` so the helpers can access either through the cursor handle.
/// The pre-W5.c helpers' `input: &[u8]` parameter survives because the
/// emitted walker keeps the `input` binding visible at every arm body
/// for `cargo asm` parity; the cursor's `src` field mirrors it for
/// callers that prefer to work with the cursor handle alone.
#[derive(Debug)]
pub struct Cursor<'a> {
    /// Input byte slice the cursor scans.
    pub src: &'a [u8],
    /// Stage-1 structural index. Empty when the per-grammar SIMD
    /// scanner has not yet been wired in (pre-W5.b integration); the
    /// dual-cursor's slot-indexed arms then degrade to no-ops and the
    /// byte-stepping fallback survives.
    pub idx: &'a StructuralIndex,
    /// Byte position into `src`. Strictly monotone across a successful
    /// parse; rewound atomically with `slot` on failed Alt branches /
    /// Repeat iter restarts / Minus probes.
    pub pos: u32,
    /// Structural-index slot — the position into `idx.positions` /
    /// `idx.kinds`. Mirrors `pos` such that
    /// `idx.positions[slot] >= pos` always (once `idx` is non-empty).
    pub slot: u32,
}

impl<'a> Cursor<'a> {
    /// Construct a cursor at the start of `src`, anchored to `idx`.
    /// `pos` and `slot` initialise to `0`.
    #[inline]
    pub fn new(src: &'a [u8], idx: &'a StructuralIndex) -> Self {
        Self { src, idx, pos: 0, slot: 0 }
    }

    /// Advance `slot` past the entry whose `position == pos`. Used by
    /// `ByteDispatch` and `ConsumeToNextStructural` after consuming a
    /// structural delimiter.
    #[inline]
    pub fn advance_slot(&mut self) {
        self.slot += 1;
    }

    /// AW-III.W5.c — O(1) jump to the next structural slot's byte
    /// position. Used by `ConsumeToNextStructural` and `WsTrim`'s
    /// stage-1-aware collapse. Returns `false` when no further slot
    /// exists — callers fall back to byte-stepping or terminate.
    #[inline]
    pub fn jump_to_next_structural(&mut self) -> bool {
        let idx = self.slot as usize;
        if let Some(&p) = self.idx.positions.get(idx) {
            self.pos = p;
            true
        } else {
            false
        }
    }
}

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
///
/// AW-III.W5.c — added `slot: u32` so the dual-cursor's structural
/// position snapshots atomically with the stack lengths. Pre-W5.c the
/// AQ-5 unsaved-cursor failure mode was the consequence of a parallel
/// savepoint structure that left `structural_cursor` un-snapshotted;
/// extending the existing record removes the parallel-structure
/// possibility.
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
    /// AW-III.W5.c — structural-index cursor slot at capture time.
    /// The companion to `pos`; restored by [`FrameStack::restore`]'s
    /// caller alongside `pos` so a failed Alt branch / Repeat iter /
    /// Minus probe rewinds the dual cursor atomically.
    pub slot: u32,
}

/// Deep probe-snapshot — captures the live in-place contents of
/// counters / iter_savepoints / inline frames in addition to the
/// length state recorded by [`FrameStackSavepoint`].
///
/// AW-III.W2 — required by the Minus arm and any zero-width probe
/// (lookahead) whose nested dispatch may walk back up the stack and
/// mutate the enclosing Repeat's counter / iter-savepoint slot. The
/// length-only `FrameStackSavepoint` truncates new pushes but cannot
/// rewind in-place mutations to slots that remain live across the
/// probe boundary; the deep snapshot captures those values verbatim
/// so the probe is side-effect-free regardless of the dispatch chain
/// it triggers internally.
///
/// Cost is bounded by `inline_len + overflow_len + counters_len +
/// iter_savepoints_len + op_stack_len` words at probe entry — small
/// in practice (max nesting in the corpus is ≤ 12 frames; counters
/// rarely exceed 8).
#[derive(Clone, Debug)]
pub struct FrameStackProbeSnapshot {
    /// Length-state portion (mirror of `FrameStackSavepoint`).
    /// AW-III.W5.c — exposed as `pub` so emitted walker code can
    /// restore the dual-cursor's `slot` field via
    /// `*slot = probe_snapshot.base.slot;` before invoking
    /// `restore_probe`. Without this access the W4 emitter would have
    /// to add a dedicated accessor; field-level visibility is the
    /// simpler shape.
    pub base: FrameStackSavepoint,
    /// In-place inline frame contents at indices `[0..base.inline_len)`.
    inline_snapshot: Vec<Frame>,
    /// In-place overflow frame contents at indices `[0..base.overflow_len)`.
    overflow_snapshot: Vec<Frame>,
    /// In-place counter values at indices `[0..base.counters_len)`.
    counters_snapshot: Vec<u32>,
    /// In-place iter-savepoint contents at indices `[0..base.iter_savepoints_len)`.
    iter_savepoints_snapshot: Vec<IterSavepoint>,
    /// In-place op-stack contents at indices `[0..base.op_stack_len)`.
    op_stack_snapshot: Vec<OpStackEntry>,
    /// `pending_variant_idx` at probe entry.
    pending_variant_idx: u8,
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

    /// Walk the stack from the topmost frame downward and return the
    /// first frame whose `variant_idx != u8::MAX`.
    ///
    /// AW-III.W1.A — used by [`emit_leaf_with_payload`] to stamp
    /// structural literals (`,` / `]` / `}`) with the nearest
    /// enclosing rule's variant_idx. Pre-W1.A the inheritance
    /// short-circuited on the immediate top frame, so structural
    /// literals nested beneath a transparent-rule body Seq fell back
    /// to `0` (unintentionally aliasing the first-indexed rule);
    /// walking the stack recovers the first non-anonymous frame.
    #[inline]
    pub fn nearest_variant_frame(&self) -> Option<&Frame> {
        // Walk overflow from top down.
        for f in self.overflow.iter().rev() {
            if f.variant_idx != u8::MAX {
                return Some(f);
            }
        }
        // Then walk inline from top down.
        for i in (0..self.inline_len as usize).rev() {
            let f = &self.inline[i];
            if f.variant_idx != u8::MAX {
                return Some(f);
            }
        }
        None
    }

    /// Capture a savepoint — the full stack's length state plus the
    /// dual-cursor's structural slot so a caller can restore the entire
    /// runtime state after a failed branch probe.
    ///
    /// AW-III.W5.c — `slot` parameter added so the structural cursor
    /// snapshots atomically with the stack lengths. The caller threads
    /// its current `cursor.slot` value; `pos` is captured externally
    /// by `IterSavepoint` / branch-savepoint locals because the
    /// `FrameStack` does not own the byte cursor.
    ///
    /// Infrastructure for the `AltLinear` arm and Repeat iteration
    /// boundaries. Not a separate module because the savepoint is
    /// intimately tied to `FrameStack`'s internal lengths and the two
    /// are never used independently.
    #[inline]
    pub fn savepoint(&self, slot: u32) -> FrameStackSavepoint {
        FrameStackSavepoint {
            inline_len: self.inline_len,
            overflow_len: self.overflow.len() as u32,
            counters_len: self.counters.len() as u32,
            op_stack_len: self.op_stack.len() as u32,
            iter_savepoints_len: self.iter_savepoints.len() as u32,
            slot,
        }
    }

    /// Restore the stack to a prior savepoint. Truncates each backing
    /// buffer to the captured length; no cloning, no deep restore —
    /// only the lengths matter because every push either overwrites
    /// an inline slot or appends to a `Vec`, so future pushes paper
    /// over any stale data beyond the truncation boundary.
    ///
    /// AW-III.W5.c — caller is responsible for restoring `pos` and
    /// `slot` from `sp.slot` + the externally-captured `pos`. Returning
    /// the restored slot from this method (vs. caller-managed) would
    /// scatter the cursor invariants; the explicit caller-side restore
    /// keeps the dual-cursor visible at every backtracking site.
    #[inline]
    pub fn restore(&mut self, sp: FrameStackSavepoint) {
        self.inline_len = sp.inline_len;
        self.overflow.truncate(sp.overflow_len as usize);
        self.counters.truncate(sp.counters_len as usize);
        self.op_stack.truncate(sp.op_stack_len as usize);
        self.iter_savepoints.truncate(sp.iter_savepoints_len as usize);
    }

    /// AW-III.W2 — capture a deep snapshot for a probe boundary
    /// (Minus excluded probe; zero-width lookahead). Records both
    /// the length state and the in-place contents of every live
    /// slot so a subsequent [`Self::restore_probe`] reverts every
    /// side effect — including mutations to counters/iter_savepoints/
    /// inline frame fields that the probe's nested dispatch may
    /// trigger via [`advance_or_pop_with`] walking up the stack.
    ///
    /// AW-III.W5.c — `slot` parameter mirrors [`Self::savepoint`]; the
    /// dual cursor's structural index slot snapshots into the embedded
    /// `base.slot` so the probe restore is total.
    pub fn snapshot_probe(&self, slot: u32) -> FrameStackProbeSnapshot {
        let inline_len = self.inline_len as usize;
        let overflow_len = self.overflow.len();
        let counters_len = self.counters.len();
        let iter_savepoints_len = self.iter_savepoints.len();
        let op_stack_len = self.op_stack.len();
        FrameStackProbeSnapshot {
            base: FrameStackSavepoint {
                inline_len: self.inline_len,
                overflow_len: overflow_len as u32,
                counters_len: counters_len as u32,
                op_stack_len: op_stack_len as u32,
                iter_savepoints_len: iter_savepoints_len as u32,
                slot,
            },
            inline_snapshot: self.inline[..inline_len].to_vec(),
            overflow_snapshot: self.overflow.clone(),
            counters_snapshot: self.counters.clone(),
            iter_savepoints_snapshot: self.iter_savepoints.clone(),
            op_stack_snapshot: self.op_stack.clone(),
            pending_variant_idx: self.pending_variant_idx,
        }
    }

    /// AW-III.W2 — restore from a probe snapshot. Reverts both
    /// the length state and the in-place contents of every slot
    /// that was live at probe entry.
    pub fn restore_probe(&mut self, snapshot: FrameStackProbeSnapshot) {
        // Length truncation first — drop pushes that occurred
        // entirely inside the probe.
        self.inline_len = snapshot.base.inline_len;
        self.overflow.truncate(snapshot.base.overflow_len as usize);
        self.counters.truncate(snapshot.base.counters_len as usize);
        self.op_stack.truncate(snapshot.base.op_stack_len as usize);
        self.iter_savepoints.truncate(snapshot.base.iter_savepoints_len as usize);
        // In-place restore — paste the captured contents back over
        // any mutations the probe's nested dispatch made to live
        // slots.
        let inline_len = snapshot.base.inline_len as usize;
        self.inline[..inline_len].copy_from_slice(&snapshot.inline_snapshot);
        // overflow / counters / iter_savepoints / op_stack: the
        // truncate above set the length; now overwrite contents.
        for (i, frame) in snapshot.overflow_snapshot.iter().enumerate() {
            self.overflow[i] = *frame;
        }
        for (i, &v) in snapshot.counters_snapshot.iter().enumerate() {
            self.counters[i] = v;
        }
        for (i, &v) in snapshot.iter_savepoints_snapshot.iter().enumerate() {
            self.iter_savepoints[i] = v;
        }
        for (i, &v) in snapshot.op_stack_snapshot.iter().enumerate() {
            self.op_stack[i] = v;
        }
        self.pending_variant_idx = snapshot.pending_variant_idx;
    }
}

impl Default for FrameStack {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

// ── Driver errors ───────────────────────────────────────────────────

/// Error surface for [`dta_run_cold`] and the per-grammar specialised
/// walker emitted by W4.b's `emit_specialised_walker` pass. Kept flat
/// — the generated `parse()` converts to its own `ParseErr` shape at
/// the crate boundary.
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

/// Cold-path DTA dispatch loop — replay surface only.
///
/// Runs the DTA against `input`, populating `columns`, `psi`, and
/// `frame_depth` by walking [`DtaTable::states`] one state at a time
/// through [`dispatch_one`]. The 14-arm enum match in `dispatch_one`
/// is the canonical state-machine semantics; this function preserves
/// it verbatim so the replay subsystem (AX) can re-derive any parse's
/// decision trace by single-stepping with the same dispatch path the
/// original parse took.
///
/// # AW-III.W4 — hot/cold split (R3 §1.b transposition)
///
/// Pre-W4, every grammar's `parse()` routed through this loop. The
/// 14-arm enum match floored at ~24% self-time (R01 §3, R03 §1
/// per-byte cycle attribution) — log₂(14) ≈ 4 cmp/jmp dispatch +
/// ~15 cyc branch-miss every state visit, multiplied by 3–8 visits
/// per input byte. This was the canonical state-machine-as-tagged-
/// union interpreter overhead.
///
/// Post-W4, every `parse()` routes through a per-grammar
/// `dta_run_<grammar>` function emitted by
/// [`emit_specialised_walker`](crates/core/src/backend/rust/emitter/dta_walker.rs).
/// The emitted walker lowers each [`DtaState`] variant to inlined
/// Rust labels; the enum match disappears at the *output* call site
/// (the table still carries `DtaState` so this cold loop remains
/// semantically authoritative). Helpers
/// ([`emit_leaf`], [`emit_leaf_with_payload`],
/// [`close_compound`], [`advance_or_pop_with`], [`frame_to_tape_kind`],
/// [`stage_literal_payload_in_arena`], [`pop_and_release`],
/// [`stack_top`], [`saturating_u16`], [`emit_reducer_compound`],
/// [`lookup_precedence`], [`first_ws_pattern`], [`trim_with_pattern`],
/// [`trim_ascii_ws`], [`handle_repeat_failure`],
/// [`handle_repeat_failure_bounded`], [`try_branch`], [`frame_at`],
/// [`dispatch_one`]) are all exposed `pub` so the emitted walker
/// can call them directly.
///
/// This function is **never** called from the parse hot path; only
/// the replay subsystem (`dta_run_with_replay`) and the walker-arm
/// regression tests in `tests/walker_arms.rs` consume it.
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
pub fn dta_run_cold(
    table: &DtaTable,
    input: &[u8],
    scanner: &dyn RegexScanner,
    columns: &mut Columns,
    psi: &mut PayloadStream,
    frame_depth: &mut Vec<u8>,
) -> Result<TapeOffset, DtaError> {
    dta_run_inner(table, input, scanner, columns, psi, frame_depth)
}

/// Replay-enabled variant of the cold-path dispatch loop —
/// feature-gated behind `dta-replay`. When the feature is off, only
/// [`dta_run_cold`] is emitted so LLVM has no `Option` to hoist on
/// the cold path (R01 §5).
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
    // AW-III.W5.c — dual cursor's structural slot. The cold-path
    // doesn't run the SIMD scanner, so the index stays empty; the
    // structural-aware arms degrade to byte-stepping fallbacks.
    let mut slot: u32 = 0;
    let idx = StructuralIndex::new();
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
            table, input, scanner, &idx, columns, psi, frame_depth, &mut stack,
            state, &mut pos, &mut slot,
        ) {
            Ok(StepResult::Next(next)) => state = next,
            Ok(StepResult::Done) => break,
            Err(e @ DtaError::Syntax { .. }) => {
                match handle_repeat_failure(
                    table, input, &idx, columns, psi, frame_depth, &mut stack,
                    &mut pos, &mut slot,
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
    // AW-III.W5.c — dual cursor's structural slot. The cold-path
    // doesn't run the SIMD scanner, so the index stays empty; the
    // structural-aware arms degrade to byte-stepping fallbacks. The
    // hot-path emitted walker can supply a populated index once the
    // W5.b/W6 wiring lands; the cold path stays index-free for AX
    // replay determinism.
    let mut slot: u32 = 0;
    let idx = StructuralIndex::new();
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


    // AW-III.W2: ws_fallback removed.
    //
    // The pre-W4γ.2 ws_fallback hack trimmed ASCII whitespace before
    // every dispatch when the table contained no explicit
    // `DtaState::WsTrim` state — covering the gap when stale
    // `generated.rs` was emitted by the pre-W4γ.2 lifter. Post-W4γ.2
    // every `?w` site lifts to an explicit `WsTrim`, so the fallback
    // is dead weight for grammars that DO use `?w` and actively
    // wrong for grammars that don't (CSV's `\n` separator gets eaten
    // before its `\r?\n` regex can match it). Removing it lets
    // grammars that handle whitespace explicitly via regex (CSV,
    // JSON, EBNF) parse correctly while leaving `?w`-using grammars
    // (BBNF, CSS, Sheets) intact via their lifted WsTrim states.
    //
    // Boundary trim: extract the first WsTrim state's pattern from
    // the table (if any) so the leading + trailing whitespace at the
    // input boundary uses the grammar's declared semantics. JSON
    // declares `?w` via its `comma`/`colon` rules but has no
    // top-level wrapper, so the entry rule's first compound expects
    // input bytes immediately. RFC 8259 admits surrounding
    // whitespace; the boundary trim honours that without the
    // grammar needing a synthetic top-level wrapper. Grammars
    // without any `?w` site (CSV) get no boundary trim — their
    // whitespace is structurally meaningful.
    let boundary_ws = first_ws_pattern(table);
    if let Some(pat) = boundary_ws {
        trim_with_pattern(scanner, pat, input, &mut pos);
    }

    loop {
        match dispatch_one(
            table, input, scanner, &idx, columns, psi, frame_depth, &mut stack,
            state, &mut pos, &mut slot,
        ) {
            Ok(StepResult::Next(next)) => state = next,
            Ok(StepResult::Done) => break,
            Err(e @ DtaError::Syntax { .. }) => {
                match handle_repeat_failure(
                    table, input, &idx, columns, psi, frame_depth, &mut stack,
                    &mut pos, &mut slot,
                )? {
                    RepeatAbsorbResult::Continue(next) => state = next,
                    RepeatAbsorbResult::Done => break,
                    RepeatAbsorbResult::NotAbsorbed => return Err(e),
                }
            }
            Err(e) => return Err(e),
        }
    }
    if let Some(pat) = boundary_ws {
        trim_with_pattern(scanner, pat, input, &mut pos);
    }
    if (pos as usize) < input.len() {
        return Err(DtaError::UnexpectedEnd { offset: pos });
    }
    Ok(TapeOffset(root_rec))
}

/// AW-III.W2 — locate the WsTrim pattern the grammar uses, if any.
/// Returns `Some(Some(pattern))` when the grammar declared `@ws`
/// with a regex (the WsTrim state carries the pattern str),
/// `Some(None)` when the grammar uses `?w` without `@ws` (default
/// ASCII whitespace fallback), and `None` when the grammar has no
/// `?w` sites at all (CSV / similar).
///
/// AW-III.W4.c — `pub` for consumption by W4.b's emitted walker.
#[inline]
pub fn first_ws_pattern(table: &DtaTable) -> Option<Option<&'static str>> {
    for state in table.states.iter() {
        if let DtaState::WsTrim { pattern } = state {
            return Some(*pattern);
        }
    }
    None
}

/// AW-III.W2 — trim using the grammar's declared whitespace
/// semantics. Mirrors the `DtaState::WsTrim` arm: when `pattern`
/// carries a regex pattern, scan with the supplied scanner;
/// otherwise fall back to ASCII whitespace (matches `?w` default
/// semantics).
///
/// AW-III.W4.c — `pub` for consumption by W4.b's emitted walker.
#[inline]
pub fn trim_with_pattern(
    scanner: &dyn RegexScanner,
    pattern: Option<&'static str>,
    input: &[u8],
    pos: &mut u32,
) {
    if let Some(pat) = pattern {
        if let Some(len) = scanner.scan(pat, input, *pos as usize) {
            *pos += len;
        }
    } else {
        trim_ascii_ws(input, pos);
    }
}

/// AW-I.W4ε bootstrap fallback: trim ASCII whitespace in-place at
/// `*pos`. Mirrors `DtaState::WsTrim { pattern: None }` semantics.
///
/// AW-III.W4.c — `pub` for consumption by W4.b's emitted walker.
#[inline]
pub fn trim_ascii_ws(input: &[u8], pos: &mut u32) {
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
///
/// AW-III.W4.c — `pub` so W4.b's emitted walker, which inlines each
/// state's transition logic, can return the same `Next(state)` /
/// `Done` shape from helper invocations like [`advance_or_pop_with`].
#[derive(Clone, Copy, Debug)]
pub enum StepResult {
    /// The walker should dispatch the wrapped state next.
    Next(DtaStateId),
    /// The frame stack has fully drained; parse is complete.
    Done,
}

/// Result of a Repeat-failure absorption attempt.
///
/// AW-III.W4.c — `pub` so W4.b's emitted walker can route absorbed
/// failures through the same control flow the cold loop uses.
#[derive(Clone, Copy, Debug)]
pub enum RepeatAbsorbResult {
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
///
/// AW-III.W4.c — `pub` so W4.b's emitted walker can route Syntax
/// failures through the same absorption logic the cold loop uses.
///
/// AW-III.W5.c — `slot: &mut u32` carries the dual-cursor's structural
/// index slot alongside `pos`. Restoring an iter savepoint rewinds
/// both atomically — addresses the AQ-5 "unsaved structural cursor on
/// checkpoint" failure mode.
#[allow(clippy::too_many_arguments)]
pub fn handle_repeat_failure(
    table: &DtaTable,
    input: &[u8],
    idx: &StructuralIndex,
    columns: &mut Columns,
    psi: &mut PayloadStream,
    frame_depth: &mut Vec<u8>,
    stack: &mut FrameStack,
    pos: &mut u32,
    slot: &mut u32,
) -> Result<RepeatAbsorbResult, DtaError> {
    // AW-III.W5.c — `idx` threaded for signature uniformity with
    // dispatch_one + try_branch; the absorption logic itself does not
    // consult the structural index (the slot restore happens via
    // `sp.stack.slot` below).
    let _ = idx;
    let mut scan_depth = stack.depth();
    while scan_depth > 0 {
        let frame_idx = (scan_depth - 1) as usize;
        let frame = frame_at(stack, frame_idx);
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
                // AW-III.W5.c — restore the structural cursor slot
                // captured into `sp.stack.slot` at iteration entry.
                *slot = sp.stack.slot;
                close_compound(columns, frame_depth, stack, *pos);
                pop_and_release(stack);
                let res = advance_or_pop_with(
                    Some(table), Some(input), columns, frame_depth, psi, stack, pos, slot,
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
/// — used by [`try_branch`] so a Repeat inside the branch can absorb
/// its own failures without leaking into the Alt's outer context.
///
/// AW-III.W4.c — `pub` for W4.b's emitted walker. Same absorption
/// invariants as [`handle_repeat_failure`] but bounded to a stack
/// region above the AltLinear frame.
#[allow(clippy::too_many_arguments)]
pub fn handle_repeat_failure_bounded(
    table: &DtaTable,
    input: &[u8],
    idx: &StructuralIndex,
    columns: &mut Columns,
    psi: &mut PayloadStream,
    frame_depth: &mut Vec<u8>,
    stack: &mut FrameStack,
    pos: &mut u32,
    slot: &mut u32,
    bound_depth: u8,
) -> Result<RepeatAbsorbResult, DtaError> {
    // AW-III.W5.c — same uniformity threading as `handle_repeat_failure`.
    let _ = idx;
    let mut scan_depth = stack.depth();
    while scan_depth > bound_depth {
        let frame_idx = (scan_depth - 1) as usize;
        let frame = frame_at(stack, frame_idx);
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
                // AW-III.W5.c — restore the structural cursor slot
                // captured into `sp.stack.slot` at iteration entry.
                *slot = sp.stack.slot;
                close_compound(columns, frame_depth, stack, *pos);
                pop_and_release(stack);
                let res = advance_or_pop_with(
                    Some(table), Some(input), columns, frame_depth, psi, stack, pos, slot,
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
///
/// AW-III.W4.c — `pub` + `#[inline(always)]` so W4.b's emitted
/// walker inlines the counter-release at every Repeat-frame pop site.
#[inline(always)]
pub fn pop_and_release(stack: &mut FrameStack) -> Option<Frame> {
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
///
/// AW-III.W4.c — `pub` so W4.b's emitted walker can introspect frames
/// during its own absorption logic (the absorber walks the stack
/// looking for the innermost Repeat frame).
#[inline]
pub fn frame_at(stack: &FrameStack, idx: usize) -> Frame {
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
/// [`handle_repeat_failure_bounded`] if an enclosing Repeat has
/// `counter >= lo` — the branch continues past the Repeat's
/// successful close. Failures that cannot be absorbed propagate to
/// the AltLinear caller.
///
/// AW-III.W4.c — `pub` for W4.b's emitted walker. The emitted walker's
/// AltLinear arm uses `try_branch` directly to attempt each branch
/// with savepoint backtracking; the helper preserves the cold-path
/// dispatch_one route so probes nested inside branches retain
/// identical semantics.
#[allow(clippy::too_many_arguments)]
pub fn try_branch(
    table: &DtaTable,
    input: &[u8],
    scanner: &dyn RegexScanner,
    idx: &StructuralIndex,
    columns: &mut Columns,
    psi: &mut PayloadStream,
    frame_depth: &mut Vec<u8>,
    stack: &mut FrameStack,
    entry_state: DtaStateId,
    pos: &mut u32,
    slot: &mut u32,
    stop_depth: u8,
) -> Result<StepResult, DtaError> {
    let mut state = entry_state;
    loop {
        match dispatch_one(
            table, input, scanner, idx, columns, psi, frame_depth, stack, state, pos, slot,
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
                    table, input, idx, columns, psi, frame_depth, stack, pos, slot, stop_depth,
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

/// One step of the cold-path dispatch loop — the canonical
/// state-machine semantic for every [`DtaState`] variant.
///
/// AW-III.W4.c — `pub` so:
/// 1. The replay subsystem (AX) can re-derive a parse's decision
///    trace by single-stepping with the same dispatch path the
///    original parse took.
/// 2. W4.b's emitted walker can fall back to `dispatch_one` for any
///    state-shape that doesn't gain from inlining (rare per the R3
///    cycle audit; the typical state inlines profitably).
///
/// **The post-W4 hot path does NOT call `dispatch_one`.** The W4 hard
/// gate verifies via `cargo asm` that the per-grammar `dta_run_<grammar>`
/// function body is `dispatch_one`-symbol-free — every state's logic
/// is inlined directly. `dispatch_one` survives in the binary as the
/// cold-path replay surface, never invoked from the parse hot path.
///
/// AW-III.W5.c — dual-cursor parameters: `pos: &mut u32` is the byte
/// cursor (unchanged), `slot: &mut u32` is the structural-index slot,
/// `idx: &StructuralIndex` is the per-parse index built by the stage-1
/// scanner. When `idx.is_empty()` (grammar without stage-1 enrichment,
/// or pre-W5.b scanner integration), the structural-aware shortcuts
/// degrade and the byte-stepping fallback survives. `ConsumeToNext-
/// Structural` and the structural-aware shape of `WsTrim` consult
/// `idx`; legacy arms (`ByteDispatch`, `Regex`) keep their pre-W5.c
/// byte-driven semantics here in the cold path. The hot-path emitted
/// walker can opt into stronger shortcuts at codegen time.
#[allow(clippy::too_many_arguments)]
pub fn dispatch_one(
    table: &DtaTable,
    input: &[u8],
    scanner: &dyn RegexScanner,
    idx: &StructuralIndex,
    columns: &mut Columns,
    psi: &mut PayloadStream,
    frame_depth: &mut Vec<u8>,
    stack: &mut FrameStack,
    state: DtaStateId,
    pos: &mut u32,
    slot: &mut u32,
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
            advance_or_pop_with(Some(table), Some(input), columns, frame_depth, psi, stack, pos, slot)
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
            advance_or_pop_with(Some(table), Some(input), columns, frame_depth, psi, stack, pos, slot)
        }
        DtaState::Regex { pattern, payload } => {
            // AW-III.W5.c / W5.d — the W5.c bound `[pos, idx.positions[slot])`
            // assumes the regex pattern's matchable alphabet is disjoint
            // from the grammar's structural alphabet so the next
            // structural byte is a hard match boundary. JSON satisfies
            // that (a number's `[0-9.]` doesn't overlap with `,]}`),
            // but CSS L4 mines `[0..127]` into its structural set —
            // every byte is "structural" — and the bound collapses to
            // `[pos, pos)`, making every regex scan zero-width. The
            // alphabet-disjoint precondition is grammar-IR data the
            // current pass doesn't surface; until it does, the regex
            // arm scans the full input slice. The cursor still
            // advances post-scan via the `slot` resync inside
            // `advance_or_pop_with`, so the index stays usable for
            // ConsumeToNextStructural / WsTrim's slot-aware paths.
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
                    // String payloads include a 4-byte length prefix
                    // ahead of the bytes per the
                    // `Tape::payload_string_bytes` reader contract.
                    let width = match (kind, kind.arena_byte_width()) {
                        (crate::psi::PayloadKind::String, _) => 4 + match_len as usize,
                        (_, 0) => match_len as usize,
                        (_, w) => w,
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
            advance_or_pop_with(Some(table), Some(input), columns, frame_depth, psi, stack, pos, slot)
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
            let tape_kind = frame_to_tape_kind(frame);
            // AW-III.W5.c — fused compound push. `push_compound_fused`
            // returns the row index it wrote at; `frame_depth` carries
            // the per-row depth stamp on a parallel column.
            let parent_rec = columns.push_compound_fused(tape_kind, *pos);
            frame_depth.push(stack.depth());
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
                    Some(table), Some(input), columns, frame_depth, psi, stack, pos, slot,
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
            //
            // AW-III.W1.A — stamp the full 8-bit discriminant. The
            // previous `& 0x3F` mask collided distinct rules whose
            // ids shared their low six bits (CSS L4's `colorProps`
            // and `namedColor`); the wire contract now reserves a
            // full byte for `variant_idx`.
            stack.pending_variant_idx = (rule.0 & 0xFF) as u8;
            Ok(StepResult::Next(chosen))
        }
        DtaState::ClassifyByte { table: disp, fallback } => {
            // AW-III.W6.3 — ClassifyByte is semantically equivalent to
            // ByteDispatch but is emitted exclusively for Alts mined by
            // the disjoint_first pass (all branches have mutually-
            // disjoint FIRST sets). The cold-path semantic here is a
            // single indexed load + NONE-fallback branch, matching the
            // ByteDispatch arm below; the hot-path emitter specialises
            // it to a `match` expression over the mined byte classes.
            let b = if !idx.positions.is_empty() {
                let slot_idx = *slot as usize;
                if slot_idx < idx.positions.len() && idx.positions[slot_idx] == *pos {
                    idx.kinds[slot_idx]
                } else {
                    input.get(*pos as usize).copied().unwrap_or(0)
                }
            } else {
                input.get(*pos as usize).copied().unwrap_or(0)
            };
            let next = disp[b as usize];
            let chosen = if next == DtaStateId::NONE { fallback } else { next };
            if chosen == DtaStateId::NONE {
                return Err(DtaError::Syntax {
                    offset: *pos,
                    failing_state: state,
                    failing_rule: DtaRuleId(u32::MAX),
                });
            }
            if let Some(top) = stack.top_mut() {
                if matches!(top.kind, DtaFrameKind::Alt) {
                    top.cursor = chosen.0;
                }
            }
            Ok(StepResult::Next(chosen))
        }
        DtaState::ByteDispatch { table: disp, fallback } => {
            // AW-III.W5.c — when the dual-cursor's structural index is
            // populated AND its current slot's position matches `pos`,
            // dispatch from `idx.kinds[slot]` directly — one indexed u8
            // load with no `input.get` bounds check. The byte is the
            // same as `input[pos]` per the scanner's positions/kinds
            // invariant; the load comes from the dense kinds column
            // already in cache from prior dispatches. Without the
            // index (cold path), the byte fallback survives unchanged.
            //
            // Slot advance happens at the dispatching frame's pop
            // time once the chosen branch's body consumes the byte;
            // ByteDispatch itself is a single-byte lookahead and
            // does NOT advance slot here (the next state may need to
            // re-read the same byte for length-aware matching).
            let b = if !idx.positions.is_empty() {
                let slot_idx = *slot as usize;
                if slot_idx < idx.positions.len() && idx.positions[slot_idx] == *pos {
                    idx.kinds[slot_idx]
                } else {
                    input.get(*pos as usize).copied().unwrap_or(0)
                }
            } else {
                input.get(*pos as usize).copied().unwrap_or(0)
            };
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
            // AW-III.W5.c — capture the structural cursor slot at Alt
            // entry so failed branches can rewind both `pos` and
            // `slot` atomically.
            let start_slot = *slot;

            // Reserve an Alt compound frame. The branch's subtree is
            // emitted into the compound's child run; on successful
            // branch close, the Alt frame's cursor carries the branch
            // index (the variant_idx). AW-III.W5.c — fused write.
            let parent_rec = columns.push_compound_fused(TapeKind::Alt, *pos);
            frame_depth.push(start_depth);
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
            // yet". AW-III.W5.c — savepoint captures the dual-cursor
            // slot via `stack.savepoint(*slot)` so branch backtracking
            // rewinds the structural cursor atomically with the stack.
            let sp_after_push = stack.savepoint(*slot);
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
                // AW-III.W5.c — restore the dual cursor's structural
                // slot to the pre-Alt-entry value before each branch
                // attempt. Without this rewind, a failed branch that
                // advanced the slot would leak into the next branch's
                // ByteDispatch / Regex bound consultation.
                *slot = start_slot;
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
                    idx,
                    columns,
                    psi,
                    frame_depth,
                    stack,
                    branch,
                    pos,
                    slot,
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
                        // AW-III.W5.c — `restore` does not touch the
                        // cursor; the explicit slot reset above is the
                        // companion. Pre-restore `slot` will be
                        // re-overwritten at the next iteration's
                        // `*slot = start_slot` line.
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
            // target. AW-III.W5.c — fused compound write.
            let parent_rec = columns.push_compound_fused(TapeKind::Rule, *pos);
            frame_depth.push(stack.depth());
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
            // AW-III.W5.c — `slot: 0` initial; the in-place fill below
            // captures the real slot via `stack.savepoint(*slot)`.
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
                    slot: 0,
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
            // body state yet". AW-III.W5.c — captures the dual-cursor
            // slot via `stack.savepoint(*slot)` so iteration restore
            // rewinds the structural cursor atomically.
            stack.iter_savepoints[counter_idx].stack = stack.savepoint(*slot);

            // Handle degenerate `hi == 0` — close immediately.
            if hi == 0 {
                close_compound(columns, frame_depth, stack, *pos);
                return advance_or_pop_with(
                    Some(table), Some(input), columns, frame_depth, psi, stack, pos, slot,
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
            //
            // AW-III.W5.c / W5.d — collapse to a cursor jump when the
            // stage-1 index proves the inter-byte span between `pos`
            // and the next structural slot is exclusively WS. The
            // scanner classifies a byte as structural iff it is in the
            // grammar's `structural_alphabet`; the WS bytes (` `, `\t`,
            // `\n`, `\r`) fall outside the alphabet for grammars whose
            // alphabet is narrow (JSON, BBNF, Sheets), so the gap
            // between `pos` and `idx.positions[slot]` is guaranteed
            // non-WS-only and the cursor jump is sound. CSS L4's
            // alphabet pulls in the full ASCII range, so the WS bytes
            // ARE in the index — the cursor jump would skip non-WS
            // bytes, breaking parsing. The arm checks the byte at
            // `pos` first: only advance when it is whitespace, exactly
            // matching the pre-W5.c semantic, but exit through the
            // index-driven path when the inter-slot gap is provably WS.
            //
            // Lazy slot resync: advance `slot` past any index entries
            // whose position is at-or-before `pos`. Literal/Regex arms
            // don't currently update `slot` when they consume bytes;
            // WsTrim's collapse needs slot to point at the NEXT
            // structural byte's index entry, not a stale earlier one.
            if !idx.positions.is_empty() {
                while (*slot as usize) < idx.positions.len()
                    && idx.positions[*slot as usize] <= *pos
                {
                    *slot += 1;
                }
            }
            // WS consumption — the scalar / regex path is the
            // single source of truth for the byte-class boundary;
            // the index serves only the slot resync above when present.
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
            advance_or_pop_with(Some(table), Some(input), columns, frame_depth, psi, stack, pos, slot)
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
            //
            // AW-III.W2 — the probe must use the deep
            // [`FrameStack::snapshot_probe`] / [`restore_probe`]
            // mechanism, not the length-only `savepoint` /`restore`
            // pair: the nested dispatch a probe triggers may walk
            // back up the stack via [`advance_or_pop_with`] and
            // mutate the enclosing Repeat's counter / iter-savepoint
            // slot in-place. Length-only restore truncates new
            // pushes but cannot rewind those in-place mutations,
            // leaving the enclosing Repeat with a counter of 1
            // (instead of 0) and an iter-savepoint pos advanced past
            // the probe match — which causes `handle_repeat_failure`
            // to absorb the wrong byte position. The deep snapshot
            // captures the active slot contents verbatim so the
            // probe is fully side-effect-free.
            let start_pos = *pos;
            // AW-III.W5.c — capture the dual-cursor's slot at probe
            // entry. The probe restore re-anchors `slot` from the
            // probe snapshot's `base.slot` so the probe is fully
            // side-effect-free for the structural cursor too.
            let probe_snapshot = stack.snapshot_probe(*slot);
            let cols_len = columns.len();
            let fd_len = frame_depth.len();
            let psi_len = psi.len();
            let pay_agg_len = columns.pay_agg.len();
            let start_depth = stack.depth();

            let probe = try_branch(
                table,
                input,
                scanner,
                idx,
                columns,
                psi,
                frame_depth,
                stack,
                excluded,
                pos,
                slot,
                start_depth,
            );

            // Restore state unconditionally — the probe's work is a
            // lookahead, never consumed into the tape. AW-III.W1
            // adds arena restoration alongside the structural
            // truncation to prevent staged constants from a probe-
            // matched sub-Literal leaking into the post-restore
            // record stream. AW-III.W2 promotes the stack restore
            // to a deep snapshot restore (see snapshot above).
            // AW-III.W5.c — restore `slot` from the probe snapshot.
            columns.truncate(cols_len);
            frame_depth.truncate(fd_len);
            psi.truncate(psi_len);
            columns.pay_agg.truncate(pay_agg_len);
            *slot = probe_snapshot.base.slot;
            stack.restore_probe(probe_snapshot);
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
        DtaState::ConsumeToNextStructural => {
            // AW-III.W5.c — O(1) cursor jump to the next structural
            // delimiter. With the dual-cursor and stage-1 SIMD index,
            // the dispatch consumes one indexed `u32` load + a slot
            // increment — replacing what pre-W5.c was a byte-stepping
            // `DtaState::Regex { pattern: "[^,}\]]+" }` scan that
            // dominated JSON `__value` self-time.
            //
            // Fallback semantics: when the index is empty (cold-path
            // dta_run_inner; pre-W5.b scanner integration), the arm
            // degrades to byte-stepping past whitespace. The downstream
            // grammar can still parse correctly because the next
            // dispatch state's match attempt drives the cursor forward
            // explicitly. The cold-path's correctness is preserved;
            // the speedup is hot-path-only by design.
            if !idx.positions.is_empty() {
                let slot_idx = *slot as usize;
                if slot_idx < idx.positions.len() {
                    *pos = idx.positions[slot_idx];
                    *slot = (slot_idx + 1) as u32;
                } else {
                    // No further structural — jump to input end.
                    *pos = input.len() as u32;
                }
            } else {
                // No index — degrade to ASCII whitespace skip. The
                // arm survives semantically; parse correctness lives
                // in the surrounding state machine, not this jump.
                trim_ascii_ws(input, pos);
            }
            stack.pending_variant_idx = u8::MAX;
            advance_or_pop_with(Some(table), Some(input), columns, frame_depth, psi, stack, pos, slot)
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
            // (it points at `parent + 1`). AW-III.W5.c — fused write.
            let parent_rec = columns.push_compound_fused(TapeKind::Rule, *pos);
            frame_depth.push(stack.depth());
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

/// Saturate a `u32` to `u16`. Used by the Repeat arm to fold `lo`/
/// `hi` bounds into the [`Frame`] struct's u16 fields.
///
/// AW-III.W4.c — `pub` so W4.b's emitted walker can fold Repeat
/// bounds at frame-push time without re-deriving the saturation
/// logic.
#[inline]
pub fn saturating_u16(v: u32) -> u16 {
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
///
/// AW-III.W4.c — `pub` + `#[inline(always)]` so W4.b's emitted walker
/// fuses the leaf emission directly into each Literal/Regex state's
/// inlined arm.
#[inline(always)]
pub fn emit_leaf(
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
/// [`TapeRec::PAYLOAD_IN_ARENA_BIT`](crate::tape::TapeRec::PAYLOAD_IN_ARENA_BIT)
/// is set when `child_off != NONE`, signalling to scalar readers
/// (`payload_u8`, `payload_scalar::<T>`) that they should slice
/// `pay_agg` directly instead of indirecting through `pay_narrow` /
/// `pay_wide`.
///
/// AW-III.W4.c — `pub` + `#[inline(always)]` so W4.b's emitted walker
/// inlines the column writes directly into every payload-emitting
/// state's arm. The 8 column stores fold into the surrounding state
/// body so LLVM's vector-of-stores optimisation can fuse them with
/// the state's other writes.
#[inline(always)]
pub fn emit_leaf_with_payload(
    columns: &mut Columns,
    frame_depth: &mut Vec<u8>,
    stack: &FrameStack,
    kind: TapeKind,
    span_lo: u32,
    span_hi: u32,
    child_off: TapeOffset,
) -> u32 {
    // AW-I.W4ζ — consume the pending rule-entry variant_idx stamp
    // into this leaf's flags. Leaf rules (`identifier = /regex/`)
    // reach here via `Ref → Regex/Literal`; the rule's discriminant
    // must survive onto the tape so `rule_kind()` can decode it.
    //
    // AW-III.W1: when no pending stamp is set, fall back to the
    // nearest enclosing compound frame whose `variant_idx` is set so
    // structural literals (`,` / `]` / `}`) inherit their containing
    // rule's discriminant rather than defaulting to `0`. The
    // pre-W1.A short-circuit on the IMMEDIATE top frame missed
    // anonymous body-Seq frames between the literal and its true
    // owner (transparent rules' inlined bodies); walking the stack
    // (`nearest_variant_frame`) recovers the first non-anonymous
    // ancestor.
    //
    // AW-III.W1.A — full-byte variant; the prior `& 0x3F` truncation
    // is gone alongside the wire-format widening.
    let variant = if stack.pending_variant_idx != u8::MAX {
        stack.pending_variant_idx
    } else if let Some(owner) = stack.nearest_variant_frame() {
        owner.variant_idx
    } else {
        0
    };
    let extra: u16 = if child_off.is_none() {
        0
    } else {
        crate::tape::TapeRec::PAYLOAD_IN_ARENA_BIT
    };
    // AW-III.W5.c — fused SoA write. One bounds-check on the dominant
    // column + 7 unchecked stores. Replaces the seven `Vec::push` calls
    // pre-W5.c paid per leaf record.
    let idx = columns.push_leaf_fused(kind, variant, extra, span_lo, span_hi, child_off);
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
///
/// AW-III.W4.c — `pub` + `#[inline(always)]` for the emitted walker;
/// the 1- to 8-byte store folds into the Literal arm with no call
/// overhead.
#[inline(always)]
pub fn stage_literal_payload_in_arena(columns: &mut Columns, payload: LiteralPayload) -> TapeOffset {
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

/// Emit a binary-operator compound for the ShuntingYard reducer.
///
/// The compound sits AFTER the RHS operand with `child_off` pointing
/// back at the LHS operand's tape row. Post-order layout for
/// reducer-produced compounds; the cursor's bounded backward-walk
/// fallback resolves the first-child lookup at read time.
///
/// AW-III.W4.c — `pub` so W4.b's emitted ShuntingYard arm can fold
/// the reducer compound writes inline.
#[inline]
pub fn emit_reducer_compound(
    columns: &mut Columns,
    frame_depth: &mut Vec<u8>,
    depth: u8,
    lhs_idx: u32,
    op_discriminant: u8,
    span_lo: u32,
    span_hi: u32,
) -> u32 {
    // AW-III.W5.c — fused SoA write. The reducer compound is a
    // single-row leaf-shape (`HAS_CHILDREN_BIT` set, `child_off`
    // points back at the LHS operand row); reuses `push_leaf_fused`'s
    // store sequence with the SY-specific flags + extra values.
    let idx = columns.push_leaf_fused(
        TapeKind::Rule,
        op_discriminant,
        crate::tape::TapeRec::HAS_CHILDREN_BIT,
        span_lo,
        span_hi,
        TapeOffset(lhs_idx),
    );
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
///
/// AW-III.W4.c — `pub` so W4.b's emitted walker's ShuntingYard arm
/// resolves operators against the per-grammar precedence slice
/// without re-implementing the lookup. The W6.5 emitted
/// `PRECEDENCE_LUT` will replace this linear scan with a packed
/// 256-entry indexed lookup.
#[inline]
pub fn lookup_precedence(
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
/// [`TapeRec::PAYLOAD_IN_ARENA_BIT`](crate::tape::TapeRec::PAYLOAD_IN_ARENA_BIT)
/// is set), the kind byte is repacked from `Seq` → `KvPair`, the
/// `has_children` bit is left clear, and the trailing child records
/// are truncated. Tape size shrinks by `(children_count - 0)` records
/// per KvPair-promoted rule body — the structural saving the layout
/// pass paid for.
///
/// AW-III.W4.c — `pub` + `#[inline(always)]` so W4.b's emitted walker
/// inlines the close logic at every frame-pop site. The KvPair-
/// promotion mining loop is bounded by the frame's children count
/// (typically ≤ 4 for KvPair shapes); LLVM should specialise the
/// loop bound when the call site has a fixed promote variant.
#[inline(always)]
pub fn close_compound(
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
                // AW-III.W1.A — the KvPair record is a leaf; preserve
                // the META_IDX_HI_BIT but drop HAS_CHILDREN_BIT, then
                // mark PAYLOAD_IN_ARENA_BIT.
                let preserved = columns.extra[parent] & crate::tape::TapeRec::META_IDX_HI_BIT;
                columns.extra[parent] = preserved | parent_extra;
                // Variant_idx stamping for the KvPair flat record:
                // the rule's variant_idx wins (frame.variant_idx);
                // fall through to the legacy Alt-cursor stamp when
                // no rule context.
                if frame.variant_idx != u8::MAX {
                    columns.flags[parent] = frame.variant_idx;
                } else if matches!(frame.kind, DtaFrameKind::Alt) {
                    columns.flags[parent] = frame.cursor as u8;
                }
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
            columns.extra[parent] |= crate::tape::TapeRec::HAS_CHILDREN_BIT;
        }
        // AW-III.W1.A — stamp the full 8-bit variant_idx into flags.
        // Precedence:
        //   1. `frame.variant_idx` — rule-entry stamp captured from
        //      `FrameStack::pending_variant_idx` at push time. This
        //      encodes the OWNING rule's discriminant so the view
        //      layer's `rule_kind()` decodes correctly.
        //   2. Alt frame's `cursor` — branch index, when no rule
        //      context is set (non-rule-entry Alt compounds).
        // The u8::MAX sentinel indicates "no rule stamp"; fall through
        // to the Alt-cursor path to preserve sub-variant dispatch.
        if frame.variant_idx != u8::MAX {
            columns.flags[parent] = frame.variant_idx;
        } else if matches!(frame.kind, DtaFrameKind::Alt) {
            columns.flags[parent] = frame.cursor as u8;
        }
    }
}

/// Peek at the topmost frame without popping. [`FrameStack::top_mut`]
/// requires `&mut`; for read-only probing we inline the logic.
///
/// AW-III.W4.c — `pub` + `#[inline(always)]` so W4.b's emitted walker
/// can read the top frame's metadata at close-compound time without
/// re-acquiring a mutable borrow.
#[inline(always)]
pub fn stack_top(stack: &FrameStack) -> Option<&Frame> {
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
///
/// AW-III.W4.c — `pub` for W4.b's emitted walker; the per-state
/// arms call this at every leaf-emit site to drive the next
/// dispatch. The function is large (the SY arm + Repeat arm carry
/// non-trivial logic).
///
/// AW-III.W4.d — Seq fast-path probe. When the topmost frame is a
/// Seq with cursor that can advance (cursor + 1 < children.len()),
/// returns `Some(Next(children[cursor + 1]))` after incrementing the
/// cursor in place. Otherwise returns `None` and the caller falls
/// through to the full [`advance_or_pop_with`] body for close + pop
/// + Alt / Repeat / SY handling.
///
/// Marked `#[inline(always)]` so the per-state arm body folds the
/// in-place cursor increment + child read directly into its tail —
/// the JSON struct-traversal hot loop never crosses a function-call
/// boundary on the dominant Seq advance.
#[inline(always)]
pub fn advance_seq_fast(
    stack: &mut FrameStack,
) -> Option<StepResult> {
    let top = stack.top_mut()?;
    if matches!(top.kind, DtaFrameKind::Seq) {
        let next_cursor = top.cursor + 1;
        if (next_cursor as usize) < top.children.len() {
            top.cursor = next_cursor;
            return Some(StepResult::Next(top.children[next_cursor as usize]));
        }
    }
    None
}

/// AW-III.W4.d — split into a Seq-fast-path inline shim
/// ([`advance_seq_fast`]) plus the full-body helper here.
/// Most leaf emit sites (Literal, Regex, Epsilon, WsTrim) hit a
/// Seq frame whose cursor advances and dispatches the next child
/// immediately — that case folds directly into the call site via
/// the inline-always shim. The full body below covers the
/// fall-through cases (Seq close, Alt close, Repeat re-entry,
/// ShuntingYard reducer) that LLVM should not inline due to their
/// size + dynamic precedence-table consumption.
///
/// AW-III.W5.c — `slot: &mut u32` mirrors the dual cursor; the
/// Repeat re-entry path captures it via [`FrameStack::savepoint`] so
/// a subsequent absorption rewinds slot atomically with `pos`.
#[allow(clippy::too_many_arguments)]
#[inline]
pub fn advance_or_pop_with(
    _table: Option<&DtaTable>,
    _input: Option<&[u8]>,
    columns: &mut Columns,
    frame_depth: &mut Vec<u8>,
    psi: &mut PayloadStream,
    stack: &mut FrameStack,
    pos: &mut u32,
    slot: &mut u32,
) -> Result<StepResult, DtaError> {
    // AW-III.W5.c — `slot` participates in iter-savepoint capture so a
    // body absorption restores the structural cursor atomically. The
    // helper itself does not consult the structural index — the
    // dual-cursor's per-arm shortcuts live in `dispatch_one`'s
    // ConsumeToNextStructural / WsTrim arms.
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
                    // AW-III.W5.c — capture the structural cursor slot
                    // alongside the stack lengths. A later body failure
                    // restores both atomically via `handle_repeat_failure`.
                    let new_stack_sp = stack.savepoint(*slot);
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
                    let op_lo = *pos;
                    *pos = pos.saturating_add(op_width);
                    // AW-III.W1: emit a payload-bearing Span leaf
                    // carrying the op's u8 discriminant so downstream
                    // walkers (`typed_u8_payloads`,
                    // `find_named_color_payload`-style readers) can
                    // surface every operator the SY chain consumed.
                    // Without this the SY collapse intercepted the
                    // per-branch Map { Literal "+", IntLit(0) } shape
                    // before its U8 payload had a chance to land — the
                    // walker advanced past `+`/`-`/`*`/`/` opcodes
                    // without writing anything to the tape.
                    let op_arena_off = columns.pay_agg.len() as u32;
                    columns.pay_agg.push(entry.op_discriminant);
                    let _op_rec = emit_leaf_with_payload(
                        columns,
                        frame_depth,
                        stack,
                        TapeKind::Span,
                        op_lo,
                        *pos,
                        TapeOffset(op_arena_off),
                    );
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
                    columns.extra[sy_parent] |= crate::tape::TapeRec::HAS_CHILDREN_BIT;
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

/// Project a [`DtaFrameKind`] to its tape [`TapeKind`] discriminant.
///
/// Used at frame-push time so the reserved compound row's `kind` byte
/// is set before any children land. The Seq / Alt / Repeat / SY
/// triage is a 4-arm enum match — folds to a single byte indexed
/// load at codegen time.
///
/// AW-III.W4.c — `pub` + `#[inline(always)]` so W4.b's emitted walker
/// folds the projection into each frame-push site (Seq → 0, Alt → 1,
/// Repeat → 5, ShuntingYard → 5; the constant projections collapse
/// at LLVM peephole time).
#[inline(always)]
pub fn frame_to_tape_kind(frame: DtaFrameKind) -> TapeKind {
    match frame {
        DtaFrameKind::Seq => TapeKind::Seq,
        DtaFrameKind::Alt => TapeKind::Alt,
        DtaFrameKind::Repeat => TapeKind::Rule,
        DtaFrameKind::ShuntingYard => TapeKind::Rule,
    }
}
