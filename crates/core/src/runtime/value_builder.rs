//! `ValueBuilder<R>` — the fused-pipeline value-construction substrate.
//!
//! # Role (Tranche AY-II.W0.c)
//!
//! AY-II lands a single-pass parse that writes both the canonical
//! tape substrate (via [`TapeBuilder`](tape::TapeBuilder)) AND the
//! grammar-emitted `<Grammar>Value` structure. Pre-AY-II the default
//! `Parsed::to_value()` dispatched through `view_to_value`, which
//! reconstructed the typed value enum by walking the tape via a
//! [`TapeCursor`](tape::TapeCursor) and reading payload columns for
//! every leaf. That walk is the 3.47× / 3.48× gap AUDIT-D §2
//! attributes to the tape-first consumer: the parser emits at
//! near-sonic byte cost when the tape-then-walk reconstruction is
//! skipped; the default `to_value()` pays it unconditionally.
//!
//! `ValueBuilder<R>` is the parallel write-time substrate that
//! closes the gap. The emitter (AY-II.W0.b) wires every shape's
//! emitted parse fn to push into BOTH the `TapeBuilder` and a
//! `ValueBuilder<R>` — structural compounds call
//! [`ValueBuilder::begin_compound`] / [`ValueBuilder::end_compound`]
//! in lockstep with `TapeBuilder::begin_compound` /
//! `TapeBuilder::end_compound`; leaves call [`ValueBuilder::push_leaf`]
//! in lockstep with the tape's `push_leaf_*` family; retry-IIFE
//! failure paths call [`ValueBuilder::rollback_to`] in lockstep with
//! [`Columns::rollback_to`](tape::columns::Columns::rollback_to).
//!
//! # Frame stack
//!
//! The builder's frame stack mirrors the tape's open/close
//! discipline. `begin_compound` pushes a [`ValueFrame`] of kind
//! `Compound` onto the frame arena and records the pre-push arena
//! size in a [`ValueCheckpoint`] — the symmetric counterpart to the
//! `open_offset` the tape builder returns. `end_compound` patches
//! the frame's `span_hi` + converts the open children run into a
//! frame slice. `push_leaf` appends a leaf frame to the arena.
//! `rollback_to` truncates the arena + frame stack to a previously-
//! issued checkpoint — matching the atomic rollback the emitter
//! invokes at every retry-IIFE boundary (W0.a + W0.c agreed
//! contract).
//!
//! # Thin projector
//!
//! `Parsed::to_value()` becomes a thin projector over the
//! [`ValueBuilderOutput`] produced at parse time. The projection
//! step runs a single descent of the frame tree and constructs the
//! grammar's `<Grammar>Value<'p>` enum in one pass. No parse
//! invocation, no `parse_with_visitor` call, no tape-walking
//! materializer path — the tape substrate is present but not the
//! source of `to_value`'s output.
//!
//! # Grammar-agnostic storage
//!
//! The frame arena stores a grammar-agnostic representation
//! ([`ValueFrame`]) with enough information for the per-grammar
//! projection pass to reconstruct any typed value. Concretely each
//! frame records:
//!
//! - `kind` — the [`TapeKind`](tape::TapeKind) tag (`Seq`, `Rule`,
//!   `Span`, `Regex`, etc.).
//! - `span_lo` / `span_hi` — source byte range (leaf span for
//!   leaves, full compound span for compounds).
//! - `variant_idx` — the rule-identity discriminator the grammar's
//!   `RuleKind` dispatch table keys on; stamped verbatim from what
//!   the emitted compound entry would write into the tape.
//! - `first_child` / `child_count` — contiguous run of children
//!   inside the arena (0 / 0 for leaves).
//! - `payload_tag` — index into a parallel `payloads` column where
//!   scalar leaves store their decoded value. For compounds the
//!   payload tag is [`PayloadTag::NONE`].
//!
//! The `<Grammar>Value` projection reads these fields and picks
//! the matching variant constructor via the grammar's emitted
//! projection logic; no tape lookup happens during projection.

use std::marker::PhantomData;

use tape::TapeKind;

use crate::runtime::parsed::ValueRoot;

/// Grammar-agnostic structural frame inside a [`ValueBuilder`].
///
/// One frame per tape record — compounds carry a contiguous child
/// range (`first_child` + `child_count`) into the same arena, leaves
/// carry a [`PayloadTag`] pointing at the scalar payload column.
///
/// The frame is `Copy` and 24 bytes (5 × 4 + 1 × 4): small enough to
/// vectorise push and rollback over, large enough to encode every
/// leaf + compound distinction the projection layer needs. The
/// encoding is stable across grammars — a frame tree produced by
/// parse A is structurally compatible with the projection logic
/// emitted for grammar A; the projection dispatches on `variant_idx`
/// and `kind` (grammar-derived) to pick the right `<Grammar>Value`
/// variant.
#[derive(Clone, Copy, Debug)]
pub struct ValueFrame {
    /// Source span lower bound (byte offset).
    pub span_lo: u32,
    /// Source span upper bound (byte offset). For compounds this is
    /// the full compound span; stamped at `end_compound` time.
    pub span_hi: u32,
    /// Index of the first child frame in the builder's arena; unused
    /// for leaves (`child_count == 0`).
    pub first_child: u32,
    /// Number of direct child frames for compounds; `0` for leaves.
    pub child_count: u32,
    /// Rule-identity discriminator matching the emitter's
    /// `RuleKind` dispatch. `0` for structural shapes without a
    /// declared rule binding (the projection layer re-derives via
    /// `kind` + `variant_idx` fallback).
    pub variant_idx: u8,
    /// Kind discriminator — mirrors the tape record's [`TapeKind`].
    pub kind: TapeKind,
    /// Scalar payload handle or [`PayloadTag::NONE`] for compounds.
    pub payload_tag: PayloadTag,
}

/// Handle into a [`ValueBuilder`]'s scalar payload column.
///
/// Payloads are appended in push order; leaves record the rank +
/// kind. Wide scalars (`f64`, `u64`) land in the `wide` column;
/// narrow scalars (`u32`, `bool`, `u8`) land in the `narrow` column.
/// [`PayloadTag::NONE`] marks a leaf that carries no typed payload
/// beyond its source span.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PayloadTag(u32);

impl PayloadTag {
    /// Sentinel — no typed payload; the leaf's value must be read
    /// from its source span.
    pub const NONE: Self = Self(u32::MAX);

    /// Wrap a narrow-column rank.
    #[inline]
    pub const fn narrow(rank: u32) -> Self {
        debug_assert!(rank < u32::MAX >> 2);
        Self(rank << 2)
    }

    /// Wrap a wide-column rank.
    #[inline]
    pub const fn wide(rank: u32) -> Self {
        debug_assert!(rank < u32::MAX >> 2);
        Self((rank << 2) | 1)
    }

    /// `true` iff the tag is [`PayloadTag::NONE`].
    #[inline]
    pub const fn is_none(&self) -> bool {
        self.0 == u32::MAX
    }

    /// `true` iff the payload lives in the narrow column.
    #[inline]
    pub const fn is_narrow(&self) -> bool {
        !self.is_none() && (self.0 & 0b11) == 0
    }

    /// `true` iff the payload lives in the wide column.
    #[inline]
    pub const fn is_wide(&self) -> bool {
        !self.is_none() && (self.0 & 0b11) == 1
    }

    /// Extract the column rank.
    #[inline]
    pub const fn rank(&self) -> u32 {
        self.0 >> 2
    }
}

/// Opaque checkpoint produced by [`ValueBuilder::begin_compound`]
/// and consumed by [`ValueBuilder::end_compound`] /
/// [`ValueBuilder::rollback_to`].
///
/// Encodes the arena size at the open point. Symmetric with the
/// `open_offset: u32` the tape builder returns — both substrates
/// truncate to the same "before open" state on rollback.
#[derive(Clone, Copy, Debug)]
pub struct ValueCheckpoint {
    /// Arena frame offset at open time. `frames.len()` snaps back to
    /// this value on rollback; the frame itself is pushed at this
    /// index.
    frame_offset: u32,
    /// Narrow payload column rank at open time.
    narrow_rank: u32,
    /// Wide payload column rank at open time.
    wide_rank: u32,
}

impl ValueCheckpoint {
    /// The arena frame offset this checkpoint recorded. Exposed so
    /// downstream emitters can thread it through their own
    /// bookkeeping in lockstep with the tape's `open_offset`.
    #[inline]
    pub fn frame_offset(&self) -> u32 {
        self.frame_offset
    }
}

/// Parallel value-construction substrate written in lockstep with
/// [`TapeBuilder`](tape::TapeBuilder) during parse.
///
/// Emitters call [`ValueBuilder::begin_compound`] on compound entry,
/// [`ValueBuilder::push_leaf`] on each leaf, and
/// [`ValueBuilder::end_compound`] on compound exit. Retry-IIFE
/// failure paths call [`ValueBuilder::rollback_to`] with the
/// [`ValueCheckpoint`] returned at the open point. The builder
/// owns an arena of [`ValueFrame`] entries + parallel scalar payload
/// columns; `finish()` consumes the builder and returns a
/// [`ValueBuilderOutput`] the [`Parsed`](crate::runtime::Parsed)
/// carries to its `to_value()` consumer.
///
/// `R` is the grammar marker type; the phantom parameter ties the
/// builder to its [`ValueRoot`] implementation so projection can
/// read the frame arena without reasserting the binding.
pub struct ValueBuilder<R> {
    /// Nested frame arena — one entry per tape push, laid out in the
    /// emitter's push order. Compounds reference their children by
    /// `(first_child, child_count)`; leaves carry `child_count == 0`.
    frames: Vec<ValueFrame>,
    /// Narrow-column scalar payloads (u32 / bool / u8). Indexed by
    /// [`PayloadTag::narrow`] rank.
    payloads_narrow: Vec<u32>,
    /// Wide-column scalar payloads (f64 / u64 / u32-pair). Indexed
    /// by [`PayloadTag::wide`] rank.
    payloads_wide: Vec<u64>,
    /// Open compound stack — one entry per `begin_compound` without
    /// a matching `end_compound`. Tracks the frame offset of each
    /// open frame so `end_compound` can patch child-run metadata at
    /// close time.
    open_stack: Vec<u32>,
    /// Phantom marker — binds the builder to its grammar root.
    _root_marker: PhantomData<R>,
}

impl<R: ValueRoot> ValueBuilder<R> {
    /// Construct a new empty builder with capacity hints.
    ///
    /// `capacity_hint` should approximate the expected frame count
    /// (input bytes / 8 is a reasonable default for JSON-like
    /// grammars, mirroring the tape builder's sizing rule).
    #[inline]
    pub fn new(capacity_hint: usize) -> Self {
        #[cfg(test)]
        NEW_CALL_COUNT.with(|c| c.set(c.get() + 1));
        Self {
            frames: Vec::with_capacity(capacity_hint),
            payloads_narrow: Vec::with_capacity(capacity_hint / 4),
            payloads_wide: Vec::with_capacity(capacity_hint / 4),
            open_stack: Vec::with_capacity(16),
            _root_marker: PhantomData,
        }
    }

    /// Open a compound frame.
    ///
    /// Appends a [`ValueFrame`] with `kind` set to the compound's
    /// kind, reserving the child run for later population. Returns
    /// a [`ValueCheckpoint`] the caller must pass to
    /// [`ValueBuilder::end_compound`] (on success) or
    /// [`ValueBuilder::rollback_to`] (on failure). Mirrors the
    /// `TapeBuilder::begin_compound` signature agreed for W0.a.
    #[inline]
    pub fn begin_compound(&mut self, kind: TapeKind, span_lo: u32, variant_idx: u8) -> ValueCheckpoint {
        let frame_offset = self.frames.len() as u32;
        self.frames.push(ValueFrame {
            span_lo,
            span_hi: span_lo,
            first_child: frame_offset + 1,
            child_count: 0,
            variant_idx,
            kind,
            payload_tag: PayloadTag::NONE,
        });
        self.open_stack.push(frame_offset);
        ValueCheckpoint {
            frame_offset,
            narrow_rank: self.payloads_narrow.len() as u32,
            wide_rank: self.payloads_wide.len() as u32,
        }
    }

    /// Close the most recently opened compound.
    ///
    /// Patches the opened frame's `span_hi` + `child_count` to the
    /// populated run. `checkpoint` must be the value returned from
    /// the paired `begin_compound` call; mismatched pairings are
    /// diagnosed via `debug_assert`. Mirrors the
    /// `TapeBuilder::end_compound` contract agreed for W0.a.
    #[inline]
    pub fn end_compound(&mut self, checkpoint: ValueCheckpoint, span_hi: u32) {
        let expected = checkpoint.frame_offset;
        let popped = self
            .open_stack
            .pop()
            .expect("ValueBuilder::end_compound called with empty open_stack");
        debug_assert_eq!(
            popped, expected,
            "ValueBuilder::end_compound checkpoint mismatch: popped {popped}, expected {expected}",
        );
        let frame_offset = expected as usize;
        // Direct children occupy the range [frame_offset+1,
        // frames.len()); compute the direct-child count by scanning
        // the top-level span — nested compounds handled their own
        // closes already, so every frame between frame_offset+1 and
        // frames.len() is either a direct child or inside a direct
        // child's subtree. Direct children are those whose
        // `first_child` is the next available frame after their own
        // subtree — we reconstruct the count by walking the direct-
        // child chain via `first_child + subtree_size`.
        let mut cursor = frame_offset + 1;
        let total = self.frames.len();
        let mut direct_count: u32 = 0;
        while cursor < total {
            let child = &self.frames[cursor];
            let subtree_size = Self::subtree_size(&self.frames, cursor);
            cursor += subtree_size;
            direct_count += 1;
            let _ = child;
        }
        let frame = &mut self.frames[frame_offset];
        frame.span_hi = span_hi;
        frame.child_count = direct_count;
    }

    /// Push a leaf frame carrying a source span + typed payload tag.
    ///
    /// `payload_tag` is [`PayloadTag::NONE`] for leaves whose value
    /// must be read from the source span (`Span`, `Literal`,
    /// `Epsilon`), otherwise it points at the scalar payload this
    /// leaf recorded via [`ValueBuilder::push_payload_narrow`] /
    /// [`ValueBuilder::push_payload_wide`].
    #[inline]
    pub fn push_leaf(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        variant_idx: u8,
        payload_tag: PayloadTag,
    ) {
        self.frames.push(ValueFrame {
            span_lo,
            span_hi,
            first_child: 0,
            child_count: 0,
            variant_idx,
            kind,
            payload_tag,
        });
    }

    /// Append a narrow-column scalar payload, returning a
    /// [`PayloadTag`] the caller plants into the next pushed leaf.
    #[inline]
    pub fn push_payload_narrow(&mut self, value: u32) -> PayloadTag {
        let rank = self.payloads_narrow.len() as u32;
        self.payloads_narrow.push(value);
        PayloadTag::narrow(rank)
    }

    /// Append a wide-column scalar payload, returning a
    /// [`PayloadTag`] the caller plants into the next pushed leaf.
    #[inline]
    pub fn push_payload_wide(&mut self, value: u64) -> PayloadTag {
        let rank = self.payloads_wide.len() as u32;
        self.payloads_wide.push(value);
        PayloadTag::wide(rank)
    }

    /// Atomically truncate the arena + frame stack + payload columns
    /// to the state at `checkpoint`. Symmetric with
    /// [`Columns::rollback_to`](tape::columns::Columns::rollback_to)
    /// on the tape side — the emitter's retry-IIFE failure path
    /// calls both primitives in lockstep so the two substrates
    /// stay in agreement on which rows exist.
    ///
    /// After rollback, the open stack only retains entries whose
    /// frame offset is below `checkpoint.frame_offset`. Entries at
    /// or above the checkpoint are orphaned frames from the failed
    /// branch and are discarded — the emitter's rollback contract
    /// requires it.
    #[inline]
    pub fn rollback_to(&mut self, checkpoint: ValueCheckpoint) {
        self.frames.truncate(checkpoint.frame_offset as usize);
        self.payloads_narrow.truncate(checkpoint.narrow_rank as usize);
        self.payloads_wide.truncate(checkpoint.wide_rank as usize);
        while let Some(&top) = self.open_stack.last() {
            if top >= checkpoint.frame_offset {
                self.open_stack.pop();
            } else {
                break;
            }
        }
    }

    /// Finish the builder and produce its output.
    ///
    /// The emitter calls this at the parse entry's epilogue, handing
    /// the result to [`Parsed::new_fused`](crate::runtime::Parsed::new_fused).
    /// `root_offset` marks the root frame — typically 0 (the first
    /// frame pushed) but the emitter may override if wrapping frames
    /// were elided.
    #[inline]
    pub fn finish(self, root_offset: u32) -> ValueBuilderOutput<R> {
        debug_assert!(
            self.open_stack.is_empty(),
            "ValueBuilder::finish called with {} open frames remaining",
            self.open_stack.len(),
        );
        ValueBuilderOutput {
            frames: self.frames,
            payloads_narrow: self.payloads_narrow,
            payloads_wide: self.payloads_wide,
            root_offset,
            _root_marker: PhantomData,
        }
    }

    /// Current frame count — emitters that need to compute the next
    /// frame offset before calling `begin_compound` read this.
    #[inline]
    pub fn frame_count(&self) -> u32 {
        self.frames.len() as u32
    }

    /// Compute the subtree size (in frames) of the frame at `offset`.
    ///
    /// For a leaf this is `1`; for a compound this is `1 + sum of
    /// child subtree sizes`. The frames vector is laid out in pre-
    /// order push order, so the subtree is a contiguous range
    /// starting at `offset`.
    #[inline]
    fn subtree_size(frames: &[ValueFrame], offset: usize) -> usize {
        let frame = &frames[offset];
        if frame.child_count == 0 {
            1
        } else {
            // Walk forward `child_count` times, accumulating each
            // subtree size.
            let mut cursor = offset + 1;
            for _ in 0..frame.child_count {
                let size = Self::subtree_size(frames, cursor);
                cursor += size;
            }
            cursor - offset
        }
    }
}

impl<R: ValueRoot> Default for ValueBuilder<R> {
    #[inline]
    fn default() -> Self {
        Self::new(0)
    }
}

/// The finished output of a [`ValueBuilder`] run, carried by
/// [`Parsed`](crate::runtime::Parsed) and consumed by
/// `Parsed::to_value()`.
///
/// Holds the frame arena + scalar payload columns + the root frame
/// offset. Consumers project into `R::Value<'p>` via the grammar-
/// emitted projection logic; the output is opaque from `bbnf` core —
/// it is a grammar-agnostic storage layout that the emitter binds
/// against.
///
/// The output is `Sized` and the frame vector owns its backing
/// memory, so `Parsed<'p, R>` remains `'p`-parameterized solely by
/// the input string borrow; the value substrate is owned.
pub struct ValueBuilderOutput<R> {
    frames: Vec<ValueFrame>,
    payloads_narrow: Vec<u32>,
    payloads_wide: Vec<u64>,
    root_offset: u32,
    _root_marker: PhantomData<R>,
}

impl<R> ValueBuilderOutput<R> {
    /// An empty output, used by substrate-only
    /// [`Parsed::new`](crate::runtime::Parsed::new) constructions
    /// that never reach `to_value()`. Grammar-emitted fused parse
    /// entries populate the full output via
    /// [`ValueBuilder::finish`]; callers reaching for `to_value()`
    /// through the fused entry see the populated arena.
    ///
    /// Empty output is distinguishable via
    /// [`ValueBuilderOutput::is_empty`]; the grammar's emitted
    /// projection treats an empty output as an IR invariant
    /// violation and panics (no silent fallback to tape-walking).
    #[inline]
    pub fn empty() -> Self {
        Self {
            frames: Vec::new(),
            payloads_narrow: Vec::new(),
            payloads_wide: Vec::new(),
            root_offset: 0,
            _root_marker: PhantomData,
        }
    }

    /// Frame count — `0` iff the output is empty.
    #[inline]
    pub fn frame_count(&self) -> usize {
        self.frames.len()
    }

    /// `true` iff the output carries no frames.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.frames.is_empty()
    }

    /// Borrow the frame arena. Grammar-emitted projection logic
    /// reads this to reconstruct the typed `<Grammar>Value`.
    #[inline]
    pub fn frames(&self) -> &[ValueFrame] {
        &self.frames
    }

    /// The root frame's offset within the arena.
    #[inline]
    pub fn root_offset(&self) -> u32 {
        self.root_offset
    }

    /// Read a narrow-column payload by rank.
    #[inline]
    pub fn payload_narrow(&self, rank: u32) -> Option<u32> {
        self.payloads_narrow.get(rank as usize).copied()
    }

    /// Read a wide-column payload by rank.
    #[inline]
    pub fn payload_wide(&self, rank: u32) -> Option<u64> {
        self.payloads_wide.get(rank as usize).copied()
    }

    /// Look up the scalar payload for a leaf frame, if any.
    #[inline]
    pub fn payload_for(&self, frame: &ValueFrame) -> Option<PayloadValue> {
        let tag = frame.payload_tag;
        if tag.is_none() {
            None
        } else if tag.is_narrow() {
            self.payload_narrow(tag.rank()).map(PayloadValue::Narrow)
        } else {
            self.payload_wide(tag.rank()).map(PayloadValue::Wide)
        }
    }

    /// Borrow a frame by offset.
    #[inline]
    pub fn frame(&self, offset: u32) -> Option<&ValueFrame> {
        self.frames.get(offset as usize)
    }

    /// Borrow the root frame directly. Returns `None` for empty
    /// outputs.
    #[inline]
    pub fn root_frame(&self) -> Option<&ValueFrame> {
        self.frame(self.root_offset)
    }

    /// Iterator over the direct children of the compound frame at
    /// `offset`. For leaf frames the iterator is empty.
    ///
    /// Walks the pre-order push arena, taking each direct child and
    /// stepping past its subtree via frame-count accumulation.
    #[inline]
    pub fn children(&self, offset: u32) -> ValueChildren<'_, R> {
        let frame = match self.frames.get(offset as usize) {
            Some(f) => f,
            None => {
                return ValueChildren {
                    output: self,
                    next: u32::MAX,
                    remaining: 0,
                };
            }
        };
        ValueChildren {
            output: self,
            next: frame.first_child,
            remaining: frame.child_count,
        }
    }
}

/// Scalar payload decoded from a [`ValueBuilderOutput`] leaf.
#[derive(Clone, Copy, Debug)]
pub enum PayloadValue {
    /// Narrow-column payload (u32 / bool-as-u32 / u8-as-u32).
    Narrow(u32),
    /// Wide-column payload (f64-bits / u64).
    Wide(u64),
}

impl PayloadValue {
    /// Decode as `f64` (wide column).
    #[inline]
    pub fn as_f64(self) -> Option<f64> {
        match self {
            PayloadValue::Wide(bits) => Some(f64::from_bits(bits)),
            _ => None,
        }
    }

    /// Decode as `u32` (narrow column).
    #[inline]
    pub fn as_u32(self) -> Option<u32> {
        match self {
            PayloadValue::Narrow(v) => Some(v),
            _ => None,
        }
    }

    /// Decode as `u64` (wide column).
    #[inline]
    pub fn as_u64(self) -> Option<u64> {
        match self {
            PayloadValue::Wide(v) => Some(v),
            _ => None,
        }
    }

    /// Decode as `bool` (narrow column; non-zero = true).
    #[inline]
    pub fn as_bool(self) -> Option<bool> {
        match self {
            PayloadValue::Narrow(v) => Some(v != 0),
            _ => None,
        }
    }
}

/// Iterator over the direct children of a compound frame inside a
/// [`ValueBuilderOutput`]. Yields `(offset, frame)` pairs in push
/// order.
pub struct ValueChildren<'o, R> {
    output: &'o ValueBuilderOutput<R>,
    next: u32,
    remaining: u32,
}

impl<'o, R> Iterator for ValueChildren<'o, R> {
    type Item = (u32, &'o ValueFrame);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining == 0 {
            return None;
        }
        let offset = self.next;
        let frame = self.output.frames.get(offset as usize)?;
        // Step past this child's full subtree to locate the next
        // direct sibling.
        let size = Self::subtree_size(&self.output.frames, offset as usize);
        self.next = offset + size as u32;
        self.remaining -= 1;
        Some((offset, frame))
    }
}

impl<'o, R> ValueChildren<'o, R> {
    #[inline]
    fn subtree_size(frames: &[ValueFrame], offset: usize) -> usize {
        let frame = &frames[offset];
        if frame.child_count == 0 {
            1
        } else {
            let mut cursor = offset + 1;
            for _ in 0..frame.child_count {
                let size = Self::subtree_size(frames, cursor);
                cursor += size;
            }
            cursor - offset
        }
    }
}

// ─── Test-only parse-count instrumentation ─────────────────────────
//
// AY-II.W0.c invariant — the apples-to-apples test asserts that
// `Parsed::to_value()` does NOT trigger a second parse. We
// instrument `ValueBuilder::new` with a per-thread counter so the
// test can observe how many times a value substrate was constructed
// (one per parse invocation). The counter is gated on `#[cfg(test)]`
// so production builds pay zero overhead.
#[cfg(test)]
thread_local! {
    static NEW_CALL_COUNT: ::core::cell::Cell<u64> = const { ::core::cell::Cell::new(0) };
}

/// Test-only accessor — returns the count of [`ValueBuilder::new`]
/// invocations on the current thread. `Parsed::to_value()` must not
/// increment this counter.
#[cfg(test)]
pub fn value_builder_new_call_count() -> u64 {
    NEW_CALL_COUNT.with(|c| c.get())
}

/// Test-only reset — sets the [`ValueBuilder::new`] counter to `0`.
#[cfg(test)]
pub fn reset_value_builder_new_call_count() {
    NEW_CALL_COUNT.with(|c| c.set(0));
}
