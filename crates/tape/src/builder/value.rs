//! Value-side state the fused builder owns alongside the structural
//! columns.
//!
//! # Role (Tranche AY-II.W0'.a)
//!
//! W0.c introduced a standalone `ValueBuilder<R>` type at
//! `crates/core/src/runtime/value_builder.rs` that every emitted shape
//! was supposed to drive in lockstep with the structural tape push.
//! The threading never actually landed — the slab stayed empty and
//! `Parsed::to_value()` panicked. W0'.a absorbs the substrate into
//! [`FusedBuilder`](super::FusedBuilder) at the type level so there
//! is no second builder to thread: every `begin_compound` /
//! `end_compound` / `push_leaf_*` stamps BOTH columns atomically.
//!
//! The types in this module are the write-side bookkeeping the
//! fused builder carries and the read-side surface it hands off at
//! `finish` time. They mirror the pre-W0'.a
//! `runtime::value_builder::{ValueFrame, PayloadTag, ValueCheckpoint,
//! ValueBuilderOutput}` surface so downstream emitted projection code
//! compiles unchanged while regen composes in the renamed type.
//!
//! # Grammar-agnostic storage
//!
//! [`ValueFrame`] records are laid out in emitter push order; the
//! compound open/close discipline threads `(first_child, child_count)`
//! so the projection layer can walk direct children without a second
//! index. Scalar payloads live in two parallel typed columns
//! ([`ValueFramesOutput::payloads_narrow`] /
//! [`ValueFramesOutput::payloads_wide`]) and leaves carry a
//! [`PayloadTag`] identifying the column + rank.
//!
//! The `R` phantom on [`FusedOutput`] is the grammar-root binding the
//! emitted `project_value_output<'p>(output: &FusedOutput<R>, input)`
//! reads — the storage is otherwise grammar-agnostic.

use std::marker::PhantomData;

use crate::kind::TapeKind;

/// Grammar-agnostic structural frame inside the fused builder's value
/// arena.
///
/// One frame per emitter `begin_compound` / `push_leaf_*` call.
/// Compounds carry a contiguous child range (`first_child` +
/// `child_count`) indexing back into the arena; leaves carry
/// `child_count == 0` and a [`PayloadTag`] pointing at the scalar
/// payload column.
///
/// The encoding is stable across grammars — a frame tree produced by
/// parse A is structurally compatible with the projection logic
/// emitted for grammar A; the projection dispatches on
/// `(kind, variant_idx)` to pick the right `<Grammar>Value` variant.
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
    /// Rule-identity discriminator matching the emitter's `RuleKind`
    /// dispatch. `0` for structural shapes without a declared rule
    /// binding (the projection layer re-derives via `kind` +
    /// `variant_idx` fallback).
    pub variant_idx: u8,
    /// Kind discriminator — mirrors the tape record's [`TapeKind`].
    pub kind: TapeKind,
    /// Scalar payload handle or [`PayloadTag::NONE`] for compounds.
    pub payload_tag: PayloadTag,
}

/// Handle into the fused builder's scalar payload columns.
///
/// Payloads are appended in push order; leaves record the rank +
/// column tag. Wide scalars (`f64`, `u64`) land in the `wide` column;
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

/// Opaque checkpoint produced by the value substrate at compound-open
/// time and consumed by rollback.
///
/// Encodes the arena + payload-column sizes at the open point so
/// rollback truncates every family atomically. The fused builder
/// surfaces a single `u32` tape-offset to the emitter; the value
/// substrate maintains this richer checkpoint internally alongside
/// each open frame.
#[derive(Clone, Copy, Debug)]
pub(super) struct ValueCheckpoint {
    /// Arena frame offset at open time. `frames.len()` snaps back to
    /// this value on rollback; the frame itself is pushed at this
    /// index.
    pub(super) frame_offset: u32,
    /// Narrow payload column rank at open time.
    pub(super) narrow_rank: u32,
    /// Wide payload column rank at open time.
    pub(super) wide_rank: u32,
}

/// The finished value substrate handed off to
/// [`Parsed`](crate::runtime::Parsed)-style consumers at
/// [`FusedBuilder::finish`](super::FusedBuilder::finish) time.
///
/// Holds the frame arena + scalar payload columns + root-frame offset.
/// Consumers project into `R::Value<'p>` via the grammar-emitted
/// projection logic; the output is opaque from `tape` — it is a
/// grammar-agnostic storage layout that the emitter binds against.
///
/// Carried by [`FusedOutput`] alongside the finalised [`Tape`] so the
/// fused parse entry returns both substrates in one call.
pub struct ValueFramesOutput<R> {
    pub(super) frames: Vec<ValueFrame>,
    pub(super) payloads_narrow: Vec<u32>,
    pub(super) payloads_wide: Vec<u64>,
    pub(super) root_offset: u32,
    pub(super) _root_marker: PhantomData<R>,
}

impl<R> ValueFramesOutput<R> {
    /// An empty value substrate — used by substrate-only
    /// [`Parsed::new`](crate::runtime::Parsed::new) constructions that
    /// never reach `to_value()`. Grammar-emitted fused parse entries
    /// populate the full output via
    /// [`FusedBuilder::finish`](super::FusedBuilder::finish); callers
    /// reaching for `to_value()` through the fused entry see the
    /// populated arena.
    ///
    /// Empty output is distinguishable via [`Self::is_empty`]; the
    /// grammar's emitted projection treats an empty output as an IR
    /// invariant violation and panics (no silent fallback to
    /// tape-walking).
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

    /// Borrow the frame arena. Grammar-emitted projection logic reads
    /// this to reconstruct the typed `<Grammar>Value`.
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

/// Scalar payload decoded from a [`ValueFramesOutput`] leaf.
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
/// [`ValueFramesOutput`]. Yields `(offset, frame)` pairs in push
/// order.
pub struct ValueChildren<'o, R> {
    output: &'o ValueFramesOutput<R>,
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
        let size = subtree_size(&self.output.frames, offset as usize);
        self.next = offset + size as u32;
        self.remaining -= 1;
        Some((offset, frame))
    }
}

/// Compute the subtree size (in frames) of the frame at `offset`.
///
/// For a leaf this is `1`; for a compound this is `1 + sum of child
/// subtree sizes`. The frames vector is laid out in pre-order push
/// order, so the subtree is a contiguous range starting at `offset`.
#[inline]
pub(super) fn subtree_size(frames: &[ValueFrame], offset: usize) -> usize {
    let frame = &frames[offset];
    if frame.child_count == 0 {
        1
    } else {
        let mut cursor = offset + 1;
        for _ in 0..frame.child_count {
            let size = subtree_size(frames, cursor);
            cursor += size;
        }
        cursor - offset
    }
}
