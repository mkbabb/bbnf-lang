//! Value-side substrate types — frames, payload tags, projection
//! handles.
//!
//! # Role (B5.W1 substrate boundary restoration)
//!
//! Pre-B5.W1 the value substrate lived behind a `FusedBuilder` /
//! `FusedOutput<R>` / `ValueFramesOutput<R>` triumvirate that welded
//! the structural tape to a parallel value-frame arena across two
//! crates. B5.W1 promotes the substrate's value-side state (frames,
//! payload columns, open-stack checkpoints) onto [`Columns`](crate::Columns)
//! and the read-side accessors onto [`Tape<R>`](crate::Tape) directly,
//! retiring the wrapper triumvirate. The types in this module are the
//! storage shapes [`Columns`](crate::Columns) holds and the projection
//! consumers read.
//!
//! # Grammar-agnostic storage
//!
//! [`ValueFrame`] records are laid out in emitter push order; the
//! compound open/close discipline threads `(first_child, child_count)`
//! so the projection layer can walk direct children without a second
//! index. Scalar payloads live in two parallel typed columns
//! ([`Columns::value_payloads_narrow`](crate::Columns) /
//! [`Columns::value_payloads_wide`](crate::Columns)) and leaves carry
//! a [`PayloadTag`] identifying the column + rank.
//!
//! The `R` phantom on [`crate::Tape`] is the grammar-root binding the
//! emitted `project_value_output<'p>(tape: &Tape<R>, input)` reads —
//! the storage is otherwise grammar-agnostic.

use crate::kind::TapeKind;

/// Grammar-agnostic structural frame inside the value substrate.
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
    /// Index of the first child frame in the value arena; unused for
    /// leaves (`child_count == 0`).
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

/// Handle into the value substrate's scalar payload columns.
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
///
/// # Direct-child counter (AY-II.W0'.d3)
///
/// `direct_child_count` is the in-stack tally the builder increments
/// on every frame push that lands as a direct child of this open
/// compound — every `push_value_leaf` call while this checkpoint is
/// the top-of-stack, and every nested `value_begin_compound` call
/// whose parent checkpoint is this one (incremented on the parent's
/// counter, second-from-top after the new checkpoint pushes). At
/// `value_end_compound` time the counter is read directly into
/// `ValueFrame::child_count`, replacing the O(subtree_size) walk
/// landed in W0'.a.
#[derive(Clone, Copy, Debug)]
pub struct ValueCheckpoint {
    /// Arena frame offset at open time. `frames.len()` snaps back to
    /// this value on rollback; the frame itself is pushed at this
    /// index.
    pub frame_offset: u32,
    /// Narrow payload column rank at open time.
    pub narrow_rank: u32,
    /// Wide payload column rank at open time.
    pub wide_rank: u32,
    /// Count of direct children pushed under this checkpoint since
    /// `value_begin_compound`. Incremented by every `push_value_leaf`
    /// whose parent is this checkpoint (top-of-stack) and by every
    /// nested `value_begin_compound` (increment on the parent, i.e.
    /// second-from-top after the nested push). Consumed by
    /// `value_end_compound` into `ValueFrame::child_count` — O(1)
    /// replacement for the pre-W0'.d3 `subtree_size` walk.
    pub direct_child_count: u32,
    /// Tape-side row offset for the compound row this checkpoint
    /// pairs with. Stamped at `begin_compound` time alongside the
    /// matching tape row push; consumed by `Columns::rollback_to` to
    /// identify every checkpoint whose paired compound row lives at
    /// or above the rollback boundary, so a single
    /// `rollback_to(open_offset)` call unwinds tape and value
    /// substrates atomically regardless of how many compounds the
    /// failed branch opened.
    pub tape_idx: u32,
}

/// Scalar payload decoded from a value-substrate leaf.
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

/// Iterator over the direct children of a value-substrate compound
/// frame. Yields `(offset, frame)` pairs in push order.
pub struct ValueChildren<'t, R> {
    pub(crate) tape: &'t crate::tape::Tape<R>,
    pub(crate) next: u32,
    pub(crate) remaining: u32,
}

impl<'t, R> Iterator for ValueChildren<'t, R> {
    type Item = (u32, &'t ValueFrame);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining == 0 {
            return None;
        }
        let offset = self.next;
        let frame = self.tape.columns().value_frames.get(offset as usize)?;
        // Step past this child's full subtree to locate the next
        // direct sibling.
        let size = subtree_size(&self.tape.columns().value_frames, offset as usize);
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
pub(crate) fn subtree_size(frames: &[ValueFrame], offset: usize) -> usize {
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
