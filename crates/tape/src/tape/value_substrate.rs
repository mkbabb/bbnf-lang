//! Value-substrate accessors and write helpers for `Tape<R>`.
//!
//! Pre-B5.W1 these surfaces lived on `FusedOutput<R>` (read) and
//! `FusedBuilder` (write). B5.W1 absorbs both onto `Tape<R>` directly.
//! This sub-module owns frame query, value-payload column reads,
//! frame-children iteration, and the lockstep value-arena writes
//! (`value_begin_compound`, `value_end_compound`, `push_value_leaf`)
//! the structural push paths in `push.rs` invoke.

use crate::kind::TapeKind;
use crate::value::{
    PayloadTag, PayloadValue, ValueChildren, ValueCheckpoint, ValueFrame,
};

use super::Tape;

impl<R> Tape<R> {
    // ── Value substrate read accessors (was FusedOutput<R>) ──────────

    /// Total value-frame count.
    #[inline]
    pub fn frame_count(&self) -> usize {
        self.columns.value_frames.len()
    }

    /// `true` iff the value substrate carries no frames.
    #[inline]
    pub fn frames_is_empty(&self) -> bool {
        self.columns.value_frames.is_empty()
    }

    /// Borrow the value-frame arena directly.
    #[inline]
    pub fn frames(&self) -> &[ValueFrame] {
        &self.columns.value_frames
    }

    /// The root frame's offset within the value-frame arena.
    /// Projection consumers begin descent here.
    #[inline]
    pub fn root_offset(&self) -> u32 {
        self.root_offset
    }

    /// Borrow a value frame by offset.
    #[inline]
    pub fn frame(&self, offset: u32) -> Option<&ValueFrame> {
        self.columns.value_frames.get(offset as usize)
    }

    /// Borrow the root value frame directly. Returns `None` for
    /// substrate-empty tapes.
    #[inline]
    pub fn root_frame(&self) -> Option<&ValueFrame> {
        self.frame(self.root_offset)
    }

    /// Read a narrow-column value-substrate payload by rank.
    #[inline]
    pub fn value_payload_narrow(&self, rank: u32) -> Option<u32> {
        self.columns.value_payloads_narrow.get(rank as usize).copied()
    }

    /// Read a wide-column value-substrate payload by rank.
    #[inline]
    pub fn value_payload_wide(&self, rank: u32) -> Option<u64> {
        self.columns.value_payloads_wide.get(rank as usize).copied()
    }

    /// Look up the scalar payload for a leaf value-substrate frame.
    #[inline]
    pub fn payload_for(&self, frame: &ValueFrame) -> Option<PayloadValue> {
        let tag = frame.payload_tag;
        if tag.is_none() {
            None
        } else if tag.is_narrow() {
            self.value_payload_narrow(tag.rank())
                .map(PayloadValue::Narrow)
        } else {
            self.value_payload_wide(tag.rank()).map(PayloadValue::Wide)
        }
    }

    /// Iterator over the direct children of the value-substrate
    /// compound frame at `offset`. For leaf frames the iterator is
    /// empty.
    #[inline]
    pub fn children(&self, offset: u32) -> ValueChildren<'_, R> {
        let frame = match self.columns.value_frames.get(offset as usize) {
            Some(f) => f,
            None => {
                return ValueChildren {
                    tape: self,
                    next: u32::MAX,
                    remaining: 0,
                };
            }
        };
        ValueChildren {
            tape: self,
            next: frame.first_child,
            remaining: frame.child_count,
        }
    }

    // ── Value substrate write helpers (was FusedBuilder internals) ──

    /// Open a value-arena frame in lockstep with the tape's
    /// `begin_compound`. Pushes a compound frame + checkpoint onto
    /// the open-stack and bumps the parent checkpoint's
    /// `direct_child_count`.
    #[inline(always)]
    pub(super) fn value_begin_compound(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        variant_idx: u8,
        tape_idx: u32,
    ) {
        if let Some(parent) = self.columns.value_open_stack.last_mut() {
            parent.direct_child_count += 1;
        }
        let frame_offset = self.columns.value_frames.len() as u32;
        self.columns.value_frames.push(ValueFrame {
            span_lo,
            span_hi: span_lo,
            first_child: frame_offset + 1,
            child_count: 0,
            variant_idx,
            kind,
            payload_tag: PayloadTag::NONE,
        });
        self.columns.value_open_stack.push(ValueCheckpoint {
            frame_offset,
            narrow_rank: self.columns.value_payloads_narrow.len() as u32,
            wide_rank: self.columns.value_payloads_wide.len() as u32,
            direct_child_count: 0,
            tape_idx,
        });
    }

    /// Close the most recently opened value frame.
    #[inline(always)]
    pub(super) fn value_end_compound(&mut self, span_hi: u32) {
        let checkpoint = self
            .columns
            .value_open_stack
            .pop()
            .expect("Tape::value_end_compound called with empty open_stack");
        let frame =
            &mut self.columns.value_frames[checkpoint.frame_offset as usize];
        frame.span_hi = span_hi;
        frame.child_count = checkpoint.direct_child_count;
    }

    /// Append a leaf value frame.
    #[inline(always)]
    pub(super) fn push_value_leaf(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        variant_idx: u8,
        payload_tag: PayloadTag,
    ) {
        if let Some(parent) = self.columns.value_open_stack.last_mut() {
            parent.direct_child_count += 1;
        }
        self.columns.value_frames.push(ValueFrame {
            span_lo,
            span_hi,
            first_child: 0,
            child_count: 0,
            variant_idx,
            kind,
            payload_tag,
        });
    }
}
