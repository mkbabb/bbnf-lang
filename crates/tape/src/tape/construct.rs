//! Construction, capacity, querying, and finalisation surfaces for `Tape<R>`.
//!
//! Pre-B5.W1 these primitives lived split across `Tape::new`,
//! `FusedBuilder::new`, and `FusedOutput::*`. B5.W1 collapses them onto
//! one type; this sub-module owns the constructors, position cursor,
//! row-by-offset materialisation, error stamping, rollback, and
//! `finish` finalisation.

use core::marker::PhantomData;

use crate::columns::Columns;

use super::{NEW_CALL_COUNT, Tape, TapeBuildError, TapeIter, TapeOffset, TapeRec};

impl<R> Tape<R> {
    /// Construct an empty tape.
    #[inline]
    pub fn new() -> Self {
        NEW_CALL_COUNT.with(|c| c.set(c.get() + 1));
        Self::default()
    }

    /// Construct an empty tape with `expected` records pre-reserved
    /// across every column the substrate owns.
    ///
    /// Callers presize via the per-grammar push fingerprint:
    /// `GRAMMAR_PROFILE.capacity_for(input.len())`. The reservation
    /// covers `records` (16 B AoS rows) + `sib_skip` (4 B parallel
    /// column) + value-side substrate columns in lockstep so the hot
    /// push path never trips a `Vec::push` realloc on corpus input.
    #[inline]
    pub fn with_capacity(expected: usize) -> Self {
        NEW_CALL_COUNT.with(|c| c.set(c.get() + 1));
        Self {
            columns: Columns::with_capacity(expected),
            error: None,
            root_offset: 0,
            _root_marker: PhantomData,
        }
    }

    /// Construct an empty tape sized from a [`crate::GrammarProfile`]
    /// + `input_len`.
    #[inline]
    pub fn with_capacity_for(profile: &crate::GrammarProfile, input_len: usize) -> Self {
        Self::with_capacity(profile.capacity_for(input_len))
    }

    /// Number of records appended to the tape so far.
    #[inline]
    pub fn len(&self) -> usize {
        self.columns.len()
    }

    /// True iff no records have been appended.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.columns.is_empty()
    }

    /// The current write position — the offset where the NEXT
    /// `push_*` will land.
    ///
    /// B5.W1: replaces the pre-W1 `builder.columns_mut().len() as u32`
    /// idiom every emitter retry-IIFE used to capture an open offset
    /// before probing a branch. Generated parsers call
    /// `tape.position()` before each rollback-eligible branch and pass
    /// the returned `u32` to [`Self::rollback_to`] on failure.
    #[inline(always)]
    pub fn position(&self) -> u32 {
        self.columns.records.len() as u32
    }

    /// Borrow the underlying [`Columns`] substrate.
    #[inline]
    pub fn columns(&self) -> &Columns {
        &self.columns
    }

    // ── Read accessors — column-indexed materialisation ──────────────

    /// Look up a record by offset. Panics on out-of-range offsets.
    #[inline(always)]
    pub fn get(&self, offset: TapeOffset) -> TapeRec {
        debug_assert!(
            !offset.is_none(),
            "Tape::get called with TapeOffset::NONE sentinel"
        );
        self.columns.materialize(offset.0)
    }

    /// Look up a record by offset **without bounds checking**.
    ///
    /// # Safety
    ///
    /// The caller must guarantee that `offset` is not
    /// [`TapeOffset::NONE`] and that `offset.0 as usize` is less
    /// than `self.len()`.
    #[inline(always)]
    pub unsafe fn get_unchecked(&self, offset: TapeOffset) -> TapeRec {
        debug_assert!(
            !offset.is_none(),
            "Tape::get_unchecked called with TapeOffset::NONE sentinel"
        );
        debug_assert!(
            (offset.0 as usize) < self.columns.len(),
            "Tape::get_unchecked: offset {} out of range (len {})",
            offset.0,
            self.columns.len()
        );
        // SAFETY: caller guarantees offset is in bounds.
        unsafe { self.columns.materialize_unchecked(offset.0) }
    }

    /// Look up a record by offset, returning `None` for the sentinel
    /// or out-of-range offsets.
    #[inline]
    pub fn try_get(&self, offset: TapeOffset) -> Option<TapeRec> {
        if offset.is_none() {
            return None;
        }
        let idx = offset.0 as usize;
        if idx >= self.columns.len() {
            return None;
        }
        Some(self.columns.materialize(offset.0))
    }

    /// Iterate every record in insertion order.
    pub fn iter(&self) -> TapeIter<'_, R> {
        TapeIter::new(&self.columns)
    }

    /// Mark the parse as failed with an offset and optional rule
    /// label.
    pub fn set_error(&mut self, offset: u32, rule_label: u32) {
        if self.error.is_none() {
            self.error = Some(TapeBuildError::ParseFailed {
                offset,
                rule_label,
            });
        }
    }

    // ── Rollback ──────────────────────────────────────────────────

    /// Rewind every column family — structural tape, inline
    /// `frame_depth`, and the value substrate — back to the state at
    /// the matching `begin_compound` whose `open_offset` the caller
    /// passes in.
    ///
    /// B5.W1: the sole rollback primitive across both column families.
    /// Delegates to [`Columns::rollback_to`] which handles tape-side +
    /// value-side substrates atomically in one call.
    #[inline(always)]
    pub fn rollback_to(&mut self, open_offset: u32) {
        self.columns.rollback_to(open_offset);
    }

    // ── Finalisation — Stage-C sib_skip + close-compound back-patch ──

    /// Consume the tape's write surface, run the Stage-C finaliser,
    /// stamp the root offset, and return `Self` ready for read access.
    ///
    /// B5.W1: replaces the pre-W1 `FusedBuilder::finish_fused` /
    /// `FusedBuilder::finish` pair with a single `finish` that
    /// preserves both the structural tape and the value substrate
    /// inside the same `Tape<R>`. The grammar-emitted parse entry
    /// calls `tape.finish(root_off)` and the returned tape feeds
    /// `Parsed::new`.
    #[inline(always)]
    pub fn finish(mut self, root_off: u32) -> Result<Self, TapeBuildError> {
        if let Some(err) = self.error.take() {
            return Err(err);
        }
        debug_assert!(
            self.columns.value_open_stack.is_empty(),
            "Tape::finish called with {} open value frames remaining",
            self.columns.value_open_stack.len(),
        );
        debug_assert_eq!(
            self.columns.frame_depth.len(),
            self.columns.len(),
            "frame_depth length {} != records length {} \
             (every structural push must stamp frame_depth in lockstep)",
            self.columns.frame_depth.len(),
            self.columns.len(),
        );
        self.columns.run_finalise();
        self.root_offset = root_off;
        Ok(self)
    }
}
