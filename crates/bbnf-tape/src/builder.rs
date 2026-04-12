//! `TapeBuilder` — the parser's write interface to the [`Tape`].
//!
//! The generated Rust parser calls `TapeBuilder::push_*` methods to
//! append records as each rule / Seq / Alt matches. The builder owns
//! the growing [`Tape`] plus sticky error state so failed sub-tree
//! matches don't poison the rest of the parse.

use crate::kind::TapeKind;
use crate::tape::{Tape, TapeOffset, TapeRec};

/// The parser's write interface to the tape.
///
/// Held by `&mut` for the duration of a parse. The generated parser
/// functions thread it through every rule call alongside the
/// `ParserState`:
///
/// ```ignore
/// fn __pair<'i>(
///     state: &mut parse_that::ParserState<'i>,
///     tape: &mut bbnf_tape::TapeBuilder,
/// ) -> Option<bbnf_tape::TapeOffset> {
///     let start_off = tape.mark_children();
///     let _key = __string(state, tape)?;
///     state.eat_byte(b':')?;
///     state.skip_ws();
///     let _value = __value(state, tape)?;
///     Some(tape.push_compound(TapeKind::Rule, start_off, state.offset))
/// }
/// ```
#[derive(Debug, Default)]
pub struct TapeBuilder {
    /// The tape being assembled. Owned by the builder for the
    /// duration of the parse; consumed via [`Self::finish`] at the
    /// end.
    pub(crate) tape: Tape,
    /// Sticky error state. Once set, subsequent `push_*` calls are
    /// still accepted (so mid-recovery parses can continue producing
    /// records for partial success), but `finish` returns the error.
    pub(crate) error: Option<TapeBuildError>,
}

/// Error state surfaced through [`TapeBuilder::finish`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TapeBuildError {
    /// The caller reported an unrecoverable parse failure. Constructed
    /// by the generated parser when a non-recoverable branch fails.
    ParseFailed {
        /// Input byte offset where the failure was detected.
        offset: u32,
        /// Optional rule-name id / label for diagnostics.
        rule_label: u32,
    },
}

impl TapeBuilder {
    /// Construct a fresh builder with an empty tape.
    pub fn new() -> Self {
        Self::default()
    }

    /// Construct a builder sized for `expected` records.
    pub fn with_capacity(expected: usize) -> Self {
        Self {
            tape: Tape::with_capacity(expected),
            error: None,
        }
    }

    /// Record the current tape length as the start of a children run.
    ///
    /// Call this before pushing a compound's children. The returned
    /// offset is passed to [`Self::push_compound`] as the `child_off`
    /// field.
    #[inline]
    pub fn mark_children(&self) -> TapeOffset {
        TapeOffset(self.tape.len() as u32)
    }

    /// Append a leaf record with a concrete kind + span.
    ///
    /// Leaves have no children, so `child_off` is forced to
    /// [`TapeOffset::NONE`].
    #[inline]
    pub fn push_leaf(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        variant_idx: u8,
    ) -> TapeOffset {
        debug_assert!(kind.is_leaf(), "push_leaf on compound kind {:?}", kind);
        let idx = self.tape.records.len();
        self.tape.records.push(TapeRec {
            kind,
            flags: variant_idx & 0x3F,
            _reserved: [0, 0],
            span_lo,
            span_hi,
            child_off: TapeOffset::NONE,
        });
        TapeOffset(idx as u32)
    }

    /// Append a compound record pointing at a previously-marked
    /// children run.
    ///
    /// The caller must have called [`Self::mark_children`] BEFORE
    /// pushing the first child, and must pass the returned offset as
    /// `child_off`. `span_hi` is the parser state's current offset
    /// (end of the compound's source range).
    #[inline]
    pub fn push_compound(
        &mut self,
        kind: TapeKind,
        child_off: TapeOffset,
        span_lo: u32,
        span_hi: u32,
        variant_idx: u8,
    ) -> TapeOffset {
        debug_assert!(
            kind.is_compound(),
            "push_compound on leaf/annotation kind {:?}",
            kind
        );
        // `has_children` is true iff the caller actually pushed
        // records between `mark_children` and this call. When the
        // child run is empty, `child_off` equals the parent's own
        // index, which would form a cycle for `TapeCursor::children`
        // / `subtree_size`. The safe thing is to clear the flag and
        // leave the child_off field untouched — cursor accessors
        // check `has_children` first.
        let parent_idx = self.tape.records.len();
        let has_children = (child_off.0 as usize) < parent_idx;
        let flags = (variant_idx & 0x3F) | if has_children { 0x40 } else { 0 };
        let idx = parent_idx;
        self.tape.records.push(TapeRec {
            kind,
            flags,
            _reserved: [0, 0],
            span_lo,
            span_hi,
            child_off,
        });
        TapeOffset(idx as u32)
    }

    /// Mark the parse as failed with an offset and optional rule label.
    /// The builder continues to accept pushes (so recovery paths can
    /// produce partial tapes) but [`Self::finish`] returns the error.
    pub fn set_error(&mut self, offset: u32, rule_label: u32) {
        if self.error.is_none() {
            self.error = Some(TapeBuildError::ParseFailed {
                offset,
                rule_label,
            });
        }
    }

    /// Consume the builder and return the finished tape. Returns the
    /// sticky error if one was set during parsing.
    pub fn finish(self) -> Result<Tape, TapeBuildError> {
        match self.error {
            Some(err) => Err(err),
            None => Ok(self.tape),
        }
    }

    /// Access the in-progress tape for debug / intermediate inspection.
    /// Primarily a test hook — production parsers use `finish()`.
    pub fn tape(&self) -> &Tape {
        &self.tape
    }
}
