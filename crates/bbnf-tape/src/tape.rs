//! `Tape`, `TapeRec`, `TapeOffset` — the core fixed-size record
//! substrate that replaces the eager typed AST.

use crate::arena::ChunkedArena;
use crate::kind::TapeKind;

/// Stable index into a [`Tape`]'s record stream.
///
/// Constructed by [`TapeBuilder::push_leaf`] /
/// [`TapeBuilder::push_compound`] and consumed by view-layer accessors
/// via [`Tape::get`]. Two offsets compare equal iff they point to the
/// same record in the same tape; cross-tape comparison is a logic bug
/// the view codegen prevents by tying every view type to a `'tape`
/// lifetime parameter.
///
/// [`TapeBuilder::push_leaf`]: crate::builder::TapeBuilder::push_leaf
/// [`TapeBuilder::push_compound`]: crate::builder::TapeBuilder::push_compound
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct TapeOffset(pub u32);

impl TapeOffset {
    /// Sentinel used for "no children" / "end of a compound run".
    pub const NONE: TapeOffset = TapeOffset(u32::MAX);

    /// Treat this offset as a raw integer index.
    #[inline]
    pub fn as_u32(self) -> u32 {
        self.0
    }

    /// Is this the sentinel "none" offset?
    #[inline]
    pub fn is_none(self) -> bool {
        self.0 == u32::MAX
    }
}

/// Fixed-size tape record.
///
/// Layout: 16 bytes, `#[repr(C)]`. One quarter of a 64-byte cache line
/// so four consecutive records fit in one line and sequential scans
/// enjoy the hardware prefetcher without strided pointer chasing.
///
/// - `kind` — classifies the record (rule entry, leaf span, compound,
///   keyword tag, etc.). The view layer dispatches on this byte.
/// - `flags` — bitfield: variant index within the rule's enum (low 6
///   bits), `has_children`, `span_only`. Codegen assigns.
/// - `span_lo` / `span_hi` — byte offsets into the source input.
///   `span_hi == span_lo` means a zero-width record (epsilon match).
/// - `child_off` — `TapeOffset` of the first child record for
///   compound nodes. `TapeOffset::NONE` for leaves. The children run
///   from `child_off` (inclusive) to the next compound's `child_off`
///   (exclusive) — the tape is written in pre-order, so siblings are
///   contiguous.
#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TapeRec {
    /// Classification tag — dispatches the view layer.
    pub kind: TapeKind,
    /// Bitfield: variant index (low 6 bits), has_children, span_only.
    pub flags: u8,
    /// Reserved bits; zero-initialized by the builder.
    pub _reserved: [u8; 2],
    /// Byte offset into the source input where this record's span begins.
    pub span_lo: u32,
    /// Byte offset into the source input where this record's span ends.
    /// `span_hi == span_lo` represents epsilon / zero-width matches.
    pub span_hi: u32,
    /// First child's offset, or [`TapeOffset::NONE`] for leaves.
    pub child_off: TapeOffset,
}

// Compile-time size + alignment sanity check — any change to the
// layout that grows the record unexpectedly breaks CI.
const _: () = {
    assert!(std::mem::size_of::<TapeRec>() == 16);
    assert!(std::mem::align_of::<TapeRec>() == 4);
};

impl TapeRec {
    /// Extract the variant index from `flags` (low 6 bits).
    #[inline]
    pub fn variant_idx(&self) -> u8 {
        self.flags & 0x3F
    }

    /// Does this record have children? (Bit 6 of `flags`.)
    #[inline]
    pub fn has_children(&self) -> bool {
        (self.flags & 0x40) != 0
    }

    /// Is this record span-only? (Bit 7 of `flags` — set by the tape
    /// emitter for rules projected as `Span` rather than as a typed
    /// compound.)
    #[inline]
    pub fn is_span_only(&self) -> bool {
        (self.flags & 0x80) != 0
    }

    /// Byte length of the record's source span.
    #[inline]
    pub fn span_len(&self) -> u32 {
        self.span_hi.saturating_sub(self.span_lo)
    }
}

/// The parser's output tape.
///
/// Owns the chunked arena of [`TapeRec`]s plus any per-tape scratch.
/// Built by [`crate::TapeBuilder`] during parsing; read by the generated
/// view layer via [`Tape::get`] / children offsets. Lives for the
/// lifetime of the input buffer — the `'tape` lifetime on every view
/// type ties back to this.
#[derive(Debug)]
pub struct Tape {
    /// Chunked record storage. Append-only during parsing; immutable
    /// during view-layer reads.
    pub(crate) records: ChunkedArena<TapeRec>,
}

impl Tape {
    /// Construct an empty tape with a single 64 KB chunk preallocated.
    pub fn new() -> Self {
        Self { records: ChunkedArena::new() }
    }

    /// Construct an empty tape sized for `expected` records. Rounds up
    /// to the next chunk boundary.
    pub fn with_capacity(expected: usize) -> Self {
        Self {
            records: ChunkedArena::with_capacity(expected),
        }
    }

    /// Number of records appended to the tape so far.
    #[inline]
    pub fn len(&self) -> usize {
        self.records.len()
    }

    /// True iff no records have been appended.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.records.is_empty()
    }

    /// Look up a record by offset. Panics on out-of-range offsets —
    /// view codegen never produces out-of-range offsets because every
    /// offset originates from a `TapeBuilder::push_*` call.
    #[inline]
    pub fn get(&self, offset: TapeOffset) -> &TapeRec {
        debug_assert!(
            !offset.is_none(),
            "Tape::get called with TapeOffset::NONE sentinel"
        );
        self.records.get(offset.0 as usize)
    }

    /// Look up a record by offset, returning `None` for the sentinel
    /// or out-of-range offsets.
    #[inline]
    pub fn try_get(&self, offset: TapeOffset) -> Option<&TapeRec> {
        if offset.is_none() {
            return None;
        }
        self.records.try_get(offset.0 as usize)
    }

    /// Iterate every record in insertion order.
    pub fn iter(&self) -> impl Iterator<Item = &TapeRec> + '_ {
        self.records.iter()
    }
}

impl Default for Tape {
    fn default() -> Self {
        Self::new()
    }
}
