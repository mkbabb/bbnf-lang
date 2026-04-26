//! `FusedOutput` — the combined substrate
//! [`FusedBuilder::finish`](super::FusedBuilder::finish) returns.
//!
//! Wraps the finalised [`Tape`](crate::Tape) alongside the
//! [`ValueFramesOutput`] produced during the fused parse. `Parsed<'p,
//! R>` carries one `FusedOutput<R>`; the `to_value()` projector reads
//! value frames directly off the output without touching the tape.
//!
//! W0.c's standalone value-builder output retired in favour of
//! `FusedOutput<R>` at AY-II.W0'.a; the projection emitter consumes
//! the fused output directly.

use crate::tape::Tape;

use super::value::{PayloadValue, ValueChildren, ValueFrame, ValueFramesOutput};

/// Combined tape + value output produced by
/// [`FusedBuilder::finish`](super::FusedBuilder::finish).
///
/// Holds the finalised [`Tape`] plus the grammar-agnostic
/// [`ValueFramesOutput<R>`] the fused parse wrote alongside every
/// compound open/close and leaf push. Downstream callers project:
///
/// - structural reads go through [`Self::tape`] → cursor-backed view
///   surface.
/// - semantic reads go through the value accessors
///   ([`Self::value_frame_at`], [`Self::value_payload_for`],
///   [`Self::value_children`]) into the grammar's emitted projection
///   logic.
///
/// The `R` phantom ties the output to its grammar root so the
/// `ValueRoot::project_value_output<'p>(output: &FusedOutput<R>, ...)`
/// signature stays well-typed across grammars coexisting in one
/// binary.
pub struct FusedOutput<R> {
    tape: Tape,
    value: ValueFramesOutput<R>,
}

impl<R> FusedOutput<R> {
    /// Construct a fused output from the two finalised substrates.
    /// The fused builder is the sole caller in production; tests
    /// exercise the constructor directly for substrate-only fixtures.
    #[inline]
    pub fn new(tape: Tape, value: ValueFramesOutput<R>) -> Self {
        Self { tape, value }
    }

    /// An empty output — tape-only fixtures + substrate-level tests
    /// that never reach `to_value()` use this to satisfy the
    /// `FusedOutput<R>`-typed slot on [`Parsed`](crate::runtime::Parsed)
    /// without staging a real parse.
    #[inline]
    pub fn empty() -> Self {
        Self {
            tape: Tape::default(),
            value: ValueFramesOutput::empty(),
        }
    }

    /// Borrow the finalised tape. Cursor-backed consumers (`view()`,
    /// `get()`) read from here.
    #[inline]
    pub fn tape(&self) -> &Tape {
        &self.tape
    }

    /// Consume the output and return the owned tape. Handy for
    /// tape-only pipelines that discard the value substrate.
    #[inline]
    pub fn into_tape(self) -> Tape {
        self.tape
    }

    /// Consume the output and return the owned value-frames slab.
    /// Used by projection consumers that want ownership of the typed
    /// enum without retaining the tape.
    #[inline]
    pub fn into_value(self) -> ValueFramesOutput<R> {
        self.value
    }

    /// Split the output into its `(tape, value)` components.
    #[inline]
    pub fn into_parts(self) -> (Tape, ValueFramesOutput<R>) {
        (self.tape, self.value)
    }

    // ── Value-surface accessors ───────────────────────────────────
    //
    // The grammar-emitted projection code reads the value substrate
    // solely through these accessors.

    /// Total value-frame count. `0` iff the output is empty (e.g.
    /// substrate-only [`Parsed::new`](crate::runtime::Parsed::new)).
    #[inline]
    pub fn frame_count(&self) -> usize {
        self.value.frame_count()
    }

    /// `true` iff the value substrate is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.value.is_empty()
    }

    /// Borrow the value-frame arena directly.
    #[inline]
    pub fn frames(&self) -> &[ValueFrame] {
        self.value.frames()
    }

    /// Borrow the value substrate as a whole — occasionally useful
    /// for tests asserting shape contracts or for projection code
    /// that wants the raw `ValueFramesOutput` handle.
    #[inline]
    pub fn value(&self) -> &ValueFramesOutput<R> {
        &self.value
    }

    /// The root frame's offset within the arena. Projection consumers
    /// begin descent here.
    #[inline]
    pub fn root_offset(&self) -> u32 {
        self.value.root_offset()
    }

    /// Borrow a value frame by offset.
    #[inline]
    pub fn frame(&self, offset: u32) -> Option<&ValueFrame> {
        self.value.frame(offset)
    }

    /// Borrow a value frame by offset (alias mirroring the agreed
    /// W0'.a contract name).
    #[inline]
    pub fn value_frame_at(&self, offset: u32) -> Option<&ValueFrame> {
        self.value.frame(offset)
    }

    /// Borrow the root value frame directly. Returns `None` for empty
    /// substrates.
    #[inline]
    pub fn root_frame(&self) -> Option<&ValueFrame> {
        self.value.root_frame()
    }

    /// Look up the scalar payload for a leaf frame.
    #[inline]
    pub fn payload_for(&self, frame: &ValueFrame) -> Option<PayloadValue> {
        self.value.payload_for(frame)
    }

    /// Look up the scalar payload for a leaf frame (alias mirroring
    /// the agreed W0'.a contract name).
    #[inline]
    pub fn value_payload_for(&self, frame: &ValueFrame) -> Option<PayloadValue> {
        self.value.payload_for(frame)
    }

    /// Read a narrow-column payload by rank.
    #[inline]
    pub fn payload_narrow(&self, rank: u32) -> Option<u32> {
        self.value.payload_narrow(rank)
    }

    /// Read a wide-column payload by rank.
    #[inline]
    pub fn payload_wide(&self, rank: u32) -> Option<u64> {
        self.value.payload_wide(rank)
    }

    /// Iterator over the direct children of the compound frame at
    /// `offset`. For leaf frames the iterator is empty.
    #[inline]
    pub fn children(&self, offset: u32) -> ValueChildren<'_, R> {
        self.value.children(offset)
    }

    /// Iterator over the direct children of the compound frame at
    /// `offset` (alias mirroring the agreed W0'.a contract name).
    #[inline]
    pub fn value_children(&self, offset: u32) -> ValueChildren<'_, R> {
        self.value.children(offset)
    }

    /// Compatibility shim for emitted projection code that reads
    /// frames via `output.frame(offset)`. Delegates to
    /// [`Self::frame`].
    #[inline]
    pub fn value_root_offset(&self) -> u32 {
        self.value.root_offset()
    }

    /// Re-export the phantom-bounded `ValueFramesOutput<R>` for
    /// callers that want the pre-W0'.a surface name. Not used inside
    /// `tape`; exposed for the downstream runtime re-export path.
    #[inline]
    pub fn as_value_output(&self) -> &ValueFramesOutput<R> {
        &self.value
    }
}

impl<R> Default for FusedOutput<R> {
    #[inline]
    fn default() -> Self {
        Self::empty()
    }
}
