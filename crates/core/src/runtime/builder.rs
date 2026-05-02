//! AZ-I.W2.A — `StructBuilder` trait.
//!
//! The `StructBuilder` trait is the one consumer surface the per-shape
//! emitters target on grammars that have severed the tape substrate
//! (JSON in W2.A, Sheets in W2.B, CSS L4 in W3). The grammar's parse
//! function calls trait methods unconditionally; per-grammar selection
//! between tape and struct happens at codegen time inside
//! `emit_grammar_impl`'s `parse_body` arm — `feedback_no-orthogonal-codepaths`
//! is in force: ONE codegen path per grammar; the trait surface is
//! pluggable so concrete builders are data, not branches.
//!
//! # Method surface
//!
//! `begin_compound` / `end_compound` open and close a compound shape
//! (object / array / pair / paired-Alt branch). The `StructLayout`
//! handed to `begin_compound` carries the [`bbnf_ir::registry::LayoutKind`]
//! that drives field placement inside the concrete builder; the
//! `CompoundHandle` returned is opaque to the emitter and is paired
//! with `end_compound` at the matching `]` / `}` / branch close.
//!
//! `push_leaf_with_*` pushes a typed scalar field. The variant
//! corresponds to the grammar's `->` projection: `-> f64` routes to
//! `push_leaf_with_f64`, `-> i64` to `push_leaf_with_i64`, `-> bool`
//! to `push_leaf_with_bool`, `-> Span` (or owned-string projection)
//! to `push_leaf_with_str`, `-> 0u8` (null) to `push_leaf_with_unit`.
//!
//! `push_branch_tag` records the `Alt` sub-variant index for the
//! current open compound — Alt-typed compounds carry the tag in
//! addition to the branch's payload pushes.
//!
//! # Wire contract
//!
//! Every typed `->` annotation in the grammar reaches one
//! `push_leaf_with_*` call (`feedback_typed-materialization-invariant`).
//! Every compound record pushes one `begin_compound` / `end_compound`
//! pair. Every Alt sub-variant boundary pushes one `push_branch_tag`.
//!
//! Speculative parse sites call `checkpoint` before trying a branch and
//! `rollback` on failure. Checkpoints are concrete-builder values so
//! each grammar can snapshot only its mutable stack/root state plus arena
//! slab lengths; rollback must not clone full arenas on the hot path.

use bbnf_ir::registry::StructLayout;

use crate::runtime::handle::CompoundHandle;

/// Pluggable struct-emission surface targeted by generated parse
/// functions on grammars whose tape has been severed.
///
/// Implementors maintain an in-flight typed stack of partially-built
/// compounds; `begin_compound` pushes a frame keyed on the layout's
/// `LayoutKind`; intervening `push_leaf_with_*` / `push_branch_tag`
/// calls land fields on the open frame; `end_compound` pops and
/// stores the completed compound into its parent (or, for the root
/// compound, into the document's root slot).
///
/// # Layout coupling
///
/// The `StructLayout` reference passed to `begin_compound` originates
/// from `GrammarIR::struct_registry`, populated by `project_types`
/// closure in W1. Concrete builders dispatch on
/// [`StructLayout::is_struct`] / [`StructLayout::is_tagged_enum`] /
/// [`StructLayout::is_newtype`] to construct the appropriate concrete
/// shape. Per `feedback_pluggable-components`, the discriminator is
/// data; concrete builders query it.
pub trait StructBuilder {
    /// Concrete snapshot used to restore builder state when a speculative
    /// parse attempt mutates the builder but then fails.
    type Checkpoint;

    /// Capture a rollback point before a speculative branch/repeat/guard.
    fn checkpoint(&self) -> Self::Checkpoint;

    /// Restore a previously captured checkpoint.
    fn rollback(&mut self, checkpoint: Self::Checkpoint);

    /// Mark a checkpoint as successfully consumed. Most builders have
    /// nothing to do here, but keeping the method explicit lets emitted
    /// code state branch intent symmetrically.
    #[inline]
    fn commit(&mut self, _checkpoint: Self::Checkpoint) {}

    /// Open a compound. Returns a [`CompoundHandle`] that the matching
    /// [`Self::end_compound`] receives. `layout` describes the typed
    /// shape the emitter believes it is building; concrete
    /// implementations may panic on layout mismatch in debug builds.
    fn begin_compound(&mut self, layout: &StructLayout) -> CompoundHandle;

    /// Close a compound previously opened via [`Self::begin_compound`].
    /// `handle` must be the most recent unmatched handle the builder
    /// produced; the trait does not enforce this — the emitter's
    /// generation order does.
    fn end_compound(&mut self, handle: CompoundHandle);

    /// Push an `f64` leaf onto the most recently opened compound.
    /// Corresponds to a grammar `-> f64` typed projection.
    fn push_leaf_with_f64(&mut self, value: f64);

    /// Push an `i64` leaf. Corresponds to `-> i64` projection.
    fn push_leaf_with_i64(&mut self, value: i64);

    /// Push a `u64` leaf. Corresponds to `-> u64` / `-> u32` (after
    /// widening) projection.
    fn push_leaf_with_u64(&mut self, value: u64);

    /// Push a `bool` leaf. Corresponds to `-> true` / `-> false` /
    /// `-> bool` projection.
    fn push_leaf_with_bool(&mut self, value: bool);

    /// Push a string leaf — borrowed from the input or arena-decoded;
    /// the slice carries the same `'p` lifetime as the document arena.
    /// Corresponds to `-> Span` (raw) and `-> decode_*_to_arena(input)`
    /// (arena-decoded) projections.
    fn push_leaf_with_str(&mut self, value: &str);

    /// Push a unit-typed leaf. Corresponds to `-> 0u8` (null marker)
    /// and `-> ()` projections — the field is admitted but carries no
    /// payload byte.
    fn push_leaf_with_unit(&mut self);

    /// Stamp the active branch tag for an `Alt`-typed open compound.
    /// `branch_index` corresponds to the matching
    /// [`bbnf_ir::registry::FieldSource::BranchTag`] entry on the
    /// layout passed to the enclosing [`Self::begin_compound`].
    fn push_branch_tag(&mut self, branch_index: u32);

    /// AZ-IV.W1.9 — record the topmost open compound's start byte
    /// offset. Default no-op: only the BBNF builder consumes this to
    /// preserve the compound's actual byte extent (alt-branch
    /// literals consumed without a Span push break the leftmost-Span-
    /// leaf union otherwise). JSON / Sheets / CSS L4 builders ignore.
    #[inline]
    #[allow(unused_variables)]
    fn record_compound_bounds_start(&mut self, offset: u32) {}

    /// AZ-IV.W1.9 — record the topmost open compound's end byte
    /// offset. Default no-op; see [`Self::record_compound_bounds_start`].
    #[inline]
    #[allow(unused_variables)]
    fn record_compound_bounds_end(&mut self, offset: u32) {}
}
