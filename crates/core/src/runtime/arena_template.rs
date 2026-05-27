//! AZ-IV.W5.3 — generic compound-slab arena template.
//!
//! Per Q-final B2 = (a) (structural skeleton dedup; typed `*Value` enums
//! survive untouched), this module factors the slab-of-Vec compound
//! arena into a generic [`CompoundSlabArena<C>`] parameterised by a
//! per-grammar [`CompoundEntry`] type. The seven grammars whose runtime
//! compound shape collapses onto a single uniform `Compound { kind,
//! branch_tag, children }` shape (BBNF, BNF, CSV, CssPretty, EBNF, Math,
//! and the structural skeleton of Sheets) instantiate the template
//! rather than duplicating the slab discipline per grammar.
//!
//! # Why a template here
//!
//! Pre-W5 each per-grammar arena reproduced the same six items:
//!
//! 1. An opaque handle struct (`FooCompoundId`) wrapping a `u32` whose
//!    `0` value reserves [`Self::EMPTY`].
//! 2. A `slab_index()` helper translating handle → slab index.
//! 3. A `FooArena<'p>` owning a `Vec<FooCompound<'p>>` plus a stable
//!    `empty: FooCompound<'p>` for the empty resolution branch.
//! 4. `new` / `with_capacity` / `push_compound` / `compound` /
//!    `compound_count` / `truncate` whose bodies are byte-identical
//!    save for type substitutions.
//! 5. The handle's `EMPTY` constant and `is_empty` accessor.
//! 6. The empty-handle resolution (`None → &empty`) branch.
//!
//! Per `feedback_no-god-modules`, the dedup target is the structural
//! skeleton: items (1)–(6). The grammar-specific [`CompoundEntry`]
//! payload (the `kind` enum + the `branch_tag` slot + the
//! `children` Vec's element type) survives unchanged on the per-grammar
//! arena's `arena.rs` file.
//!
//! # Scope
//!
//! This template is the shared implementation for single-compound-slab
//! runtimes. Multi-slab and typed-value runtimes now live in generated
//! per-grammar projection output, so this module no longer records the
//! grammar-specific roster. The invariant is structural: when a
//! grammar's arena needs more than one slab family or a non-uniform
//! entry payload, its runtime projection emits that shape directly
//! from metadata instead of extending this template's responsibilities.
//!
//! # Lifetimes
//!
//! [`CompoundSlabArena<C>`] is generic over the entry payload `C`.
//! Per-grammar arenas instantiate `C = FooCompound<'p>` so the slab's
//! interior `'p` borrow flows from the parse-input lifetime. The
//! template is `'p`-agnostic; the per-grammar arena.rs ties the lifetime.

/// Marker trait for entries stored in [`CompoundSlabArena`].
///
/// A blanket impl below covers any `Default + Clone + core::fmt::Debug`
/// type — the per-grammar `FooCompound<'p>` structs all derive these
/// already, so no per-grammar trait impl is needed.
pub trait CompoundEntry: Default + Clone + core::fmt::Debug {}

impl<T: Default + Clone + core::fmt::Debug> CompoundEntry for T {}

/// Generic slab-of-Vec compound arena.
///
/// Owns one entry per non-empty handle; empty compounds resolve to a
/// stable default via the `EMPTY` sentinel without consulting the slab.
/// Capacity hints come from
/// [`bbnf_ir::registry::StructRegistry`]'s per-grammar capacity
/// estimates threaded through the emitter at codegen time.
///
/// # Handle discipline
///
/// Per-grammar `FooCompoundId` newtypes wrap a `u32`; the `0` value is
/// reserved for the empty-handle constant; non-empty handles take values
/// `1..=u32::MAX`. The template's `push` / `resolve` / `truncate`
/// methods accept raw `u32` indices so per-grammar arenas can keep their
/// typed handle constructors / accessors and forward through the
/// template.
#[derive(Debug, Default)]
pub struct CompoundSlabArena<C: CompoundEntry> {
    /// Per-handle compound entries. Index = `handle.0 - 1`.
    compounds: Vec<C>,
    /// Stable default for the empty-compound resolution branch.
    empty: C,
}

impl<C: CompoundEntry> CompoundSlabArena<C> {
    /// Construct an empty arena.
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    /// Construct an arena with pre-sized slab capacity.
    #[inline]
    pub fn with_capacity(compounds: usize) -> Self {
        Self {
            compounds: Vec::with_capacity(compounds),
            empty: C::default(),
        }
    }

    /// Push a populated compound into the slab and return the
    /// resolving raw index `1..=u32::MAX`. The caller wraps the index
    /// into its typed handle.
    #[inline]
    pub fn push_compound(&mut self, compound: C) -> u32 {
        self.compounds.push(compound);
        self.compounds.len() as u32
    }

    /// Resolve a raw handle index to its entry. Returns the stable
    /// default when `index_plus_one == 0`.
    #[inline]
    pub fn compound(&self, index_plus_one: u32) -> &C {
        if index_plus_one == 0 {
            &self.empty
        } else {
            &self.compounds[(index_plus_one - 1) as usize]
        }
    }

    /// Number of registered compounds.
    #[inline]
    pub fn compound_count(&self) -> usize {
        self.compounds.len()
    }

    /// Roll back the arena to a prior compound-count snapshot.
    #[inline]
    pub fn truncate(&mut self, compounds: usize) {
        self.compounds.truncate(compounds);
    }
}
