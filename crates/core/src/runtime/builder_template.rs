//! AZ-IV.W5.3 — generic struct-builder template for the simple-grammar
//! cohort.
//!
//! Per Q-final B2 = (a) (skeleton dedup; typed `*Value` enums survive
//! untouched), this module factors the [`StructBuilder`] impl shared by
//! the simple grammars whose runtime compound shape collapses onto
//! `Compound { kind, branch_tag, children: Vec<FooValue> }` with every
//! scalar leaf depositing either `FooValue::Span` or `FooValue::Unit`.
//!
//! The genuine cohort: BNF, EBNF, CSV, CSS Pretty, Math. Five builders
//! whose pre-W5 bodies were near-byte-identical (`diff -u` shows only
//! type-name substitutions and module-doc comments differ) — five
//! ≈165-LOC files that landed identical [`StructBuilder`] semantics.
//!
//! # Scope
//!
//! This template is the shared implementation for runtimes whose
//! builder shape is exactly one compound stack plus a uniform
//! span/unit/compound value alphabet. Typed-value runtimes now arrive
//! through generated per-grammar projection output, so this module no
//! longer documents a grammar-by-grammar exception roster. The
//! invariant is structural: if a grammar needs additional open-frame
//! state, specialised leaf deposition, or compound-bound recording, its
//! runtime projection emits that shape directly from metadata instead
//! of expanding this template's contract.
//!
//! # Wire contract via [`SimpleValue`] trait
//!
//! The five simple-cohort grammars project every leaf to one of three
//! [`FooValue`] arms: `Span(&'p str)`, `Unit`, `Compound(FooCompoundId)`.
//! The [`SimpleValue`] trait surfaces the three constructors so the
//! template's body deposits the matching arm without naming the
//! per-grammar enum.

use core::marker::PhantomData;

use bbnf_ir::registry::StructLayout;

use crate::runtime::arena_template::{CompoundEntry, CompoundSlabArena};
use crate::runtime::builder::StructBuilder;
use crate::runtime::handle::CompoundHandle;

/// Constructor surface for the simple-cohort `FooValue` enum.
///
/// The five simple grammars (BNF / EBNF / CSV / CSS Pretty / Math) all
/// project every leaf to one of three arms: a borrowed span, a unit
/// marker, or an arena compound handle. The trait surfaces the three
/// arms so [`SimpleStructBuilder`]'s body deposits the matching value
/// without naming the per-grammar typed enum directly.
///
/// `CompoundHandleU32` is the per-grammar handle's raw `u32` index — the
/// per-grammar instantiation passes the typed handle's wrapped index in
/// (the typed `FooCompoundId` newtype is constructed at the per-grammar
/// arena instantiation site).
pub trait SimpleValue<'p>: Copy + core::fmt::Debug {
    /// `FooValue::Span(s)` arm constructor.
    fn from_span(s: &'p str) -> Self;
    /// `FooValue::Unit` arm constructor.
    fn unit() -> Self;
    /// `FooValue::Compound(id)` arm constructor — `id_plus_one` is the
    /// raw `u32` index returned by [`CompoundSlabArena::push_compound`].
    fn from_compound_index(id_plus_one: u32) -> Self;
}

/// Per-grammar entry payload: the typed compound entry.
///
/// The trait surfaces the three setters [`SimpleStructBuilder`] needs to
/// finalise an open frame onto a slab entry: kind from the layout,
/// branch tag from `push_branch_tag`, children from interior leaf /
/// compound pushes. The per-grammar `FooCompound<'p>` struct implements
/// the trait directly — the body is mechanical (one setter per field).
pub trait SimpleCompound<'p, V: SimpleValue<'p>>: CompoundEntry {
    /// Construct a compound entry from a [`StructLayout`] reference,
    /// branch tag, and child slice.
    fn new_entry(layout: &StructLayout, branch_tag: Option<u32>, children: Vec<V>) -> Self;
}

/// One open compound frame on [`SimpleStructBuilder`]'s stack.
///
/// Mirrors the pre-W5 per-grammar `OpenFrame { kind, branch_tag,
/// children }` shape. `layout_ref` is taken at `begin_compound` time so
/// `end_compound` can finalise the entry without re-resolving the
/// layout.
#[derive(Debug, Clone)]
struct Frame<'p, V: SimpleValue<'p>> {
    /// Owned-cloned snapshot of the layout that opened this frame.
    /// Cloned so the builder doesn't tie itself to the IR's
    /// `'ir`-lifetime; the registry's layouts are `Clone`-able.
    layout: StructLayout,
    /// Alt sub-variant index, when the rule is Alt-typed; `None`
    /// otherwise. Recorded via [`StructBuilder::push_branch_tag`].
    branch_tag: Option<u32>,
    /// Child values, in source order.
    children: Vec<V>,
    /// Anchors the `'p` lifetime through the `SimpleValue<'p>` bound
    /// when `V` itself does not name `'p` in a field. Zero-sized.
    _phantom: PhantomData<&'p ()>,
}

/// Generic struct-builder for the simple-cohort grammars.
///
/// Owns a [`CompoundSlabArena<C>`] and a stack of in-flight open frames.
/// The generated parse function constructs a builder, threads it
/// through every per-shape parse fn, and calls `finalise` at EOF to
/// recover the per-grammar document type.
///
/// Per-grammar instantiations are at most ≈30 LOC (one `pub type`
/// alias, a thin `finalise` returning the grammar's `Document`, and a
/// trivial `SimpleCompound::new_entry` impl on the grammar's
/// `FooCompound`). The body of the [`StructBuilder`] impl lives here
/// once.
#[derive(Debug)]
pub struct SimpleStructBuilder<'p, V: SimpleValue<'p>, C: SimpleCompound<'p, V>> {
    /// Owning arena.
    pub arena: CompoundSlabArena<C>,
    /// In-flight open compound stack.
    stack: Vec<Frame<'p, V>>,
    /// The root value, set when the outermost emission reaches an
    /// empty stack.
    pub root: Option<V>,
    /// Monotonic compound-handle counter.
    next_handle: u64,
}

/// Rollback snapshot for [`SimpleStructBuilder`].
#[derive(Debug, Clone)]
pub struct SimpleCheckpoint<'p, V: SimpleValue<'p>> {
    compounds: usize,
    stack: Vec<Frame<'p, V>>,
    root: Option<V>,
    next_handle: u64,
}

impl<'p, V: SimpleValue<'p>, C: SimpleCompound<'p, V>> Default for SimpleStructBuilder<'p, V, C> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'p, V: SimpleValue<'p>, C: SimpleCompound<'p, V>> SimpleStructBuilder<'p, V, C> {
    /// Construct a fresh builder.
    #[inline]
    pub fn new() -> Self {
        Self {
            arena: CompoundSlabArena::new(),
            stack: Vec::with_capacity(16),
            root: None,
            next_handle: 0,
        }
    }

    /// Construct a builder with pre-sized arena capacity.
    #[inline]
    pub fn with_capacity(compounds: usize) -> Self {
        Self {
            arena: CompoundSlabArena::with_capacity(compounds),
            stack: Vec::with_capacity(16),
            root: None,
            next_handle: 0,
        }
    }

    /// Land a finalised value on the topmost open frame, or store it as
    /// the root if the stack is empty.
    #[inline]
    fn deposit(&mut self, value: V) {
        match self.stack.last_mut() {
            None => self.root = Some(value),
            Some(frame) => frame.children.push(value),
        }
    }

    /// Drain the builder's state for the per-grammar `finalise` to
    /// inspect. Used by per-grammar instantiations to assemble the
    /// grammar's `Document` shape.
    #[inline]
    pub fn into_finalised(self) -> (CompoundSlabArena<C>, V) {
        debug_assert!(
            self.stack.is_empty(),
            "SimpleStructBuilder::into_finalised called with {} open frame(s)",
            self.stack.len()
        );
        let root = self
            .root
            .expect("SimpleStructBuilder::into_finalised called before any value emission");
        (self.arena, root)
    }
}

impl<'p, V: SimpleValue<'p>, C: SimpleCompound<'p, V>> StructBuilder
    for SimpleStructBuilder<'p, V, C>
{
    type Checkpoint = SimpleCheckpoint<'p, V>;

    #[inline]
    fn checkpoint(&self) -> Self::Checkpoint {
        SimpleCheckpoint {
            compounds: self.arena.compound_count(),
            stack: self.stack.clone(),
            root: self.root,
            next_handle: self.next_handle,
        }
    }

    #[inline]
    fn rollback(&mut self, checkpoint: Self::Checkpoint) {
        self.arena.truncate(checkpoint.compounds);
        self.stack = checkpoint.stack;
        self.root = checkpoint.root;
        self.next_handle = checkpoint.next_handle;
    }

    fn begin_compound(&mut self, layout: &StructLayout) -> CompoundHandle {
        self.stack.push(Frame {
            layout: layout.clone(),
            branch_tag: None,
            children: Vec::new(),
            _phantom: PhantomData,
        });
        self.next_handle = self.next_handle.wrapping_add(1);
        CompoundHandle::new(self.next_handle, 0)
    }

    fn end_compound(&mut self, _handle: CompoundHandle) {
        let frame = self
            .stack
            .pop()
            .expect("SimpleStructBuilder::end_compound on empty stack");
        let entry = C::new_entry(&frame.layout, frame.branch_tag, frame.children);
        let id_plus_one = self.arena.push_compound(entry);
        self.deposit(V::from_compound_index(id_plus_one));
    }

    #[inline]
    fn push_leaf_with_f64(&mut self, _v: f64) {
        self.deposit(V::unit());
    }

    #[inline]
    fn push_leaf_with_i64(&mut self, _v: i64) {
        self.deposit(V::unit());
    }

    #[inline]
    fn push_leaf_with_u64(&mut self, _v: u64) {
        self.deposit(V::unit());
    }

    #[inline]
    fn push_leaf_with_bool(&mut self, _v: bool) {
        self.deposit(V::unit());
    }

    #[inline]
    fn push_leaf_with_str(&mut self, value: &str) {
        // SAFETY: the slice's lifetime is bound to the parse call site
        // by the generated function's signature; the trait surface
        // elides this so concrete builders can specialise. The
        // lifetime extension to `'p` is sound because the parse-input
        // outlives the builder; the builder is consumed at parse end
        // (`finalise`) and the resulting document carries the same
        // lifetime.
        let lifetime_extended: &'p str = unsafe { core::mem::transmute(value) };
        self.deposit(V::from_span(lifetime_extended));
    }

    #[inline]
    fn push_leaf_with_unit(&mut self) {
        self.deposit(V::unit());
    }

    #[inline]
    fn push_branch_tag(&mut self, branch_index: u32) {
        if let Some(frame) = self.stack.last_mut() {
            frame.branch_tag = Some(branch_index);
        }
    }
}
