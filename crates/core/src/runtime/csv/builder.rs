//! AZ-II.cutover.E (Phase 2) — `CsvStructBuilder` — the concrete
//! [`StructBuilder`] impl that the post-regen CSV parse function will
//! target.
//!
//! Maintains an in-flight typed stack of partially-built compounds.
//! Each frame collects per-shape state (kind, branch tag, children)
//! and is finalised by [`CsvStructBuilder::end_compound`] into a
//! [`CsvValue::Compound`] that lands on the parent frame's pending
//! slot.
//!
//! `begin_compound` consults the [`StructLayout::rule_id`] to
//! discover the [`CsvCompoundKind`] for the frame; `end_compound`
//! finalises the frame into a [`CsvValue::Compound`] arena handle.
//!
//! Mirrors [`crate::runtime::bbnf::BbnfStructBuilder`] discipline.

use bbnf_ir::registry::StructLayout;

use crate::runtime::builder::StructBuilder;
use crate::runtime::csv::arena::{CsvArena, CsvCompound, CsvCompoundKind};
use crate::runtime::csv::document::CsvDocument;
use crate::runtime::csv::value::CsvValue;
use crate::runtime::handle::CompoundHandle;

/// One open compound frame on the builder's stack.
#[derive(Debug, Clone)]
struct OpenFrame<'p> {
    kind: CsvCompoundKind,
    branch_tag: Option<u32>,
    children: Vec<CsvValue<'p>>,
}

/// Concrete `StructBuilder` for the CSV grammar.
///
/// Owns a [`CsvArena`] and a stack of open frames. The generated
/// parse function constructs a builder, threads it through every
/// per-shape parse fn, and calls [`Self::finalise`] at EOF to
/// recover the [`CsvDocument`].
#[derive(Debug)]
pub struct CsvStructBuilder<'p> {
    arena: CsvArena<'p>,
    stack: Vec<OpenFrame<'p>>,
    /// The root value, set when the outermost compound (or scalar
    /// against an empty stack) finalises.
    root: Option<CsvValue<'p>>,
    /// Monotonic compound handle counter.
    next_handle: u64,
}

/// Rollback snapshot for [`CsvStructBuilder`].
#[derive(Debug, Clone)]
pub struct CsvStructCheckpoint<'p> {
    compounds: usize,
    stack: Vec<OpenFrame<'p>>,
    root: Option<CsvValue<'p>>,
    next_handle: u64,
}

impl<'p> Default for CsvStructBuilder<'p> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'p> CsvStructBuilder<'p> {
    /// Construct a fresh builder with an empty arena and no open
    /// frames.
    #[inline]
    pub fn new() -> Self {
        Self {
            arena: CsvArena::new(),
            stack: Vec::with_capacity(8),
            root: None,
            next_handle: 0,
        }
    }

    /// Construct a builder with arena capacity hints from
    /// [`bbnf_ir::registry::StructRegistry`] estimates folded into the
    /// generated `parse()` body at codegen time.
    #[inline]
    pub fn with_capacity(compounds: usize) -> Self {
        Self {
            arena: CsvArena::with_capacity(compounds),
            stack: Vec::with_capacity(8),
            root: None,
            next_handle: 0,
        }
    }

    /// Finalise the builder into a [`CsvDocument`]. Panics if no
    /// root value emitted, or if any open frame remains.
    #[inline]
    pub fn finalise(mut self, input: &'p str) -> CsvDocument<'p> {
        debug_assert!(
            self.stack.is_empty(),
            "CsvStructBuilder::finalise called with {} open frame(s)",
            self.stack.len()
        );
        let root = self
            .root
            .take()
            .expect("CsvStructBuilder::finalise called before any value emission");
        CsvDocument::new(self.arena, root, input)
    }

    /// Land a finalised [`CsvValue`] on the topmost open frame, or
    /// store it as the root if the stack is empty.
    #[inline]
    fn deposit(&mut self, value: CsvValue<'p>) {
        match self.stack.last_mut() {
            None => {
                self.root = Some(value);
            }
            Some(frame) => {
                frame.children.push(value);
            }
        }
    }
}

impl<'p> StructBuilder for CsvStructBuilder<'p> {
    type Checkpoint = CsvStructCheckpoint<'p>;

    #[inline]
    fn checkpoint(&self) -> Self::Checkpoint {
        CsvStructCheckpoint {
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
        let kind = CsvCompoundKind::from_layout(layout);
        self.stack.push(OpenFrame {
            kind,
            branch_tag: None,
            children: Vec::new(),
        });
        self.next_handle = self.next_handle.wrapping_add(1);
        CompoundHandle::new(self.next_handle, 0)
    }

    fn end_compound(&mut self, _handle: CompoundHandle) {
        let frame = self
            .stack
            .pop()
            .expect("CsvStructBuilder::end_compound on empty stack");
        let id = self.arena.push_compound(CsvCompound {
            kind: frame.kind,
            branch_tag: frame.branch_tag,
            children: frame.children,
        });
        self.deposit(CsvValue::Compound(id));
    }

    #[inline]
    fn push_leaf_with_f64(&mut self, _value: f64) {
        // CSV does not project f64 through any rule today; widen
        // unconditionally to a unit deposit so consumers see a
        // consistent placeholder rather than a panic.
        self.deposit(CsvValue::Unit);
    }

    #[inline]
    fn push_leaf_with_i64(&mut self, _value: i64) {
        self.deposit(CsvValue::Unit);
    }

    #[inline]
    fn push_leaf_with_u64(&mut self, _value: u64) {
        self.deposit(CsvValue::Unit);
    }

    #[inline]
    fn push_leaf_with_bool(&mut self, _value: bool) {
        self.deposit(CsvValue::Unit);
    }

    #[inline]
    fn push_leaf_with_str(&mut self, value: &str) {
        // SAFETY: the slice's lifetime is bound to the parse call
        // site by the generated function's signature; the trait
        // surface elides this so concrete builders can specialise.
        let lifetime_extended: &'p str = unsafe { std::mem::transmute(value) };
        self.deposit(CsvValue::Span(lifetime_extended));
    }

    #[inline]
    fn push_leaf_with_unit(&mut self) {
        self.deposit(CsvValue::Unit);
    }

    #[inline]
    fn push_branch_tag(&mut self, branch_index: u32) {
        if let Some(frame) = self.stack.last_mut() {
            frame.branch_tag = Some(branch_index);
        }
    }
}
