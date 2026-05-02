//! AZ-II.cutover.A — `BbnfStructBuilder` — the concrete `StructBuilder`
//! impl that the regenerated BBNF parse function targets.
//!
//! Maintains an in-flight typed stack of partially-built compounds.
//! Each frame collects per-shape state (kind, branch tag, children)
//! and is finalised by [`BbnfStructBuilder::end_compound`] into a
//! [`BbnfValue::Compound`] that lands on the parent frame's pending
//! slot.
//!
//! Scalar pushes (`push_leaf_with_*`) deposit a typed [`BbnfValue`]
//! on the topmost frame's children vec, or onto the document root if
//! the stack is empty.
//!
//! `begin_compound` consults the [`StructLayout::rule_id`] to
//! discover the [`BbnfCompoundKind`] for the frame; `end_compound`
//! finalises the frame into a [`BbnfValue::Compound`] arena handle.

use bbnf_ir::registry::StructLayout;

use crate::runtime::bbnf::arena::{BbnfArena, BbnfCompound, BbnfCompoundKind};
use crate::runtime::bbnf::document::BbnfDocument;
use crate::runtime::bbnf::value::BbnfValue;
use crate::runtime::builder::StructBuilder;
use crate::runtime::handle::CompoundHandle;

/// One open compound frame on the builder's stack.
#[derive(Debug, Clone)]
struct OpenFrame<'p> {
    kind: BbnfCompoundKind,
    branch_tag: Option<u32>,
    children: Vec<BbnfValue<'p>>,
}

/// Concrete `StructBuilder` for the BBNF grammar.
///
/// Owns a [`BbnfArena`] and a stack of open frames. The generated
/// parse function constructs a builder, threads it through every
/// per-shape parse fn, and calls [`Self::finalise`] at EOF to
/// recover the [`BbnfDocument`].
#[derive(Debug)]
pub struct BbnfStructBuilder<'p> {
    arena: BbnfArena<'p>,
    stack: Vec<OpenFrame<'p>>,
    /// The root value, set when the outermost compound (or scalar
    /// against an empty stack) finalises.
    root: Option<BbnfValue<'p>>,
    /// Monotonic compound handle counter.
    next_handle: u64,
}

/// Rollback snapshot for [`BbnfStructBuilder`].
#[derive(Debug, Clone)]
pub struct BbnfStructCheckpoint<'p> {
    compounds: usize,
    stack: Vec<OpenFrame<'p>>,
    root: Option<BbnfValue<'p>>,
    next_handle: u64,
}

impl<'p> Default for BbnfStructBuilder<'p> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'p> BbnfStructBuilder<'p> {
    /// Construct a fresh builder with an empty arena and no open
    /// frames.
    #[inline]
    pub fn new() -> Self {
        Self {
            arena: BbnfArena::new(),
            stack: Vec::with_capacity(16),
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
            arena: BbnfArena::with_capacity(compounds),
            stack: Vec::with_capacity(16),
            root: None,
            next_handle: 0,
        }
    }

    /// Finalise the builder into a [`BbnfDocument`]. Panics if no
    /// root value emitted, or if any open frame remains.
    #[inline]
    pub fn finalise(mut self, input: &'p str) -> BbnfDocument<'p> {
        debug_assert!(
            self.stack.is_empty(),
            "BbnfStructBuilder::finalise called with {} open frame(s)",
            self.stack.len()
        );
        let root = self
            .root
            .take()
            .expect("BbnfStructBuilder::finalise called before any value emission");
        BbnfDocument::new(self.arena, root, input)
    }

    /// Land a finalised [`BbnfValue`] on the topmost open frame, or
    /// store it as the root if the stack is empty.
    #[inline]
    fn deposit(&mut self, value: BbnfValue<'p>) {
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

impl<'p> StructBuilder for BbnfStructBuilder<'p> {
    type Checkpoint = BbnfStructCheckpoint<'p>;

    #[inline]
    fn checkpoint(&self) -> Self::Checkpoint {
        BbnfStructCheckpoint {
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
        let kind = BbnfCompoundKind::from_rule_id(layout.rule_id);
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
            .expect("BbnfStructBuilder::end_compound on empty stack");
        let id = self.arena.push_compound(BbnfCompound {
            kind: frame.kind,
            branch_tag: frame.branch_tag,
            children: frame.children,
        });
        self.deposit(BbnfValue::Compound(id));
    }

    #[inline]
    fn push_leaf_with_f64(&mut self, value: f64) {
        self.deposit(BbnfValue::Float(value));
    }

    #[inline]
    fn push_leaf_with_i64(&mut self, value: i64) {
        self.deposit(BbnfValue::Int(value));
    }

    #[inline]
    fn push_leaf_with_u64(&mut self, value: u64) {
        // BBNF does not project u64 through any rule today; widen to
        // i64 so consumers querying through `BbnfValue::Int` see a
        // consistent integral surface.
        self.deposit(BbnfValue::Int(value as i64));
    }

    #[inline]
    fn push_leaf_with_bool(&mut self, value: bool) {
        self.deposit(BbnfValue::Bool(value));
    }

    #[inline]
    fn push_leaf_with_str(&mut self, value: &str) {
        // SAFETY: the slice's lifetime is bound to the parse call
        // site by the generated function's signature; the trait
        // surface elides this so concrete builders can specialise.
        let lifetime_extended: &'p str = unsafe { std::mem::transmute(value) };
        self.deposit(BbnfValue::Span(lifetime_extended));
    }

    #[inline]
    fn push_leaf_with_unit(&mut self) {
        self.deposit(BbnfValue::Unit);
    }

    #[inline]
    fn push_branch_tag(&mut self, branch_index: u32) {
        if let Some(frame) = self.stack.last_mut() {
            frame.branch_tag = Some(branch_index);
        }
    }
}
