//! AZ-II.cutover.E (Phase 2) — `MathStructBuilder` — concrete
//! [`StructBuilder`] impl for the math grammar.
//!
//! Mirror of `CsvStructBuilder` / `BbnfStructBuilder` discipline.

use bbnf_ir::registry::StructLayout;

use crate::runtime::builder::StructBuilder;
use crate::runtime::handle::CompoundHandle;
use crate::runtime::math::arena::{MathArena, MathCompound, MathCompoundKind};
use crate::runtime::math::document::MathDocument;
use crate::runtime::math::value::MathValue;

/// One open compound frame on the builder's stack.
#[derive(Debug)]
struct OpenFrame<'p> {
    kind: MathCompoundKind,
    branch_tag: Option<u32>,
    children: Vec<MathValue<'p>>,
}

/// Concrete `StructBuilder` for the math grammar.
#[derive(Debug)]
pub struct MathStructBuilder<'p> {
    arena: MathArena<'p>,
    stack: Vec<OpenFrame<'p>>,
    root: Option<MathValue<'p>>,
    next_handle: u64,
}

impl<'p> Default for MathStructBuilder<'p> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'p> MathStructBuilder<'p> {
    #[inline]
    pub fn new() -> Self {
        Self {
            arena: MathArena::new(),
            stack: Vec::with_capacity(16),
            root: None,
            next_handle: 0,
        }
    }

    #[inline]
    pub fn with_capacity(compounds: usize) -> Self {
        Self {
            arena: MathArena::with_capacity(compounds),
            stack: Vec::with_capacity(16),
            root: None,
            next_handle: 0,
        }
    }

    #[inline]
    pub fn finalise(mut self, input: &'p str) -> MathDocument<'p> {
        debug_assert!(
            self.stack.is_empty(),
            "MathStructBuilder::finalise called with {} open frame(s)",
            self.stack.len()
        );
        let root = self
            .root
            .take()
            .expect("MathStructBuilder::finalise called before any value emission");
        MathDocument::new(self.arena, root, input)
    }

    #[inline]
    fn deposit(&mut self, value: MathValue<'p>) {
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

impl<'p> StructBuilder for MathStructBuilder<'p> {
    fn begin_compound(&mut self, layout: &StructLayout) -> CompoundHandle {
        let kind = MathCompoundKind::from_rule_name(layout.rule_name.as_str());
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
            .expect("MathStructBuilder::end_compound on empty stack");
        let id = self.arena.push_compound(MathCompound {
            kind: frame.kind,
            branch_tag: frame.branch_tag,
            children: frame.children,
        });
        self.deposit(MathValue::Compound(id));
    }

    #[inline]
    fn push_leaf_with_f64(&mut self, _value: f64) {
        // Math does not project f64 through any rule today.
        self.deposit(MathValue::Unit);
    }

    #[inline]
    fn push_leaf_with_i64(&mut self, _value: i64) {
        self.deposit(MathValue::Unit);
    }

    #[inline]
    fn push_leaf_with_u64(&mut self, _value: u64) {
        self.deposit(MathValue::Unit);
    }

    #[inline]
    fn push_leaf_with_bool(&mut self, _value: bool) {
        self.deposit(MathValue::Unit);
    }

    #[inline]
    fn push_leaf_with_str(&mut self, value: &str) {
        // SAFETY: see CsvStructBuilder::push_leaf_with_str.
        let lifetime_extended: &'p str = unsafe { std::mem::transmute(value) };
        self.deposit(MathValue::Span(lifetime_extended));
    }

    #[inline]
    fn push_leaf_with_unit(&mut self) {
        self.deposit(MathValue::Unit);
    }

    #[inline]
    fn push_branch_tag(&mut self, branch_index: u32) {
        if let Some(frame) = self.stack.last_mut() {
            frame.branch_tag = Some(branch_index);
        }
    }
}
