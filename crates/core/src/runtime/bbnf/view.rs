//! AZ-II.cutover.A — [`crate::runtime::RuntimeView`] impl for
//! [`super::BbnfView`].
//!
//! The BBNF struct-direct runtime focuses on a [`BbnfValue`] within a
//! [`BbnfDocument`]; the trait's `Kind` associated type is the
//! existing [`BbnfKind`] discriminator. `kind()` reports the focused
//! value's typed shape; `span()` returns the borrowed source slice
//! for [`BbnfValue::Span`] focuses; `input()` returns the full input
//! the parse consumed; `children()` walks the focused compound's
//! structural children.
//!
//! # AZ-II.cutover.D1 — consumer-migration accessor surface
//!
//! The host-grammar / lower passes consume a richer accessor set
//! than [`RuntimeView`] mandates: positional child access by index,
//! byte-offset spans for sub-string slicing, compound-kind matching,
//! and Alt branch-tag lookup. These extensions live here (the
//! migration-eligible surface per the cutover.D1 contract) and
//! complement the trait without duplicating its semantics.
//!
//! - [`BbnfView::byte_span`] — byte-offset `(lo, hi)` span recovered
//!   from the focused value: trivial for [`BbnfValue::Span`] (pointer
//!   arithmetic against the document input), recursive for compounds
//!   (union of the leftmost and rightmost child spans). Returns
//!   `None` for leaves with no source position (numeric / boolean /
//!   tag / unit).
//! - [`BbnfView::child`] — positional child access mirroring the
//!   tape-direct `child(i)` accessor. Returns `None` for leaves and
//!   for out-of-range indices.
//! - [`BbnfView::compound_kind`] — focused compound's structural
//!   discriminator. Returns `None` for non-compound focuses.
//! - [`BbnfView::branch_tag`] — focused compound's Alt branch
//!   discriminator (replaces the tape-era `variant_idx`). Returns
//!   `None` when the focus is not a compound or no branch tag was
//!   recorded.
//! - [`BbnfView::span_text`] — convenience: byte-span resolved against
//!   the document input. Empty string for spanless focuses.

use crate::runtime::bbnf::arena::{BbnfCompound, BbnfCompoundKind};
use crate::runtime::bbnf::document::{BbnfKind, BbnfView};
use crate::runtime::bbnf::value::BbnfValue;
use crate::runtime::RuntimeView;

impl<'a, 'p: 'a> RuntimeView<'p> for BbnfView<'a, 'p> {
    type Kind = BbnfKind;

    #[inline]
    fn kind(&self) -> Self::Kind {
        match self.focus {
            BbnfValue::Int(_) => BbnfKind::Int,
            BbnfValue::Float(_) => BbnfKind::Float,
            BbnfValue::Bool(_) => BbnfKind::Bool,
            BbnfValue::Span(_) => BbnfKind::Span,
            BbnfValue::Tag(_) => BbnfKind::Tag,
            BbnfValue::Unit => BbnfKind::Unit,
            BbnfValue::Compound(_) => BbnfKind::Compound,
        }
    }

    #[inline]
    fn span(&self) -> Option<&'p str> {
        // Span leaves carry the borrowed input slice; other shapes
        // do not project to a single contiguous source slice.
        match self.focus {
            BbnfValue::Span(s) => Some(s),
            _ => None,
        }
    }

    #[inline]
    fn input(&self) -> &'p str {
        self.doc.input
    }

    fn children(&self) -> impl Iterator<Item = Self> + '_ {
        let doc = self.doc;
        let focus = self.focus;
        BbnfChildrenIter {
            doc,
            focus,
            index: 0,
        }
    }
}

impl<'a, 'p: 'a> BbnfView<'a, 'p> {
    /// Recover the focused value's byte-offset span as `(lo, hi)`
    /// against the document input.
    ///
    /// Span leaves resolve via pointer arithmetic against the
    /// document input slice. Compound focuses recurse into their
    /// children to derive `lo` (leftmost descendant's start) and
    /// `hi` (rightmost descendant's end). Numeric / boolean / tag /
    /// unit leaves carry no source position (their typed projections
    /// dropped the textual form during parse) and return `None`.
    pub fn byte_span(&self) -> Option<(u32, u32)> {
        compute_byte_span(self.doc, self.focus)
    }

    /// Convenience wrapper over [`Self::byte_span`] returning the
    /// borrowed source slice covered by the focus, or `""` when
    /// the focus has no contiguous span.
    #[inline]
    pub fn span_text(&self) -> &'p str {
        match self.byte_span() {
            Some((lo, hi)) if hi >= lo => &self.doc.input[lo as usize..hi as usize],
            _ => "",
        }
    }

    /// Optional flavor of [`Self::span_text`]: `None` instead of `""`
    /// when the focus has no contiguous span. Used by D2's value_expr
    /// chain-folder and lifetime-extending callers.
    #[inline]
    pub fn span_text_opt(&self) -> Option<&'p str> {
        match self.byte_span() {
            Some((lo, hi)) if hi >= lo => Some(&self.doc.input[lo as usize..hi as usize]),
            _ => None,
        }
    }

    /// Alias of [`Self::byte_span`] — recovers the byte-offset bounds
    /// `(span_lo, span_hi)` for the focus.
    #[inline]
    pub fn span_bounds(&self) -> Option<(u32, u32)> {
        self.byte_span()
    }

    /// Positional child access. Leaves yield `None`; compounds yield
    /// the `i`th child's view, or `None` when `i` is out of range.
    pub fn child(&self, i: usize) -> Option<BbnfView<'a, 'p>> {
        match self.focus {
            BbnfValue::Compound(id) => {
                let entry = self.doc.compound(id);
                entry.children.get(i).map(|v| BbnfView::focused(self.doc, *v))
            }
            _ => None,
        }
    }

    /// Focused compound's structural-shape discriminator.
    /// Returns `None` for non-compound focuses.
    #[inline]
    pub fn compound_kind(&self) -> Option<BbnfCompoundKind> {
        match self.focus {
            BbnfValue::Compound(id) => Some(self.doc.compound(id).kind),
            _ => None,
        }
    }

    /// Focused compound's Alt branch-tag (recorded by
    /// [`crate::runtime::builder::StructBuilder::push_branch_tag`]).
    /// Returns `None` for non-compound focuses or when no branch
    /// tag was set.
    #[inline]
    pub fn branch_tag(&self) -> Option<u32> {
        match self.focus {
            BbnfValue::Compound(id) => self.doc.compound(id).branch_tag,
            _ => None,
        }
    }

    /// True iff the focused compound has the given structural kind.
    #[inline]
    pub fn is_compound_kind(&self, kind: BbnfCompoundKind) -> bool {
        self.compound_kind() == Some(kind)
    }

    /// Identity probe for views: the address of the focused
    /// compound's arena entry, or `None` for leaves. Used by
    /// traversal helpers to detect self-cycles when walking
    /// malformed compounds (a compound can never have itself in
    /// its child list at the same address). The arena's compound
    /// entries don't relocate post-parse, so the address is stable
    /// across the document lifetime.
    #[inline]
    pub fn compound_identity(&self) -> Option<usize> {
        match self.focus {
            BbnfValue::Compound(id) => {
                let entry = self.doc.compound(id);
                Some(entry as *const _ as usize)
            }
            _ => None,
        }
    }

    /// Borrow the focused compound's entry, when the focus is a
    /// compound. Returns `None` for leaves.
    #[inline]
    pub fn compound_entry(&self) -> Option<&'a BbnfCompound<'p>> {
        match self.focus {
            BbnfValue::Compound(id) => Some(self.doc.compound(id)),
            _ => None,
        }
    }
}

/// Recursive helper: derive the focused value's byte-offset span by
/// walking down through compounds until a [`BbnfValue::Span`] leaf
/// surfaces. The returned `(lo, hi)` covers the leftmost descendant
/// span's start through the rightmost descendant span's end. Returns
/// `None` when no descendant carries a source-borrowed slice.
fn compute_byte_span<'p>(
    doc: &crate::runtime::bbnf::document::BbnfDocument<'p>,
    focus: BbnfValue<'p>,
) -> Option<(u32, u32)> {
    match focus {
        BbnfValue::Span(s) => {
            let input_ptr = doc.input.as_ptr() as usize;
            let s_ptr = s.as_ptr() as usize;
            // Defensive: if the span doesn't lie inside the input
            // (shouldn't happen for grammar-emitted spans but the
            // arena's lifetime erasure during builder push leaves
            // it possible), report no-span rather than panicking.
            if s_ptr < input_ptr {
                return None;
            }
            let lo = (s_ptr - input_ptr) as u32;
            let hi = lo + s.len() as u32;
            Some((lo, hi))
        }
        BbnfValue::Compound(id) => {
            let entry = doc.compound(id);
            let mut lo: Option<u32> = None;
            let mut hi: Option<u32> = None;
            for child in &entry.children {
                if let Some((clo, chi)) = compute_byte_span(doc, *child) {
                    lo = Some(lo.map_or(clo, |existing| existing.min(clo)));
                    hi = Some(hi.map_or(chi, |existing| existing.max(chi)));
                }
            }
            match (lo, hi) {
                (Some(l), Some(h)) => Some((l, h)),
                _ => None,
            }
        }
        // Numeric / boolean / tag / unit leaves: typed projections
        // dropped the textual form at parse time; no source span
        // recoverable from a typed-leaf BbnfValue.
        _ => None,
    }
}

/// Child iterator for BBNF views.
///
/// Compound focuses yield one sub-view per child entry, in source
/// order. Leaf shapes yield nothing.
pub struct BbnfChildrenIter<'a, 'p: 'a> {
    doc: &'a crate::runtime::bbnf::BbnfDocument<'p>,
    focus: BbnfValue<'p>,
    index: usize,
}

impl<'a, 'p: 'a> Iterator for BbnfChildrenIter<'a, 'p> {
    type Item = BbnfView<'a, 'p>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.focus {
            BbnfValue::Compound(id) => {
                let entry = self.doc.compound(id);
                let item = entry.children.get(self.index)?;
                self.index += 1;
                Some(BbnfView::focused(self.doc, *item))
            }
            _ => None,
        }
    }
}
