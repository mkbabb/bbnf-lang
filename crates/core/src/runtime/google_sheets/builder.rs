//! AZ-I.W2-act.B2 — `SheetsStructBuilder` — the concrete `StructBuilder`
//! impl that the generated Sheets parse function targets.
//!
//! The builder maintains an in-flight typed stack of partially-built
//! compounds. Each frame is a [`Frame`] holding the originating rule's
//! [`SheetsCompoundKind`] plus the in-flight `Vec<SheetsValue>` of
//! children pushed inside the compound's open / close window.
//!
//! Scalar pushes (`push_leaf_with_*`) deposit a typed
//! [`SheetsValue`] onto the topmost open frame's `children` Vec
//! (or, when the stack is empty, set the document root).
//!
//! `begin_compound` opens a new frame keyed on the layout's
//! `rule_id` via [`SheetsCompoundKind::from_rule_id`].
//! `end_compound` finalises the frame's children into a
//! [`crate::runtime::google_sheets::SheetsCompound`] entry on the
//! arena, then deposits the resulting
//! [`SheetsValue::Compound`] handle on the parent frame (or the root
//! slot for the outermost compound).
//!
//! # Wire contract
//!
//! Every typed `->` annotation in `grammar/google-sheets/google-sheets.bbnf`
//! reaches a [`StructBuilder`] method:
//!
//! - `number = /…/ -> f64`                    → `push_leaf_with_f64(value)`
//! - `string = /…/ -> input : Span`           → `push_leaf_with_str(slice)`
//! - `boolean = /TRUE/i -> true | /FALSE/i -> false` → `push_leaf_with_bool(value)`
//! - `error_literal = … -> Nu8`               → `push_leaf_with_u8(value)` *(via Tag dispatch)*
//! - `sheet_prefix = … -> Nu8`                → `push_leaf_with_u8` + branch tag
//! - `cell_ref = /…/ -> input : Span`         → `push_leaf_with_str(slice)` *(in Cell context)*
//! - `identifier = /…/ -> input : Span`       → `push_leaf_with_str(slice)` *(top-level)*
//! - `compare_op` / `add_op` / `mul_op` / `unary_prefix` -> Nu8` →
//!   `push_branch_tag(idx)` (operator-tag projection)
//! - Compound rules → `begin_compound(layout)` / `end_compound`
//!
//! The emitter's per-shape body knows which leaf surface to call from
//! the rule's typed projection; the builder's job is to keep the
//! frame-stack invariants and finalise compounds correctly.

use bbnf_ir::registry::StructLayout;

use crate::runtime::builder::StructBuilder;
use crate::runtime::google_sheets::arena::{SheetsArena, SheetsCompoundKind};
use crate::runtime::google_sheets::document::SheetsDocument;
use crate::runtime::google_sheets::value::SheetsValue;
use crate::runtime::handle::CompoundHandle;

/// One open compound frame on the builder's stack.
///
/// Mirrors `JsonStructBuilder::OpenFrame` in shape but uniform across
/// every Sheets compound — Sheets's grammar collapses every compound
/// into a children-Vec layout (no per-shape specialisation analogous
/// to JSON's `Object` / `Pair`). The frame's `kind` retains the
/// originating rule's structural shape so the finalised
/// [`crate::runtime::google_sheets::SheetsCompound`] entry preserves
/// it for downstream consumers.
#[derive(Debug, Clone)]
struct Frame<'p> {
    /// The compound's structural-kind tag, derived from the layout's
    /// `rule_id` at `begin_compound` time.
    kind: SheetsCompoundKind,
    /// The frame's pending children. Each leaf push or completed
    /// compound below this frame appends here.
    children: Vec<SheetsValue<'p>>,
    /// Handle counter mirror used to build the
    /// [`CompoundHandle`] returned to the emitter; opaque, ordered
    /// only for handle-pair invariant assertion in debug builds.
    #[allow(dead_code)]
    handle_token: u64,
}

/// Concrete `StructBuilder` for the Google Sheets grammar.
///
/// Owns a [`SheetsArena`] and a stack of open frames. The generated
/// parse function constructs a builder, threads it through every
/// per-shape parse fn, and calls [`SheetsStructBuilder::finalise`] at
/// EOF to recover the [`SheetsDocument`].
#[derive(Debug)]
pub struct SheetsStructBuilder<'p> {
    /// Owning arena — every closed compound's child slice lands
    /// here.
    arena: SheetsArena<'p>,
    /// In-flight open compound stack.
    stack: Vec<Frame<'p>>,
    /// The root value, set when the outermost compound (or scalar
    /// against an empty stack) finalises. The generated parse
    /// function reads this via [`Self::finalise`].
    root: Option<SheetsValue<'p>>,
    /// Monotonic compound-handle counter. The handle is opaque to
    /// the emitter — it pairs `begin_compound` with the matching
    /// `end_compound` only.
    next_handle: u64,
}

/// Rollback snapshot for [`SheetsStructBuilder`].
#[derive(Debug, Clone)]
pub struct SheetsStructCheckpoint<'p> {
    compounds: usize,
    stack: Vec<Frame<'p>>,
    root: Option<SheetsValue<'p>>,
    next_handle: u64,
}

impl<'p> Default for SheetsStructBuilder<'p> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'p> SheetsStructBuilder<'p> {
    /// Construct a fresh builder with an empty arena and no open
    /// frames.
    #[inline]
    pub fn new() -> Self {
        Self {
            arena: SheetsArena::new(),
            stack: Vec::with_capacity(8),
            root: None,
            next_handle: 0,
        }
    }

    /// Construct a builder with arena slab capacity hints. The
    /// emitter folds capacity estimates from
    /// [`bbnf_ir::registry::StructRegistry`] / `GrammarProfile` into
    /// this constructor at codegen time.
    #[inline]
    pub fn with_capacity(compounds: usize) -> Self {
        Self {
            arena: SheetsArena::with_capacity(compounds),
            stack: Vec::with_capacity(8),
            root: None,
            next_handle: 0,
        }
    }

    /// Finalise the builder into a [`SheetsDocument`]. Panics if no
    /// root value was emitted (the generated parse fn guarantees one
    /// scalar / compound emission against an empty stack), or if any
    /// open frame remains.
    ///
    /// `input` is the slice the parse consumed, threaded through to
    /// the produced [`SheetsDocument::input`] so view callers can
    /// retrieve the source without re-acquiring it.
    #[inline]
    pub fn finalise(mut self, input: &'p str) -> SheetsDocument<'p> {
        debug_assert!(
            self.stack.is_empty(),
            "SheetsStructBuilder::finalise called with {} open frame(s)",
            self.stack.len()
        );
        let root = self
            .root
            .take()
            .expect("SheetsStructBuilder::finalise called before any value emission");
        SheetsDocument::new(self.arena, root, input)
    }

    /// Land a finalised [`SheetsValue`] on the topmost open frame, or
    /// store it as the document root if the stack is empty.
    #[inline]
    fn deposit(&mut self, value: SheetsValue<'p>) {
        match self.stack.last_mut() {
            None => {
                // Root-position emission. Generated parse fn invokes
                // exactly one such deposit per parse — the outermost
                // compound's `end_compound` or a scalar root.
                self.root = Some(value);
            }
            Some(frame) => {
                frame.children.push(value);
            }
        }
    }
}

impl<'p> StructBuilder for SheetsStructBuilder<'p> {
    type Checkpoint = SheetsStructCheckpoint<'p>;

    #[inline]
    fn checkpoint(&self) -> Self::Checkpoint {
        SheetsStructCheckpoint {
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
        // Layouts emitted by `populate_struct_registry` carry the
        // grammar's stable rule id verbatim; resolve the structural
        // kind via the registry-projected discriminator. Layouts
        // outside the kind alphabet (defensive fallback) collapse onto
        // Wrap.
        let kind = SheetsCompoundKind::from_rule_id(layout.rule_id);
        self.next_handle = self.next_handle.wrapping_add(1);
        let handle_token = self.next_handle;
        self.stack.push(Frame {
            kind,
            children: Vec::new(),
            handle_token,
        });
        CompoundHandle::new(handle_token, 0)
    }

    fn end_compound(&mut self, _handle: CompoundHandle) {
        let frame = self
            .stack
            .pop()
            .expect("SheetsStructBuilder::end_compound on empty stack");
        // Transparent-wrap compounds with exactly one child collapse
        // to the child's value — the wrap layer is structurally
        // transparent. Matches the grammar's `primary | range_end |
        // cell_or_range | expression` Wrap-Alt semantics: the
        // dispatched branch's value is the compound's value, no
        // arena slab entry needed.
        let value = if frame.kind.is_transparent_wrap() && frame.children.len() == 1 {
            frame.children[0]
        } else {
            let id = self.arena.push_compound(frame.kind, frame.children);
            SheetsValue::Compound(id)
        };
        self.deposit(value);
    }

    #[inline]
    fn push_leaf_with_f64(&mut self, value: f64) {
        self.deposit(SheetsValue::Number(value));
    }

    #[inline]
    fn push_leaf_with_i64(&mut self, value: i64) {
        // No grammar rule projects to i64 in Sheets; widening into
        // f64 is the only typed-compatible projection. Consumers
        // reading `Number(f64)` recover the integer with full
        // precision in the f64-lossless range.
        self.deposit(SheetsValue::Number(value as f64));
    }

    #[inline]
    fn push_leaf_with_u64(&mut self, value: u64) {
        // Same widening discipline as `push_leaf_with_i64` — Sheets
        // has no native u64 projection; collapse onto Number(f64).
        self.deposit(SheetsValue::Number(value as f64));
    }

    #[inline]
    fn push_leaf_with_bool(&mut self, value: bool) {
        self.deposit(SheetsValue::Bool(value));
    }

    #[inline]
    fn push_leaf_with_str(&mut self, value: &str) {
        // The emitter passes a slice borrowed from the input or the
        // arena; both carry the `'p` lifetime via the parse function's
        // signature. The trait's elided lifetime collapses; re-project
        // to the parse-arena lifetime so the deposited value matches
        // [`SheetsValue::String`]'s `&'p str`.
        //
        // SAFETY: the slice's lifetime is bound to the parse call
        // site by the generated function's signature; the trait
        // surface elides this so concrete builders can specialise.
        let lifetime_extended: &'p str = unsafe { core::mem::transmute(value) };
        // Sheets has multiple string-shaped projections (string,
        // cell_ref, identifier, sheet_prefix); the builder cannot
        // discriminate from the trait surface alone — the emitter
        // chooses the surface call. Default to the generic `String`
        // arm; per-rule specialised emitters call
        // [`Self::push_leaf_cell_ref`] / [`Self::push_leaf_identifier`]
        // / [`Self::push_leaf_sheet_prefix`] directly.
        self.deposit(SheetsValue::String(lifetime_extended));
    }

    #[inline]
    fn push_leaf_with_unit(&mut self) {
        // No unit-typed projection in the Sheets grammar — every
        // projection has a payload. Defensive emission deposits a
        // synthetic `Tag(0)` so consumers reading the slot observe a
        // well-formed value rather than a panic; this branch should
        // not fire in well-formed generation.
        debug_assert!(
            false,
            "SheetsStructBuilder::push_leaf_with_unit invoked; \
             Sheets grammar has no unit-typed projection"
        );
        self.deposit(SheetsValue::Tag(0));
    }

    #[inline]
    fn push_branch_tag(&mut self, branch_index: u32) {
        // Operator-tag rules (compare_op, add_op, mul_op,
        // unary_prefix, error_literal, sheet_prefix) project the
        // branch index as the typed payload. The grammar declares
        // each rule's branch count under 256, so the index narrows
        // to u8 cleanly.
        debug_assert!(
            branch_index < 256,
            "SheetsStructBuilder::push_branch_tag: branch_index {} \
             out of u8 range",
            branch_index
        );
        self.deposit(SheetsValue::Tag(branch_index as u8));
    }
}

impl<'p> SheetsStructBuilder<'p> {
    /// Specialised string-leaf emission for `cell_ref` rules.
    /// Distinguishes the borrowed slice from the generic `String`
    /// projection at the emitter call site.
    ///
    /// The emitter calls this directly when the originating rule's
    /// shape is `cell_ref`; the trait surface (`push_leaf_with_str`)
    /// stays the generic default.
    #[inline]
    pub fn push_leaf_cell_ref(&mut self, value: &str) {
        let extended: &'p str = unsafe { core::mem::transmute(value) };
        self.deposit(SheetsValue::CellRef(extended));
    }

    /// Specialised string-leaf emission for `identifier` rules.
    #[inline]
    pub fn push_leaf_identifier(&mut self, value: &str) {
        let extended: &'p str = unsafe { core::mem::transmute(value) };
        self.deposit(SheetsValue::Identifier(extended));
    }

    /// Specialised projection for `sheet_prefix`. The two-branch
    /// projection combines the typed Nu8 tag with the borrowed span;
    /// the emitter pairs them via this entry point so the deposited
    /// value carries both.
    #[inline]
    pub fn push_leaf_sheet_prefix(&mut self, tag: u8, value: &str) {
        let extended: &'p str = unsafe { core::mem::transmute(value) };
        self.deposit(SheetsValue::SheetPrefix {
            tag,
            text: extended,
        });
    }

    /// Specialised projection for `error_literal` rules — preserves
    /// the discriminator's role-tag distinction from the generic
    /// `Tag` projection used by operator rules.
    #[inline]
    pub fn push_leaf_error(&mut self, value: u8) {
        self.deposit(SheetsValue::Error(value));
    }
}
