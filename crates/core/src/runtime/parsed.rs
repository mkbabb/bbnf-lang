//! `Parsed<'p, R>` — the zero-copy parse result type.
//!
//! Tranche AJ.2 rewrites the parse result to borrow the input string
//! instead of owning a copy. `Parsed<'p, R>` carries a `&'p str`
//! reference to the caller's input buffer, eliminating the `memcpy`
//! that the pre-AJ `parse()` performed on every call.
//!
//! The lifetime chain is:
//!   caller's `input: &str` → `Parsed<'p, R>` borrows input as `'p`
//!   → `Parsed::view()` lends `R::View<'v>` where `'v ≤ 'p`
//!
//! Callers hold `Parsed<'_, Grammar>` and call `.view()` to obtain
//! a cursor-backed typed view bound to the borrow.
//!
//! # Tranche B5.W1 — substrate boundary restoration
//!
//! Pre-B5.W1 `Parsed` carried a `FusedOutput<R>` (tape + value
//! substrate welded behind a wrapper) plus a `_root_marker:
//! PhantomData<R>` field. B5.W1 promotes the value substrate onto
//! `Tape<R>` directly and collapses `Parsed` to a 3-field record:
//! `{ tape: Tape<R>, input: &'p str, root_offset: TapeOffset }`.
//! Both `Parsed::view()` and `Parsed::to_value()` now read off the
//! single substrate.

use tape::{Tape, TapeOffset};

use crate::runtime::path::Path;

/// Binding between a grammar marker type and the root view it
/// produces over a parsed tape.
///
/// Every grammar struct implements this trait via generated code.
/// The GAT `type View<'p>` gives [`Parsed`] a way to lend a cursor-
/// backed root view whose lifetime is tied to `&self`, without
/// forcing the grammar struct itself to carry a lifetime parameter.
///
/// `make_view` receives both the borrowed tape and the borrowed
/// source input so every schema accessor can slice text out of the
/// original buffer at zero cost.
///
/// # B5.W1 — generic tape parameter
///
/// The trait accepts `&'p Tape<()>` (the unit-marker tape) directly.
/// `Tape<R>` carries `R` purely as `PhantomData<fn() -> R>`, so
/// `Tape<()>` and `Tape<Self>` are layout-identical. The
/// `Parsed::view()` caller transmutes `&Tape<Self>` to `&Tape<()>`
/// in zero-cost fashion to satisfy the trait shape; cursor-backed
/// view types do not depend on `R` for any structural read.
pub trait Root: Sized {
    /// The grammar's root view type, parameterized by the lifetime
    /// of the borrow on the owning [`Parsed`].
    type View<'p>
    where
        Self: 'p;

    /// Construct the root view from a borrowed tape, the borrowed
    /// source input, and the root record's offset.
    fn make_view<'p>(
        tape: &'p Tape<()>,
        input: &'p str,
        root: TapeOffset,
    ) -> Self::View<'p>;
}

/// Zero-copy parse result — wraps a finished tape + borrowed input
/// + root offset, lends out typed views over it.
///
/// `'p` is the lifetime of the source input string. `R` is the
/// grammar marker struct (e.g. `Json` for a JSON grammar). The
/// actual root view type is resolved through `R`'s [`Root`] impl
/// when [`Parsed::view`] is called.
///
/// # B5.W1 — 3-field record
///
/// `Parsed` carries the unified [`Tape<R>`] substrate plus the
/// borrowed input and root offset. The structural-cursor surface
/// (`view()` / `get()`) and the typed-value surface (`to_value()`)
/// both read off `tape` directly; pre-B5.W1's `FusedOutput<R>`
/// wrapper retired alongside the welded builder.
pub struct Parsed<'p, R> {
    /// Unified substrate — structural columns + value-frame arena +
    /// payload columns in one type. `R` ties parse-time emission to
    /// projection-time consumption.
    pub tape: Tape<R>,
    /// Borrowed source input. Views carry a `&'p str` slice of this
    /// field and use it for every text-extraction accessor.
    pub input: &'p str,
    /// Offset of the root record within the tape.
    pub root_offset: TapeOffset,
}

impl<'p, R> ::core::fmt::Debug for Parsed<'p, R> {
    fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
        f.debug_struct("Parsed")
            .field("tape_len", &self.tape.len())
            .field("input_len", &self.input.len())
            .field("root_offset", &self.root_offset)
            .field("frame_count", &self.tape.frame_count())
            .finish()
    }
}

impl<'p, R> Parsed<'p, R> {
    /// Construct a `Parsed` from a finished tape, a borrowed input
    /// string, and the root record's offset.
    ///
    /// B5.W1: collapsed to a single constructor. Pre-B5.W1 separate
    /// `new` (tape-only) / `new_fused_output` (fused) entry points
    /// existed; the substrate transposition retires the duality.
    #[inline]
    pub fn new(tape: Tape<R>, input: &'p str, root_offset: TapeOffset) -> Self {
        Self {
            tape,
            input,
            root_offset,
        }
    }


    /// Borrow the underlying tape (the unified substrate).
    #[inline]
    pub fn tape(&self) -> &Tape<R> {
        &self.tape
    }

    /// Borrow the source input string.
    #[inline]
    pub fn input(&self) -> &'p str {
        self.input
    }

    /// The root record's offset within the tape.
    #[inline]
    pub fn root_offset(&self) -> TapeOffset {
        self.root_offset
    }

    /// Consume the `Parsed` and return ownership of the tape.
    #[inline]
    pub fn into_tape(self) -> Tape<R> {
        self.tape
    }

    /// Borrow the value-substrate frame arena directly. Renamed from
    /// pre-B5.W1's `value_frames_output` to match the unified
    /// [`Tape<R>`] surface (`tape.frames()` returns the same slice).
    #[inline]
    pub fn frames(&self) -> &[tape::ValueFrame] {
        self.tape.frames()
    }

    /// Consume the `Parsed` and hand back the owned tape — used by
    /// consumers that need ownership of the substrate without the
    /// rest of the `Parsed` surface.
    #[inline]
    pub fn into_frames(self) -> Tape<R> {
        self.tape
    }

}

impl<'p, R: Root> Parsed<'p, R> {
    /// Lend out the grammar's root view, bound by the borrow on
    /// `self`. The view is constructed on each call from the stored
    /// `(tape, input, root_offset)` triple via the grammar's
    /// [`Root::make_view`] impl — constant-cost, no allocation.
    ///
    /// # Tranche AY.W6.1 — single substrate, single cursor
    ///
    /// `view()` wraps the root record in a
    /// [`TapeCursor`](tape::TapeCursor) and hands it to the generated
    /// view type. Every universal accessor (`.kind()`, `.span()`,
    /// `.children()`, `.child(i)`, `.variant_idx()`) resolves to a
    /// column-indexed read through that cursor.
    #[inline]
    pub fn view(&self) -> R::View<'_> {
        // B5.W1: `Tape<R>` and `Tape<()>` are layout-identical (R is
        // `PhantomData<fn() -> R>`); transmute is zero-cost and
        // sound. The trait carries `&Tape<()>` so generated impls
        // need not name `R` in their signature.
        // SAFETY: Tape<R>'s only `R`-dependent field is
        // `_root_marker: PhantomData<fn() -> R>`, which is a ZST
        // with identical representation across all R.
        let tape_unit: &Tape<()> = unsafe {
            &*(&self.tape as *const Tape<R> as *const Tape<()>)
        };
        R::make_view(tape_unit, self.input, self.root_offset)
    }
}

/// Grammars that can materialise their full parsed tree into an
/// eager `<Grammar>Value` enum.
///
/// AY.W3a substrate; AY-II.W0.c lands the fused-pipeline reading
/// discipline. B5.W1: the projection reads frames off the unified
/// [`Tape<Self>`] substrate (no FusedOutput wrapper); semantics are
/// preserved bit-for-bit.
pub trait ValueRoot: Root {
    /// The grammar's root value type, parameterised by the lifetime
    /// of the borrow on the owning [`Parsed`].
    type Value<'p>
    where
        Self: 'p;

    /// B5.W1 — project the unified substrate into the grammar's
    /// `Value<'p>`. Reads frames from `Tape<Self>` and constructs
    /// the typed enum without touching the structural tape columns.
    /// The signature uses `Tape<Self>` since the projection emitter
    /// dispatches on `Self`'s rule space; the substrate's `R`
    /// phantom and the trait's `Self` are unified by construction.
    fn project_value_output<'p>(
        tape: &Tape<Self>,
        input: &'p str,
    ) -> Self::Value<'p>
    where
        Self: 'p;
}

/// Grammars that support lazy `get_by_path` queries yielding a
/// single leaf of type `T`.
///
/// `T` is the leaf shape the caller wants — `&str`, `f64`, `bool`,
/// or the grammar's own `Value<'p>` enum. The emitted impl walks
/// the tape from the root, following [`PathSegment`](crate::runtime::PathSegment)
/// steps without materialising intermediate compounds.
pub trait PathQuery<T>: Root {
    /// Resolve `path` against `view`, yielding the extracted leaf
    /// or `None` if the path does not match.
    fn query<'p>(view: Self::View<'p>, path: Path<'_>) -> Option<T>
    where
        Self: 'p;
}

impl<'p, R> Parsed<'p, R> {
    /// Project the unified substrate into the grammar's
    /// `<Grammar>Value` enum.
    ///
    /// # Tranche AY-II.W0.c — fused pipeline, thin projector
    ///
    /// `to_value()` is a thin projection over [`Tape<R>`] populated
    /// during the single-pass parse. The body forwards to the grammar
    /// -emitted [`ValueRoot::project_value_output`] which reads
    /// frames + payload columns from the value substrate and
    /// constructs the typed enum in one pass. No second parse call;
    /// no visitor reconstruction; no tape-walking materializer path.
    #[inline]
    pub fn to_value(&self) -> R::Value<'_>
    where
        R: ValueRoot,
    {
        R::project_value_output(&self.tape, self.input)
    }

    /// Resolve a lazy path query against the parsed tree.
    #[inline]
    pub fn get<T>(&self, path: Path<'_>) -> Option<T>
    where
        R: PathQuery<T>,
    {
        R::query(self.view(), path)
    }
}
