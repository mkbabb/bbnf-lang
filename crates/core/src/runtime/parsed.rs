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
//! # Tranche AY-II.W0.c — fused parse-value pipeline
//!
//! The default `parse()` entry writes the canonical tape substrate
//! AND the grammar-emitted value substrate (via
//! [`ValueBuilder<R>`](crate::runtime::ValueBuilder)) in a single
//! walk. `Parsed<'p, R>` carries the [`ValueBuilderOutput<R>`] the
//! fused pipeline produced; `Parsed::to_value()` becomes a thin
//! projector over the already-constructed substrate — no second
//! parse call, no visitor-driven reconstruction, no tape-walking
//! materializer path. The tape remains available through `view()` /
//! `get()` for the structural-cursor surface; the typed-value
//! surface lives on the parallel substrate.
//!
//! The `view()` / `get()` consumer paths retain their cursor-backed
//! discipline: every structural accessor (`.kind()`, `.span()`,
//! `.children()`, `.child(i)`, `.variant_idx()`) resolves to a
//! column-indexed read through a [`TapeCursor`](tape::TapeCursor)
//! constructed on demand. No intermediate tree is rebuilt, no shadow
//! cursor is maintained, and no routing branch picks between cursor-
//! backed and alternative read paths for the structural view.
//!
//! # Example (generated code shape)
//!
//! ```ignore
//! pub fn parse(input: &str) -> Result<Parsed<'_, Json>, ParseErr> {
//!     let mut state = ParserState::new(input);
//!     let mut builder = TapeBuilder::with_capacity(input.len() / 4);
//!     let mut value_builder = ValueBuilder::<Self>::new(input.len() / 8);
//!     let root_off = Self::__value(&mut state, &mut builder, &mut value_builder)
//!         .ok_or(ParseErr::Syntax { offset: state.offset as u32, rule: None })?;
//!     // Skip trailing whitespace before EOF check.
//!     while state.offset < input.len()
//!         && input.as_bytes()[state.offset].is_ascii_whitespace()
//!     {
//!         state.offset += 1;
//!     }
//!     if state.offset < input.len() {
//!         return Err(ParseErr::Syntax { offset: state.offset as u32, rule: None });
//!     }
//!     let tape = builder.finish().map_err(ParseErr::Tape)?;
//!     let value_output = value_builder.finish(0);
//!     Ok(Parsed::new_fused(tape, input, root_off, value_output))
//! }
//! ```

use std::marker::PhantomData;

use tape::{Tape, TapeOffset};

use crate::runtime::path::Path;
use crate::runtime::value_builder::ValueBuilderOutput;

/// Binding between a grammar marker type and the root view it
/// produces over a parsed tape.
///
/// Every grammar struct with `#[derive(Parser)]` implements this
/// trait via generated code. The GAT `type View<'p>` gives
/// [`Parsed`] a way to lend a cursor-backed root view whose
/// lifetime is tied to `&self`, without forcing the grammar
/// struct itself to carry a lifetime parameter.
///
/// `make_view` receives both the borrowed tape and the borrowed
/// source input so every schema accessor can slice text out of
/// the original buffer at zero cost.
pub trait Root {
    /// The grammar's root view type, parameterized by the lifetime
    /// of the borrow on the owning [`Parsed`].
    type View<'p>
    where
        Self: 'p;

    /// Construct the root view from a borrowed tape, the borrowed
    /// source input, and the root record's offset. Generated
    /// parsers call this from [`Parsed::view`] to lend the view on
    /// demand.
    fn make_view<'p>(tape: &'p Tape, input: &'p str, root: TapeOffset) -> Self::View<'p>;
}

/// Zero-copy parse result — wraps a finished tape + borrowed input
/// + root offset, lends out typed views over it.
///
/// `'p` is the lifetime of the source input string. `R` is the
/// grammar marker struct (e.g. `Json` for a JSON grammar). The
/// actual root view type is resolved through `R`'s [`Root`] impl
/// when [`Parsed::view`] is called; callers never instantiate the
/// view directly.
///
/// # Tranche AY-II.W0.c — fused-pipeline value substrate
///
/// `Parsed` additionally carries a [`ValueBuilderOutput<R>`]
/// populated during the single-pass fused parse. The value output is
/// the sole source for `Parsed::to_value()` — no reparse, no tape
/// walk, no visitor-driven second pass reaches the consumer surface.
/// Substrate-only constructions (see [`Parsed::new`]) that never
/// exercise `to_value()` carry [`ValueBuilderOutput::empty`]; the
/// grammar-emitted projection expects a non-empty output on the
/// `to_value()` path and that expectation is an IR invariant the
/// emitter upholds by construction.
pub struct Parsed<'p, R> {
    /// The finished tape. Owned by the `Parsed` so view lifetimes
    /// naturally bind to `&self`.
    tape: Tape,
    /// Borrowed source input. Views carry a `&'p str` slice of this
    /// field and use it for every text-extraction accessor.
    input: &'p str,
    /// Offset of the root record within `tape`.
    root_offset: TapeOffset,
    /// AY-II.W0.c — the fused-pipeline value substrate. Populated at
    /// parse time in lockstep with the tape; consumed by
    /// `to_value()` as a thin projection target. `ValueBuilderOutput`
    /// owns its backing storage so `Parsed<'p, R>` remains lifetime-
    /// parameterised solely by the input borrow.
    value_builder_output: ValueBuilderOutput<R>,
    /// Phantom marker for the grammar's `Root` binding.
    _root_marker: PhantomData<R>,
}

impl<'p, R> ::core::fmt::Debug for Parsed<'p, R> {
    fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
        f.debug_struct("Parsed")
            .field("tape", &self.tape)
            .field("input_len", &self.input.len())
            .field("root_offset", &self.root_offset)
            .field("value_frames", &self.value_builder_output.frame_count())
            .finish()
    }
}

impl<'p, R> Parsed<'p, R> {
    /// Construct a new substrate-only `Parsed` from a finished tape,
    /// a borrowed input string, and the root record's offset within
    /// it.
    ///
    /// This form is reserved for tape-substrate tests and internal
    /// constructions that never reach `to_value()`. Grammar-emitted
    /// `parse()` entries use [`Parsed::new_fused`] instead so
    /// `to_value()` has a populated [`ValueBuilderOutput`] to project
    /// over.
    #[inline]
    pub fn new(tape: Tape, input: &'p str, root_offset: TapeOffset) -> Self {
        Self {
            tape,
            input,
            root_offset,
            value_builder_output: ValueBuilderOutput::empty(),
            _root_marker: PhantomData,
        }
    }

    /// Construct a fused-pipeline `Parsed` — carries both the
    /// canonical tape substrate AND the parallel value substrate the
    /// grammar-emitted parse entry populated in lockstep. The
    /// `to_value()` consumer projects over `value_builder_output`
    /// alone; the tape remains the canonical structural substrate
    /// for `view()` / `get()`.
    #[inline]
    pub fn new_fused(
        tape: Tape,
        input: &'p str,
        root_offset: TapeOffset,
        value_builder_output: ValueBuilderOutput<R>,
    ) -> Self {
        Self {
            tape,
            input,
            root_offset,
            value_builder_output,
            _root_marker: PhantomData,
        }
    }

    /// Borrow the underlying tape.
    #[inline]
    pub fn tape(&self) -> &Tape {
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
    pub fn into_tape(self) -> Tape {
        self.tape
    }

    /// Borrow the fused value substrate. The projection emitter
    /// reads this to construct the typed `<Grammar>Value` without
    /// touching the tape.
    #[inline]
    pub fn value_builder_output(&self) -> &ValueBuilderOutput<R> {
        &self.value_builder_output
    }

    /// Consume the `Parsed` and hand back the owned value output —
    /// used by consumers that need ownership of the projected
    /// `<Grammar>Value` without the rest of the `Parsed` surface.
    #[inline]
    pub fn into_value_builder_output(self) -> ValueBuilderOutput<R> {
        self.value_builder_output
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
    /// view type (`<Grammar>View<'p> { cursor, input }`). Every
    /// universal accessor (`.kind()`, `.span()`, `.children()`,
    /// `.child(i)`, `.variant_idx()`) resolves to a column-indexed
    /// read through that cursor; no intermediate tree, sidecar index,
    /// or shadow surface sits between the caller and the canonical
    /// packed substrate produced by `open_compound` / `close_compound`.
    /// W5.c documented the cursor as the single read surface across
    /// both write-time-stamped and finaliser-stamped emission modes —
    /// `view()` is the entry point into that surface.
    #[inline]
    pub fn view(&self) -> R::View<'_> {
        R::make_view(&self.tape, self.input, self.root_offset)
    }
}

/// Grammars that can materialise their full parsed tree into an
/// eager `<Grammar>Value` enum.
///
/// AY.W3a substrate; AY-II.W0.c lands the fused-pipeline reading
/// discipline: `Value<'p>` borrows from the input for lifetime `'p`
/// and is reconstructed via [`project_value_output`] from the
/// [`ValueBuilderOutput<Self>`](crate::runtime::ValueBuilderOutput)
/// that the fused parse entry populated alongside the tape. The
/// projection does not walk the tape; it reads frames from the
/// value substrate directly.
///
/// A10 §d correction: `ValueRoot` is supplied directly on the
/// grammar marker; there is no user-facing `ToValue` trait bound
/// layered on top. Callers reach `to_value` exclusively through
/// [`Parsed::to_value`].
///
/// The `Sized` supertrait is implied: grammar markers are ZSTs
/// by derive convention, and
/// [`ValueBuilderOutput`](crate::runtime::ValueBuilderOutput) holds
/// a phantom `R` that requires `R: Sized`.
///
/// [`project_value_output`]: ValueRoot::project_value_output
pub trait ValueRoot: Root + Sized {
    /// The grammar's root value type, parameterised by the lifetime
    /// of the borrow on the owning [`Parsed`]. Mirrors the shape of
    /// [`Root::View`] — same lifetime, same `Self: 'p` bound.
    type Value<'p>
    where
        Self: 'p;

    /// AY-II.W0.c — project the fused-pipeline value substrate
    /// into the grammar's `Value<'p>`. Emitted per-grammar by the
    /// Rust backend's `view/value.rs::emit_value_root_impl`; reads
    /// frames from [`ValueBuilderOutput`](crate::runtime::ValueBuilderOutput)
    /// and constructs the typed enum without touching the tape.
    ///
    /// `output` is the value substrate `ValueBuilder<Self>`
    /// produced at parse time (via
    /// [`ValueBuilder::finish`](crate::runtime::ValueBuilder::finish));
    /// `input` is the borrowed source text the leaf accessors slice
    /// against.
    ///
    /// The default emitter-provided body descends the frame arena
    /// from the root offset and builds each `<Grammar>Value`
    /// variant via the grammar's declared field order + CSP-inferred
    /// field types. The emitter projects leaves directly from
    /// [`PayloadTag`](crate::runtime::PayloadTag) /
    /// [`PayloadValue`](crate::runtime::PayloadValue) where those
    /// are populated and falls back to source-span decoding
    /// otherwise.
    fn project_value_output<'p>(
        output: &crate::runtime::ValueBuilderOutput<Self>,
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
/// steps without materialising intermediate compounds, and returns
/// `None` if any step misses.
pub trait PathQuery<T>: Root {
    /// Resolve `path` against `view`, yielding the extracted leaf
    /// or `None` if the path does not match.
    fn query<'p>(view: Self::View<'p>, path: Path<'_>) -> Option<T>
    where
        Self: 'p;
}

impl<'p, R> Parsed<'p, R> {
    /// Project the fused-pipeline value substrate into the grammar's
    /// `<Grammar>Value` enum.
    ///
    /// # Tranche AY-II.W0.c — fused pipeline, thin projector
    ///
    /// `to_value()` is a thin projection over the
    /// [`ValueBuilderOutput<R>`](crate::runtime::ValueBuilderOutput)
    /// populated during the single-pass fused parse. The body
    /// forwards to the grammar-emitted
    /// [`ValueRoot::project_value_output`] which reads frames +
    /// payload columns from the value substrate and constructs the
    /// typed enum in one pass. No second parse call. No visitor
    /// reconstruction. No tape-walking materializer path. The tape
    /// remains available through [`Parsed::view`] / [`Parsed::get`]
    /// for the structural-cursor surface; the typed-value surface
    /// lives on the parallel substrate exclusively.
    ///
    /// The returned value borrows from `self` — specifically from
    /// the input slice — for the duration of the re-borrow on
    /// `&self`.
    #[inline]
    pub fn to_value(&self) -> R::Value<'_>
    where
        R: ValueRoot,
    {
        R::project_value_output(&self.value_builder_output, self.input)
    }

    /// Resolve a lazy path query against the parsed tree. Returns
    /// `None` if any segment of `path` does not match.
    ///
    /// Implemented per-grammar in AY.W3b for the common leaf types
    /// (`&str`, `f64`, `bool`, `<Grammar>Value<'p>`).
    ///
    /// # Tranche AY.W6.1 — single substrate, single cursor
    ///
    /// `get()` reads the canonical packed substrate directly. The
    /// emitted `PathQuery<T>` impl constructs a
    /// [`TapeCursor`](tape::TapeCursor)-backed generic `NodeView`
    /// from `self.view()`, descends each [`PathSegment`](crate::runtime::PathSegment)
    /// via
    /// [`TapeCursor::children`](tape::TapeCursor::children) /
    /// [`TapeCursor::child`](tape::TapeCursor::child), and extracts
    /// the typed leaf at the hit via `tape.payload_*(rec)` — no
    /// tree is materialised, no sidecar index is consulted, and no
    /// Vec mirrors tape state. Every step is a column-indexed read
    /// through the same cursor substrate `view()` and `to_value()`
    /// consume.
    #[inline]
    pub fn get<T>(&self, path: Path<'_>) -> Option<T>
    where
        R: PathQuery<T>,
    {
        R::query(self.view(), path)
    }
}
