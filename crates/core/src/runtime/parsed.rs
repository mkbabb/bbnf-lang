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
//! # Tranche AY-II.W0'.a — fused parse-value pipeline
//!
//! The default `parse()` entry allocates a single
//! [`FusedBuilder`](tape::FusedBuilder) that owns the canonical
//! tape substrate AND the grammar-emitted value-frame arena. Every
//! shape emitter's `begin_compound` / `end_compound` / `push_leaf_*`
//! writes BOTH column families atomically. `Parsed<'p, R>` carries
//! the resulting [`FusedOutput<R>`](tape::FusedOutput);
//! `Parsed::to_value()` projects from the value column of that
//! output — no second parse call, no visitor-driven reconstruction,
//! no tape-walking materializer path. The tape column remains
//! available through `view()` / `get()` for the structural-cursor
//! surface; the typed-value surface lives on the paired value
//! column inside the same `FusedOutput`.
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
//!     let mut builder = FusedBuilder::with_capacity(input.len() / 4);
//!     let root_off = Self::__value(&mut state, &mut builder)
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
//!     let output = builder.finish_fused::<Self>(root_off.0).map_err(ParseErr::Tape)?;
//!     Ok(Parsed::new_fused(output, input, root_off))
//! }
//! ```

use std::marker::PhantomData;

use tape::{FusedOutput, Tape, TapeOffset};

use crate::runtime::path::Path;

/// Binding between a grammar marker type and the root view it
/// produces over a parsed tape.
///
/// Every grammar struct with `the proc-macro derive (retired B2)` implements this
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
/// # Tranche AY-II.W0'.a — fused-pipeline output
///
/// `Parsed` carries a [`FusedOutput<R>`] — tape + value substrate in
/// one handle. `Parsed::to_value()` reads the value column off the
/// fused output; `Parsed::view()` / `Parsed::get()` read the tape
/// column. Substrate-only constructions (see [`Parsed::new`]) that
/// never exercise `to_value()` carry [`FusedOutput::empty`]; the
/// grammar-emitted projection expects a non-empty output on the
/// `to_value()` path and that expectation is an IR invariant the
/// emitter upholds by construction.
pub struct Parsed<'p, R> {
    /// Fused parse output — tape + value substrates in one handle.
    /// Owned by the `Parsed` so view lifetimes naturally bind to
    /// `&self`.
    output: FusedOutput<R>,
    /// Borrowed source input. Views carry a `&'p str` slice of this
    /// field and use it for every text-extraction accessor.
    input: &'p str,
    /// Offset of the root record within the tape.
    root_offset: TapeOffset,
    /// Phantom marker for the grammar's `Root` binding.
    _root_marker: PhantomData<R>,
}

impl<'p, R> ::core::fmt::Debug for Parsed<'p, R> {
    fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
        f.debug_struct("Parsed")
            .field("tape", self.output.tape())
            .field("input_len", &self.input.len())
            .field("root_offset", &self.root_offset)
            .field("value_frames", &self.output.frame_count())
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
    /// `to_value()` has a populated [`FusedOutput<R>`] to project
    /// over.
    #[inline]
    pub fn new(tape: Tape, input: &'p str, root_offset: TapeOffset) -> Self {
        let output = FusedOutput::new(tape, tape::ValueFramesOutput::empty());
        Self {
            output,
            input,
            root_offset,
            _root_marker: PhantomData,
        }
    }

    /// Construct a fused-pipeline `Parsed` — carries the fused
    /// (tape + value) output the grammar-emitted parse entry
    /// populated in one walk. The `to_value()` consumer projects
    /// over the value column of the fused output; the tape column
    /// remains the canonical structural substrate for `view()` /
    /// `get()`.
    ///
    /// Post-W0'.a the grammar-emitted parse entry hands a
    /// `FusedOutput<R>` directly. Pre-regen `generated.rs` still
    /// calls the 4-arg legacy shape `new_fused(tape, input,
    /// root_offset, value_builder_output)`; the `tape` arg and the
    /// `value_builder_output` arg reassemble into a `FusedOutput<R>`
    /// at this level so the bootstrap escape window compiles
    /// without emitter regen. Orchestrator regen collapses to the
    /// 3-arg entry via [`Self::new_fused_output`].
    #[inline]
    pub fn new_fused(
        tape: Tape,
        input: &'p str,
        root_offset: TapeOffset,
        value_builder_output: FusedOutput<R>,
    ) -> Self {
        // Pre-regen bootstrap escape: the shim `ValueBuilder::finish`
        // returns an empty output — the fused value substrate lives
        // inside `tape` when it came out of `FusedBuilder::finish`,
        // not in `value_builder_output`. We can't recover the fused
        // value column from the bare `Tape` post-finish, so the
        // emitter change in the same commit retires the dual
        // allocator + the 4-arg call shape.
        //
        // Post-regen this entry point receives the bare `tape` + an
        // empty `value_builder_output` because the bona-fide fused
        // output moves through `new_fused_output`. This signature
        // survives as the pre-regen compose-boundary only.
        let (_, empty_value) = value_builder_output.into_parts();
        let output = FusedOutput::new(tape, empty_value);
        Self {
            output,
            input,
            root_offset,
            _root_marker: PhantomData,
        }
    }

    /// Construct a fused-pipeline `Parsed` from a pre-assembled
    /// [`FusedOutput<R>`]. Post-W0'.a + post-regen the grammar
    /// emitter calls this 3-arg entry point directly; the 4-arg
    /// [`Self::new_fused`] remains as the pre-regen bootstrap
    /// escape.
    #[inline]
    pub fn new_fused_output(
        output: FusedOutput<R>,
        input: &'p str,
        root_offset: TapeOffset,
    ) -> Self {
        Self {
            output,
            input,
            root_offset,
            _root_marker: PhantomData,
        }
    }

    /// Borrow the underlying tape (the structural column of the
    /// fused output).
    #[inline]
    pub fn tape(&self) -> &Tape {
        self.output.tape()
    }

    /// Borrow the fused output directly. Consumers that need both
    /// tape + value in one handle read through this accessor.
    #[inline]
    pub fn output(&self) -> &FusedOutput<R> {
        &self.output
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

    /// Consume the `Parsed` and return ownership of the tape column.
    #[inline]
    pub fn into_tape(self) -> Tape {
        self.output.into_tape()
    }

    /// Consume the `Parsed` and return ownership of the fused
    /// output.
    #[inline]
    pub fn into_output(self) -> FusedOutput<R> {
        self.output
    }

    /// Borrow the fused value substrate's [`ValueFramesOutput<R>`]
    /// directly. Pre-W0'.a alias — the projection emitter reads
    /// through [`Self::output`] post-regen.
    #[inline]
    pub fn value_builder_output(&self) -> &tape::ValueFramesOutput<R> {
        self.output.as_value_output()
    }

    /// Consume the `Parsed` and hand back the owned value output —
    /// used by consumers that need ownership of the projected
    /// `<Grammar>Value` without the rest of the `Parsed` surface.
    #[inline]
    pub fn into_value_builder_output(self) -> tape::ValueFramesOutput<R> {
        self.output.into_value()
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
        R::make_view(self.output.tape(), self.input, self.root_offset)
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

    /// AY-II.W0'.a — project the fused-pipeline value substrate
    /// into the grammar's `Value<'p>`. Emitted per-grammar by the
    /// Rust backend's `view/value.rs::emit_value_root_impl`; reads
    /// frames from [`FusedOutput<R>`](tape::FusedOutput) and
    /// constructs the typed enum without touching the tape column.
    ///
    /// `output` is the fused substrate `FusedBuilder` produced at
    /// parse time (via
    /// [`FusedBuilder::finish_fused`](tape::FusedBuilder::finish_fused));
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
        R::project_value_output(&self.output, self.input)
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
