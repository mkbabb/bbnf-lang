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
//! # Tranche AY.W6.1 — unified runtime consumers
//!
//! `view()`, `to_value()`, and `get()` all read the same canonical
//! packed substrate through a single
//! [`TapeCursor`](tape::TapeCursor); see each consumer's per-fn
//! documentation for the specifics. No intermediate tree is rebuilt,
//! no shadow cursor is maintained, and no routing `if` decides
//! between cursor-backed and alternative code paths — the cursor is
//! the single read surface over both write-time-stamped (AY.W5
//! `open_compound` / `close_compound`) and finaliser-stamped
//! emission modes. The `Vec<<Grammar>Value<'p>>` that `to_value()`
//! returns inside compound variants is the user-facing materialised
//! result, not an internal structural rebuild; `view()` and `get()`
//! allocate nothing beyond the cursor wrapper itself.
//!
//! # Example (generated code shape)
//!
//! ```ignore
//! pub fn parse(input: &str) -> Result<Parsed<'_, Json>, ParseErr> {
//!     let mut state = ParserState::new(input);
//!     let mut builder = TapeBuilder::with_capacity(input.len() / 4);
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
//!     let tape = builder.finish().map_err(ParseErr::Tape)?;
//!     Ok(Parsed::new(tape, input, root_off))
//! }
//! ```

use std::marker::PhantomData;

use tape::{Tape, TapeOffset};

use crate::runtime::path::Path;

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
#[derive(Debug)]
pub struct Parsed<'p, R> {
    /// The finished tape. Owned by the `Parsed` so view lifetimes
    /// naturally bind to `&self`.
    tape: Tape,
    /// Borrowed source input. Views carry a `&'p str` slice of this
    /// field and use it for every text-extraction accessor.
    input: &'p str,
    /// Offset of the root record within `tape`.
    root_offset: TapeOffset,
    /// Phantom marker for the grammar's `Root` binding.
    _root_marker: PhantomData<R>,
}

impl<'p, R> Parsed<'p, R> {
    /// Construct a new `Parsed` from a finished tape, a borrowed
    /// input string, and the root record's offset within it.
    /// Called by generated `parse` functions at the end of a
    /// successful parse.
    #[inline]
    pub fn new(tape: Tape, input: &'p str, root_offset: TapeOffset) -> Self {
        Self {
            tape,
            input,
            root_offset,
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
/// AY.W3a substrate; the emitted impl lands in AY.W3b. The contract:
/// `Value<'p>` borrows from the tape / input for lifetime `'p`, and
/// `view_to_value` is the pure transformation `View<'p> ->
/// Value<'p>` — no new allocation beyond the Value enum itself.
///
/// A10 §d correction: `ValueRoot` is supplied directly on the
/// grammar marker; there is no user-facing `ToValue` trait bound
/// layered on top. Callers reach `to_value` exclusively through
/// [`Parsed::to_value`].
pub trait ValueRoot: Root {
    /// The grammar's root value type, parameterised by the lifetime
    /// of the borrow on the owning [`Parsed`]. Mirrors the shape of
    /// [`Root::View`] — same lifetime, same `Self: 'p` bound.
    type Value<'p>
    where
        Self: 'p;

    /// Eagerly materialise a root view into the grammar's value
    /// enum. Emitted per-grammar by AY.W3b; dispatches through the
    /// per-shape inline fns (`materialize_object_*`, etc.) to match
    /// the json-prototype speed ceiling.
    fn view_to_value<'p>(view: Self::View<'p>) -> Self::Value<'p>
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
    /// Eagerly materialise the parsed tree into the grammar's
    /// `<Grammar>Value` enum. Implemented per-grammar in AY.W3b;
    /// dispatches through `ValueRoot::view_to_value`.
    ///
    /// The returned value borrows from `self` — specifically from
    /// the tape and the input slice — for the duration of the
    /// re-borrow on `&self`.
    ///
    /// # Tranche AY.W6.1 — single substrate, single cursor
    ///
    /// `to_value()` reads the canonical packed substrate via the
    /// same [`TapeCursor`](tape::TapeCursor) `view()` exposes. The
    /// emitted `materialize_value_<Grammar>` root dispatches on the
    /// cursor's `rule_kind()`, walks children through
    /// [`TapeCursor::children`](tape::TapeCursor::children), and
    /// reads scalar payloads through `tape.payload_*(rec)` — no
    /// intermediate tree is rebuilt between the tape and the
    /// returned `<Grammar>Value`. The `Vec<<Grammar>Value<'p>>`
    /// carried inside `Compound` variants IS the user-facing
    /// materialisation contract, not an internal rebuild: it is the
    /// leaf of the eager materialiser, produced once per
    /// `to_value()` call and handed directly to the caller.
    #[inline]
    pub fn to_value(&self) -> R::Value<'_>
    where
        R: ValueRoot,
    {
        R::view_to_value(self.view())
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
