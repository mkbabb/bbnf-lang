//! `Parsed<R>` — the owning parse result type.
//!
//! Tranche AB.2a introduced the storage form; AC.2 lands the `Root`
//! trait and the `.view()` constructor that together define the
//! public API every generated `Grammar::parse` function returns.
//!
//! `Parsed<R>` is marker-typed over the grammar struct itself —
//! never over the view type. The root view's lifetime is lent by
//! `&self` on `Parsed` via the [`Root`] trait's GAT `type View<'tape>`.
//! Callers never name a `'tape` lifetime directly; they hold
//! `Parsed<Grammar>` and call [`Parsed::view`] to obtain a cursor-
//! backed root view bound to the borrow.
//!
//! # Example (generated code shape)
//!
//! ```ignore
//! pub fn parse(input: &str) -> Result<Parsed<Json>, ParseErr> {
//!     let mut state = ParserState::new(input);
//!     let mut builder = TapeBuilder::with_capacity(1024);
//!     let root_off = Self::__value(&mut state, &mut builder)
//!         .ok_or(ParseErr::Syntax { offset: state.offset as u32, rule: None })?;
//!     let tape = builder.finish().map_err(ParseErr::Tape)?;
//!     Ok(Parsed::new(tape, root_off))
//! }
//!
//! impl ::bbnf::runtime::Root for Json {
//!     type View<'tape> = JsonRootView<'tape>;
//!     fn make_view(tape: &Tape, root: TapeOffset) -> Self::View<'_> {
//!         JsonRootView::new(tape, root)
//!     }
//! }
//! ```
//!
//! The caller holds the `Parsed<Json>` and calls `.view()` to obtain
//! a cursor-backed typed view:
//!
//! ```ignore
//! let parsed = Json::parse(input)?;
//! let root = parsed.view();
//! for pair in root.as_object()?.pairs() {
//!     // ... walk the tape via TapeCursor accessors
//! }
//! ```
//!
//! The lifetime relationship is natural: views borrow from the
//! `Parsed`, so `Parsed` must outlive them. Callers that need to
//! give up the typed surface and keep only the raw tape can call
//! [`Parsed::into_tape`].

use std::marker::PhantomData;

use bbnf_tape::{Tape, TapeOffset};

/// Binding between a grammar marker type and the root view it
/// produces over a parsed tape.
///
/// Every grammar struct with `#[derive(Parser)]` implements this
/// trait via generated code. The GAT `type View<'tape>` gives
/// [`Parsed`] a way to lend a cursor-backed root view whose
/// lifetime is tied to `&self`, without forcing the grammar
/// struct itself to carry a lifetime parameter.
pub trait Root {
    /// The grammar's root view type, parameterized by the lifetime
    /// of the borrow on the owning [`Parsed`].
    type View<'tape>
    where
        Self: 'tape;

    /// Construct the root view from a borrowed tape and the root
    /// record's offset. Generated parsers call this from
    /// [`Parsed::view`] to lend the view on demand.
    fn make_view(tape: &Tape, root: TapeOffset) -> Self::View<'_>;
}

/// Owning parse result — wraps a finished tape + root offset, lends
/// out typed views over it.
///
/// `R` is the grammar marker struct (e.g. `Json` for a JSON grammar).
/// The actual root view type is resolved through `R`'s [`Root`] impl
/// when [`Parsed::view`] is called; callers never instantiate the
/// view directly.
#[derive(Debug)]
pub struct Parsed<R> {
    /// The finished tape. Owned by the `Parsed` so view lifetimes
    /// naturally bind to `&self`.
    tape: Tape,
    /// Offset of the root record within `tape`.
    root_offset: TapeOffset,
    /// Phantom marker for the grammar's `Root` binding.
    _root_marker: PhantomData<R>,
}

impl<R> Parsed<R> {
    /// Construct a new `Parsed` from a finished tape and the root
    /// record's offset within it. Called by generated `parse`
    /// functions at the end of a successful parse.
    #[inline]
    pub fn new(tape: Tape, root_offset: TapeOffset) -> Self {
        Self {
            tape,
            root_offset,
            _root_marker: PhantomData,
        }
    }

    /// Borrow the underlying tape. Useful for callers that want to
    /// walk the raw records directly (diagnostics, serialization,
    /// schema emitters).
    #[inline]
    pub fn tape(&self) -> &Tape {
        &self.tape
    }

    /// The root record's offset within the tape.
    #[inline]
    pub fn root_offset(&self) -> TapeOffset {
        self.root_offset
    }

    /// Consume the `Parsed` and return ownership of the tape.
    /// Callers that no longer need the typed view can go through
    /// this to keep the tape alive for further processing.
    #[inline]
    pub fn into_tape(self) -> Tape {
        self.tape
    }
}

impl<R: Root> Parsed<R> {
    /// Lend out the grammar's root view, bound by the borrow on
    /// `self`. The view is constructed on each call from the stored
    /// `(tape, root_offset)` pair via the grammar's [`Root::make_view`]
    /// impl — constant-cost, no allocation.
    #[inline]
    pub fn view(&self) -> R::View<'_> {
        R::make_view(&self.tape, self.root_offset)
    }
}
