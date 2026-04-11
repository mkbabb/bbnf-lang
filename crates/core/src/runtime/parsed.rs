//! `Parsed<View>` — the owning parse result type.
//!
//! Tranche AB.2. `Parsed` is returned by generated parsers as the
//! public API. It owns the [`bbnf_tape::Tape`] inline and lends out
//! typed lazy views over it via [`Parsed::view`], so callers never
//! deal with the `(View, Tape)` tuple the earlier tape-first plan
//! proposed.
//!
//! # Example (generated code shape)
//!
//! ```ignore
//! pub fn parse<'a>(input: &'a str) -> Result<Parsed<ValueView<'a>>, ParseErr> {
//!     let mut state = ParserState::new(input);
//!     let mut builder = TapeBuilder::with_capacity(1024);
//!     let root_off = __value(&mut state, &mut builder).ok_or(ParseErr::Syntax)?;
//!     let tape = builder.finish().map_err(ParseErr::Tape)?;
//!     Ok(Parsed::new(tape, root_off))
//! }
//! ```
//!
//! The caller holds the `Parsed<_>` and calls `.view()` to obtain a
//! cursor-backed typed view:
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

/// Owning parse result — wraps a finished tape + root offset, lends
/// out typed views over it.
///
/// `View` is the root view type (e.g. `ValueView<'tape>` for a JSON
/// grammar). The type parameter exists so the `impl Parsed<View>`
/// block can constrain view construction via a trait (not yet
/// defined in this tranche — the tape-to-view conversion is
/// currently done by the generated `parse` fn directly).
#[derive(Debug)]
pub struct Parsed<View> {
    /// The finished tape. Owned by the `Parsed` so view lifetimes
    /// naturally bind to `&self`.
    tape: Tape,
    /// Offset of the root record within `tape`.
    root_offset: TapeOffset,
    /// Phantom lifetime anchor for the view type.
    _view_marker: PhantomData<View>,
}

impl<View> Parsed<View> {
    /// Construct a new `Parsed` from a finished tape and the root
    /// record's offset within it. Called by generated `parse`
    /// functions at the end of a successful parse.
    #[inline]
    pub fn new(tape: Tape, root_offset: TapeOffset) -> Self {
        Self {
            tape,
            root_offset,
            _view_marker: PhantomData,
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
