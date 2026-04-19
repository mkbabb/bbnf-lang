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
    #[inline]
    pub fn view(&self) -> R::View<'_> {
        R::make_view(&self.tape, self.input, self.root_offset)
    }
}
