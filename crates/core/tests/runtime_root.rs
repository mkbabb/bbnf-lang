//! Unit tests for the `bbnf::runtime::{Parsed, Root}` public API.
//!
//! Verifies that:
//! 1. A grammar marker struct can implement `Root` with a concrete
//!    GAT view type.
//! 2. `Parsed::<Grammar>::view(&self)` lends a cursor-backed view
//!    bound by `&self`.
//! 3. Multiple views can be requested from the same `Parsed` —
//!    they are independent snapshots into the same tape.
//! 4. `Parsed::into_tape` surrenders ownership of the underlying
//!    tape for callers that no longer want the typed surface.
//!
//! Tranche AC.2 prep — the real grammar-generated code will bind
//! against this API once the emitter rewrite lands. These tests
//! exercise the API in isolation using a hand-written `Root` impl.

use bbnf::runtime::{Parsed, Root};
use bbnf_tape::{Tape, TapeBuilder, TapeCursor, TapeKind, TapeOffset};

/// Stand-in grammar marker for the tests — no parser, just a type
/// used to carry the `Root` trait impl.
struct TestGrammar;

/// Minimal view wrapping a cursor — matches the shape
/// `generate_views` emits.
#[derive(Clone, Copy, Debug)]
struct TestRootView<'tape> {
    cursor: TapeCursor<'tape>,
}

impl<'tape> TestRootView<'tape> {
    fn new(tape: &'tape Tape, offset: TapeOffset) -> Self {
        Self {
            cursor: TapeCursor::new(tape, offset),
        }
    }

    fn kind(&self) -> TapeKind {
        self.cursor.kind()
    }

    fn span(&self) -> (u32, u32) {
        self.cursor.span()
    }

    fn variant_idx(&self) -> u8 {
        self.cursor.variant_idx()
    }
}

impl Root for TestGrammar {
    type View<'tape> = TestRootView<'tape>;

    fn make_view(tape: &Tape, root: TapeOffset) -> Self::View<'_> {
        TestRootView::new(tape, root)
    }
}

/// Build a parsed result holding a single leaf span record.
fn parsed_with_one_leaf() -> Parsed<TestGrammar> {
    let mut builder = TapeBuilder::new();
    let leaf_off = builder.push_leaf(TapeKind::Span, 0, 5, 7);
    let tape = builder.finish().expect("tape finish");
    Parsed::new(tape, leaf_off)
}

#[test]
fn view_exposes_root_record_fields() {
    let parsed = parsed_with_one_leaf();
    let view = parsed.view();
    assert_eq!(view.kind(), TapeKind::Span);
    assert_eq!(view.span(), (0, 5));
    assert_eq!(view.variant_idx(), 7);
}

#[test]
fn multiple_views_are_independent_snapshots() {
    let parsed = parsed_with_one_leaf();
    let a = parsed.view();
    let b = parsed.view();
    assert_eq!(a.kind(), b.kind());
    assert_eq!(a.span(), b.span());
    assert_eq!(a.variant_idx(), b.variant_idx());
}

#[test]
fn tape_accessor_borrows_underlying_tape() {
    let parsed = parsed_with_one_leaf();
    let tape = parsed.tape();
    assert!(!tape.is_empty());
    let root_off = parsed.root_offset();
    let rec = tape.get(root_off);
    assert_eq!(rec.kind, TapeKind::Span);
}

#[test]
fn into_tape_surrenders_ownership() {
    let parsed = parsed_with_one_leaf();
    let tape: Tape = parsed.into_tape();
    assert!(!tape.is_empty());
}

#[test]
fn view_is_copy_and_cheap_to_clone() {
    let parsed = parsed_with_one_leaf();
    let view = parsed.view();
    let view_copy = view;
    // Both still usable after the move — confirms `Copy`.
    assert_eq!(view.kind(), view_copy.kind());
}
