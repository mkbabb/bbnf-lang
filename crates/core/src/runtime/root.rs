//! Grammar root traits shared by generated and hand-written runtimes.
//!
//! These traits bind grammar marker types to their typed view, eager
//! value, and path-query surfaces. Concrete document parse results own
//! their arenas directly; the root traits remain for generated view and
//! value projection code that still needs a grammar-indexed contract.

use tape::{Tape, TapeOffset};

use crate::runtime::path::Path;

/// Binding between a grammar marker type and the root view it produces.
///
/// Every grammar struct implements this trait via generated code. The
/// GAT `type View<'p>` gives generated document APIs a way to lend a
/// cursor-backed root view whose lifetime is tied to the borrowed
/// document and input.
pub trait Root: Sized {
    /// The grammar's root view type, parameterized by the lifetime of
    /// the document borrow.
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

/// Grammars that can materialize their full parsed tree into an eager
/// `<Grammar>Value` enum.
pub trait ValueRoot: Root {
    /// The grammar's root value type, parameterized by the lifetime of
    /// the document borrow.
    type Value<'p>
    where
        Self: 'p;

    /// Project the unified substrate into the grammar's value enum.
    fn project_value_output<'p>(
        tape: &Tape<Self>,
        input: &'p str,
    ) -> Self::Value<'p>
    where
        Self: 'p;
}

/// Grammars that support lazy path queries yielding a single leaf.
pub trait PathQuery<T>: Root {
    /// Resolve `path` against `view`, yielding the extracted leaf or
    /// `None` if the path does not match.
    fn query<'p>(view: Self::View<'p>, path: Path<'_>) -> Option<T>
    where
        Self: 'p;
}
