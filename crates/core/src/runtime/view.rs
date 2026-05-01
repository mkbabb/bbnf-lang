//! Grammar-agnostic typed-view trait.
//!
//! Per-grammar `*View` types drift in surface area (`JsonView`
//! exposes kind/root/arena/is_*; `CssView` / `SheetsView` are
//! thinner). The path executor consumes a UNIFORM surface — this
//! trait IS that surface.
//!
//! The trait is implemented by each grammar's `*View` type
//! (`JsonView`, `SheetsView`, `CssView`). The associated `Kind` type
//! is the per-grammar discriminator (`JsonKind`, `SheetsKind`,
//! `CssDocumentKind`); `kind()` reports the focused node's typed
//! shape, `span()` returns the source slice the focused node
//! covered (or `None` for compound focuses without a single
//! contiguous span), `input()` returns the full input slice the
//! parse consumed, and `children()` walks the structural children
//! of the focused node yielding sub-views with the same `'p`
//! lifetime.
//!
//! # Lifetime discipline
//!
//! The trait is parameterised by `'p` — the input slice lifetime
//! threaded through every per-grammar [`crate::runtime`] type. The
//! associated `Kind` type is `Copy + Debug + Eq` so view callers can
//! match on it without re-cloning.

/// Grammar-agnostic typed-view trait.
///
/// Implemented on each grammar's `*View` type. The trait gives the
/// W2-act.close B-stages and BA path executor a uniform navigation
/// surface across the JSON / CSS L4 / Sheets struct-direct runtimes.
///
/// # Associated items
///
/// - [`Self::Kind`] — the per-grammar typed-shape discriminator
///   (`JsonKind`, `SheetsKind`, `CssDocumentKind`).
/// - [`Self::kind`] — typed-shape discriminator on the focused node.
/// - [`Self::span`] — source slice the focused node covered, when
///   the node has a single contiguous span; `None` for compound
///   focuses whose span is a union of disjoint child spans.
/// - [`Self::input`] — full input slice the parse consumed.
/// - [`Self::children`] — iterator over structural children of the
///   focused node; yields the same `Self` type so navigation
///   composes uniformly.
///
/// # Lifetime parameter
///
/// `'p` is the input slice lifetime. Every `&str` returned by the
/// trait surface borrows from `'p` so consumers can compose the
/// returned slices without lifetime widening.
pub trait RuntimeView<'p>: Sized {
    /// Per-grammar typed-shape discriminator.
    type Kind: Copy + std::fmt::Debug + Eq;

    /// Report the focused node's typed shape.
    fn kind(&self) -> Self::Kind;

    /// Return the source slice the focused node covered, when the
    /// node has a single contiguous span. Returns `None` for compound
    /// focuses whose span is a union of disjoint child spans, and
    /// `None` for scalar focuses whose typed payload is not a
    /// borrowed slice (e.g. `JsonValue::Number`,
    /// `JsonValue::Bool`).
    fn span(&self) -> Option<&'p str>;

    /// Return the full input slice the parse consumed. Threaded
    /// through the per-grammar `*Document` from
    /// `crate::runtime::*::*StructBuilder::finalise(input)`.
    fn input(&self) -> &'p str;

    /// Walk the structural children of the focused node, yielding a
    /// new view per child. Compound focuses (arrays / objects /
    /// stylesheets / rules / declarations / sheet compounds) iterate
    /// their structural children in source order; leaf focuses
    /// return an empty iterator.
    fn children(&self) -> impl Iterator<Item = Self> + '_;
}
