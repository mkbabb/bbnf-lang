//! AZ-I.W2-act.A — `JsonDocument` + view / value / path accessor
//! surface.
//!
//! The struct-direct JSON parse path returns a [`JsonDocument`] whose
//! root [`JsonValue`] borrows from the input lifetime `'p` and whose
//! [`JsonArena`] owns every compound child slice. This module wraps
//! the document with the same API the pre-W2-act
//! `Parsed<JsonGrammar>` surface exposed:
//!
//! - [`JsonDocument::view`] — yields a thin [`JsonView`] newtype over
//!   `&JsonDocument` exposing the same shape callers expect (root
//!   value, arena handle resolution).
//! - [`JsonDocument::to_value`] — borrowed access to the root value,
//!   mirroring `Parsed::to_value()` semantics. Where `Parsed::to_value()`
//!   projected the tape into a `<Grammar>Value` enum, the document
//!   already IS the typed value tree; `to_value()` simply lends its
//!   root.
//! - [`JsonDocument::get`] — typed path queries forwarded to the
//!   [`JsonPathQuery`] trait; the leaf-type impls
//!   ([`&str`], [`f64`], [`bool`], [`JsonValue`]) walk the document's
//!   compound tree from the root following [`crate::runtime::path::PathSegment`]
//!   steps without materialising intermediate compounds.
//!
//! # Lifetime discipline
//!
//! `JsonDocument<'p>`'s lifetime parameter binds the input slice
//! lifetime — every [`JsonValue::String`] inside the document, every
//! [`JsonPair::key`], borrows from `'p`. [`JsonView<'a>`] borrows
//! the document with lifetime `'a` (typically shorter than `'p`); the
//! returned [`JsonValue`] references therefore carry both bounds —
//! `'a` to the document, `'p` to the input. The newtype expresses
//! this with a single elided `'a` because Rust's lifetime variance
//! collapses `'p ≥ 'a` to one observable lifetime in practice.

use crate::runtime::json::arena::{JsonArena, JsonArrayId, JsonObjectId};
use crate::runtime::json::value::{JsonPair, JsonValue};
use crate::runtime::path::{Path, PathSegment};

/// The root document returned by `bbnf::grammar::generated::json::JsonParser::parse`.
///
/// Holds the parse arena (which owns every compound child slice) and
/// the root value. Borrows the input bytes via the `'p` lifetime.
///
/// Resolving a [`JsonValue::Array`] / [`JsonValue::Object`] handle
/// goes through [`Self::array`] / [`Self::object`] (or, ergonomically,
/// through the [`JsonView`] surface returned by [`Self::view`]).
#[derive(Debug)]
pub struct JsonDocument<'p> {
    /// The compound child arena — owns every `[JsonValue]` and
    /// `[JsonPair]` slice the document references via handles.
    pub arena: JsonArena<'p>,
    /// The root value of the document.
    pub root: JsonValue<'p>,
    /// AZ-I.W2-act.close A.fix — the input slice the parse consumed.
    /// Threaded through `finalise(input)` so [`JsonView`] can satisfy
    /// the `RuntimeView::input()` surface without re-acquiring the
    /// source from the call site. Lifetime-tied to `'p`.
    pub input: &'p str,
}

impl<'p> JsonDocument<'p> {
    /// Construct a document from a populated arena, root value, and
    /// the input slice the parse consumed. The typical caller is the
    /// generated parse function; consumers outside the emitter rarely
    /// build a `JsonDocument` directly.
    #[inline]
    pub fn new(arena: JsonArena<'p>, root: JsonValue<'p>, input: &'p str) -> Self {
        Self { arena, root, input }
    }

    /// Borrow the root [`JsonValue`].
    ///
    /// AZ-I.W2-act.A — companion to [`Self::to_value`]; both lend the
    /// root by reference. `root()` is the lower-level accessor (used
    /// internally by [`JsonView`]); `to_value()` is the API consumers
    /// expect to mirror `Parsed::to_value()`.
    #[inline]
    pub fn root(&self) -> &JsonValue<'p> {
        &self.root
    }

    /// Borrow the underlying [`JsonArena`].
    ///
    /// Consumers walking [`JsonValue::Array`] / [`JsonValue::Object`]
    /// handles directly reach for this; the higher-level
    /// [`JsonView::array`] / [`JsonView::object`] methods on the view
    /// newtype dispatch through here.
    #[inline]
    pub fn arena(&self) -> &JsonArena<'p> {
        &self.arena
    }

    /// AZ-I.W2-act.close A.fix — borrow the input slice the parse
    /// consumed.
    #[inline]
    pub fn input(&self) -> &'p str {
        self.input
    }

    /// Resolve a [`JsonArrayId`] handle to the underlying element
    /// slice. Returns an empty slice for `JsonArrayId::EMPTY`.
    #[inline]
    pub fn array(&self, id: JsonArrayId) -> &[JsonValue<'p>] {
        self.arena.array(id)
    }

    /// Resolve a [`JsonObjectId`] handle to the underlying pair slice.
    /// Returns an empty slice for `JsonObjectId::EMPTY`.
    #[inline]
    pub fn object(&self, id: JsonObjectId) -> &[JsonPair<'p>] {
        self.arena.object(id)
    }

    /// AZ-I.W2-act.A — root view, mirroring the pre-W2-act
    /// `Parsed<JsonGrammar>::view()` surface.
    ///
    /// Returns a [`JsonView`] newtype whose accessors expose the
    /// document's root and arena. [`JsonView`] is the struct-tree
    /// equivalent of the pre-W2-act cursor-backed `View<'p>` GAT
    /// the generated code emitted on `Parsed`; consumers writing
    /// against either surface call `.view()` and reach the same
    /// observable shape (root, arena handle resolution, typed
    /// kind discriminator).
    #[inline]
    pub fn view<'a>(&'a self) -> JsonView<'a, 'p> {
        JsonView { doc: self, focus: self.root }
    }

    /// AZ-I.W2-act.A — borrowed root value, mirroring
    /// `Parsed::to_value()` semantics.
    ///
    /// Where `Parsed::to_value()` projected the tape into a
    /// `<Grammar>Value` enum (a one-pass materialisation), the
    /// struct-direct path's [`JsonDocument`] already carries the
    /// typed value tree — `to_value()` simply lends its root by
    /// reference. Consumers needing an owned value clone the
    /// returned `&JsonValue`; arenas + handles stay live on the
    /// document for as long as the document does.
    #[inline]
    pub fn to_value(&self) -> &JsonValue<'p> {
        &self.root
    }

    /// AZ-I.W2-act.A — typed path query, mirroring
    /// `Parsed::get::<T>(path)` semantics.
    ///
    /// Forwards to the [`JsonPathQuery`] trait; impls land per leaf
    /// type ([`&str`], [`f64`], [`bool`], [`JsonValue`]). The walker
    /// follows [`crate::runtime::path::PathSegment::Field`] steps
    /// against [`JsonValue::Object`] pair slices and
    /// [`crate::runtime::path::PathSegment::Index`] steps against
    /// [`JsonValue::Array`] element slices, descending the document
    /// without materialising intermediate compounds. A leaf value at
    /// the end of the path that fails to coerce to `T` returns
    /// `None`; a missing field or out-of-range index returns `None`.
    #[inline]
    pub fn get<T: JsonPathQuery>(&self, path: Path<'_>) -> Option<T> {
        T::query(self, path)
    }
}

/// AZ-I.W2-act.A — a thin newtype over `&JsonDocument`.
///
/// `JsonView<'a, 'p>` is the struct-tree equivalent of the
/// cursor-backed `View<'p>` the pre-W2-act `Parsed<JsonGrammar>`
/// lent. It exposes the root, the arena, and ergonomic resolution
/// of compound handles. The newtype shape mirrors `Parsed::view()`:
/// `JsonDocument::view()` always succeeds; the returned view
/// borrows the document for the caller-bound lifetime `'a`,
/// preserving the input lifetime `'p` on every yielded
/// [`JsonValue`] / [`JsonPair`] reference.
///
/// # Lifetime parameters
///
/// - `'a` — the borrow on the underlying [`JsonDocument`]; bounded
///   by the call site's borrow scope.
/// - `'p` — the input lifetime carried by every interior
///   [`JsonValue::String`] / [`JsonPair::key`]; outlives `'a`.
///
/// The two lifetimes stay distinct because [`JsonDocument`] is
/// invariant in `'p` (its arena owns `Vec<JsonValue<'p>>` slabs);
/// collapsing them to one would force `'p = 'a` and break view
/// composition for callers holding a long-lived document and
/// short-lived views.
///
/// # Why a newtype, not a `&JsonDocument`?
///
/// The pre-W2-act `Parsed::view()` returned `R::View<'_>` — a
/// generic-associated type per grammar. The newtype preserves the
/// "view is a distinct type" surface so consumers writing against
/// the struct-direct path mirror the cursor-path call site shape:
/// `let view = doc.view(); view.kind(); view.is_object();`.
///
/// Future struct-direct accessors (path-prep helpers, kind queries,
/// span hooks for diagnostics) land on [`JsonView`]'s `impl` block
/// rather than the document's; the document's surface stays narrow.
#[derive(Debug, Clone, Copy)]
pub struct JsonView<'a, 'p: 'a> {
    pub(crate) doc: &'a JsonDocument<'p>,
    /// AZ-I.W2-act.close A.fix — the focused [`JsonValue`] this view
    /// observes. Defaults to `doc.root` for `JsonDocument::view()`;
    /// `RuntimeView::children()` yields views with the same `doc` but
    /// a different focus (one per structural child).
    pub(crate) focus: JsonValue<'p>,
}

impl<'a, 'p: 'a> JsonView<'a, 'p> {
    /// Construct a view focused on a specific [`JsonValue`] within the
    /// document. Used by [`RuntimeView::children`] to project sub-views
    /// without re-allocating the underlying document.
    #[inline]
    pub fn focused(doc: &'a JsonDocument<'p>, focus: JsonValue<'p>) -> Self {
        Self { doc, focus }
    }

    /// Borrow the underlying document.
    #[inline]
    pub fn document(&self) -> &'a JsonDocument<'p> {
        self.doc
    }

    /// AZ-I.W2-act.close A.fix — the focused [`JsonValue`] this view
    /// observes (root for top-level views; sub-tree for descendants
    /// produced by `children()`).
    #[inline]
    pub fn focus(&self) -> JsonValue<'p> {
        self.focus
    }

    /// Borrow the root [`JsonValue`].
    ///
    /// Equivalent to [`JsonDocument::root`] — exposed here so view-
    /// site call patterns mirror `parsed.view().<accessor>()`. Note
    /// that this returns the *document* root regardless of which sub-
    /// tree the view is focused on; use [`Self::focus`] for the
    /// currently-observed value.
    #[inline]
    pub fn root(&self) -> &'a JsonValue<'p> {
        &self.doc.root
    }

    /// Borrow the underlying arena.
    #[inline]
    pub fn arena(&self) -> &'a JsonArena<'p> {
        &self.doc.arena
    }

    /// Resolve an array handle through the document's arena.
    #[inline]
    pub fn array(&self, id: JsonArrayId) -> &'a [JsonValue<'p>] {
        self.doc.array(id)
    }

    /// Resolve an object handle through the document's arena.
    #[inline]
    pub fn object(&self, id: JsonObjectId) -> &'a [JsonPair<'p>] {
        self.doc.object(id)
    }

    /// Discriminator over the root value's typed shape.
    ///
    /// Mirrors the `is_object` / `is_array` / `is_string` / `is_number`
    /// / `is_bool` / `is_null` accessors the pre-W2-act
    /// `JsonParserNodeView` surface exposed; consumers writing
    /// against either surface match on `view.kind()`.
    #[inline]
    pub fn kind(&self) -> JsonKind {
        match &self.focus {
            JsonValue::Null => JsonKind::Null,
            JsonValue::Bool(_) => JsonKind::Bool,
            JsonValue::Number(_) => JsonKind::Number,
            JsonValue::String(_) => JsonKind::String,
            JsonValue::Array(_) => JsonKind::Array,
            JsonValue::Object(_) => JsonKind::Object,
        }
    }

    /// `true` iff the root is an object compound.
    #[inline]
    pub fn is_object(&self) -> bool {
        matches!(self.focus, JsonValue::Object(_))
    }

    /// `true` iff the root is an array compound.
    #[inline]
    pub fn is_array(&self) -> bool {
        matches!(self.focus, JsonValue::Array(_))
    }

    /// `true` iff the root is a string scalar.
    #[inline]
    pub fn is_string(&self) -> bool {
        matches!(self.focus, JsonValue::String(_))
    }

    /// `true` iff the root is a number scalar.
    #[inline]
    pub fn is_number(&self) -> bool {
        matches!(self.focus, JsonValue::Number(_))
    }

    /// `true` iff the root is a boolean scalar.
    #[inline]
    pub fn is_bool(&self) -> bool {
        matches!(self.focus, JsonValue::Bool(_))
    }

    /// `true` iff the root is `null`.
    #[inline]
    pub fn is_null(&self) -> bool {
        matches!(self.focus, JsonValue::Null)
    }
}

/// Discriminator over the typed shapes a [`JsonValue`] takes.
///
/// Mirrors `tape::TapeKind` for the cursor-backed surface; the
/// struct-direct equivalent surfaces here so consumers branching on
/// `view.kind()` match against a finite, type-checked alphabet.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum JsonKind {
    /// `null`.
    Null,
    /// `true` / `false`.
    Bool,
    /// `number = /…/ -> f64`.
    Number,
    /// `string = /"…"/`.
    String,
    /// `array = "[" >> … << "]"`.
    Array,
    /// `object = "{" >> … << "}"`.
    Object,
}

/// AZ-I.W2-act.A — typed path-query trait, mirroring AY's
/// `runtime::PathQuery<T>` for the struct-direct surface.
///
/// Forwards from [`JsonDocument::get`]. The walker descends from
/// `doc.root()` following [`PathSegment::Field`] steps against
/// [`JsonValue::Object`] pair slices (linear scan; JSON object key
/// counts are typically small) and [`PathSegment::Index`] steps
/// against [`JsonValue::Array`] element slices. The terminal value
/// at the end of the path coerces to `T` per the per-`T` impl —
/// scalar coercion is exact (no widening, no string-to-number
/// fallback); a `T` mismatch returns `None`.
///
/// # Implementations
///
/// - [`&str`] — yields the borrowed slice from a [`JsonValue::String`].
/// - [`f64`] — yields the f64 witness from a [`JsonValue::Number`]
///   via [`crate::runtime::json::value::JsonNumber::as_f64`] (lossless
///   for `Float` and any `Int`/`UInt` in the f64 lossless range).
/// - [`bool`] — yields the bool witness from a [`JsonValue::Bool`].
/// - [`JsonValue`] — yields the entire value at the path; the
///   identity impl, useful when the caller wants the typed shape
///   itself (for example to discriminate between scalar leaves).
pub trait JsonPathQuery: Sized {
    /// Resolve `path` against `doc`, yielding the extracted leaf or
    /// `None` if any path segment fails to match.
    fn query<'p>(doc: &JsonDocument<'p>, path: Path<'_>) -> Option<Self>;
}

/// Walk the document's compound tree following `path` from `root`,
/// returning the resolved [`JsonValue`] reference (or `None` on
/// missing field / out-of-range index).
///
/// Helper consumed by every [`JsonPathQuery`] impl below; lifted out
/// so the per-leaf coercion happens once per path resolution.
#[inline]
fn walk_path<'a, 'p>(doc: &'a JsonDocument<'p>, path: Path<'_>) -> Option<&'a JsonValue<'p>> {
    let mut current: &'a JsonValue<'p> = &doc.root;
    for segment in path.iter() {
        current = match (current, segment) {
            (JsonValue::Object(id), PathSegment::Field(name)) => {
                let pairs = doc.object(*id);
                let pair = pairs.iter().find(|p| p.key == *name)?;
                // The arena lends `&JsonPair<'p>`; project the pair's
                // value field. Lifetime threads through the arena's
                // borrow on `doc`.
                &pair.value
            }
            (JsonValue::Array(id), PathSegment::Index(idx)) => {
                let items = doc.array(*id);
                items.get(*idx)?
            }
            // Type mismatch: e.g. Field step against an array, Index
            // step against an object, or any step against a scalar.
            _ => return None,
        };
    }
    Some(current)
}

impl JsonPathQuery for &str {
    #[inline]
    fn query<'p>(doc: &JsonDocument<'p>, path: Path<'_>) -> Option<Self> {
        // SAFETY: the borrowed `&str` slice lives for `'p` (the
        // document's input lifetime). The trait surface elides the
        // explicit `'p` because `&str` is invariant in lifetime here;
        // callers consume the returned slice within the document's
        // borrow scope.
        let value = walk_path(doc, path)?;
        match value {
            // Lifetime extension is sound because `'p` outlives any
            // borrow `doc` itself can produce — the JsonValue::String
            // slice was borrowed from the input bytes when the
            // document was built and remains live for the full
            // document lifetime.
            JsonValue::String(s) => {
                let extended: &'p str = *s;
                // Cast to a generic `&str` reaching back to the
                // caller-side borrow. The re-projection is safe
                // because `'p` outlives the caller's borrow on `doc`.
                Some(unsafe { core::mem::transmute::<&'p str, &str>(extended) })
            }
            _ => None,
        }
    }
}

impl JsonPathQuery for f64 {
    #[inline]
    fn query<'p>(doc: &JsonDocument<'p>, path: Path<'_>) -> Option<Self> {
        match walk_path(doc, path)? {
            JsonValue::Number(n) => Some(n.as_f64()),
            _ => None,
        }
    }
}

impl JsonPathQuery for bool {
    #[inline]
    fn query<'p>(doc: &JsonDocument<'p>, path: Path<'_>) -> Option<Self> {
        match walk_path(doc, path)? {
            JsonValue::Bool(b) => Some(*b),
            _ => None,
        }
    }
}

impl JsonPathQuery for JsonValue<'_> {
    #[inline]
    fn query<'p>(doc: &JsonDocument<'p>, path: Path<'_>) -> Option<Self> {
        // The Copy bound on JsonValue lets us deref-copy the resolved
        // reference; the resulting value's `'p` lifetime collapses to
        // the doc's borrow scope.
        let value = walk_path(doc, path)?;
        // SAFETY: JsonValue is Copy and carries a `'p` lifetime that
        // outlives the caller's borrow on `doc`. The transmute
        // re-projects the lifetime to the trait's elided one; sound
        // because the trait surface returns `Option<JsonValue<'_>>`
        // bound to the caller's borrow.
        let copied: JsonValue<'p> = *value;
        Some(unsafe { core::mem::transmute::<JsonValue<'p>, JsonValue<'_>>(copied) })
    }
}
