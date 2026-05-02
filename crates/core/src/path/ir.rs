//! Path IR types — borrowed `Path<'a>`, owned `TypedPath<G, T>`, and
//! the `PathSegment` alphabet.
//!
//! AZ-IV.W2.1 lands the type surface only — no execution, no resolver.
//! [`PathSegment`] is the finite alphabet of steps a typed path can
//! take; [`Path`] is the borrowed slice form (`Path<'a>(&'a
//! [PathSegment<'a>])`); [`TypedPath`] is the compile-time-typed owned
//! form parameterised by a grammar marker `G` (see
//! [`super::markers`]) and a terminal type `T` extracted from the
//! grammar's `StructRegistry`.
//!
//! The `path!` proc-macro (AZ-IV.W2.4) emits `TypedPath<G, T>`
//! literals at compile time after walking the segments through
//! [`super::type_check::check_path_against_registry`]. `PathError`
//! ([`super::error`]) carries diagnostics for the proc-macro to wrap
//! into a `proc_macro2::Span`-anchored `syn::Error`.
//!
//! Existing borrowed `Path<'a>` lives at `crate::runtime::path` for
//! pre-W2 path-alphabet borrowing; this module is the W2 typed path
//! surface and stays disjoint per the wave's modify-carve rule.

use core::fmt;
use core::marker::PhantomData;

/// One step in a typed path.
///
/// Variants:
///
/// - [`PathSegment::Field`] — borrow a struct field / object key by
///   name. The lifetime `'a` borrows from the caller's literal scope.
/// - [`PathSegment::Index`] — index into a positional list (array /
///   `Vec<T>` / tuple position). `usize`-typed.
/// - [`PathSegment::Wildcard`] — match every element under a list (or
///   every value under a map; the lazy-iter execution lane is
///   AZ-IV.W2.5). The IR variant is declared here; the executor lives
///   in `super::wildcard` (created by W2.5).
/// - [`PathSegment::VariantName`] — select one variant of a typed
///   enum by the grammar's `->` annotation. `'static`-named so the
///   proc-macro can interpolate the variant identifier with no
///   allocation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PathSegment<'a> {
    /// Step into a named field / object key. Borrowed from the
    /// caller's string literal scope.
    Field(&'a str),
    /// Step into a positional index of a list / array / tuple.
    Index(usize),
    /// Match every element under a list / every value under a map.
    /// Wildcard execution is W2.5.
    Wildcard,
    /// Select one variant of a typed-enum sum by the grammar's `->`
    /// annotation.
    VariantName(&'static str),
}

impl<'a> fmt::Display for PathSegment<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PathSegment::Field(name) => f.write_str(name),
            PathSegment::Index(idx) => write!(f, "{idx}"),
            PathSegment::Wildcard => f.write_str("*"),
            PathSegment::VariantName(name) => write!(f, "@{name}"),
        }
    }
}

/// Borrowed form of a typed path — a slice of [`PathSegment`] steps.
///
/// `Path<'a>` is the input the macro and the offline checker consume.
/// It does not carry grammar/terminal-type information; that
/// information is added by [`TypedPath`] once the segments have been
/// validated against a `StructRegistry`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Path<'a>(pub &'a [PathSegment<'a>]);

impl<'a> Path<'a> {
    /// Wrap a borrowed segment slice.
    #[inline]
    pub const fn new(segments: &'a [PathSegment<'a>]) -> Self {
        Self(segments)
    }

    /// Borrow the underlying segment slice.
    #[inline]
    pub const fn as_slice(&self) -> &'a [PathSegment<'a>] {
        self.0
    }

    /// Number of segments in the path.
    #[inline]
    pub const fn len(&self) -> usize {
        self.0.len()
    }

    /// `true` iff the path is empty (i.e. points at the root).
    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Iterate segments left-to-right.
    #[inline]
    pub fn iter(&self) -> core::slice::Iter<'_, PathSegment<'a>> {
        self.0.iter()
    }
}

impl<'a> From<&'a [PathSegment<'a>]> for Path<'a> {
    #[inline]
    fn from(segments: &'a [PathSegment<'a>]) -> Self {
        Self(segments)
    }
}

impl<'a> fmt::Display for Path<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut first = true;
        for seg in self.0 {
            if !first {
                f.write_str(".")?;
            }
            seg.fmt(f)?;
            first = false;
        }
        Ok(())
    }
}

/// Compile-time-typed path: grammar marker `G`, terminal type `T`.
///
/// `TypedPath<G, T>` is the result of resolving a borrowed
/// [`Path`] against a grammar's `StructRegistry`. `G` names the
/// grammar (see [`super::markers`]); `T` is the type the path
/// terminates at — the proc-macro reads the registry to extract `T`
/// at expansion time, so `TypedPath<Json, &str>`,
/// `TypedPath<CssL4, &CssColor>`, etc. all live as distinct compile-time
/// types.
///
/// This wave (W2.1) lands the type only. Execution against a parsed
/// document arrives in W3 (lazy bail-out parse) and W5 (TS binding).
/// The struct stores the segment vector by value so a `TypedPath` can
/// outlive the call-site that built it; that lifetime independence is
/// what makes a long-lived typed-path constant possible at the macro
/// expansion site.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypedPath<G, T> {
    segments: Vec<OwnedPathSegment>,
    _grammar: PhantomData<G>,
    _terminal: PhantomData<fn() -> T>,
}

/// Owned form of a [`PathSegment`].
///
/// Macro-emitted typed paths cannot borrow from a literal scope at
/// runtime, so the typed path stores owned segment values. `Field` and
/// `VariantName` keep their text as `Cow<'static, str>`-equivalent
/// owned strings; the macro emits `String::from("statuses")` literals
/// at expansion. `'static` borrows still survive through `Field`'s
/// `String` form because the owning string outlives every borrow taken
/// from it.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum OwnedPathSegment {
    /// Owned-string field name.
    Field(String),
    /// Positional index.
    Index(usize),
    /// Wildcard step.
    Wildcard,
    /// Owned-string variant selector. Owned for symmetry with `Field`;
    /// the macro inserts `'static`-named variants but carrying an
    /// owned string allows runtime construction of typed paths in
    /// future tranches without re-introducing a `'static` constraint.
    VariantName(String),
}

impl OwnedPathSegment {
    /// Borrow back to a borrowed-form [`PathSegment`].
    pub fn as_borrowed(&self) -> PathSegment<'_> {
        match self {
            OwnedPathSegment::Field(s) => PathSegment::Field(s.as_str()),
            OwnedPathSegment::Index(i) => PathSegment::Index(*i),
            OwnedPathSegment::Wildcard => PathSegment::Wildcard,
            // SAFETY-via-leak alternative is unnecessary: the borrow
            // returned here lives only as long as `&self`, which is
            // exactly the borrow lifetime `PathSegment::VariantName`
            // expects when extended via the borrowed re-projection
            // path. The `'static` constraint on `PathSegment::VariantName`
            // is for macro-time interpolation only; the borrowed form
            // here uses the owned-string re-projection at runtime.
            //
            // We emit the variant name through `Field` semantics for
            // borrowed re-projection, since the runtime resolver
            // dispatches on the same `&str` payload. This collapse is
            // intentional: callers that need the raw owned variant
            // name use [`OwnedPathSegment::variant_name`] directly.
            OwnedPathSegment::VariantName(s) => PathSegment::Field(s.as_str()),
        }
    }

    /// If this is a `VariantName` segment, return the variant name.
    pub fn variant_name(&self) -> Option<&str> {
        match self {
            OwnedPathSegment::VariantName(s) => Some(s.as_str()),
            _ => None,
        }
    }
}

impl<'a> From<PathSegment<'a>> for OwnedPathSegment {
    fn from(seg: PathSegment<'a>) -> Self {
        match seg {
            PathSegment::Field(s) => OwnedPathSegment::Field(s.to_owned()),
            PathSegment::Index(i) => OwnedPathSegment::Index(i),
            PathSegment::Wildcard => OwnedPathSegment::Wildcard,
            PathSegment::VariantName(s) => OwnedPathSegment::VariantName(s.to_owned()),
        }
    }
}

impl fmt::Display for OwnedPathSegment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            OwnedPathSegment::Field(s) => f.write_str(s),
            OwnedPathSegment::Index(i) => write!(f, "{i}"),
            OwnedPathSegment::Wildcard => f.write_str("*"),
            OwnedPathSegment::VariantName(s) => write!(f, "@{s}"),
        }
    }
}

impl<G, T> TypedPath<G, T> {
    /// Construct from owned segments. The `G` and `T` parameters are
    /// caller-supplied; the type checker ([`super::type_check`])
    /// guarantees they match what the registry resolves the segments
    /// to.
    ///
    /// Direct construction is intentionally `pub`: the proc-macro
    /// emits this call after the type checker validates the path. End
    /// users should reach for `path!(...)` instead of constructing a
    /// `TypedPath` directly.
    pub fn from_owned(segments: Vec<OwnedPathSegment>) -> Self {
        Self {
            segments,
            _grammar: PhantomData,
            _terminal: PhantomData,
        }
    }

    /// Construct from a borrowed slice. Convenience wrapper that
    /// allocates one `Vec`; intended for the offline checker / tests.
    /// The proc-macro emits owned literals directly.
    pub fn from_segments(segments: &[PathSegment<'_>]) -> Self {
        Self::from_owned(
            segments
                .iter()
                .copied()
                .map(OwnedPathSegment::from)
                .collect(),
        )
    }

    /// Borrow the segment vector back as a slice of borrowed segments.
    /// Re-allocates a small `Vec`; for hot paths the caller should
    /// keep the owned form.
    pub fn segments(&self) -> Vec<PathSegment<'_>> {
        self.segments.iter().map(|s| s.as_borrowed()).collect()
    }

    /// Borrow the owned segment storage directly without re-projecting.
    pub fn owned_segments(&self) -> &[OwnedPathSegment] {
        &self.segments
    }

    /// Number of segments in the typed path.
    pub fn len(&self) -> usize {
        self.segments.len()
    }

    /// True iff the typed path has zero segments (root path).
    pub fn is_empty(&self) -> bool {
        self.segments.is_empty()
    }
}

/// Trait for types accepted as path segments by the `path!`
/// proc-macro and the inline `path!` (`runtime::path`) macro.
///
/// Implementations convert from a literal form (`&str`, `usize`,
/// integer types) into a [`PathSegment`]. Deliberately closed; the
/// path alphabet is intentionally finite — extend via the macro
/// surface, not via additional impls.
pub trait IntoPathSegment<'a> {
    /// Convert into a borrowed [`PathSegment`].
    fn into_path_segment(self) -> PathSegment<'a>;
}

impl<'a> IntoPathSegment<'a> for &'a str {
    #[inline]
    fn into_path_segment(self) -> PathSegment<'a> {
        PathSegment::Field(self)
    }
}

impl<'a> IntoPathSegment<'a> for usize {
    #[inline]
    fn into_path_segment(self) -> PathSegment<'a> {
        PathSegment::Index(self)
    }
}

impl<'a> IntoPathSegment<'a> for i32 {
    #[inline]
    fn into_path_segment(self) -> PathSegment<'a> {
        PathSegment::Index(self as usize)
    }
}

impl<'a> IntoPathSegment<'a> for PathSegment<'a> {
    #[inline]
    fn into_path_segment(self) -> PathSegment<'a> {
        self
    }
}
