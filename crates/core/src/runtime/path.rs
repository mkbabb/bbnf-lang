//! Path-query input types shared by document-owned runtime surfaces.
//!
//! Generated parse entry points return concrete grammar documents.
//! Each document module owns its typed path-query trait; this module
//! only defines the borrowed path alphabet consumed by those traits.
//! `PathSegment` is the finite alphabet of steps, and [`Path`] is a
//! borrowed slice of those steps so bench and call sites can reuse
//! literals without allocation.
//!
//! The [`path!`] macro sugars the common literal case:
//!
//! ```ignore
//! use bbnf::runtime::path;
//!
//! // equivalent to &[PathSegment::Field("statuses"),
//! //              PathSegment::Index(0),
//! //              PathSegment::Field("text")]
//! let p = path!["statuses", 0, "text"];
//! ```
//!
//! Grammar documents expose path lookups through their own
//! document-level traits, such as `JsonPathQuery`, `CssPathQuery`,
//! or `BbnfPathQuery`.

use core::fmt;

/// One step in a lazy path query. `Field` names a record field
/// (object key / struct field); `Index` picks an element in a
/// positional sequence (array / vec).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PathSegment<'a> {
    /// Step into a named child (object key / struct field). Borrowed
    /// from the caller's string — there is no interning at the path
    /// level; the emitted `PathQuery` impl handles comparison.
    Field(&'a str),
    /// Step into a positional child (array index / tuple position).
    Index(usize),
}

impl<'a> fmt::Display for PathSegment<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PathSegment::Field(s) => f.write_str(s),
            PathSegment::Index(i) => write!(f, "{i}"),
        }
    }
}

/// Borrowed path — a slice of [`PathSegment`] steps.
///
/// `Path<'a>` is a thin wrapper over `&'a [PathSegment<'a>]`. The
/// two lifetimes collapse in practice: path slices live for the
/// duration of a single query and never outlive the string literals
/// that populate the `Field` variants.
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

/// Trait for types accepted as path segments in the [`path!`] macro.
///
/// Implementations convert from the literal form (`&str` / `usize`
/// / integer types) into a [`PathSegment`]. Deliberately closed —
/// the path substrate is intentionally small; extend the macro
/// instead of this trait.
pub trait IntoPathSegment<'a> {
    /// Convert into a segment.
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

/// Construct a [`Path`]-friendly literal from a mixed list of
/// field names and indices.
///
/// Expands to a borrowed slice of [`PathSegment`] values. The
/// resulting slice lives in the caller's scope for the duration
/// of the expression — no allocation, no heap traffic.
///
/// ```ignore
/// let segs = bbnf::path!["statuses", 0, "text"];
/// let p = bbnf::runtime::Path::new(segs);
/// ```
#[macro_export]
macro_rules! path {
    [ $( $seg:expr ),* $(,)? ] => {
        &[
            $(
                $crate::runtime::path::IntoPathSegment::into_path_segment($seg),
            )*
        ]
    };
}
