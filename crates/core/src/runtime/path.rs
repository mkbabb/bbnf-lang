//! AY.W3a — path-query types for the lazy `get_by_path` lane.
//!
//! The lazy lane mirrors `sonic_rs::get_by_path(src, path)`: instead
//! of materialising the entire parsed tree, the caller walks a
//! narrow `[PathSegment]` slice against the tape and extracts a
//! single leaf. `PathSegment` is the finite alphabet of steps; a
//! [`Path`] is a borrowed slice of those steps, kept `Copy`-ish so
//! bench call sites re-use the literal without re-allocating.
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
//! Grammars that participate in path queries implement
//! `runtime::PathQuery<T>` via emitted code in AY.W3b; this module
//! defines the input side and small leaf-extraction helpers the
//! emitter calls into for typed-leaf decoding on a resolved offset.
//!
//! # Substrate navigation history (AY.W6.c → AY-II.W0.c)
//!
//! A substrate-level tape-walker helper landed at AY.W6.c to walk a
//! compound subtree from a substrate-level `(Tape, input, root)`
//! triple — intended to back the emitted `PathQuery` impls. AUDIT-B
//! §7 classified it DEAD at AY close: the emitted `__path_walk` in
//! the view layer continued to drive generic child-walk iteration
//! and no production consumer called it. AY-II.W0.c retires the
//! helper; path navigation routes exclusively through the
//! view-layer `TapeCursor` surface via the emitted `__path_walk` +
//! `PathQuery<T>` impls, with the structural-scan policy (W0.e)
//! gating the hot-path fast lane per grammar.

use core::fmt;

use tape::{Tape, TapeKind, TapeOffset};

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
/// Resolve a leaf [`TapeOffset`] to its source-text span. Returns
/// `None` when the span is empty. Convenience wrapper for substrate
/// callers that want `&'p str` without routing through the view layer.
///
/// JSON-string-style quote trimming is NOT applied here — this is a
/// raw span accessor. Callers that need a trimmed value call
/// [`leaf_str_trim_quotes`] instead.
#[inline]
pub fn leaf_str<'p>(tape: &'p Tape, input: &'p str, leaf: TapeOffset) -> Option<&'p str> {
    let (lo, hi) = tape.columns().span_at(leaf.0);
    if lo > hi {
        return None;
    }
    let bytes = input.as_bytes();
    if (hi as usize) > bytes.len() {
        return None;
    }
    // SAFETY: `input` is a `&str`; slicing by byte offsets that
    // originate from the tape's span column preserves UTF-8 validity
    // because the emitter only records span endpoints at character
    // boundaries (parser positions are byte offsets aligned by
    // preceding token scanners).
    Some(&input[lo as usize..hi as usize])
}

/// Resolve a leaf tape offset, trimming JSON-style `"..."` quotes
/// when present. Returns `None` when the span is empty or malformed.
#[inline]
pub fn leaf_str_trim_quotes<'p>(
    tape: &'p Tape,
    input: &'p str,
    leaf: TapeOffset,
) -> Option<&'p str> {
    let raw = leaf_str(tape, input, leaf)?;
    let bytes = raw.as_bytes();
    if bytes.len() >= 2 && bytes[0] == b'"' && bytes[bytes.len() - 1] == b'"' {
        Some(&raw[1..raw.len() - 1])
    } else {
        Some(raw)
    }
}

/// Look up a scalar leaf's `f64` payload via the tape's packed
/// numeric column, falling back to parsing the span text. Returns
/// `None` when the offset is not a numeric leaf.
#[inline]
pub fn leaf_f64(tape: &Tape, input: &str, leaf: TapeOffset) -> Option<f64> {
    // SAFETY: leaf offsets originate from `TapeCursor` walks on the
    // same tape (via the emitted `PathQuery<T>::query` impls) —
    // never the `NONE` sentinel.
    let rec = unsafe { tape.get_unchecked(leaf) };
    if rec.kind() == TapeKind::Regex {
        if let Some(v) = tape.payload_f64(rec) {
            return Some(v);
        }
    }
    leaf_str(tape, input, leaf).and_then(|s| s.parse::<f64>().ok())
}

/// Look up a boolean leaf's payload — the `payload_bool` accessor on
/// the tape, falling back to span-text parse for literal-bodied rules.
#[inline]
pub fn leaf_bool(tape: &Tape, input: &str, leaf: TapeOffset) -> Option<bool> {
    // SAFETY: see `leaf_f64`.
    let rec = unsafe { tape.get_unchecked(leaf) };
    if let Some(v) = tape.payload_bool(rec) {
        return Some(v);
    }
    match leaf_str(tape, input, leaf)? {
        "true" => Some(true),
        "false" => Some(false),
        _ => None,
    }
}
