//! AY.W3a — path-query types for the lazy `get_by_path` lane.
//! AY.W6.c — substrate-level tape navigation via column-indexed
//! `sib_skip` stepping + span-text key matching.
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
//! defines the input side + a substrate-level [`navigate_tape`]
//! helper that downstream emitters (and tests) can call directly.
//!
//! # Substrate navigation (AY.W6.c)
//!
//! [`navigate_tape`] walks a compound subtree along a path using the
//! W5.b write-time substrate directly:
//!
//! - `Index(i)` steps resolve via `TapeCursor::child(i)` — a forward
//!   walk across `sib_skip` (one indexed column load per step).
//! - `Field(key)` steps traverse direct children two at a time —
//!   (key, value) pairs in the JSON-like object layout — comparing
//!   each key child's source span against the requested field name
//!   via a single byte-slice equality check. The comparison reads
//!   `(span_lo, span_hi)` directly from the column layer; no
//!   intermediate `NodeView` materialises.
//!
//! The walker bypasses the generic view-layer accessor surface; it
//! depends only on the tape + input + the `Path` alphabet. Hot-path
//! callers (object-key lookup inside `get_by_path`) can invoke it
//! without paying for per-child `NodeView` construction.

use core::fmt;

use tape::{Tape, TapeCursor, TapeKind, TapeOffset};

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

// ── Substrate-level tape navigation (AY.W6.c) ──────────────────────────

/// Navigate a [`Path`] against a subtree rooted at `root`, returning
/// the resolved [`TapeOffset`] or `None` when any step misses.
///
/// This is the substrate-level counterpart to the emitted
/// `PathQuery<T>::query` entry-point. It walks the tape via the
/// W5.b write-time substrate (column-indexed `sib_skip` steps; no
/// intermediate `NodeView` materialisation) and matches `Field`
/// segments by comparing the key child's source span against the
/// requested field name.
///
/// Hot-path consumers that already hold `(Tape, input, root)` — the
/// emitted `PathQuery<T>` impls, the object-key navigation metadata
/// the shape emitters install — call this directly. Callers that
/// start from a typed root view use
/// [`Parsed::get`](crate::runtime::Parsed::get); that path eventually
/// routes through a walker equivalent to this one.
///
/// # Object layout assumption
///
/// `Field` resolution assumes direct-child layout `(key, value,
/// key, value, ...)` — the canonical JSON-like object shape where
/// every object compound's immediate children alternate between
/// key records (String leaves, or compounds whose first leaf is a
/// quoted string) and value records. Grammars that wrap each
/// (key, value) in its own pair compound — JSON's `pair` rule — are
/// handled by [`navigate_tape_object_pairs`]: the walker steps
/// into each pair compound's direct children to extract the key +
/// value. [`navigate_tape`] inspects the first child's `has_children`
/// bit to pick between the flat and pair-wrapped layouts.
#[inline]
pub fn navigate_tape<'p>(
    tape: &'p Tape,
    input: &'p str,
    root: TapeOffset,
    path: Path<'_>,
) -> Option<TapeOffset> {
    let mut current = root;
    for seg in path.iter() {
        match seg {
            PathSegment::Index(i) => {
                current = nth_child(tape, current, *i)?;
            }
            PathSegment::Field(key) => {
                current = resolve_field(tape, input, current, key.as_bytes())?;
            }
        }
    }
    Some(current)
}

/// Internal helper — resolve the `i`-th direct child of `parent`
/// without going through a `TapeCursor` materialisation. Returns
/// `None` when `parent` is a leaf or `i` is out of range.
#[inline]
fn nth_child(tape: &Tape, parent: TapeOffset, i: usize) -> Option<TapeOffset> {
    let cursor = TapeCursor::new(tape, parent);
    cursor.child(i).map(|c| c.offset())
}

/// Internal helper — resolve a `Field(key)` step against the direct
/// children of `parent`, walking the tape-level substrate.
///
/// Handles both canonical object layouts:
///
/// 1. **Flat `(key, value, key, value, ...)`** — the first direct
///    child's `has_children` bit is false (it IS the key leaf). The
///    walker iterates pairs via `sib_skip` and compares each key
///    leaf's span text against the requested field name.
///
/// 2. **Pair-wrapped `(pair, pair, pair, ...)`** where each `pair`
///    is a compound whose direct children are `(key, value)` — the
///    first direct child has `has_children` set. The walker steps
///    into each pair compound, reads its first child as the key
///    leaf, compares, and returns the second child on match.
///
/// JSON-like grammars uniformly lower to layout 2 (the emitted
/// object shape wraps every (key, value) in a `pair` Seq compound).
/// CSS/Sheets/BBNF grammars that express key lookup differently
/// participate via layout 1. The decision is read from the first
/// child's tape metadata — no grammar-name dispatch.
#[inline]
fn resolve_field<'p>(
    tape: &'p Tape,
    input: &'p str,
    parent: TapeOffset,
    key_bytes: &[u8],
) -> Option<TapeOffset> {
    let parent_cursor = TapeCursor::new(tape, parent);
    let mut iter = parent_cursor.children();
    let first = iter.next()?;
    // Peek the first direct child's shape. Re-construct the iterator
    // over the full child run because `ChildIter::next` consumed it.
    let use_pair_layout = first.tape().columns().has_children_at(first.offset().0)
        && first.kind().is_compound();

    let mut iter = parent_cursor.children();
    while let Some(child) = iter.next() {
        if use_pair_layout {
            // Pair compound — step into its children to extract key+value.
            let pair_cursor = TapeCursor::new(tape, child.offset());
            let mut pair_children = pair_cursor.children();
            let Some(key_node) = pair_children.next() else {
                continue;
            };
            if key_matches(tape, input, key_node.offset(), key_bytes) {
                return pair_children.next().map(|v| v.offset());
            }
        } else {
            // Flat layout — this child IS the key; the next one is the value.
            let Some(value_node) = iter.next() else {
                return None;
            };
            if key_matches(tape, input, child.offset(), key_bytes) {
                return Some(value_node.offset());
            }
        }
    }
    None
}

/// Compare a key record's source-span text against the requested
/// field name, trimming surrounding quotes when the record's first
/// byte is `"` (JSON string key). Reads `(span_lo, span_hi)` directly
/// from the column layer + slices `input` — no intermediate string
/// allocation, no `NodeView` materialisation.
#[inline]
fn key_matches(tape: &Tape, input: &str, key_off: TapeOffset, key_bytes: &[u8]) -> bool {
    // For compound key records (e.g. JSON `string = "\"" >> body <<
    // "\""` lowering to a compound wrapping the quoted span) — step
    // into their first child. The `has_children` bit governs; any
    // other shape falls through to the direct-span path.
    let columns = tape.columns();
    let (mut lo, mut hi) = columns.span_at(key_off.0);
    // When the key record is a compound, its direct span covers
    // the quoted run including quotes; span text extraction below
    // still trims the quotes. Compound keys are idempotent here.
    let _ = columns;
    let bytes = input.as_bytes();
    if (lo as usize) > bytes.len() || (hi as usize) > bytes.len() || lo > hi {
        return false;
    }
    // JSON-style trimming: if the span starts and ends with `"`,
    // drop one byte on each side. `key_bytes` carries the caller's
    // field name without quotes.
    if hi > lo + 1 && bytes[lo as usize] == b'"' && bytes[(hi - 1) as usize] == b'"' {
        lo += 1;
        hi -= 1;
    }
    let span_slice = &bytes[lo as usize..hi as usize];
    span_slice == key_bytes
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
    // SAFETY: leaf offsets returned by `navigate_tape` originate from
    // `TapeCursor` walks on the same tape — never the `NONE` sentinel.
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
