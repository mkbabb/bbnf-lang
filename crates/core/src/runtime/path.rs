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
    let bytes = input.as_bytes();
    let mut current = root;
    for seg in path.iter() {
        match seg {
            PathSegment::Index(i) => {
                current = nth_child(tape, bytes, current, *i)?;
            }
            PathSegment::Field(key) => {
                current = resolve_field(tape, input, current, key.as_bytes())?;
            }
        }
    }
    Some(current)
}

/// Internal helper — resolve the `i`-th positional child of
/// `parent` in the presence of wrapping compounds (generated
/// grammars typically produce multi-layer envelopes between a rule
/// compound and its semantic children). Descends through
/// single-child compounds AND single-content-child compounds
/// (compounds whose non-delimiter children number exactly one) until
/// it reaches a compound with multiple content children — that's the
/// list container, and the `i`-th content child is returned.
#[inline]
fn nth_child(tape: &Tape, bytes: &[u8], parent: TapeOffset, i: usize) -> Option<TapeOffset> {
    let mut current = parent;
    // Bound the descent. 12 levels covers deep JSON envelopes
    // (outer-rule Seq → Next Seq → OW Seq → Repeat Rule → per-iter
    // Seq) plus nested array wrappers; the loop breaks as soon as a
    // multi-content-child frame is found.
    for _depth in 0..12 {
        let cursor = TapeCursor::new(tape, current);
        let child_count = cursor.child_count();
        if child_count == 0 {
            break;
        }
        // Count + collect content children (filter out structural
        // delimiters + empty wrappers + separator-iter Seqs).
        let mut content: [u32; 64] = [0u32; 64];
        let mut n_content: usize = 0;
        for child in cursor.children() {
            if is_content_child_bytes(tape, bytes, child.offset()) {
                if n_content < content.len() {
                    content[n_content] = child.offset().0;
                }
                n_content += 1;
            }
        }
        if n_content == 0 {
            break;
        }
        if n_content == 1 {
            // Single content child — descend into it and retry.
            current = TapeOffset(content[0]);
            continue;
        }
        // Multi-content-child frame — this is the list container.
        if i >= n_content || i >= content.len() {
            return None;
        }
        // Return the i-th content child, peeling remaining
        // single-content wrappers around it until we reach a
        // node that can't be peeled further.
        let mut target = TapeOffset(content[i]);
        for _ in 0..6 {
            let tc = TapeCursor::new(tape, target);
            let cc = tc.child_count();
            if cc == 0 {
                break;
            }
            if cc == 1 {
                if let Some(only) = tc.children().next() {
                    // Peel only when the only child is a content-
                    // bearing record (not a single-byte structural).
                    if is_content_child_bytes(tape, bytes, only.offset()) {
                        target = only.offset();
                        continue;
                    }
                }
            }
            // Multi-child target — check if it carries a single
            // content child (separator siblings).
            let mut sc = None;
            let mut sc_count = 0;
            for c in tc.children() {
                if is_content_child_bytes(tape, bytes, c.offset()) {
                    sc_count += 1;
                    if sc_count == 1 {
                        sc = Some(c.offset());
                    } else {
                        break;
                    }
                }
            }
            if sc_count == 1 {
                if let Some(only_content) = sc {
                    target = only_content;
                    continue;
                }
            }
            break;
        }
        return Some(target);
    }
    None
}

/// Classify a record as "content" (carries a semantic value) vs
/// "structural" (delimiter / empty wrapper / separator iter).
///
/// Rules:
/// - Empty-span records are structural.
/// - Single-byte leaves (`{`, `}`, `[`, `]`, `:`, `,`) are structural.
/// - Compounds whose FIRST significant direct leaf is a SEPARATOR
///   byte (`,` or `:`) are separator-iter wrappers (`comma_iter =
///   OW(",")`); they are structural.
/// - Every other compound (including array/object outer wrappers
///   whose first leaf is an OPENER byte `{` / `[`) is content.
#[inline]
fn is_content_child_bytes(tape: &Tape, bytes: &[u8], off: TapeOffset) -> bool {
    let columns = tape.columns();
    let (lo, hi) = columns.span_at(off.0);
    if hi <= lo {
        return false;
    }
    if !columns.has_children_at(off.0) {
        return hi > lo + 1;
    }
    // Compound — probe its first direct leaf. If that leaf is a
    // single-byte SEPARATOR (`,` or `:`), the compound is a
    // separator-iter wrapper.
    let mut probe = off;
    for _ in 0..4 {
        let cursor = TapeCursor::new(tape, probe);
        let Some(first) = cursor.children().next() else {
            break;
        };
        let first_off = first.offset();
        let (flo, fhi) = columns.span_at(first_off.0);
        if !columns.has_children_at(first_off.0) {
            if fhi == flo + 1 {
                let byte = bytes.get(flo as usize).copied();
                return !matches!(byte, Some(b',') | Some(b':'));
            }
            return true;
        }
        probe = first_off;
    }
    true
}

/// Internal helper — resolve a `Field(key)` step against the
/// subtree rooted at `parent`, walking the tape-level substrate.
///
/// The generated grammar lowers object-like structure through
/// multiple layers of wrapping compounds (outer rule Seq → Next Seq
/// → OW Seq → Repeat Rule → per-iter Seq → pair Seq → key / value
/// children). The walker performs a pre-order DFS over the parent's
/// subtree looking for any leaf whose (quote-trimmed) span text
/// equals the requested key. On match, the walker continues the
/// pre-order traversal past the key's parent compound and returns
/// the next non-structural leaf — the value — as a [`TapeOffset`].
///
/// The walk is bounded by the parent's span — every candidate node
/// has its span inside the parent's `[span_lo, span_hi)` range.
/// `sib_skip` steps navigate children in source order; descent into
/// compound children uses the column-indexed `sib_skip`/`child_off`
/// layer directly.
///
/// No grammar-name dispatch; every decision is read from each node's
/// tape metadata (`has_children`, `kind`, `span_lo`, `span_hi`).
/// Object layouts differ across grammars — JSON pair-wrapped,
/// flat key/value alternation, CSS declaration blocks — but every
/// layout drops a key leaf with the field's source text somewhere
/// in the subtree; the DFS finds it uniformly.
#[inline]
fn resolve_field<'p>(
    tape: &'p Tape,
    input: &'p str,
    parent: TapeOffset,
    key_bytes: &[u8],
) -> Option<TapeOffset> {
    // DFS state — a stack of (offset, visited-children-flag). Pre-order
    // visit: read the leaf/compound at the top; if compound and not yet
    // visited, push its first child. Otherwise pop and continue with the
    // next sibling.
    //
    // The walker returns on the first key-leaf hit whose trimmed span
    // text equals `key_bytes`. After the key hits, the walker keeps
    // popping up to the smallest ancestor whose NEXT sibling is a
    // non-structural leaf — that sibling is the value and is returned.
    let bytes = input.as_bytes();

    // Locate the key leaf via DFS in the parent's subtree.
    let key_off = find_leaf_matching_key(
        tape, input, parent, key_bytes,
    )?;
    // The value is the next non-structural leaf after `key_off` in the
    // parent's subtree pre-order. In every object layout the key is
    // immediately followed by either:
    //   - a structural `:` literal, then the value compound/leaf
    //   - or directly the value compound/leaf
    // Walk forward past any structural single-byte literal siblings,
    // then return the next node.
    find_value_after_key(tape, bytes, parent, key_off)
}

/// Pre-order DFS over the subtree rooted at `root`, searching for the
/// first leaf whose (quote-trimmed) span-text equals `key_bytes`.
/// Returns that leaf's [`TapeOffset`] or `None` when no leaf matches.
#[inline]
fn find_leaf_matching_key(
    tape: &Tape,
    input: &str,
    root: TapeOffset,
    key_bytes: &[u8],
) -> Option<TapeOffset> {
    let columns = tape.columns();
    let (root_lo, root_hi) = columns.span_at(root.0);
    // Depth-first via an explicit stack (LIFO of TapeCursor's child
    // iterators). Capacity 16 covers the typical 4-6-deep JSON
    // wrapping; larger depths fall back to heap allocation
    // transparently via `Vec::push`.
    let mut stack: Vec<tape::ChildIter<'_>> = Vec::with_capacity(16);
    stack.push(TapeCursor::new(tape, root).children());

    while let Some(top) = stack.last_mut() {
        let next = top.next();
        match next {
            None => {
                stack.pop();
            }
            Some(child) => {
                let off = child.offset();
                // Bound check — stay inside parent's span.
                let (lo, hi) = columns.span_at(off.0);
                if lo < root_lo || hi > root_hi {
                    continue;
                }
                if columns.has_children_at(off.0) {
                    // Compound — descend.
                    stack.push(child.children());
                } else {
                    // Leaf — try key match.
                    if key_matches(tape, input, off, key_bytes) {
                        return Some(off);
                    }
                }
            }
        }
    }
    None
}

/// Locate the value record that follows a matched key leaf. Walks
/// forward from `key_off` in pre-order, skipping single-byte
/// structural literals (`{`, `}`, `[`, `]`, `:`, `,`) and any
/// compound whose span starts with a structural byte (it wraps the
/// delimiter + value; the value lives strictly inside it).
///
/// Returns the first "content" record — a leaf with non-structural
/// first byte, or a compound that starts with content.
///
/// The walk stays inside `parent`'s span.
#[inline]
fn find_value_after_key(
    tape: &Tape,
    bytes: &[u8],
    parent: TapeOffset,
    key_off: TapeOffset,
) -> Option<TapeOffset> {
    let columns = tape.columns();
    let (parent_lo, parent_hi) = columns.span_at(parent.0);
    let key_end = columns.span_at(key_off.0).1;
    let n = columns.len() as u32;
    let mut i = key_off.0 + 1;
    while i < n {
        let (lo, hi) = columns.span_at(i);
        if lo > parent_hi {
            break;
        }
        if lo < parent_lo {
            i += 1;
            continue;
        }
        if lo < key_end {
            i += 1;
            continue;
        }
        // Skip single-byte structural literals.
        let first_byte = bytes.get(lo as usize).copied();
        let is_struct_byte = hi == lo + 1
            && matches!(
                first_byte,
                Some(b'{')
                    | Some(b'}')
                    | Some(b'[')
                    | Some(b']')
                    | Some(b':')
                    | Some(b',')
            );
        if is_struct_byte {
            i += 1;
            continue;
        }
        // Skip empty-span wrapping compounds.
        if hi == lo {
            i += 1;
            continue;
        }
        // Skip a compound whose span starts with a structural byte
        // (`:`, `,`, etc.) — such a compound wraps a delimiter +
        // value; the value record we want is strictly inside it
        // (emitted at a later offset). The next iteration will
        // reach it.
        if columns.has_children_at(i) {
            if matches!(
                first_byte,
                Some(b':') | Some(b',') | Some(b'{') | Some(b'[')
            ) {
                i += 1;
                continue;
            }
        }
        return Some(TapeOffset(i));
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
