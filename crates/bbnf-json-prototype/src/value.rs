//! Materialised JSON value tree — isomorphic to `sonic_rs::Value` in
//! shape but owned by the prototype crate (no `sonic-rs` runtime
//! dependency for the `ValueVisitor` materialisation path).
//!
//! The intent is sonic-parity; the enum shape matches sonic's
//! `value.rs` closely enough that a head-to-head bench lands the two
//! builders in the same cache-line budget. Strings borrow from the
//! input slice when no escape bytes are present — the zero-copy path
//! sonic's `payload_string_with_source` equivalent relies on.

/// A JSON number — f64-backed per the prototype's number kernel.
///
/// Mirrors `sonic_rs::Number`'s wire contract with a single f64
/// representative; integer / float distinction the sonic shape carries
/// is folded out here because every JSON literal resolves through
/// Eisel-Lemire + the Clinger fast path into an f64 anyway. The
/// visitor's [`crate::JsonVisitor::number_f64`] hook takes an f64
/// directly.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Number(pub f64);

/// Compact per-node JSON value shared between the arena-backed
/// document and its readers.
///
/// Sonic-rs packs every node into a 24-byte representation: a tag
/// byte, a `(offset, len)` pair for strings pointing into a shared
/// arena, and a child-count for compounds whose children live
/// contiguously in the document's node stream. The prototype mirrors
/// that shape — [`Value`] variants are cheap to produce (no per-node
/// allocation for scalars), strings reference a shared arena owned by
/// the [`Document`], and compounds carry a `(child_start, child_len)`
/// span into the same node stream.
///
/// The shape is what the sonic twin-pair bench is measuring against;
/// per-node `Vec<(Vec<u8>, Value)>` allocation — the naive tree
/// materialisation — is a ~30% tax per the AW-V.W2 first-cut
/// samply profile. Packed Value + shared arena reclaims that
/// margin.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    /// The JSON literal `null`.
    Null,
    /// The JSON literal `true` / `false`.
    Bool(bool),
    /// A JSON number — f64 representative.
    Number(Number),
    /// A JSON string — pointer/length span resolvable via
    /// [`Document::bytes`].
    String(StringSpan),
    /// A JSON array — child nodes live at `[start, start + len)` in
    /// the document's node stream.
    Array(NodeSpan),
    /// A JSON object — key nodes (all `String`) and value nodes
    /// alternate in the document stream starting at `start`;
    /// `len` is the entry count (so `2 * len` consecutive nodes).
    Object(NodeSpan),
}

/// Addressable location of a JSON string's bytes.
///
/// String bytes live in one of two places:
///
/// 1. **Input borrow** — bytes are a sub-slice of the original input
///    slice. Zero copy; the input must out-live the [`Document`].
///    Sonic-rs's `visit_borrowed_str` shape.
/// 2. **Arena copy** — bytes live in the [`Document::arena`]. Used
///    for escape-bearing strings where the decoded bytes don't match
///    the source span.
///
/// The two cases are distinguished by the high bit of `loc`:
///
/// - `loc & ARENA_TAG == 0`: `loc` is a raw pointer into the input
///   slice.
/// - `loc & ARENA_TAG != 0`: `loc & !ARENA_TAG` is a byte offset into
///   [`Document::arena`].
///
/// On aarch64 / x86_64 user-space, the high 16 bits of a valid
/// pointer are zero, so setting bit 63 never collides with an actual
/// pointer. Resolution via [`Document::bytes`] reads the tag and
/// dispatches.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StringSpan {
    /// Packed location — raw pointer (tag bit clear) or arena offset
    /// (tag bit set).
    pub loc: u64,
    /// Byte length (UTF-8 bytes; not code points).
    pub len: u32,
}

/// High-bit tag distinguishing arena-offset from raw-pointer
/// [`StringSpan::loc`] values.
pub const ARENA_TAG: u64 = 1u64 << 63;

impl StringSpan {
    /// Construct a borrowed span from a raw pointer into the input
    /// slice.
    #[inline(always)]
    pub fn borrowed(ptr: *const u8, len: usize) -> Self {
        debug_assert!((ptr as u64) & ARENA_TAG == 0, "user-space pointer must not set bit 63");
        Self {
            loc: ptr as u64,
            len: len as u32,
        }
    }

    /// Construct an arena-backed span from a byte offset into
    /// [`Document::arena`].
    #[inline(always)]
    pub fn arena(offset: u32, len: usize) -> Self {
        Self {
            loc: (offset as u64) | ARENA_TAG,
            len: len as u32,
        }
    }

    /// True iff this span points into the input (borrow path).
    #[inline(always)]
    pub fn is_borrowed(self) -> bool {
        (self.loc & ARENA_TAG) == 0
    }
}

/// Alias retained for compatibility with the B1 spec's naming.
pub type ArenaSpan = StringSpan;

/// `(start, subtree_len, entries)` pointer into the
/// [`Document::nodes`] vector.
///
/// The prototype follows sonic-rs's "packed subtree length" shape
/// (`value/node.rs:1436+` `DocumentVisitor` + `MetaNode` at
/// `value/node.rs:1471`). In this layout the compound's children are
/// emitted **inline** in the node stream in pre-order; the compound's
/// own `Value::Array` / `Value::Object` slot lives at `start - 1` and
/// its `subtree_len` field names the total number of nodes (including
/// nested descendants) that belong to this compound. This shape
/// avoids memcpy-ing per-compound children into their own sub-region
/// on close: the visitor writes once in emission order; the reader
/// uses `subtree_len` to skip siblings.
///
/// - Arrays: `entries` = direct element count; each element occupies
///   exactly one `Value` slot in the stream (which may itself carry a
///   `subtree_len` for nested compounds).
/// - Objects: `entries` = entry count. Each entry is two adjacent
///   slots: the key `Value::String` + the value `Value` (which may
///   recurse).
///
/// A reader iterating direct children of a compound advances one slot
/// at a time; scalars consume one slot, compounds consume
/// `1 + subtree_len` slots (skip forward). Total child slots after
/// the compound's own header = `subtree_len`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NodeSpan {
    /// First descendant node index (one past the compound's own
    /// slot in the flat stream). Children live at
    /// `nodes[start..start + subtree_len]`.
    pub start: u32,
    /// Total descendant node count — including nested compounds'
    /// interior nodes. Readers skipping this compound advance by
    /// `1 + subtree_len` slots.
    pub subtree_len: u32,
    /// Direct child count — arrays: element count; objects: pair
    /// count (each pair is 2 slots in the stream).
    pub entries: u32,
}

/// A materialised JSON document — owns the arena + node stream.
///
/// Sonic-rs's `Value` is a `Box<Shared>` holding the same data:
/// a bump arena for strings + a flat node stream. The prototype uses
/// owned `Vec<u8>` + `Vec<Value>` for the same shape, just without
/// the `Arc`-shared runtime model (the prototype is single-parse).
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Document {
    /// Root value — `None` only before parse completes.
    pub root: Option<Value>,
    /// Flat node stream — every sub-value (array children, object
    /// keys, object values) lives here in document order.
    pub nodes: Vec<Value>,
    /// Byte arena — every string's bytes live here contiguously.
    pub arena: Vec<u8>,
}

impl Document {
    /// Construct an empty document.
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    /// Construct a document with pre-sized storage. Mirrors sonic's
    /// document initialisation: nodes pre-sized to ~1 per 8 bytes of
    /// input (empirically tight on mixed workloads); arena pre-sized
    /// to `input_len / 2` (strings + copies).
    #[inline]
    pub fn with_input_len(input_len: usize) -> Self {
        Self {
            root: None,
            nodes: Vec::with_capacity(input_len / 8 + 1),
            arena: Vec::with_capacity(input_len / 2 + 2),
        }
    }

    /// Resolve a [`StringSpan`] to its byte slice.
    ///
    /// Borrowed spans (`loc` with bit 63 clear) resolve to their raw
    /// pointer; arena spans (`loc` with bit 63 set) resolve by
    /// offsetting into `self.arena`.
    ///
    /// # Safety
    ///
    /// For borrowed spans the caller must guarantee the input slice
    /// is still valid (this is a [`ValueVisitor::with_input`]
    /// contract — the input out-lives the [`Document`]).
    #[inline]
    pub fn bytes<'a>(&'a self, span: StringSpan) -> &'a [u8] {
        if span.is_borrowed() {
            // SAFETY: `loc` was stored via `StringSpan::borrowed`
            // from within the input slice; caller contract keeps it
            // live.
            unsafe { std::slice::from_raw_parts(span.loc as *const u8, span.len as usize) }
        } else {
            let offset = (span.loc & !ARENA_TAG) as usize;
            &self.arena[offset..offset + span.len as usize]
        }
    }

    /// Resolve a [`StringSpan`] to a UTF-8 `&str`.
    #[inline]
    pub fn str<'a>(&'a self, span: StringSpan) -> Option<&'a str> {
        std::str::from_utf8(self.bytes(span)).ok()
    }

    /// Yield each direct child of the compound named by `span`.
    ///
    /// Walks `nodes[span.start..span.start + span.subtree_len]` one
    /// compound at a time; arrays yield element `Value`s, objects
    /// yield pair slots (key then value then key then value…). The
    /// iterator elides nested descendants — it advances by `1 +
    /// subtree_len` when it sees a compound.
    ///
    /// Returns an empty iterator when the compound has no direct
    /// children.
    #[inline]
    pub fn children(&self, span: NodeSpan) -> ChildIter<'_> {
        ChildIter {
            doc: self,
            pos: span.start as usize,
            end: span.start as usize + span.subtree_len as usize,
        }
    }

    /// For an object [`NodeSpan`], yield each key-value pair as
    /// `(key_str_slice, value_node)`. Compound values that nest
    /// further are returned by-value; the caller can re-resolve them
    /// via this method.
    #[inline]
    pub fn object_entries(
        &self,
        span: NodeSpan,
    ) -> impl Iterator<Item = (&[u8], Value)> + '_ {
        let mut iter = self.children(span);
        core::iter::from_fn(move || {
            let k = iter.next()?;
            let v = iter.next().unwrap_or(Value::Null);
            let key_bytes = match k {
                Value::String(s) => self.bytes(s),
                _ => &[] as &[u8],
            };
            Some((key_bytes, v))
        })
    }
}

/// Cursor over a compound's direct children in a packed
/// [`Document::nodes`] stream. Advances by `1 + subtree_len` when the
/// next slot is a compound; by `1` otherwise.
pub struct ChildIter<'a> {
    doc: &'a Document,
    pos: usize,
    end: usize,
}

impl<'a> Iterator for ChildIter<'a> {
    type Item = Value;

    #[inline]
    fn next(&mut self) -> Option<Value> {
        if self.pos >= self.end {
            return None;
        }
        let v = self.doc.nodes[self.pos];
        let step = match v {
            Value::Array(sp) | Value::Object(sp) => 1 + sp.subtree_len as usize,
            _ => 1,
        };
        self.pos += step;
        Some(v)
    }
}

impl Value {
    /// True iff `self` is [`Value::Null`].
    #[inline]
    pub fn is_null(self) -> bool {
        matches!(self, Value::Null)
    }

    /// Inner bool if `self` is [`Value::Bool`].
    #[inline]
    pub fn as_bool(self) -> Option<bool> {
        match self {
            Value::Bool(b) => Some(b),
            _ => None,
        }
    }

    /// Inner f64 if `self` is [`Value::Number`].
    #[inline]
    pub fn as_f64(self) -> Option<f64> {
        match self {
            Value::Number(Number(f)) => Some(f),
            _ => None,
        }
    }

    /// Inner string span if `self` is [`Value::String`].
    #[inline]
    pub fn as_string_span(self) -> Option<StringSpan> {
        match self {
            Value::String(s) => Some(s),
            _ => None,
        }
    }

    /// Inner array node span if `self` is [`Value::Array`].
    #[inline]
    pub fn as_array_span(self) -> Option<NodeSpan> {
        match self {
            Value::Array(s) => Some(s),
            _ => None,
        }
    }

    /// Inner object node span if `self` is [`Value::Object`].
    #[inline]
    pub fn as_object_span(self) -> Option<NodeSpan> {
        match self {
            Value::Object(s) => Some(s),
            _ => None,
        }
    }
}
