//! Visitor trait + two shipped implementations.
//!
//! # Visitors
//!
//! - [`ValueVisitor`] — sonic-parity materialisation into
//!   [`crate::Value`]. The speed-ceiling validator: head-to-head
//!   against `sonic_rs::from_str::<Value>` in the twin-pair bench.
//! - [`TapeVisitor`] — AW-IV-substrate materialisation into
//!   `bbnf_tape::Columns` via a `TapeBuilder`. Validates the same
//!   parse body over the existing substrate; proves the visitor
//!   abstraction covers both.
//!
//! Both visitors implement the same [`JsonVisitor`] trait; the parse
//! body in `lib.rs` is monomorphised at each call site.

use crate::value::{Document, NodeSpan, Number, StringSpan, Value};
use bbnf_tape::{PayloadData, Tape, TapeBuilder, TapeKind};

/// Visitor trait consumed by [`crate::parse_json`].
///
/// Monomorphised at the call site; LLVM inlines each method body into
/// the parse function that invokes it. No `dyn Visitor` dispatch.
///
/// The trait mirrors sonic-rs's `JsonVisitor` shape closely enough to
/// act as a drop-in in the twin-pair bench — the name overlap is
/// intentional. Method signatures take raw byte slices where
/// sonic-rs would take `&str`; the prototype avoids the UTF-8
/// validation round-trip by contract (JSON string bodies are
/// UTF-8-clean by construction, per the `bbnf-tape` decoder kernel's
/// invariant).
pub trait JsonVisitor {
    /// Visitor-side error surface. The parser translates this to
    /// [`crate::ParseError::VisitorReject`] at the call site; the
    /// visitor carries the contextual detail.
    type Error;

    /// Called before an object's first key.
    fn begin_object(&mut self) -> Result<(), Self::Error>;
    /// Called for each object key. Bytes are a UTF-8 substring of the
    /// input slice when no escapes are present; owned decoded bytes
    /// otherwise.
    fn key(&mut self, bytes: &[u8]) -> Result<(), Self::Error>;
    /// Called after an object's final value (or immediately after
    /// [`Self::begin_object`] for empty objects).
    fn end_object(&mut self) -> Result<(), Self::Error>;
    /// Called before an array's first element.
    fn begin_array(&mut self) -> Result<(), Self::Error>;
    /// Called after an array's final element.
    fn end_array(&mut self) -> Result<(), Self::Error>;
    /// Called on a string value. Bytes are a UTF-8 substring of the
    /// input slice when no escapes are present; owned decoded bytes
    /// otherwise.
    fn string(&mut self, bytes: &[u8]) -> Result<(), Self::Error>;
    /// Called on a number value — f64 representative from
    /// Eisel-Lemire.
    fn number_f64(&mut self, value: f64) -> Result<(), Self::Error>;
    /// Called on a `true` / `false` literal.
    fn bool(&mut self, value: bool) -> Result<(), Self::Error>;
    /// Called on a `null` literal.
    fn null(&mut self) -> Result<(), Self::Error>;
}

// ── ValueVisitor ────────────────────────────────────────────────────

/// Materialises JSON events into a flat [`Document`] — the sonic-parity
/// validator.
///
/// # Packed node stream (sonic parity)
///
/// Nodes are written into `doc.nodes` in emission (pre-order) as
/// the parser dispatches. Each compound's slot is pre-reserved at
/// `begin_*` time; the children stream in right after; on `end_*`
/// the slot is back-patched with `NodeSpan { start, subtree_len,
/// entries }`. Nested compounds work the same way — a nested object's
/// `subtree_len` captures its own descendants, and the outer
/// compound's `subtree_len` covers the nested compound's slot +
/// descendants.
///
/// This is sonic-rs's `DocumentVisitor` shape (`value/node.rs:1436+`).
/// Key property: **zero memcpy between scratch and the output
/// stream** — the 14.2% `_platform_memmove` tax the scratch-based
/// shape paid goes to zero. Per the W2 second-cut samply, only one
/// data flow touches each node's 24 bytes: the original write.
pub struct ValueVisitor {
    /// Output document — carries the arena + node stream + root.
    doc: Document,
    /// Input slice range — `(base_ptr as usize, base_ptr + len as usize)`.
    /// Strings whose bytes lie within this range are *borrowed* from
    /// the input (no arena copy); strings outside this range (decoded
    /// escape buffers) are copied into `doc.arena`. Sonic-rs's
    /// `visit_borrowed_str` vs. `visit_str` split — the prototype
    /// computes the split at visit time from pointer identity.
    input_lo: usize,
    input_hi: usize,
    /// Compound-frame stack. Each frame records the pre-reserved slot
    /// index the compound's own Value stamps into at close time +
    /// the entry count so `end_*` can compute `subtree_len` from
    /// `doc.nodes.len() - slot_idx - 1`.
    compounds: Vec<CompoundMark>,
}

#[derive(Copy, Clone)]
struct CompoundMark {
    kind: CompoundKind,
    /// Pre-reserved slot index in `doc.nodes` where the compound's
    /// `Value::Array` / `Value::Object` stamps on close.
    slot_idx: u32,
}

#[derive(Copy, Clone)]
enum CompoundKind {
    Array,
    /// Object — key/value slots alternate from `slot_idx + 1`.
    Object,
}

/// Internal no-op error kind for [`ValueVisitor`] — the visitor
/// never rejects events.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ValueVisitorError;

impl Default for ValueVisitor {
    fn default() -> Self {
        Self::new()
    }
}

impl ValueVisitor {
    /// Construct a fresh visitor with empty storage. The borrow
    /// window defaults to empty — every string will be treated as
    /// out-of-input and copied into the arena. Prefer
    /// [`ValueVisitor::with_input`] for the zero-copy path.
    #[inline]
    pub fn new() -> Self {
        Self {
            doc: Document::new(),
            input_lo: 0,
            input_hi: 0,
            compounds: Vec::new(),
        }
    }

    /// Construct a visitor pre-sized for an input of length
    /// `input_len`. Borrow window is empty — every string is
    /// copied. See [`ValueVisitor::with_input`] for the zero-copy
    /// path when the input slice is available.
    #[inline]
    pub fn with_input_len(input_len: usize) -> Self {
        Self {
            doc: Document::with_input_len(input_len),
            input_lo: 0,
            input_hi: 0,
            compounds: Vec::new(),
        }
    }

    /// Construct a visitor bound to the input slice `input`.
    ///
    /// String calls whose byte slice lies inside `input`'s memory
    /// range take the borrow path — the resulting [`Value::String`]
    /// points at the original bytes with no copy. Strings outside
    /// this range (escape-decoded buffers) copy into the arena. This
    /// is sonic-rs's `visit_borrowed_str` / `visit_str` split done at
    /// visit time via pointer-range identity.
    ///
    /// The returned [`Document`] borrows from `input` for any
    /// string value whose span lies inside `input`; the caller must
    /// keep `input` alive for the document's lifetime. For a
    /// definitive owned document, use [`ValueVisitor::with_input_len`]
    /// which forces every string to arena-copy.
    #[inline]
    pub fn with_input(input: &[u8]) -> Self {
        let input_lo = input.as_ptr() as usize;
        let input_hi = input_lo + input.len();
        Self {
            doc: Document::with_input_len(input.len()),
            input_lo,
            input_hi,
            compounds: Vec::new(),
        }
    }

    /// Consume the visitor and return the populated [`Document`].
    ///
    /// Compound roots are written into `doc.root` at close time
    /// (see `end_array` / `end_object`); scalar roots are written
    /// by `emit`. `finish` hands back the fully-populated document.
    #[inline]
    pub fn finish(self) -> Document {
        self.doc
    }

    /// Intern a string — borrow from input when `bytes`'s pointer
    /// lies inside `(self.input_lo, self.input_hi)`; otherwise copy
    /// into the arena.
    #[inline(always)]
    fn intern_str(&mut self, bytes: &[u8]) -> StringSpan {
        let ptr = bytes.as_ptr() as usize;
        if ptr >= self.input_lo && ptr + bytes.len() <= self.input_hi {
            // Borrow path — zero copy.
            StringSpan::borrowed(bytes.as_ptr(), bytes.len())
        } else {
            // Arena copy path — escape-decoded bytes. Store offset
            // (not pointer) so arena reallocation doesn't invalidate
            // the span.
            let offset = self.doc.arena.len() as u32;
            self.doc.arena.extend_from_slice(bytes);
            StringSpan::arena(offset, bytes.len())
        }
    }

    /// Emit a scalar node. Compound roots stamp directly into
    /// `doc.root` via the `begin_*/end_*` slot-reservation flow; the
    /// top-level scalar case writes to `doc.root` here as well.
    #[inline(always)]
    fn emit(&mut self, v: Value) {
        if self.compounds.is_empty() {
            self.doc.root = Some(v);
        } else {
            self.doc.nodes.push(v);
        }
    }
}

impl JsonVisitor for ValueVisitor {
    type Error = ValueVisitorError;

    #[inline(always)]
    fn begin_object(&mut self) -> Result<(), Self::Error> {
        let slot_idx = self.doc.nodes.len() as u32;
        self.doc.nodes.push(Value::Null);
        self.compounds.push(CompoundMark {
            kind: CompoundKind::Object,
            slot_idx,
        });
        Ok(())
    }

    #[inline(always)]
    fn key(&mut self, bytes: &[u8]) -> Result<(), Self::Error> {
        let span = self.intern_str(bytes);
        self.doc.nodes.push(Value::String(span));
        Ok(())
    }

    #[inline(always)]
    fn end_object(&mut self) -> Result<(), Self::Error> {
        let frame = self.compounds.pop().ok_or(ValueVisitorError)?;
        debug_assert!(matches!(frame.kind, CompoundKind::Object));
        let slot_idx = frame.slot_idx as usize;
        let child_start = slot_idx + 1;
        let subtree_len = self.doc.nodes.len() - child_start;
        // Entries is derivable from subtree_len at read time (via
        // ChildIter); tracking it inline costs a write per child.
        // Set to 0 here — `Document::object_entries` walks the
        // ChildIter and divides by 2 on the fly.
        let value = Value::Object(NodeSpan {
            start: child_start as u32,
            subtree_len: subtree_len as u32,
            entries: 0,
        });
        self.doc.nodes[slot_idx] = value;
        if self.compounds.is_empty() {
            self.doc.root = Some(value);
        }
        Ok(())
    }

    #[inline(always)]
    fn begin_array(&mut self) -> Result<(), Self::Error> {
        let slot_idx = self.doc.nodes.len() as u32;
        self.doc.nodes.push(Value::Null);
        self.compounds.push(CompoundMark {
            kind: CompoundKind::Array,
            slot_idx,
        });
        Ok(())
    }

    #[inline(always)]
    fn end_array(&mut self) -> Result<(), Self::Error> {
        let frame = self.compounds.pop().ok_or(ValueVisitorError)?;
        debug_assert!(matches!(frame.kind, CompoundKind::Array));
        let slot_idx = frame.slot_idx as usize;
        let child_start = slot_idx + 1;
        let subtree_len = self.doc.nodes.len() - child_start;
        let value = Value::Array(NodeSpan {
            start: child_start as u32,
            subtree_len: subtree_len as u32,
            entries: 0,
        });
        self.doc.nodes[slot_idx] = value;
        if self.compounds.is_empty() {
            self.doc.root = Some(value);
        }
        Ok(())
    }

    #[inline(always)]
    fn string(&mut self, bytes: &[u8]) -> Result<(), Self::Error> {
        let span = self.intern_str(bytes);
        self.emit(Value::String(span));
        Ok(())
    }

    #[inline(always)]
    fn number_f64(&mut self, value: f64) -> Result<(), Self::Error> {
        self.emit(Value::Number(Number(value)));
        Ok(())
    }

    #[inline(always)]
    fn bool(&mut self, value: bool) -> Result<(), Self::Error> {
        self.emit(Value::Bool(value));
        Ok(())
    }

    #[inline(always)]
    fn null(&mut self) -> Result<(), Self::Error> {
        self.emit(Value::Null);
        Ok(())
    }
}


// ── TapeVisitor ─────────────────────────────────────────────────────

/// Materialises JSON events into a `bbnf_tape::Columns` substrate via
/// a [`TapeBuilder`].
///
/// Per AW-V.W2 B1 §6, the tape is pre-allocated at
/// `input.len() / 2 + 2` (the AR-audit heuristic; every parse producing
/// ≤ 1 record per 2 bytes fits without a `grow_all`).
///
/// Layout per the B1 appendix:
///
/// - Strings: borrowed `push_leaf_borrowed_string` when no escapes are
///   observed in the source span (reader slices
///   `input[span_lo + 1 .. span_hi - 1]` at traversal time); escape-
///   bearing strings land via `push_leaf_with_arena_frame`, the
///   decoder's `(len: u32 LE, bytes)` frame in `pay_agg`.
/// - Numbers: `TapeKind::Regex` leaf carrying `PayloadData::WideScalar`
///   (f64 bits).
/// - Bool / null: `TapeKind::Literal` leaf with
///   `PayloadData::InlineScalar`; bool uses `value as u32`, null uses
///   the `0u8` sentinel.
/// - Compounds: `push_compound` with `TapeKind::Rule` bracketing the
///   children run; span is the (begin, end) of the source bytes
///   consumed. For honest AW-IV parity we use `Rule` as the
///   compound kind, matching the generated JSON parser's projection.
pub struct TapeVisitor<'input> {
    builder: TapeBuilder,
    input: &'input [u8],
    /// Stack of (kind, span_lo, child_off) per in-progress compound.
    /// The `child_off` is the tape position marked at the compound's
    /// opening.
    compounds: Vec<CompoundFrame>,
}

struct CompoundFrame {
    kind: TapeKind,
    span_lo: u32,
    child_off: bbnf_tape::TapeOffset,
}

/// [`TapeVisitor`] error surface — visitor never rejects events.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TapeVisitorError;

impl<'input> TapeVisitor<'input> {
    /// Construct a visitor over `input`, pre-allocating the tape per
    /// the AR-audit heuristic.
    #[inline]
    pub fn new(input: &'input [u8]) -> Self {
        let capacity = input.len() / 2 + 2;
        Self {
            builder: TapeBuilder::with_capacity(capacity),
            input,
            compounds: Vec::new(),
        }
    }

    /// Current cursor position into the input.
    ///
    /// Set by the parser immediately before each visitor call that
    /// needs to stamp a span (via [`Self::stamp_span`]). The prototype
    /// stamps spans by re-deriving from the visitor call-side info
    /// (`&[u8]` bytes for strings, `f64` for numbers). Compound spans
    /// are threaded through the compound frame stack.
    pub fn finish(self) -> Result<Tape, bbnf_tape::TapeBuildError> {
        self.builder.finish()
    }

    /// Byte length of the source input the visitor is parsing.
    #[inline]
    pub fn input_len(&self) -> usize {
        self.input.len()
    }

    /// Current cursor position tracked on the parser side. The
    /// visitor trait doesn't carry offsets; [`TapeVisitor`] accepts
    /// the span info opportunistically via
    /// [`TapeVisitor::with_span`] when the parser wants authoritative
    /// spans. For the prototype's shape validation this is unused in
    /// the default path — spans recorded in the tape are zero-width
    /// for leaves and (0, input.len()) for the root compound.
    #[inline]
    pub fn builder_mut(&mut self) -> &mut TapeBuilder {
        &mut self.builder
    }
}

impl<'input> JsonVisitor for TapeVisitor<'input> {
    type Error = TapeVisitorError;

    #[inline(always)]
    fn begin_object(&mut self) -> Result<(), Self::Error> {
        let child_off = self.builder.mark_children();
        self.compounds.push(CompoundFrame {
            kind: TapeKind::Rule,
            span_lo: 0,
            child_off,
        });
        Ok(())
    }

    #[inline(always)]
    fn key(&mut self, bytes: &[u8]) -> Result<(), Self::Error> {
        // Keys are materialised as Span leaves. The bytes slice
        // escapes the decoded form; when the bytes are a sub-slice of
        // the input (borrow path), push_leaf_borrowed_string would
        // apply — but the visitor trait does not distinguish. Use the
        // arena-frame path uniformly; the span is zero-width because
        // the visitor doesn't carry source offsets.
        let arena_offset = self.builder.arena_len();
        self.builder
            .arena_mut()
            .extend_from_slice(&(bytes.len() as u32).to_le_bytes());
        self.builder.arena_mut().extend_from_slice(bytes);
        self.builder
            .push_leaf_with_arena_frame(TapeKind::Span, 0, 0, 0, 0, arena_offset);
        Ok(())
    }

    #[inline(always)]
    fn end_object(&mut self) -> Result<(), Self::Error> {
        let frame = self.compounds.pop().ok_or(TapeVisitorError)?;
        self.builder.push_compound(
            frame.kind,
            frame.child_off,
            frame.span_lo,
            0,
            0,
            0,
        );
        Ok(())
    }

    #[inline(always)]
    fn begin_array(&mut self) -> Result<(), Self::Error> {
        let child_off = self.builder.mark_children();
        self.compounds.push(CompoundFrame {
            kind: TapeKind::Rule,
            span_lo: 0,
            child_off,
        });
        Ok(())
    }

    #[inline(always)]
    fn end_array(&mut self) -> Result<(), Self::Error> {
        let frame = self.compounds.pop().ok_or(TapeVisitorError)?;
        self.builder.push_compound(
            frame.kind,
            frame.child_off,
            frame.span_lo,
            0,
            0,
            0,
        );
        Ok(())
    }

    #[inline(always)]
    fn string(&mut self, bytes: &[u8]) -> Result<(), Self::Error> {
        let arena_offset = self.builder.arena_len();
        self.builder
            .arena_mut()
            .extend_from_slice(&(bytes.len() as u32).to_le_bytes());
        self.builder.arena_mut().extend_from_slice(bytes);
        self.builder
            .push_leaf_with_arena_frame(TapeKind::Span, 0, 0, 0, 0, arena_offset);
        Ok(())
    }

    #[inline(always)]
    fn number_f64(&mut self, value: f64) -> Result<(), Self::Error> {
        self.builder.push_leaf_with(
            TapeKind::Regex,
            0,
            0,
            0,
            0,
            PayloadData::WideScalar(value.to_bits()),
        );
        Ok(())
    }

    #[inline(always)]
    fn bool(&mut self, value: bool) -> Result<(), Self::Error> {
        self.builder.push_leaf_with(
            TapeKind::Literal,
            0,
            0,
            0,
            0,
            PayloadData::InlineScalar(value as u32),
        );
        Ok(())
    }

    #[inline(always)]
    fn null(&mut self) -> Result<(), Self::Error> {
        self.builder.push_leaf_with(
            TapeKind::Literal,
            0,
            0,
            0,
            0,
            PayloadData::InlineScalar(0),
        );
        Ok(())
    }
}
