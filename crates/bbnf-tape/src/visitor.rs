//! Backend-agnostic visitor abstraction the per-shape emitter inlines
//! into per-rule parse functions.
//!
//! # Role — AW-V.W1.3 substrate
//!
//! Per AW-V.md §W1.3 + B2 §4 + B4 §5: the per-grammar emitter monomor-
//! phises the visitor trait at every call site — no `dyn Visitor`
//! dispatch reaches the bench binary. The shape-emitter's `parse_value`
//! / `parse_object` / `parse_array` / `parse_string` / `parse_number` /
//! `parse_keyword` inner loops are generic over `V: GrammarVisitor`
//! and compose with per-shape sub-traits ([`ObjectVisitor`],
//! [`ArrayVisitor`], [`StringVisitor`], [`NumberVisitor`],
//! [`KeywordVisitor`]). Each emitter arm constrains the visitor's
//! minimum trait bound to the methods the arm actually invokes, so a
//! visitor that consumes only strings pays no compile-time surface for
//! the other shapes.
//!
//! Two concrete visitor consumers ship alongside the trait hierarchy:
//!
//! - [`TapeVisitor`] — emits into a [`bbnf_tape::Columns`] substrate via
//!   a [`TapeBuilder`]. Records the structural + payload bytes the
//!   `dispatch_one` cold-path replay would produce; parity is the
//!   sanity gate for the shape-emitter lift in W3.
//! - [`ValueVisitor`] — placeholder for the per-grammar type resolver
//!   (W3.2). Compiles against a minimal JSON enum so W1.3 lands a
//!   working monomorphic trait surface; W3.2 generates the real
//!   per-grammar `ValueVisitor`s from the type resolver.
//!
//! Both visitors are monomorphic: the parser body that drives them
//! through `parse_json::<V>` inlines each method body at the call site.
//! No `Box<dyn Visitor>`, no `&dyn Visitor`.
//!
//! # Design template
//!
//! The trait hierarchy lifts the shape established by
//! `bbnf-json-prototype`'s `JsonVisitor` (crates/bbnf-json-prototype/
//! src/visitor.rs:31) into the per-shape sub-trait composition B4 §5
//! prescribes. The prototype trait was JSON-fixed by necessity (single
//! grammar); lifting it to `bbnf-tape` generalises the method set
//! across grammars by splitting per-shape concerns into sub-traits a
//! consumer opts into.

use crate::builder::{PayloadData, TapeBuilder};
use crate::columns::Columns;
use crate::kind::TapeKind;
use crate::tape::{Tape, TapeOffset};
use crate::TapeBuildError;

// ─────────────────────────────────────────────────────────────────────
// GrammarVisitor — top-level trait
// ─────────────────────────────────────────────────────────────────────

/// Backend-agnostic visitor root. Every per-shape sub-trait extends
/// this. The per-grammar emitter generates the call sites that drive
/// whichever concrete visitor the user passes in.
///
/// # Monomorphisation contract
///
/// Every visitor method is called through `V: GrammarVisitor + …`
/// generic bounds at the emitter's parse function signatures. LLVM
/// monomorphises each `parse_<shape>::<V>` instantiation per concrete
/// visitor; the emitter's inline arms splice the visitor call inline
/// at each method site.
///
/// No `dyn GrammarVisitor` dispatch is supported — the trait is not
/// object-safe by design (associated `Error` type + per-shape methods
/// that take concrete `Self` ensure static dispatch only).
pub trait GrammarVisitor {
    /// Visitor-side error surface. The parser translates rejected
    /// events into its own error type at the call site.
    type Error;
}

// ─────────────────────────────────────────────────────────────────────
// Per-shape sub-traits
// ─────────────────────────────────────────────────────────────────────

/// Visitor surface for object-shape rules — JSON `object`, CSS
/// declaration block, BBNF key-value maps.
///
/// The emitter's object-shape arm calls [`Self::begin_object`] before
/// the first key, [`Self::key`] per key, and [`Self::end_object`] after
/// the final value (or immediately after `begin_object` for empty
/// objects). Values between keys are dispatched to other per-shape
/// sub-traits on the same visitor via generic bounds on the parse
/// function.
pub trait ObjectVisitor: GrammarVisitor {
    /// Called before an object's first key.
    fn begin_object(&mut self) -> Result<(), Self::Error>;
    /// Called for each object key. Bytes are a UTF-8 substring of the
    /// input slice when no escapes are present; owned decoded bytes
    /// otherwise. The caller's parse loop invokes value-emitting
    /// methods between `key` calls via the visitor's other sub-trait
    /// bounds (strings via [`StringVisitor`], etc.).
    fn key(&mut self, bytes: &[u8]) -> Result<(), Self::Error>;
    /// Called after an object's final value, or immediately after
    /// [`Self::begin_object`] for empty objects.
    fn end_object(&mut self) -> Result<(), Self::Error>;
}

/// Visitor surface for array-shape rules — JSON `array`, CSS selector
/// list, comma-separated values.
///
/// The emitter's array-shape arm calls [`Self::begin_array`] before
/// the first element and [`Self::end_array`] after the last; element
/// values dispatch through the visitor's other sub-trait bounds.
pub trait ArrayVisitor: GrammarVisitor {
    /// Called before an array's first element.
    fn begin_array(&mut self) -> Result<(), Self::Error>;
    /// Called after an array's final element, or immediately after
    /// [`Self::begin_array`] for empty arrays.
    fn end_array(&mut self) -> Result<(), Self::Error>;
}

/// Visitor surface for string-shape rules — JSON `string`, CSS
/// `<string>`, BBNF string literal.
///
/// Bytes are a UTF-8 substring of the input slice when no escapes are
/// present (zero-copy borrow path); owned decoded bytes otherwise.
pub trait StringVisitor: GrammarVisitor {
    /// Called on a string value.
    fn string(&mut self, bytes: &[u8]) -> Result<(), Self::Error>;
}

/// Visitor surface for number-shape rules — JSON `number`, CSS
/// `<number>`, Sheets number.
///
/// The default `number_i64` forwards to `number_f64` via lossy cast —
/// a visitor that wants to distinguish integer from float literals
/// overrides [`Self::number_i64`] and carries the integer shape
/// through its materialisation. For JSON parity the f64 representative
/// is adequate (sonic-rs's `Number` does the same; every JSON integer
/// literal resolves through Eisel-Lemire into an f64 anyway).
pub trait NumberVisitor: GrammarVisitor {
    /// Called on a numeric literal — f64 representative.
    fn number_f64(&mut self, value: f64) -> Result<(), Self::Error>;
    /// Called on an integer literal. Defaults to casting through f64;
    /// visitors that preserve the integer shape override this.
    #[inline(always)]
    fn number_i64(&mut self, value: i64) -> Result<(), Self::Error> {
        self.number_f64(value as f64)
    }
}

/// Visitor surface for keyword-shape rules — JSON `true` / `false` /
/// `null`, BBNF directive prefix, CSS `@`-rule head.
///
/// Named for the shape taxonomy (B4 §1); per-keyword dispatch lives at
/// the call site via byte-matched emitter arms.
pub trait KeywordVisitor: GrammarVisitor {
    /// Called on a `true` / `false` literal.
    fn bool(&mut self, value: bool) -> Result<(), Self::Error>;
    /// Called on a `null` literal.
    fn null(&mut self) -> Result<(), Self::Error>;
}

// ─────────────────────────────────────────────────────────────────────
// TapeVisitor — default impl that emits into Columns / TapeBuilder
// ─────────────────────────────────────────────────────────────────────

/// The `bbnf-tape` substrate visitor — emits events into a
/// [`bbnf_tape::Columns`] + [`bbnf_tape::PayloadStream`] backing store
/// via a [`TapeBuilder`].
///
/// # Layout per shape
///
/// - **Objects / Arrays** — compound records (`TapeKind::Rule`)
///   bracketing the children run. `begin_*` marks the child-run
///   position via [`TapeBuilder::mark_children`]; `end_*` closes with
///   [`TapeBuilder::push_compound`] and the accumulated span.
/// - **Keys** — span leaves (`TapeKind::Span`) carrying the decoded
///   bytes as an arena-framed `(len: u32 LE, bytes)` payload.
/// - **Strings** — span leaves (`TapeKind::Span`) carrying arena-
///   framed bytes (shape parity with keys; W3.2 may specialise
///   borrowed strings).
/// - **Numbers** — `TapeKind::Regex` leaves carrying
///   `PayloadData::WideScalar` (f64 bits).
/// - **Bools / nulls** — `TapeKind::Literal` leaves carrying
///   `PayloadData::InlineScalar` (`value as u32` / `0`).
///
/// # Monomorphisation
///
/// Every `impl GrammarVisitor for TapeVisitor` method is
/// `#[inline(always)]` so the emitter's arm calls collapse to direct
/// `Columns` / `TapeBuilder` mutations without a function-call
/// boundary. The visitor carries no state the emitter can't reach —
/// compound nesting lives on the builder's children-mark stack +
/// a sidecar `Vec<CompoundFrame>` kept on the visitor.
pub struct TapeVisitor<'input> {
    builder: TapeBuilder,
    input: &'input [u8],
    /// In-progress compound frames. Each frame records the
    /// [`mark_children`](TapeBuilder::mark_children) offset, the span
    /// start, and the compound kind so the matching `end_*` closes
    /// with the right metadata.
    compounds: Vec<CompoundFrame>,
}

/// In-progress compound frame — private to [`TapeVisitor`]; kept on
/// the visitor rather than threaded through the parse function's
/// generic bounds.
struct CompoundFrame {
    kind: TapeKind,
    span_lo: u32,
    child_off: TapeOffset,
}

/// Error surface for [`TapeVisitor`]. The visitor rejects events only
/// when its internal frame stack is unbalanced — a parser bug the
/// prototype never triggers; the error exists so the shape sub-traits'
/// `Result<(), Error>` returns have an inhabited type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TapeVisitorError {
    /// A compound `end_*` was called without a matching `begin_*`.
    UnbalancedCompound,
}

impl<'input> TapeVisitor<'input> {
    /// Construct a fresh visitor over the input slice.
    ///
    /// The tape is pre-allocated via the AR-audit `input.len() / 2 + 2`
    /// heuristic — every parse producing ≤ 1 record per 2 bytes fits
    /// without a `grow_all`.
    #[inline]
    pub fn new(input: &'input [u8]) -> Self {
        let capacity = input.len() / 2 + 2;
        Self {
            builder: TapeBuilder::with_capacity(capacity),
            input,
            compounds: Vec::new(),
        }
    }

    /// Consume the visitor and finalise the tape.
    ///
    /// Runs the builder's finalisation pipeline (derives `sib_skip`
    /// + `span_hi` / `child_off` inline for the legacy fn-per-rule
    /// path, or consumes the parallel `frame_depth` stream when the
    /// DTA driver is in inline-finalisation mode).
    #[inline]
    pub fn finish(self) -> Result<Tape, TapeBuildError> {
        self.builder.finish()
    }

    /// Borrow the underlying builder mutably. Used by parser shims
    /// that need direct arena-write access (the JSON decode kernel
    /// streams decoded bytes directly into `pay_agg` via
    /// [`TapeBuilder::arena_mut`]).
    #[inline]
    pub fn builder_mut(&mut self) -> &mut TapeBuilder {
        &mut self.builder
    }

    /// Borrow the underlying columns immutably. Test-fixture surface
    /// for asserting per-column byte contents against the reference
    /// `dispatch_one` path.
    #[inline]
    pub fn columns(&self) -> &Columns {
        self.builder.columns()
    }

    /// Byte length of the source input this visitor is parsing.
    #[inline]
    pub fn input_len(&self) -> usize {
        self.input.len()
    }

    /// Push the two-part arena frame for a string or key payload —
    /// `(len: u32 LE, bytes)` into the `pay_agg` column — and return
    /// the byte offset of the length prefix.
    #[inline(always)]
    fn push_arena_frame(&mut self, bytes: &[u8]) -> u32 {
        let arena = self.builder.arena_mut();
        let offset = arena.len() as u32;
        arena.extend_from_slice(&(bytes.len() as u32).to_le_bytes());
        arena.extend_from_slice(bytes);
        offset
    }
}

impl<'input> GrammarVisitor for TapeVisitor<'input> {
    type Error = TapeVisitorError;
}

impl<'input> ObjectVisitor for TapeVisitor<'input> {
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
        let arena_offset = self.push_arena_frame(bytes);
        self.builder
            .push_leaf_with_arena_frame(TapeKind::Span, 0, 0, 0, 0, arena_offset);
        Ok(())
    }

    #[inline(always)]
    fn end_object(&mut self) -> Result<(), Self::Error> {
        let frame = self
            .compounds
            .pop()
            .ok_or(TapeVisitorError::UnbalancedCompound)?;
        self.builder
            .push_compound(frame.kind, frame.child_off, frame.span_lo, 0, 0, 0);
        Ok(())
    }
}

impl<'input> ArrayVisitor for TapeVisitor<'input> {
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
        let frame = self
            .compounds
            .pop()
            .ok_or(TapeVisitorError::UnbalancedCompound)?;
        self.builder
            .push_compound(frame.kind, frame.child_off, frame.span_lo, 0, 0, 0);
        Ok(())
    }
}

impl<'input> StringVisitor for TapeVisitor<'input> {
    #[inline(always)]
    fn string(&mut self, bytes: &[u8]) -> Result<(), Self::Error> {
        let arena_offset = self.push_arena_frame(bytes);
        self.builder
            .push_leaf_with_arena_frame(TapeKind::Span, 0, 0, 0, 0, arena_offset);
        Ok(())
    }
}

impl<'input> NumberVisitor for TapeVisitor<'input> {
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
}

impl<'input> KeywordVisitor for TapeVisitor<'input> {
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

// ─────────────────────────────────────────────────────────────────────
// ValueVisitor — placeholder for the per-grammar type resolver (W3.2)
// ─────────────────────────────────────────────────────────────────────

/// Minimal JSON value enum the placeholder [`ValueVisitor`] materialises
/// into.
///
/// W1.3 ships only the smallest JSON-shape enum that satisfies the
/// monomorphic trait surface — W3.2 replaces this with a per-grammar
/// generated enum from the type resolver. The placeholder exists so
/// the trait hierarchy compiles against a concrete consumer without
/// a circular dep on the prototype crate.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// The JSON literal `null`.
    Null,
    /// The JSON literal `true` / `false`.
    Bool(bool),
    /// A JSON number — f64 representative (Eisel-Lemire decoded).
    Number(f64),
    /// A JSON string — owned byte vector. W3.2's per-grammar emitter
    /// replaces this with a borrow / arena-offset split mirroring the
    /// prototype's `StringSpan`.
    String(Vec<u8>),
    /// A JSON array — nested values.
    Array(Vec<Value>),
    /// A JSON object — ordered key-value pairs.
    Object(Vec<(Vec<u8>, Value)>),
}

/// Placeholder materialising visitor — emits events into a nested
/// [`Value`] tree.
///
/// W1.3 scope: compiles against the minimal JSON enum above.
/// W3.2 scope: per-grammar generated `ValueVisitor` per the type
/// resolver, with borrow-vs-arena string splits, arena-backed compound
/// storage, and compile-time-known struct layouts.
///
/// # Monomorphisation
///
/// Matches [`TapeVisitor`]'s shape — every method `#[inline(always)]`,
/// no `dyn` dispatch, all state on the visitor struct.
pub struct ValueVisitor {
    /// The document's top-level value — [`Value::Null`] until the
    /// first event arrives.
    root: Option<Value>,
    /// Compound-frame stack. Each frame holds the in-progress
    /// [`Value`] — an `Array`'s elements accumulate in its `Vec`, an
    /// `Object`'s entries accumulate in its pair list with a pending
    /// key held in [`pending_key`](Self::pending_key).
    stack: Vec<Value>,
    /// Pending object key — the key byte sequence arrived but the
    /// matching value has not yet. Consumed on the next value event.
    pending_key: Option<Vec<u8>>,
}

/// Error surface for [`ValueVisitor`]. The visitor rejects events only
/// on structural malformation (an `end_*` without a matching `begin_*`
/// / a value without a matching key inside an object).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueVisitorError {
    /// `end_*` called without a matching `begin_*`.
    UnbalancedCompound,
    /// Value event emitted inside an object without a preceding key.
    MissingKey,
}

impl Default for ValueVisitor {
    fn default() -> Self {
        Self::new()
    }
}

impl ValueVisitor {
    /// Construct a fresh visitor with empty storage.
    #[inline]
    pub fn new() -> Self {
        Self {
            root: None,
            stack: Vec::new(),
            pending_key: None,
        }
    }

    /// Consume the visitor and return the materialised root value.
    /// Returns [`None`] when no events were emitted.
    #[inline]
    pub fn finish(self) -> Option<Value> {
        self.root
    }

    /// Stamp a scalar / compound value into either the pending object
    /// entry, the top-of-stack array, or the root — whichever context
    /// is current.
    #[inline(always)]
    fn emit(&mut self, value: Value) -> Result<(), ValueVisitorError> {
        match self.stack.last_mut() {
            Some(Value::Array(arr)) => {
                arr.push(value);
                Ok(())
            }
            Some(Value::Object(pairs)) => {
                let key = self
                    .pending_key
                    .take()
                    .ok_or(ValueVisitorError::MissingKey)?;
                pairs.push((key, value));
                Ok(())
            }
            // Scalars / compounds popped up to the root - a second
            // top-level value would be a parser bug the visitor can
            // surface; for now the last write wins.
            Some(_) => {
                // Non-compound on the stack top is impossible by
                // construction — only `begin_object` / `begin_array`
                // push frames and both push compound variants.
                Ok(())
            }
            None => {
                self.root = Some(value);
                Ok(())
            }
        }
    }

    /// Pop the top-of-stack compound frame and stamp it as a value
    /// into the next-higher frame (or root).
    #[inline(always)]
    fn pop_frame(&mut self) -> Result<(), ValueVisitorError> {
        let frame = self
            .stack
            .pop()
            .ok_or(ValueVisitorError::UnbalancedCompound)?;
        self.emit(frame)
    }
}

impl GrammarVisitor for ValueVisitor {
    type Error = ValueVisitorError;
}

impl ObjectVisitor for ValueVisitor {
    #[inline(always)]
    fn begin_object(&mut self) -> Result<(), Self::Error> {
        self.stack.push(Value::Object(Vec::new()));
        Ok(())
    }

    #[inline(always)]
    fn key(&mut self, bytes: &[u8]) -> Result<(), Self::Error> {
        self.pending_key = Some(bytes.to_vec());
        Ok(())
    }

    #[inline(always)]
    fn end_object(&mut self) -> Result<(), Self::Error> {
        self.pop_frame()
    }
}

impl ArrayVisitor for ValueVisitor {
    #[inline(always)]
    fn begin_array(&mut self) -> Result<(), Self::Error> {
        self.stack.push(Value::Array(Vec::new()));
        Ok(())
    }

    #[inline(always)]
    fn end_array(&mut self) -> Result<(), Self::Error> {
        self.pop_frame()
    }
}

impl StringVisitor for ValueVisitor {
    #[inline(always)]
    fn string(&mut self, bytes: &[u8]) -> Result<(), Self::Error> {
        self.emit(Value::String(bytes.to_vec()))
    }
}

impl NumberVisitor for ValueVisitor {
    #[inline(always)]
    fn number_f64(&mut self, value: f64) -> Result<(), Self::Error> {
        self.emit(Value::Number(value))
    }
}

impl KeywordVisitor for ValueVisitor {
    #[inline(always)]
    fn bool(&mut self, value: bool) -> Result<(), Self::Error> {
        self.emit(Value::Bool(value))
    }

    #[inline(always)]
    fn null(&mut self) -> Result<(), Self::Error> {
        self.emit(Value::Null)
    }
}
