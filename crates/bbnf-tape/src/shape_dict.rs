//! BBNF shape dictionary entries — AV.5.6 / AV.6.1–6.3.
//!
//! Per-grammar metadata describing the BBNF rule bodies whose
//! multi-record tape skeletons collapse to a single `ShapeRef` record.
//!
//! Two patterns are recognised today:
//!
//! 1. **`big_comment`** — `( "/*" , /[^\*]*/ , "*/" ) ?w -> Span`.
//!    Three records (Rule → Repeat → Span leaf) collapse to one
//!    `ShapeRef` + an 8-byte `(u32 lo, u32 hi)` payload blob.
//!    Subsumes AU.6.9.
//!
//! 2. **`mapped_factor` empty branch** — `factor , ( "->" ... )?`.
//!    When the optional `->` mapping is absent (~40% of factor calls
//!    per profiling-2.md), the body is structurally just `factor`.
//!    Template captures the factor's tape offset; payload blob is
//!    empty.
//!
//! Each emitted grammar carries a `pub const BBNF_SHAPE_DICT: &[
//! BbnfShapeEntry]` produced by the codegen. The codegen for
//! `__big_comment` / `__mapped_factor` consults the dict at compile
//! time and, on match, emits a single `push_shape_ref` per recogniser
//! instead of the normal multi-record skeleton.

/// Which structural pattern this entry recognises.
///
/// Two variants today; a third (and beyond) compose additively as new
/// patterns are mined from BBNF self-hosting traces.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BbnfShapeKind {
    /// `big_comment` rule: `( "/*" , /[^\*]*/ , "*/" ) ?w -> Span`.
    /// Single-hole template; payload blob is the inner Span's
    /// `(u32 lo, u32 hi)` pair (8 bytes).
    BigComment,

    /// `mapped_factor` rule with an empty `->` branch — the optional
    /// mapping is absent (the common case, ~40% of factor calls).
    /// Template captures the `__factor` tape offset; payload blob is
    /// empty.
    MappedFactorEmpty,
}

/// One entry in the per-grammar BBNF shape dictionary.
///
/// `shape_hash` is a stable 64-bit hash derived from the
/// `BbnfShapeKind` discriminant + a domain separator
/// (`b"bbnf_shape_template"`); two entries with the same `kind`
/// always share the same hash.
///
/// `payload_bytes` records the size of the packed payload blob the
/// `ShapeRef` record carries:
///
/// * `8` for `BigComment` (one `Span` = `(u32 lo, u32 hi)`).
/// * `0` for `MappedFactorEmpty` (empty branch, no payload).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BbnfShapeEntry {
    /// Grammar rule name (e.g. `"big_comment"`, `"mapped_factor"`).
    pub rule_name: &'static str,

    /// Which structural pattern this entry recognises.
    pub kind: BbnfShapeKind,

    /// Canonical 64-bit hash for matching at emit time.
    pub shape_hash: u64,

    /// Byte size of the packed payload blob.
    pub payload_bytes: u16,
}

impl BbnfShapeEntry {
    /// True when the shape carries no payload — `MappedFactorEmpty`.
    /// The runtime allocator skips the payload column write for these
    /// records.
    #[inline]
    pub const fn is_empty_payload(&self) -> bool {
        self.payload_bytes == 0
    }
}
