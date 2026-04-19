//! Stage-1 SIMD structural-index output type.
//!
//! `StructuralIndex` is the wire contract between the SIMD scanner
//! (`bbnf-simd-scan`) and the DTA driver (`crates/bbnf-tape/src/driver.rs`).
//! The scanner builds it once per parse from the input; the driver's
//! `Cursor` consumes it via slot-indexed lookups.
//!
//! # Layout
//!
//! Two parallel `Vec`s of equal length `n`:
//!
//! - `positions[i]`: byte offset (within the parsed input) of the
//!   `i`-th structural byte. Offsets are strictly monotonically
//!   increasing and bounded by `input.len()`.
//! - `kinds[i]`: the byte value at `positions[i]` (or, for digraph
//!   matches, an encoded discriminant — see scanner contract).
//!
//! For pure single-byte alphabets, `kinds[i] == input[positions[i]]`.
//! Digraph-rich grammars (CSS `/*`, `*/`; BBNF `->`, `(*`, `*)`) may
//! choose to fold digraphs into the single-byte index by emitting
//! the digraph's first byte and letting the driver peek at
//! `positions[i] + 1` for second-byte disambiguation, OR by emitting
//! a distinct synthetic discriminant; either policy is contained in
//! the scanner's `StructuralAlphabet` configuration.
//!
//! # Why two columns
//!
//! The driver's hot loop reads `idx.kinds[slot]` to dispatch (one
//! indexed `u8` load) and `idx.positions[slot]` to bound regex /
//! literal scans (one indexed `u32` load). Splitting them keeps each
//! column dense for L1 — scans that touch only `kinds` (`ByteDispatch`
//! arm) read 1 byte/slot; scans that touch both (`Regex` arm) read
//! 5 bytes/slot still under one cacheline per 12 slots.
//!
//! # Construction
//!
//! Build via `bbnf_simd_scan::scan_structural(input, &alphabet)`. The
//! `bbnf-tape` crate holds only the type definition; no kernel code.
//! This crate owns no SIMD intrinsics — they live in the architecture-
//! neutral `bbnf-simd-scan` crate per the §architecture invariants
//! (general-purpose constructs in their own crates).

/// Stage-1 structural index: every byte in `positions` is a position
/// in the original input where the parser may need to dispatch,
/// and `kinds[i]` carries the byte (or synthetic discriminant) at
/// that position.
///
/// Both columns are guaranteed to have equal length and are sorted
/// by `positions` ascending.
#[derive(Debug, Clone, Default)]
pub struct StructuralIndex {
    /// Input byte offsets of structural bytes. Strictly monotone.
    pub positions: Vec<u32>,
    /// Per-position kind discriminant. Length matches `positions`.
    /// For single-byte alphabets, equals the literal byte at that
    /// offset. For grammars with synthetic digraph discriminants,
    /// the scanner's policy determines the encoding.
    pub kinds: Vec<u8>,
}

impl StructuralIndex {
    /// Number of structural positions in the index.
    #[inline]
    pub fn len(&self) -> usize {
        self.positions.len()
    }

    /// `true` iff the input contained no structural bytes.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.positions.is_empty()
    }

    /// Construct an empty index. Equivalent to `Default::default()`.
    #[inline]
    pub const fn new() -> Self {
        Self {
            positions: Vec::new(),
            kinds: Vec::new(),
        }
    }

    /// Construct an index with pre-allocated capacity for `n` positions.
    /// Stage-1 scanners size this from `input.len() / expected_density`.
    #[inline]
    pub fn with_capacity(n: usize) -> Self {
        Self {
            positions: Vec::with_capacity(n),
            kinds: Vec::with_capacity(n),
        }
    }

    /// Append one (position, kind) pair. Caller maintains monotonicity.
    #[inline]
    pub fn push(&mut self, pos: u32, kind: u8) {
        self.positions.push(pos);
        self.kinds.push(kind);
    }

    /// Drop both columns to length zero, retaining capacity.
    #[inline]
    pub fn clear(&mut self) {
        self.positions.clear();
        self.kinds.clear();
    }
}
