//! `GrammarProfile` — the runtime face of the per-grammar IR
//! fingerprint, emitted into every grammar's `generated.rs` as a
//! single `const` literal.
//!
//! # Architectural role (Tranche AV Phase 1)
//!
//! Pre-AV, each per-grammar bit of codegen-time knowledge lived as an
//! emitter-local constant or an ad-hoc runtime query: push counts were
//! inlined into `TapeBuilder::with_capacity` at one site, the
//! structural alphabet drove a per-site nibble-LUT at another,
//! fixed-shape e-class facts sat in materialisation analysis. Every
//! downstream consumer re-derived or re-plumbed the same data.
//!
//! AV Phase 1 promotes the fingerprint to a first-class **codegen
//! output channel**. Every grammar emits one
//! `const GRAMMAR_PROFILE: GrammarProfile = GrammarProfile { ... };`
//! at the top of its `generated.rs`. Every downstream consumer — tape
//! capacity, scanner dispatch, column-set selection, reorder visitors,
//! keyword tables, shape dictionary, runtime dedup — reads the matching
//! profile field. The struct is entirely `const`-constructible; no
//! runtime initialisation is ever performed.
//!
//! Per-wave slot population:
//!
//! | Field | Populated in |
//! |-------|--------------|
//! | `compounds_per_input_byte`, `leaves_per_input_byte` | V1 (derived from `PushFingerprint::capacity_ratio` + class ratios) |
//! | `parallel_break_even_bytes` | V6 (doc-level parallel parse) |
//! | `structural_alphabet`, `structural_digraphs` | V1 (from `StructuralAlphabet`) |
//!
//! AX.W0b.A — seven dead slots retired (active_columns,
//! branch_priors, reorder_unroll_visitors, keyword_tables,
//! dedup_eligible_rules, payload_bytes_per_input_byte,
//! expected_ns_per_byte). AY.W0.4 — five further dead slots
//! retired (`push_compound_count`, `push_leaf_count`,
//! `push_leaf_with_count`, `list_rules`, `shape_dict`); each shipped
//! substrate-side at the emitter without a downstream runtime
//! consumer.

/// Identifier for an IR rule. Mirrors `bbnf_ir::RuleId` (which is a
/// `u32`) but lives on the tape-side of the codegen boundary so the
/// profile struct has no upward dependency on the IR crate. The
/// emitter writes the same numeric id through.
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RuleId(pub u32);

/// Per-grammar codegen fingerprint. Emitted once per grammar as
/// `const GRAMMAR_PROFILE: GrammarProfile = GrammarProfile { ... };`
/// at the top of `generated.rs`, next to the grammar string array.
///
/// Every field is `const`-constructible. Static slices reference
/// grammar-local `static` arrays emitted immediately above the
/// profile literal, so the entire profile lives in `.rodata`.
#[derive(Debug, Clone, Copy)]
pub struct GrammarProfile {
    // ── Per-byte density estimates (V1, derived from push counts) ────

    /// Estimated compound records produced per input byte. Drives the
    /// `TapeBuilder::with_capacity` reservation.
    pub compounds_per_input_byte: f32,

    /// Estimated leaf records produced per input byte.
    pub leaves_per_input_byte: f32,

    // ── Parallel parse cost model (V6) ───────────────────────────────

    /// Minimum input size (bytes) at which parallel parse beats
    /// sequential. Below this threshold, the sequential path is
    /// unconditionally used.
    pub parallel_break_even_bytes: u32,

    // ── Byte-class dispatch (V1, from StructuralAlphabet; AW-III.W5.a extends) ─

    /// Sorted single-byte structural alphabet — bytes that could
    /// terminate a scanner's inner loop. Empty when the grammar has
    /// no structural-alphabet fingerprint, or when the alphabet is
    /// outside the nibble-LUT window (2..=8 bytes).
    pub structural_alphabet: &'static [u8],

    /// Observed two-byte digraphs at scanner boundaries
    /// (`/*`, `*/`, `->`, `(*`, `*)`). First byte is always in
    /// `structural_alphabet`.
    ///
    /// Stored as `(first, second)` tuples so the same static literal
    /// feeds both the tape-side profile and the SIMD scanner's
    /// [`simd_scan::StructuralAlphabet::digraph_pairs`] without
    /// a shim layer (AW-III.W5.d).
    pub structural_digraphs: &'static [(u8, u8)],

    /// 256-bit bitmap of `structural_digraphs` first-bytes, packed
    /// as four `u64` words. Word `i` covers bytes `64*i .. 64*(i+1)`.
    /// Pre-computed at codegen time so the SIMD kernel masks
    /// candidate-opener lanes in one ANDS without a derefenced loop
    /// over `structural_digraphs`. AW-III.W5.a.
    pub structural_digraph_mask: [u64; 4],

    /// Sorted bytes that toggle string mode. Drives the CLMUL/PMULL
    /// (x86) or 6-op shift-XOR (NEON) quote-parity correction the
    /// SIMD kernel applies before compaction so inside-string bytes
    /// are masked off. Mined from `IrNode::Regex` whose
    /// classification is `RegexClass::QuotedString`. AW-III.W5.a.
    pub structural_quote_classes: &'static [u8],
}

impl GrammarProfile {
    /// Empty profile — every count zero, every slice empty. Used as
    /// a `Default` and as the identity value for tests that do not
    /// depend on a concrete grammar's fingerprint.
    pub const EMPTY: GrammarProfile = GrammarProfile {
        compounds_per_input_byte: 0.0,
        leaves_per_input_byte: 0.0,
        parallel_break_even_bytes: 0,
        structural_alphabet: &[],
        structural_digraphs: &[],
        structural_digraph_mask: [0u64; 4],
        structural_quote_classes: &[],
    };

    /// Reserve size for `TapeBuilder::with_capacity` given an input
    /// length (bytes). Combines the per-grammar density estimate with
    /// the AR-audit floor `input_len / 2 + 2` (sonic-rs ratio) so the
    /// fused-push hot path in [`push_compound_fused`] /
    /// [`push_leaf_fused`] never undershoots pre-allocation — cold-path
    /// `grow_all` fall-through stays reachable but unexercised on
    /// corpus input.
    ///
    /// # Formula
    ///
    /// ```text
    /// max(
    ///     ceil((compounds_per_byte + leaves_per_byte) * input_len),
    ///     input_len / 2,
    /// ) + 2
    /// ```
    ///
    /// The `+ 2` is a one-record pad covering empty inputs and the
    /// end-of-parse compound.
    ///
    /// # AW-IV.W2.3.b — pre-allocation invariant
    ///
    /// The per-grammar term captures density for deep grammars (CSS
    /// L4: every declaration / value / selector emits ~1.0
    /// records/byte; JSON: ~1.0; Sheets: ~1.0) — those cannot fit
    /// inside `input_len / 2`. The AR-audit floor captures the
    /// `(1, 2)` degenerate fallback for sparse grammars where
    /// `PushFingerprint::capacity_ratio()` returns the conservative
    /// `(1, 2)` — so the fused path's pre-condition holds even when
    /// the V1 density estimate reads zero (the `GrammarProfile::EMPTY`
    /// case, tests, ungated grammars).
    ///
    /// Pre-AW-IV the helper used only the per-grammar term. The
    /// AR-audit floor is new in W2.3.b; it keeps `capacity_for` from
    /// undershooting on grammars whose V1 fingerprint has yet to
    /// populate.
    ///
    /// Callers that want a grammar-aware soft-hint for sub-column
    /// sizing (`pay_narrow`, `pay_wide`, `pay_agg`) read the raw
    /// per-byte fields directly.
    ///
    /// [`push_compound_fused`]: crate::columns::Columns::push_compound_fused
    /// [`push_leaf_fused`]: crate::columns::Columns::push_leaf_fused
    #[inline]
    pub fn capacity_for(&self, input_len: usize) -> usize {
        let per_byte = self.compounds_per_input_byte + self.leaves_per_input_byte;
        let density_based = ((input_len as f32) * per_byte) as usize;
        // AR-audit floor: `input_len / 2` covers sparse grammars whose
        // V1 fingerprint returns the `(1, 2)` fallback and every
        // grammar where the density estimate is below the
        // sonic-rs-baseline record-per-2-bytes ratio.
        let ar_floor = input_len / 2;
        density_based.max(ar_floor) + 2
    }

}

impl Default for GrammarProfile {
    fn default() -> Self {
        Self::EMPTY
    }
}

