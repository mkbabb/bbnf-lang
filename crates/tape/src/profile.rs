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

// ── AY-II.W0.e — Structural-scan policy (grammar-derived activation) ─

/// Alphabet-density class for a single rule, derived at codegen time
/// from the rule's FIRST set intersected with the grammar's
/// structural alphabet / digraph signature.
///
/// Drives the emitter's per-rule choice between the plain-bitmap scan
/// and the bounded structural-scan fast path. Every rule in a
/// generated grammar carries one [`ScanAlphabetClass`]; the emitter
/// reads the enum at emission time and decides inline — no runtime
/// dispatch on the value.
///
/// The variants are partitioned by the intersection shape the CSP
/// facts report:
///
/// - [`ScanAlphabetClass::Empty`] — rule FIRST set is disjoint from
///   the structural alphabet. The structural scan would return no
///   hits inside the rule's extent; no activation.
/// - [`ScanAlphabetClass::Sparse`] — FIRST set and structural
///   alphabet intersect on `1..=3` bytes. Bounded scan is profitable
///   on long runs; the emitter admits `bounded_lookahead`.
/// - [`ScanAlphabetClass::Dense`] — intersection has `>= 4` bytes or
///   the rule is a compound whose closing byte is in the alphabet
///   (object `}`, array `]`, block `)`). Hot-path object-key seek
///   and bounded compound-boundary scan both admit.
/// - [`ScanAlphabetClass::Digraph`] — the grammar carries a non-empty
///   digraph set and the rule's FIRST set covers at least one digraph
///   first-byte. Emitter emits the digraph-aware probe.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ScanAlphabetClass {
    /// Rule's FIRST set has no intersection with the grammar's
    /// structural alphabet. Structural scan is inadmissible.
    Empty = 0,
    /// 1..=3 bytes in the intersection. Bounded lookahead admits; no
    /// object-key seek fast path.
    Sparse = 1,
    /// >= 4 bytes in the intersection OR the rule's compound-closing
    /// byte is in the alphabet. Full substrate admission.
    Dense = 2,
    /// Grammar carries digraphs AND the rule covers at least one
    /// digraph opener. Digraph-aware probe emits.
    Digraph = 3,
}

/// Activation flags for a single rule — a bitmap of per-capability
/// admission decisions. Generated by the emitter at codegen time from
/// the rule's [`ScanAlphabetClass`] + grammar facts; read at emission
/// time (not at runtime) to decide which structural-scan primitives
/// the emitted parser for this rule should inline.
///
/// Flags are independent: a rule may admit `OBJECT_KEY_SEEK` and
/// `BOUNDED_LOOKAHEAD` simultaneously (JSON `object`'s value
/// position), or `BOUNDED_LOOKAHEAD` alone (CSS `declarationList`'s
/// semicolon-bounded slice), or neither (a leaf rule whose body has
/// no structural content).
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScanActivationFlags(pub u8);

impl ScanActivationFlags {
    /// Bit 0 — emitter inlines [`crate::cursor::TapeCursor::object_key_seek`]
    /// for value-position lookups in this rule's compound shape.
    /// Hot-path JSON `get` / Sheets `cell_ref` lookup.
    pub const OBJECT_KEY_SEEK: u8 = 0b0000_0001;

    /// Bit 1 — emitter inlines
    /// [`crate::cursor::TapeCursor::bounded_lookahead`] to bound the
    /// scan window of a regex / bounded-literal search inside this
    /// rule.
    pub const BOUNDED_LOOKAHEAD: u8 = 0b0000_0010;

    /// Bit 2 — emitter inlines
    /// [`crate::cursor::TapeCursor::scan_structural_bounded`] when
    /// walking this rule's children for a structural-byte query.
    pub const SCAN_STRUCTURAL_BOUNDED: u8 = 0b0000_0100;

    /// Bit 3 — grammar carries a digraph opener reachable from this
    /// rule's FIRST set; emitter adds a digraph-validation peek in
    /// the structural-scan dispatch.
    pub const DIGRAPH_ADMIT: u8 = 0b0000_1000;

    /// Empty flag set — no structural-scan capability admits.
    pub const NONE: ScanActivationFlags = ScanActivationFlags(0);

    /// Construct from a raw u8.
    #[inline]
    pub const fn from_bits(bits: u8) -> Self {
        Self(bits)
    }

    /// Is any flag set?
    #[inline]
    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }

    /// Probe a single flag.
    #[inline]
    pub const fn contains(self, flag: u8) -> bool {
        (self.0 & flag) == flag
    }
}

/// One entry in the per-grammar `STRUCTURAL_SCAN_POLICY` const table
/// emitted by the codegen pipeline.
///
/// Generated by the emitter from CSP-inferred `first_set` facts on
/// the rule meta + the grammar's mined `structural_alphabet` and
/// `structural_digraphs`. One entry per non-transparent rule. The
/// generated parser dispatches on this table at emission time — the
/// emitter resolves `(rule_id, class, flags)` at codegen and inlines
/// the admitted scan primitive where the rule's body emits. No
/// runtime flag, no hand-routed specialization.
///
/// # Wire contract
///
/// - `rule_id` mirrors [`RuleId`] numerically — the emitter writes
///   through the same `u32` encoding the IR's `RuleId` uses.
/// - `class` captures the alphabet-density classification; see
///   [`ScanAlphabetClass`].
/// - `activation` captures the per-capability admission bitmap; see
///   [`ScanActivationFlags`].
///
/// # Consumer
///
/// Emitter shapes probe the table at emission time via a
/// `STRUCTURAL_SCAN_POLICY.iter().find(|e| e.rule_id == rid)` pattern
/// spliced as a `const` lookup; the match resolves at monomorphisation
/// and the admitted primitive is inlined.
#[derive(Debug, Clone, Copy)]
pub struct ScanPolicyEntry {
    /// Rule identifier — matches [`RuleId`]'s numeric value.
    pub rule_id: u32,
    /// Alphabet-density classification, derived from the rule's
    /// FIRST set intersected with the grammar's structural alphabet.
    pub alphabet_class: ScanAlphabetClass,
    /// Per-capability admission bitmap. Empty when the rule admits
    /// no structural-scan fast path.
    pub activation: ScanActivationFlags,
}

impl ScanPolicyEntry {
    /// Empty policy — class `Empty`, no activation flags. Used as a
    /// default fill for rules outside the analysed set.
    pub const EMPTY: ScanPolicyEntry = ScanPolicyEntry {
        rule_id: 0,
        alphabet_class: ScanAlphabetClass::Empty,
        activation: ScanActivationFlags::NONE,
    };
}

