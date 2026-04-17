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
//! Some fields are populated today from existing IR facts
//! (`compute_push_fingerprint`, `compute_structural_alphabet`); the
//! remaining slots are empty slices that later AV waves fill in:
//!
//! | Field | Populated in |
//! |-------|--------------|
//! | `push_compound_count`, `push_leaf_count`, `push_leaf_with_count` | V1 (from `PushFingerprint`) |
//! | `compounds_per_input_byte`, `leaves_per_input_byte`, `payload_bytes_per_input_byte` | V1 (derived from `PushFingerprint::capacity_ratio` + class ratios) |
//! | `expected_ns_per_byte`, `parallel_break_even_bytes` | V6 (doc-level parallel parse) |
//! | `structural_alphabet`, `structural_digraphs` | V1 (from `StructuralAlphabet`) |
//! | `active_columns` | V2 (columnar substrate) |
//! | `list_rules` | V6 (document-level parallel parse) |
//! | `keyword_tables` | V7 (PHF + SIMD keyword dispatch) |
//! | `shape_dict` | V5 (ShapeDictionary) |
//! | `branch_priors` | V4 (PSI stage-B) |
//! | `dedup_eligible_rules` | V8 (runtime bloom+GADT dedup) |
//! | `reorder_unroll_visitors` | V2 (4-lane reordered unrolling) |

/// Identifier for a payload column in the columnar substrate
/// ([`GrammarProfile::active_columns`], wave V2).
///
/// Opaque wire-stable tag assigned at codegen time. V2 will introduce
/// a registry of semantic column names; at V1 the type exists so the
/// profile struct can carry an empty slice in the right shape.
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ColumnId(pub u16);

/// Identifier for an IR rule. Mirrors `bbnf_ir::RuleId` (which is a
/// `u32`) but lives on the tape-side of the codegen boundary so the
/// profile struct has no upward dependency on the IR crate. The
/// emitter writes the same numeric id through.
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RuleId(pub u32);

/// A keyword dispatch table entry ([`GrammarProfile::keyword_tables`],
/// wave V7). V7 will populate the fields; V1 carries an empty slice.
#[derive(Debug, Clone, Copy)]
pub struct KeywordTable {
    /// Rule the keyword table fires for.
    pub rule: RuleId,
    /// Sorted keyword bytes for SIMD wide-compare dispatch.
    pub keywords: &'static [&'static [u8]],
}

/// A shape-dictionary entry ([`GrammarProfile::shape_dict`], wave V5).
///
/// Each entry describes one fixed-shape compound subtree that the
/// parser collapses to a single `TapeKind::ShapeRef` record at parse
/// time. The cursor reconstitutes children from the template at
/// read time.
///
/// `child_kinds` declares the skeleton: each byte is the
/// [`TapeKind`](crate::TapeKind) discriminant of the corresponding
/// child position. `leaf_holes` gives the byte offset within the
/// packed payload blob where each non-constant leaf's span/payload
/// begins. The two slices are parallel with `child_kinds`; entries
/// where the child is structural (not a leaf hole) carry
/// `u16::MAX` as the sentinel.
#[derive(Debug, Clone, Copy)]
pub struct ShapeEntry {
    /// Canonical shape-hash from `RecognizerSignature`.
    pub shape_hash: u64,
    /// Rule id the shape-ref expands to.
    pub rule: RuleId,
    /// Per-child `TapeKind` discriminants declaring the skeleton.
    /// Length = number of direct children the collapsed compound
    /// would have emitted.
    pub child_kinds: &'static [u8],
    /// Per-child byte offset into the packed payload blob for each
    /// leaf hole. `u16::MAX` marks structural (non-hole) children.
    pub leaf_payload_offsets: &'static [u16],
    /// Total byte width of the packed payload blob.
    pub payload_bytes: u16,
}

/// A branch-prior entry ([`GrammarProfile::branch_priors`], wave V4).
/// V4 will populate the fields; V1 carries an empty slice.
#[derive(Debug, Clone, Copy)]
pub struct BranchPrior {
    /// Alt rule id the prior applies to.
    pub rule: RuleId,
    /// Selected-branch index for speculative dispatch.
    pub branch_idx: u16,
    /// Observed-prior weight in the 0..=255 quantised domain.
    pub weight_q8: u8,
}

/// Identifier for a reorder-unroll visitor
/// ([`GrammarProfile::reorder_unroll_visitors`], wave V2).
///
/// Opaque tag naming a visitor (e.g. `sum_f64`, `count`, `max_u32`)
/// the emitter emits in 4-lane reordered-unroll form. V1 carries an
/// empty slice.
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VisitorId(pub u16);

/// Per-grammar codegen fingerprint. Emitted once per grammar as
/// `const GRAMMAR_PROFILE: GrammarProfile = GrammarProfile { ... };`
/// at the top of `generated.rs`, next to the grammar string array.
///
/// Every field is `const`-constructible. Static slices reference
/// grammar-local `static` arrays emitted immediately above the
/// profile literal, so the entire profile lives in `.rodata`.
#[derive(Debug, Clone, Copy)]
pub struct GrammarProfile {
    // ── Push-site counts (V1, from PushFingerprint) ──────────────────

    /// Count of emitted rule functions that terminate with
    /// `push_compound(...)` — `MaterializationClass::MustTape`.
    pub push_compound_count: u16,

    /// Count of emitted rule functions that terminate with a plain
    /// `push_leaf(...)` — `TapeSpanOnly` without a payload.
    pub push_leaf_count: u16,

    /// Count of emitted rule functions that terminate with
    /// `push_leaf_with_*(...)` — `TapeSpanOnly` with a scalar or
    /// aggregate payload layout.
    pub push_leaf_with_count: u16,

    // ── Per-byte density estimates (V1, derived from push counts) ────

    /// Estimated compound records produced per input byte. Drives the
    /// `TapeBuilder::with_capacity` reservation.
    pub compounds_per_input_byte: f32,

    /// Estimated leaf records produced per input byte.
    pub leaves_per_input_byte: f32,

    /// Estimated decoded-payload byte output per input byte.
    /// Populated today as a conservative upper bound; V4 refines
    /// with per-scanner actuals.
    pub payload_bytes_per_input_byte: f32,

    // ── Parallel parse cost model (V6) ───────────────────────────────

    /// Calibrated cost — nanoseconds of parse work per input byte
    /// on the reference platform. Used by document-level parallel
    /// parse to decide whether the parallel path amortises its
    /// per-chunk overhead.
    pub expected_ns_per_byte: f32,

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
    pub structural_digraphs: &'static [[u8; 2]],

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

    // ── Columnar substrate selection (V2) ────────────────────────────

    /// Payload columns activated for this grammar. V2 populates.
    pub active_columns: &'static [ColumnId],

    // ── Document-level parallel parse (V6) ───────────────────────────

    /// Rules eligible for chunked parallel parse (top-level lists).
    /// V6 populates.
    pub list_rules: &'static [RuleId],

    // ── Keyword dispatch (V7) ────────────────────────────────────────

    /// Per-rule keyword tables for SIMD wide-compare dispatch. V7
    /// populates.
    pub keyword_tables: &'static [KeywordTable],

    // ── Shape dictionary (V5) ────────────────────────────────────────

    /// Fixed-shape e-class entries eligible for `ShapeRef` deduplication.
    /// V5 populates.
    pub shape_dict: &'static [ShapeEntry],

    // ── Branch priors (V4) ───────────────────────────────────────────

    /// Observed branch priors for speculative Alt dispatch. V4
    /// populates.
    pub branch_priors: &'static [BranchPrior],

    // ── Runtime dedup (V8) ───────────────────────────────────────────

    /// Rules admitted to the runtime bloom+GADT dedup. V8 populates.
    pub dedup_eligible_rules: &'static [RuleId],

    // ── Reorder-unroll visitors (V2) ─────────────────────────────────

    /// Visitor ids for which the emitter produces 4-lane reordered
    /// unrolling. V2 populates.
    pub reorder_unroll_visitors: &'static [VisitorId],
}

impl GrammarProfile {
    /// Empty profile — every count zero, every slice empty. Used as
    /// a `Default` and as the identity value for tests that do not
    /// depend on a concrete grammar's fingerprint.
    pub const EMPTY: GrammarProfile = GrammarProfile {
        push_compound_count: 0,
        push_leaf_count: 0,
        push_leaf_with_count: 0,
        compounds_per_input_byte: 0.0,
        leaves_per_input_byte: 0.0,
        payload_bytes_per_input_byte: 0.0,
        expected_ns_per_byte: 0.0,
        parallel_break_even_bytes: 0,
        structural_alphabet: &[],
        structural_digraphs: &[],
        structural_digraph_mask: [0u64; 4],
        structural_quote_classes: &[],
        active_columns: &[],
        list_rules: &[],
        keyword_tables: &[],
        shape_dict: &[],
        branch_priors: &[],
        dedup_eligible_rules: &[],
        reorder_unroll_visitors: &[],
    };

    /// Reserve size for `TapeBuilder::with_capacity` given an input
    /// length (bytes). Returns `input_len * (compounds + leaves) + 2`
    /// rounded to the nearest integer — the `+ 2` is a one-record
    /// pad covering empty inputs and the end-of-parse compound.
    ///
    /// Callers that need `const` evaluation at codegen time use the
    /// raw `compounds_per_input_byte + leaves_per_input_byte` fields
    /// directly; this helper is a runtime convenience.
    #[inline]
    pub fn capacity_for(&self, input_len: usize) -> usize {
        let per_byte = self.compounds_per_input_byte + self.leaves_per_input_byte;
        ((input_len as f32) * per_byte) as usize + 2
    }

    /// Total emitted push-site count — `push_compound + push_leaf +
    /// push_leaf_with`. Zero for the empty profile.
    #[inline]
    pub const fn total_push_sites(&self) -> u16 {
        self.push_compound_count + self.push_leaf_count + self.push_leaf_with_count
    }
}

impl Default for GrammarProfile {
    fn default() -> Self {
        Self::EMPTY
    }
}

