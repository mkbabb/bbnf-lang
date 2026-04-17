//! `GrammarIR::profile()` — consolidated per-grammar fingerprint
//! accessor (Tranche AV Phase 1).
//!
//! # Architectural role
//!
//! Pre-AV the fingerprint data was scattered: push counts on
//! [`GrammarIR::push_fingerprint`], the structural alphabet on
//! [`GrammarIR::structural_alphabet`], shape hashes on
//! `RecognizerSignature`, fixed-shape bits on [`EClassFacts`]. Every
//! downstream emitter re-derived or re-plumbed the same data.
//!
//! AV.1.1 introduces [`GrammarProfile`] — a single owned struct that
//! aggregates every already-computed fingerprint fact into one shape.
//! [`GrammarIR::profile`] is the one accessor emitters call; the
//! emitter then lowers this owned struct to the `const` declaration
//! in `generated.rs` (`crates/core/src/backend/rust/emitter/profile.rs`).
//!
//! Some slots are empty in V1 and populated in later waves (see the
//! `GrammarProfile` field docs for the V2–V9 allocation). The single
//! accessor is the handoff point; later waves extend it, never
//! bypass it.
//!
//! [`EClassFacts`]: crate::egraph::analysis::EClassFacts

use crate::GrammarIR;
use crate::passes::recognizers::shape_dict_bbnf::{
    mine_bbnf_shape_templates, BbnfShapeTemplate,
};
use crate::passes::recognizers::visitor::{mine_visitors, VisitorDescriptor};

/// Consolidated per-grammar fingerprint — the owned IR-side
/// counterpart of [`bbnf_tape::GrammarProfile`].
///
/// Every field is produced by reading already-computed IR facts:
/// [`PushFingerprint`](crate::passes::sets::PushFingerprint),
/// [`StructuralAlphabet`](crate::passes::sets::StructuralAlphabet).
/// The emitter lowers this struct to a single `const GRAMMAR_PROFILE:
/// bbnf_tape::GrammarProfile = GrammarProfile { ... };` literal —
/// the `&'static` slices in the tape-side struct become `&'static`
/// references to `static` arrays emitted alongside the literal.
///
/// The slots that later waves populate
/// (`active_columns`, `list_rules`, `keyword_tables`, `shape_dict`,
/// `branch_priors`, `dedup_eligible_rules`, `reorder_unroll_visitors`)
/// are absent from this V1 precursor. When a later wave lands, it
/// extends this struct — it does not spawn a parallel accessor.
#[derive(Clone, Debug, Default)]
pub struct GrammarProfile {
    // ── Push-site counts (V1) ────────────────────────────────────────

    /// Rules whose emitted function terminates with
    /// `push_compound(...)`.
    pub push_compound_count: u16,

    /// Rules whose emitted function terminates with a plain
    /// `push_leaf(...)` — no payload.
    pub push_leaf_count: u16,

    /// Rules whose emitted function terminates with
    /// `push_leaf_with_*(...)` — scalar or aggregate payload.
    pub push_leaf_with_count: u16,

    // ── Per-byte density estimates (V1) ──────────────────────────────

    /// Estimated compound records produced per input byte.
    pub compounds_per_input_byte: f32,

    /// Estimated leaf records produced per input byte.
    pub leaves_per_input_byte: f32,

    /// Estimated decoded-payload byte output per input byte.
    /// Conservative upper bound at V1; V4 refines with per-scanner
    /// actuals.
    pub payload_bytes_per_input_byte: f32,

    // ── Parallel parse cost model (V6) ───────────────────────────────

    /// Expected ns of parse work per input byte on the reference
    /// platform. V6 populates; V1 defaults to 0.
    pub expected_ns_per_byte: f32,

    /// Minimum input size (bytes) at which parallel parse beats
    /// sequential. V6 populates; V1 defaults to 0 (no parallel path).
    pub parallel_break_even_bytes: u32,

    // ── Byte-class dispatch (V1, from StructuralAlphabet; AW-III.W5.a extends) ─

    /// Sorted single-byte structural alphabet. Empty when the
    /// grammar has no structural-alphabet fingerprint or the
    /// alphabet lies outside the nibble-LUT window (2..=8 bytes).
    pub structural_alphabet: Vec<u8>,

    /// Two-byte digraphs observed at scanner boundaries.
    ///
    /// Stored as `(first, second)` tuples — the emitter lowers this
    /// directly to a `&'static [(u8, u8)]` that feeds the tape-side
    /// [`bbnf_tape::GrammarProfile::structural_digraphs`] and the SIMD
    /// scanner's [`bbnf_simd_scan::StructuralAlphabet::digraph_pairs`]
    /// with no shim layer (AW-III.W5.d).
    pub structural_digraphs: Vec<(u8, u8)>,

    /// 256-bit bitmap of `structural_digraphs` first-bytes, packed
    /// as four `u64` words. Pre-computed at IR time so the SIMD
    /// kernel masks candidate-opener lanes in one ANDS without a
    /// derefenced loop. AW-III.W5.a.
    pub structural_digraph_mask: [u64; 4],

    /// Sorted bytes that toggle string mode. Mined from
    /// `IrNode::Regex` whose classification is
    /// `RegexClass::QuotedString`. Drives the SIMD kernel's
    /// quote-parity correction. AW-III.W5.a.
    pub structural_quote_classes: Vec<u8>,

    // ── Reorder-unroll visitors (V2, AV.2.5) ─────────────────────────

    /// Visitor-like reductions admitted by this grammar over its typed
    /// payload columns. Each descriptor lowers to a 4-lane reordered-
    /// unrolling kernel function in `generated.rs`; the kernel pattern
    /// defeats strict-IEEE `f64` non-associativity and lets LLVM
    /// vectorise the accumulator on NEON / AVX2.
    ///
    /// Populated by [`mine_visitors`] via
    /// [`GrammarIR::profile`]. The current grammar-side
    /// `@visitor` directive is not wired; the field is empty on master
    /// grammars today, and tests populate the tape-side slice directly
    /// via [`VisitorDescriptor::canonical`] + manual profile
    /// construction. When the directive lands, `mine_visitors` walks
    /// the parsed descriptors; this accessor signature does not change.
    pub reorder_unroll_visitors: Vec<VisitorDescriptor>,

    // ── Shape dictionary (V5, AV.5.6) ───────────────────────────────

    /// BBNF-specific shape templates mined from the grammar IR.
    ///
    /// Each entry describes a rule body whose multi-record tape skeleton
    /// collapses to a single ShapeRef record. The emitter checks this
    /// list at codegen time; rules with a matching template emit the
    /// collapsed ShapeRef path instead of the normal per-rule skeleton.
    ///
    /// Populated by [`mine_bbnf_shape_templates`] via
    /// [`GrammarIR::profile`]. Empty for non-BBNF grammars.
    pub bbnf_shape_templates: Vec<BbnfShapeTemplate>,
}

impl GrammarIR {
    /// Consolidated fingerprint accessor. Must be called after the
    /// fingerprint-producing passes have run — in
    /// `crates/core/src/backend/driver/analysis.rs` this is
    /// immediately after `compute_push_fingerprint` + the prior
    /// `compute_structural_alphabet` call.
    ///
    /// Safe to call on an IR that has not yet run the fingerprint
    /// passes: the accessor falls back to `Default` for each missing
    /// slot. The emitter treats the absence as "zero counts / empty
    /// slices"; no panic, no hidden re-computation.
    pub fn profile(&self) -> GrammarProfile {
        let push = self.push_fingerprint.as_ref();
        let push_compound_count = push.map(|fp| fp.compound_pushes as u16).unwrap_or(0);
        let push_leaf_count = push.map(|fp| fp.leaf_pushes as u16).unwrap_or(0);
        let push_leaf_with_count = push.map(|fp| fp.leaf_with_pushes as u16).unwrap_or(0);

        // Density coefficients: `PushFingerprint::capacity_ratio()`
        // returns `(numer, denom)` such that `records/byte ≈ numer /
        // denom`. Split that ratio into per-class contributions by
        // the in-grammar compound/leaf/leaf_with share. The tape-side
        // reader recombines them into the same capacity estimate
        // (see `GrammarProfile::capacity_for`).
        let (numer, denom) = push
            .map(|fp| fp.capacity_ratio())
            .unwrap_or((1, 2));
        let records_per_byte = (numer as f32) / (denom as f32);
        let total_pushes =
            (push_compound_count as u32 + push_leaf_count as u32 + push_leaf_with_count as u32)
                .max(1);
        let compound_share = push_compound_count as f32 / total_pushes as f32;
        let leaf_share =
            (push_leaf_count as u32 + push_leaf_with_count as u32) as f32 / total_pushes as f32;
        let compounds_per_input_byte = records_per_byte * compound_share;
        let leaves_per_input_byte = records_per_byte * leaf_share;

        // Payload-byte density is a V4 refinement target — today a
        // conservative upper bound derived from `push_leaf_with`
        // density. Aggregates peak at 16 B per site; leaf-with rules
        // fire at most once per byte in the worst case. V4 replaces
        // this with per-scanner profiling output.
        let payload_bytes_per_input_byte =
            records_per_byte * (push_leaf_with_count as f32 / total_pushes as f32) * 16.0;

        let (
            structural_alphabet,
            structural_digraphs,
            structural_digraph_mask,
            structural_quote_classes,
        ) = match self.structural_alphabet.as_ref() {
            Some(alphabet) => (
                alphabet.single_bytes_vec(),
                alphabet.digraphs.clone(),
                alphabet.digraph_mask,
                alphabet.quote_classes_vec(),
            ),
            None => (Vec::new(), Vec::new(), [0u64; 4], Vec::new()),
        };

        // V2 AV.2.5 — visitor-recognition pass. Empty for every
        // grammar shipped today because the grammar-side `@visitor`
        // directive is not yet wired; tests exercise the emitter via
        // synthetic profile construction. See `mine_visitors` for the
        // forward plan.
        let reorder_unroll_visitors = mine_visitors(self);

        // V5 AV.5.6 — BBNF-specific shape templates. Mines rule bodies
        // for collapsible patterns (big_comment, mapped_factor empty
        // branch). Empty for non-BBNF grammars.
        let bbnf_shape_templates = mine_bbnf_shape_templates(self);

        GrammarProfile {
            push_compound_count,
            push_leaf_count,
            push_leaf_with_count,
            compounds_per_input_byte,
            leaves_per_input_byte,
            payload_bytes_per_input_byte,
            expected_ns_per_byte: 0.0,
            parallel_break_even_bytes: 0,
            structural_alphabet,
            structural_digraphs,
            structural_digraph_mask,
            structural_quote_classes,
            reorder_unroll_visitors,
            bbnf_shape_templates,
        }
    }
}
