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
use crate::passes::recognizers::shape_dict::TemplatePiece;
use crate::passes::recognizers::shape_dict_bbnf::{
    mine_bbnf_shape_templates, BbnfShapeTemplate,
};
use crate::passes::recognizers::visitor::{mine_visitors, VisitorDescriptor};

/// IR-side counterpart of [`bbnf_tape::KeywordTable`]. Carries the
/// mined Alt's rule id and the sorted keyword byte slices the emitter
/// will lower to `&'static [&'static [u8]]`.
///
/// AW-IV.W1.δ — populated from [`GrammarIR::keyword_branches`] by
/// projecting each Alt NodeId back to the enclosing rule id via the
/// rule body's DAG node. Only rules whose Alt body was directly mined
/// by [`crate::passes::recognizers::keyword_stats::KeywordStatsMiner`]
/// surface here; the AltLinear consumer reads this slot directly at
/// codegen time.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct KeywordTableIr {
    /// Rule id the keyword table fires for. Matches
    /// [`bbnf_tape::RuleId`](bbnf_tape::profile::RuleId) on emission.
    pub rule_id: u32,
    /// Keyword bytes for the Alt's literal-led branches, sorted
    /// lexicographically for binary-search dispatch. The emitter
    /// lowers this to `&'static [&'static [u8]]`.
    pub keywords: Vec<Vec<u8>>,
}

/// IR-side counterpart of [`bbnf_tape::ShapeEntry`]. Carries the
/// shape hash, owning rule id, per-child kind discriminants, per-hole
/// payload offsets, and total packed payload byte width.
///
/// AW-IV.W1.δ — populated from [`GrammarIR::shape_dict_templates`] +
/// [`GrammarIR::shape_dict_selection`]. The selection pruned the
/// candidate pool under the 256-entry budget; this projection lowers
/// the survivors to the runtime-consumable shape.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ShapeEntryIr {
    /// Canonical 64-bit shape hash — matches
    /// [`ShapeTemplate::shape_hash`](crate::passes::recognizers::shape_dict::ShapeTemplate::shape_hash).
    pub shape_hash: u64,
    /// Rule id the shape-ref expands to at view time.
    pub rule_id: u32,
    /// Per-child [`bbnf_tape::TapeKind`] discriminants declaring the
    /// skeleton. Length = number of direct children the collapsed
    /// compound would have emitted.
    pub child_kinds: Vec<u8>,
    /// Per-child byte offset into the packed payload blob for each
    /// leaf hole. `u16::MAX` marks structural (non-hole) children.
    pub leaf_payload_offsets: Vec<u16>,
    /// Total byte width of the packed payload blob.
    pub payload_bytes: u16,
}

/// IR-side counterpart of [`bbnf_tape::BranchPrior`]. V4 mining
/// substrate; empty today so the emitter carries an empty static.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BranchPriorIr {
    /// Alt rule id the prior applies to.
    pub rule_id: u32,
    /// Selected-branch index for speculative dispatch.
    pub branch_idx: u16,
    /// Observed-prior weight in the 0..=255 quantised domain.
    pub weight_q8: u8,
}

/// Consolidated per-grammar fingerprint — the owned IR-side
/// counterpart of [`bbnf_tape::GrammarProfile`].
///
/// Every field is produced by reading already-computed IR facts:
/// [`PushFingerprint`](crate::passes::sets::PushFingerprint),
/// [`StructuralAlphabet`](crate::passes::sets::StructuralAlphabet),
/// [`KeywordBranchMap`](crate::passes::recognizers::keyword_stats::KeywordBranchMap),
/// [`ShapeDictMap`](crate::passes::recognizers::shape_dict::ShapeDictMap).
/// The emitter lowers this struct to a single `const GRAMMAR_PROFILE:
/// bbnf_tape::GrammarProfile = GrammarProfile { ... };` literal —
/// the `&'static` slices in the tape-side struct become `&'static`
/// references to `static` arrays emitted alongside the literal.
///
/// AW-IV.W1.δ fills every previously-`&[]` slot (`active_columns`,
/// `list_rules`, `keyword_tables`, `shape_dict`, `branch_priors`,
/// `dedup_eligible_rules`) so the projection from IR mining through
/// the emitter pass to the runtime `pub const GRAMMAR_PROFILE` literal
/// is no longer a silent data-drop. Slots whose upstream mining has
/// not yet wired (W3 for `active_columns`, `list_rules`,
/// `branch_priors`, `dedup_eligible_rules`) default to empty Vecs —
/// the projection still flows; the empty set is the legitimate
/// current state.
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

    // ── Columnar substrate selection (V2, AW-IV.W1.δ wire-contract) ──

    /// Payload columns activated for this grammar. V2 consumer
    /// substrate. Empty today — the grammar-driven column-activation
    /// pass has not yet landed. The projection is live so the emitter
    /// emits a valid `&'static [ColumnId]` reference (empty slice).
    /// W3 wires the mining; the projection does not change.
    pub active_columns: Vec<u16>,

    // ── Document-level parallel parse (V6, AW-IV.W1.δ wire-contract) ─

    /// Rules eligible for chunked parallel parse (top-level lists).
    /// V6 substrate; empty today — the `list_rules` recogniser is
    /// scheduled in W4.4. The projection flows so the runtime const
    /// carries a valid reference.
    pub list_rules: Vec<u32>,

    // ── Keyword dispatch (V7, AW-IV.W1.δ wire-contract) ──────────────

    /// Per-rule keyword tables for SIMD / binary-search dispatch.
    /// Populated from [`GrammarIR::keyword_branches`] via
    /// [`GrammarIR::profile`]: for each Alt NodeId the miner
    /// recorded, the projection maps back to the enclosing rule id
    /// by matching the rule body's DAG node and lowers the
    /// per-branch byte vectors to the runtime
    /// [`bbnf_tape::KeywordTable`] shape.
    pub keyword_tables: Vec<KeywordTableIr>,

    // ── Shape dictionary runtime entries (V5, AW-IV.W1.δ wire-contract)

    /// Shape-dictionary entries for `ShapeRef` deduplication at the
    /// runtime tape. Populated from the CSP-pruned subset of
    /// [`GrammarIR::shape_dict_templates`] indexed by
    /// [`GrammarIR::shape_dict_selection`]. Each entry carries the
    /// canonical shape hash, owning rule id, per-child kind
    /// discriminants, per-hole payload offsets, and the total packed
    /// payload byte width the runtime uses to rebuild the collapsed
    /// subtree at view time.
    pub shape_dict: Vec<ShapeEntryIr>,

    // ── Branch priors (V4, AW-IV.W1.δ wire-contract) ─────────────────

    /// Observed branch priors for speculative Alt dispatch. V4
    /// substrate; empty today. Projection live so the emitter carries
    /// an empty static and the runtime const references it.
    pub branch_priors: Vec<BranchPriorIr>,

    // ── Runtime dedup eligibility (V8, AW-IV.W1.δ wire-contract) ─────

    /// Rules admitted to the runtime bloom + GADT dedup pass. V8
    /// substrate; empty today — W4.3 wires the `dedup_eligibility`
    /// miner. Projection live so the const flows the mined data
    /// without any further emitter changes when W4.3 lands.
    pub dedup_eligible_rules: Vec<u32>,
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

        // AW-IV.W1.δ — keyword_tables projection.
        //
        // The [`KeywordStatsMiner`] indexes its output by Alt
        // `NodeId`; the runtime `KeywordTable` struct indexes by
        // `RuleId`. Project by walking every rule, looking up its
        // body's NodeId via the DAG, and matching against the mined
        // map. Transparent rules are skipped (their Alt surfaces
        // under the enclosing rule's keyword table, consistent with
        // the AW-III.W6.2 `keyword_phf_tables` emission in
        // `emitter/grammar.rs`).
        let keyword_tables: Vec<KeywordTableIr> = if let Some(dag) = self.dag.as_ref() {
            let mut tables: Vec<KeywordTableIr> = Vec::new();
            for rule in &self.rules {
                if rule.meta.is_transparent {
                    continue;
                }
                let Some(body_id) = dag.node_for(&rule.body) else { continue };
                if let Some(branches) = self.keyword_branches.get(&body_id) {
                    if branches.is_empty() {
                        continue;
                    }
                    // Sort keywords lexicographically for determinism
                    // and to match the emitter's binary-search shape.
                    let mut kws: Vec<Vec<u8>> =
                        branches.iter().map(|b| b.bytes.clone()).collect();
                    kws.sort();
                    kws.dedup();
                    tables.push(KeywordTableIr {
                        rule_id: rule.id,
                        keywords: kws,
                    });
                }
            }
            // Deterministic output order: by rule_id ascending.
            tables.sort_by_key(|t| t.rule_id);
            tables
        } else {
            Vec::new()
        };

        // AW-IV.W1.δ — shape_dict projection.
        //
        // The CSP shape-dict selection prunes the candidate pool
        // ([`GrammarIR::shape_dict_templates`]) to the admitted set
        // ([`GrammarIR::shape_dict_selection`]). Project the admitted
        // templates to the runtime-consumable [`bbnf_tape::ShapeEntry`]
        // shape: each template's `skeleton` maps to per-child
        // `TapeKind` discriminants; each `LeafHole` contributes a
        // packed-payload offset; constant positions carry the
        // `u16::MAX` sentinel.
        let shape_dict: Vec<ShapeEntryIr> = {
            let mut entries: Vec<ShapeEntryIr> = Vec::new();
            for &idx in &self.shape_dict_selection {
                let Some((node_id, template)) = self.shape_dict_templates.get(idx) else {
                    continue;
                };
                // Resolve the owning rule by finding the rule whose
                // body DAG node matches this template's NodeId.
                let Some(dag) = self.dag.as_ref() else { continue };
                let owning_rule_id = self
                    .rules
                    .iter()
                    .find(|rule| dag.node_for(&rule.body) == Some(*node_id))
                    .map(|rule| rule.id);
                let Some(rule_id) = owning_rule_id else {
                    // Template references an interior node; without
                    // an enclosing rule id the runtime cannot route
                    // `ShapeRef` dispatch back to a view type. Skip.
                    continue;
                };

                // Build the parallel `child_kinds` + `leaf_payload_offsets`
                // arrays from the skeleton. Per-piece mapping mirrors the
                // AW-III.W6.1 mapping in
                // [`crate::backend::rust::emitter::dta::template_piece_to_kind_byte`]
                // (kept in lockstep; see module doc-comment at the
                // bottom of this file for the stable discriminant
                // table).
                let mut child_kinds: Vec<u8> = Vec::with_capacity(template.skeleton.len());
                let mut leaf_payload_offsets: Vec<u16> =
                    Vec::with_capacity(template.skeleton.len());
                let mut offset_cursor: u16 = 0;
                for piece in &template.skeleton {
                    match piece {
                        TemplatePiece::Literal(_) => {
                            child_kinds.push(TAPE_KIND_LITERAL);
                            leaf_payload_offsets.push(u16::MAX);
                        }
                        TemplatePiece::LeafHole => {
                            child_kinds.push(TAPE_KIND_SPAN);
                            leaf_payload_offsets.push(offset_cursor);
                            // Each hole contributes an (lo, hi) span
                            // pair = 8 bytes; typed payload (if any)
                            // is appended by the codegen in a later
                            // wave. V1 carries the span-only offset.
                            offset_cursor = offset_cursor.saturating_add(8);
                        }
                        TemplatePiece::Whitespace => {
                            // Whitespace pieces carry no distinct record
                            // at view time — the `emit_dta::template_piece_to_kind_byte`
                            // mapping treats them as `Span` so the
                            // synthetic cursor has a uniform shape.
                            child_kinds.push(TAPE_KIND_SPAN);
                            leaf_payload_offsets.push(u16::MAX);
                        }
                        TemplatePiece::Epsilon => {
                            child_kinds.push(TAPE_KIND_EPSILON);
                            leaf_payload_offsets.push(u16::MAX);
                        }
                    }
                }

                entries.push(ShapeEntryIr {
                    shape_hash: template.shape_hash,
                    rule_id,
                    child_kinds,
                    leaf_payload_offsets,
                    payload_bytes: offset_cursor,
                });
            }
            // Deterministic output: by (rule_id, shape_hash).
            entries.sort_by_key(|e| (e.rule_id, e.shape_hash));
            entries
        };

        // AW-IV.W1.δ — V2/V4/V6 slots whose upstream mining has
        // not yet wired. The projection carries empty Vecs so the
        // const-literal emission is always well-formed; when W3 or
        // W4 lands the mining pass, the projection composes without
        // further emitter changes.
        let active_columns: Vec<u16> = Vec::new();
        let list_rules: Vec<u32> = Vec::new();
        let branch_priors: Vec<BranchPriorIr> = Vec::new();

        // AW-IV.W4.3 — dedup-eligible rules projection. Read from the
        // IR slot populated by
        // `passes::recognizers::dedup_eligibility::mine_dedup_eligible_rules`.
        // The mining pass runs in `analyze_grammar` before this
        // accessor is called; when the mining has not yet executed
        // (tests constructing IR directly) the slot is empty and the
        // projection flows an empty Vec without panicking.
        let dedup_eligible_rules: Vec<u32> = self.dedup_eligible_rules.clone();

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
            active_columns,
            list_rules,
            keyword_tables,
            shape_dict,
            branch_priors,
            dedup_eligible_rules,
        }
    }
}

// ── TapeKind discriminant constants ──────────────────────────────────
//
// AW-IV.W1.δ — carried as `const u8` values so the IR crate doesn't
// need an upward import of `bbnf_tape`. The values match the
// `#[repr(u8)]` discriminants declared on `bbnf_tape::TapeKind`; see
// `crates/bbnf-tape/src/kind.rs` for the canonical source. Kept in
// lockstep with
// [`crate::backend::rust::emitter::dta::template_piece_to_kind_byte`]
// (the existing `SHAPE_DICT` emitter): that function uses
// `TapeKind::Literal as u8` / `TapeKind::Span as u8` /
// `TapeKind::Epsilon as u8` directly; the literal values below match.
const TAPE_KIND_SPAN: u8 = 1;
const TAPE_KIND_EPSILON: u8 = 2;
const TAPE_KIND_LITERAL: u8 = 3;
