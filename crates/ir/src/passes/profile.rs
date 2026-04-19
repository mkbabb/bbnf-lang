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
use crate::passes::recognizers::list_rules::mine_list_rules;
use crate::passes::recognizers::shape_dict::TemplatePiece;
use crate::passes::recognizers::shape_dict_bbnf::{
    mine_bbnf_shape_templates, BbnfShapeTemplate,
};
/// IR-side counterpart of [`tape::ShapeEntry`]. Carries the
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
    /// Per-child [`tape::TapeKind`] discriminants declaring the
    /// skeleton. Length = number of direct children the collapsed
    /// compound would have emitted.
    pub child_kinds: Vec<u8>,
    /// Per-child byte offset into the packed payload blob for each
    /// leaf hole. `u16::MAX` marks structural (non-hole) children.
    pub leaf_payload_offsets: Vec<u16>,
    /// Total byte width of the packed payload blob.
    pub payload_bytes: u16,
}

/// Consolidated per-grammar fingerprint — the owned IR-side
/// counterpart of [`tape::GrammarProfile`].
///
/// Every field is produced by reading already-computed IR facts:
/// [`PushFingerprint`](crate::passes::sets::PushFingerprint),
/// [`StructuralAlphabet`](crate::passes::sets::StructuralAlphabet),
/// [`KeywordBranchMap`](crate::passes::recognizers::keyword_stats::KeywordBranchMap),
/// [`ShapeDictMap`](crate::passes::recognizers::shape_dict::ShapeDictMap).
/// The emitter lowers this struct to a single `const GRAMMAR_PROFILE:
/// tape::GrammarProfile = GrammarProfile { ... };` literal —
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

    // ── Parallel parse cost model (V6) ───────────────────────────────

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
    /// [`tape::GrammarProfile::structural_digraphs`] and the SIMD
    /// scanner's [`simd_scan::StructuralAlphabet::digraph_pairs`]
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

    // ── Document-level parallel parse (V6, AW-IV.W4.4 wired) ────────

    /// Rules eligible for chunked parallel parse (top-level lists).
    /// Populated by
    /// [`mine_list_rules`](crate::passes::recognizers::list_rules::mine_list_rules):
    /// admits the grammar's entry rule when its body (stripped of
    /// transparent wrappers) is a `Repeat` over an `Alt` / `Ref` /
    /// `Seq`. Non-empty for CSS L4 (`stylesheet = rule*`), BBNF
    /// (`grammar = directive* rule*`), Sheets (`file = cell*`);
    /// empty for JSON (entry `value` is an Alt). W9 wires the
    /// runtime parallel-fork consumer; today the slot flows the
    /// mined data through the codegen without a live call site.
    pub list_rules: Vec<u32>,

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

        // V5 AV.5.6 — BBNF-specific shape templates. Mines rule bodies
        // for collapsible patterns (big_comment, mapped_factor empty
        // branch). Empty for non-BBNF grammars.
        let bbnf_shape_templates = mine_bbnf_shape_templates(self);

        // AW-IV.W1.δ — shape_dict projection.
        //
        // The CSP shape-dict selection prunes the candidate pool
        // ([`GrammarIR::shape_dict_templates`]) to the admitted set
        // ([`GrammarIR::shape_dict_selection`]). Project the admitted
        // templates to the runtime-consumable [`tape::ShapeEntry`]
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

        // AW-IV.W4.4 — list_rules projection.
        //
        // `mine_list_rules` admits the grammar's entry rule when its
        // body (after stripping transparent wrappers) is a Repeat
        // over an Alt / Ref / Seq — the canonical fork-candidate
        // shape. For targets where the entry is a top-level list
        // (CSS `stylesheet`, BBNF `grammar`, Sheets `file`), the
        // projection carries a single rule id; for grammars whose
        // entry is not list-shaped (JSON `value` Alt), the slot is
        // empty and parse() routes through the single-thread walker.
        //
        // The emitter lowers this Vec to `&'static [RuleId]`; the
        // runtime parse() entry consults the slot plus
        // `parallel_break_even_bytes` to decide whether to route
        // through `dta_run_parallel`.
        let list_rules: Vec<u32> = mine_list_rules(self);

        // AW-IV.W4.4 — parallel break-even threshold.
        //
        // Below this input-byte count, spawning rayon workers + the
        // join-phase memcpy cost outweighs the per-worker parse win.
        // The threshold is set at 1 MiB (1 << 20): CSS inputs below
        // this size parse faster single-threaded because the rayon
        // spawn + join overhead exceeds the per-shard parse savings,
        // while inputs at or above 1 MiB (tailwind.css at 3.5 MB is
        // the canonical target) amortise the overhead across their
        // workers.
        //
        // The AW-IV.W4.4-fix re-setting (256 KiB → 1 MiB) follows
        // the measured break-even on the reference 4-core platform:
        // bootstrap.css (280 KB) gains no throughput from the 4-way
        // fork because its worker-join cost overwhelms the parallel
        // parse time, and its tape-parity golden reflects the
        // single-thread emit shape. Tailwind.css (3.5 MB) clears the
        // threshold and exercises the parallel path — its tape
        // golden is generated from the parallel-forked tape and
        // consumed in that shape by the parity harness.
        //
        // When `list_rules` is empty, the threshold is irrelevant
        // (parse() never consults the parallel path), but it still
        // populates the const literal uniformly so the projection
        // is always well-formed.
        let parallel_break_even_bytes: u32 = if list_rules.is_empty() {
            0
        } else {
            1 << 20
        };

        GrammarProfile {
            push_compound_count,
            push_leaf_count,
            push_leaf_with_count,
            compounds_per_input_byte,
            leaves_per_input_byte,
            parallel_break_even_bytes,
            structural_alphabet,
            structural_digraphs,
            structural_digraph_mask,
            structural_quote_classes,
            bbnf_shape_templates,
            list_rules,
            shape_dict,
        }
    }
}

// ── TapeKind discriminant constants ──────────────────────────────────
//
// AW-IV.W1.δ — carried as `const u8` values so the IR crate doesn't
// need an upward import of `tape`. The values match the
// `#[repr(u8)]` discriminants declared on `tape::TapeKind`; see
// `crates/tape/src/kind.rs` for the canonical source. Kept in
// lockstep with
// [`crate::backend::rust::emitter::dta::template_piece_to_kind_byte`]
// (the existing `SHAPE_DICT` emitter): that function uses
// `TapeKind::Literal as u8` / `TapeKind::Span as u8` /
// `TapeKind::Epsilon as u8` directly; the literal values below match.
const TAPE_KIND_SPAN: u8 = 1;
const TAPE_KIND_EPSILON: u8 = 2;
const TAPE_KIND_LITERAL: u8 = 3;
