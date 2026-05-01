//! `ShapeDictSelection` — Tranche AV.5.3 cross-template selection
//! for shape-dictionary admission.
//!
//! Selects up to [`MAX_SHAPE_DICT_ENTRIES`] [`ShapeTemplate`]
//! candidates from the per-grammar pool emitted by
//! [`crate::passes::recognizers::ShapeDictMiner`]. Each surviving
//! template gets one `ShapeEntry` slot in
//! `GrammarProfile::shape_dict`; the runtime DTA emits a `ShapeRef`
//! record on `shape_hash` match.
//!
//! # Selection model
//!
//! Per-template variables `x_c ∈ {include, exclude}` with cost
//! `-freq(c) × savings(c) + static_entry_cost`. Constraint:
//! `Σ include ≤ MAX_SHAPE_DICT_ENTRIES` (the `flags` byte's 5-bit
//! `shape_dict_idx`).
//!
//! Per-template scores are independent (no cross-template
//! interactions on the cost surface), so the budget-cardinality
//! optimization degenerates to greedy top-N. The CSP scaffolding in
//! the per-component strategy solve carries no shape-dict
//! variables; the selection runs as a separate
//! grammar-wide pass via [`solve_shape_dict_selection`] and writes
//! the result to `ir.shape_dict_selection` for the emitter to
//! consume.
//!
//! # Frequency estimate
//!
//! `freq(c) ≈ 1 + leaf_count × 0.5 + constant_count × 0.25` — a
//! cheap, deterministic, structurally-derived estimate that
//! prefers shapes with both rich leaf content and structural
//! punctuation. Refineable from saved samply profiles in a future
//! tranche.
//!
//! # Savings estimate
//!
//! `savings(c) = (records_elided × 16 - packed_payload_size(c))`.
//! Records elided = `1 (parent) + skeleton.len()` (the per-position
//! children the collapsed compound would have emitted).

use super::ConstraintCtx;
use crate::passes::csp_strategy::StrategyDomain;
use crate::passes::recognizers::shape_dict::{ShapeTemplate, TemplatePiece};
use crate::types::TypeDesc;
use crate::GrammarIR;

use csp_solver::Csp;

/// Maximum shape-dictionary entries per grammar.
///
/// Bounded by the 5-bit `shape_dict_idx` field in the runtime
/// `ShapeRef` flags layout. The per-grammar pool of fixed-shape
/// compounds is typically ~10 in production grammars (CSS, JSON,
/// BBNF), so 32 leaves comfortable headroom.
pub const MAX_SHAPE_DICT_ENTRIES: usize = 32;

/// Per-template static cost (bytes of `.rodata` per dictionary
/// entry: ShapeEntry struct + child_kinds slice +
/// leaf_payload_offsets slice). A template's selection score must
/// beat this gate before it earns a dictionary slot.
const STATIC_ENTRY_COST_BYTES: u32 = 32;

/// Result of the shape-dict selection: indices into
/// `ir.shape_dict_templates` for the admitted set.
pub type ShapeDictSelection = Vec<usize>;

/// Run the shape-dict admission selection.
///
/// Walks `ir.shape_dict_templates`, scores each candidate, dedupes
/// by `shape_hash` (keeping the highest-scoring representative per
/// hash), and returns the indices of the top-N survivors (bounded
/// by [`MAX_SHAPE_DICT_ENTRIES`]) under the per-template cost gate.
///
/// The dedup pass is mandatory — the runtime DTA dispatch routes by
/// `shape_hash`, so two admitted entries sharing a hash would
/// silently misroute one to the other's child kinds + payload
/// offsets at parse time.
///
/// The result is sorted ascending so downstream emission sees a
/// deterministic dictionary order across compile sessions.
pub fn solve_shape_dict_selection(ir: &GrammarIR) -> ShapeDictSelection {
    if ir.shape_dict_templates.is_empty() {
        return Vec::new();
    }

    // Build per-template scores. Each score is `freq × savings -
    // static_entry_cost`. Templates whose net benefit is non-
    // positive are excluded immediately — no dictionary slot can
    // amortize a per-instance cost that exceeds its hit savings.
    let mut scored: Vec<(usize, f64)> = ir
        .shape_dict_templates
        .iter()
        .enumerate()
        .filter_map(|(idx, (_, template))| {
            let savings = savings_bytes(template) as f64;
            let freq = freq_estimate(template);
            let net = freq * savings - STATIC_ENTRY_COST_BYTES as f64;
            if net > 0.0 {
                Some((idx, net))
            } else {
                None
            }
        })
        .collect();

    // Dedup by shape_hash — keep the highest-scoring representative
    // per hash. The runtime dispatch is keyed by shape_hash; a
    // duplicate entry would silently misroute at parse time.
    scored.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));
    let mut seen_hashes = std::collections::HashSet::new();
    scored.retain(|(idx, _)| {
        let hash = ir.shape_dict_templates[*idx].1.shape_hash;
        seen_hashes.insert(hash)
    });

    // Truncate to budget after dedup so the budget is over UNIQUE
    // dictionary entries.
    scored.truncate(MAX_SHAPE_DICT_ENTRIES);

    // Stabilise the output by sorting indices ascending so the
    // emitter sees deterministic dictionary order across compile
    // sessions.
    let mut selection: Vec<usize> = scored.into_iter().map(|(idx, _)| idx).collect();
    selection.sort_unstable();
    selection
}

/// Per-component CSP install hook — no-op today.
///
/// Shape-dict selection is grammar-wide and runs separately via
/// [`solve_shape_dict_selection`]. The hook exists alongside
/// [`super::engine::install`] so the constraint pipeline has a
/// uniform shape; future tranches that grow cross-rule shape-dict
/// interactions wire them through this function without disturbing
/// the per-component CSP scaffold.
///
/// Returns the number of constraints installed (always zero today).
pub fn install(_ctx: &ConstraintCtx<'_>, _csp: &mut Csp<StrategyDomain>, _ir: &GrammarIR) -> usize {
    0
}

// ── Cost model ────────────────────────────────────────────────────

/// Estimate the byte savings per dictionary hit.
///
/// `records_elided × 16 - packed_payload_size`. Records elided =
/// `1 (parent compound) + skeleton.len()` (per-position child
/// records the compound would have emitted).
pub fn savings_bytes(template: &ShapeTemplate) -> u32 {
    let records_elided = 1 + template.skeleton.len() as u32;
    let bytes_elided = records_elided.saturating_mul(16);
    let packed_payload = packed_payload_bytes(template);
    bytes_elided.saturating_sub(packed_payload)
}

/// Estimate the byte width of the packed payload blob per instance.
///
/// Each leaf hole contributes its span pair (8 bytes, `(lo, hi)
/// u32`) plus its typed-payload byte width.
pub fn packed_payload_bytes(template: &ShapeTemplate) -> u32 {
    let mut total: u32 = 0;
    for ty in &template.leaf_holes {
        total = total.saturating_add(8); // (lo, hi) span pair
        total = total.saturating_add(type_desc_bytes(ty));
    }
    total
}

/// Approximate byte width for a `TypeDesc` payload. Conservative —
/// variable-width types (Vec, BoxedEnum, anything not directly
/// representable as a fixed-width scalar) round up to an 8-byte
/// arena offset slot.
fn type_desc_bytes(ty: &TypeDesc) -> u32 {
    match ty {
        TypeDesc::Span => 0, // span already counted in packed_payload_bytes
        TypeDesc::F64 | TypeDesc::I64 | TypeDesc::U64 => 8,
        TypeDesc::I32 | TypeDesc::U32 | TypeDesc::I16 | TypeDesc::U16 | TypeDesc::Bool => 4,
        TypeDesc::I8 | TypeDesc::U8 => 1,
        TypeDesc::Tuple(fields) => fields.iter().map(type_desc_bytes).sum(),
        TypeDesc::Option(inner) => 4 + type_desc_bytes(inner),
        // Variable-length / boxed types collapse to an 8-byte arena
        // offset.
        _ => 8,
    }
}

/// Static frequency estimate. Without per-input profiling, every
/// template is assumed to fire once per parent compound, scaled by
/// the leaf-hole and constant-piece counts.
///
/// More leaf holes → higher likelihood of recurrence in an input
/// that exercises the shape repeatedly. More structural constants
/// (literals, whitespace) → higher likelihood the template captures
/// a canonical declaration-style shape worth dedup'ing.
fn freq_estimate(template: &ShapeTemplate) -> f64 {
    let leaf_count = template.leaf_holes.len() as f64;
    let constant_count = template
        .skeleton
        .iter()
        .filter(|p| matches!(p, TemplatePiece::Literal(_) | TemplatePiece::Whitespace))
        .count() as f64;
    1.0 + leaf_count * 0.5 + constant_count * 0.25
}
