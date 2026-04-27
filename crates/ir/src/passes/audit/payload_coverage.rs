//! Typed-`->` marker coverage audit.
//!
//! `audit_payload_coverage` walks a `GrammarIR`'s rule graph and
//! reports every typed `->` site against the substrate that is
//! supposed to consume it (the per-rule `StructLayout` in
//! `StructRegistry`, populated by `project_types` once W1 closure
//! lands). The pass returns a [`GrammarCoverage`] row tagged with the
//! caller-supplied [`GrammarAuditTag`]. Reports compose into an
//! [`AuditCoverageReport`] which serialises to JSON for the build
//! gate and downstream tooling.
//!
//! The pass is read-only — never mutates `&GrammarIR`. The single
//! source of truth for what counts as a typed `->` marker is the
//! [`is_typed_arrow_fn`] classifier: every `IrNode::Map` whose
//! `FnDescriptor` is one of the compiler-recognised typed-leaf
//! shapes (`Expr` with explicit return type, plus the lowering-
//! specialised `EnumWrap`, `BoxWrap`, `NumberConvert`, `HexConvert`,
//! `SpanCapture`) qualifies. All such variants originate from a
//! grammar-author `->` site that the lowering pipeline rewrote into a
//! more specific descriptor; the audit treats them all as one
//! coverage population.
//!
//! Coverage status per marker has three values:
//!
//! - `Mapped` — the enclosing rule has a registered `StructLayout`
//!   in `StructRegistry` *and* the layout admits the marker's type.
//! - `Pending` — the enclosing rule has no `StructRegistry` entry
//!   because closure has not yet run on this grammar (W0 baseline
//!   state — the registry surface itself does not yet exist).
//! - `Missing` — the enclosing rule has a `StructRegistry` entry
//!   *but* the layout does not cover the marker's type. This is the
//!   red signal W1 onward must drive to zero on JSON, CSS L4, and
//!   Sheets.
//!
//! The classifier between `Pending` and `Missing` lives in the
//! [`StructRegistryProbe`] trait. The trait has two ready
//! implementations:
//!
//! - [`AbsentRegistryProbe`] — declares no rule mapped, every marker
//!   `Pending`. Used for the W0 baseline before W1 lands the registry
//!   substrate.
//! - [`PayloadLayoutsProbe`] — reads `GrammarIR::payload_layouts`
//!   (populated by `compute_payload_layouts` in `finalize_compile`)
//!   as a present-day proxy. Once W1 lands the proper `StructRegistry`,
//!   a third probe fronts that surface and the existing two stay in
//!   place for backward audit visualisation.
//!
//! No probe panics on unknown rule kinds; the rule walker enumerates
//! every `IrNode` variant exhaustively (see `walk_typed_arrows`). A
//! variant added without a matching arm fails the build at
//! compile time.

use std::collections::HashMap;
use std::path::Path;

use serde::{Deserialize, Serialize};

use crate::passes::PayloadLayout;
use crate::types::{FnDescriptor, FnId, GrammarIR, IrNode, IrRule, RuleId, TypeDesc};

// ── Public report shape ─────────────────────────────────────────────────

/// Identifier for the grammar a coverage row describes. `Custom`
/// admits in-tree audits of grammars not in the AZ-I three-grammar
/// slice; the `&'static str` ride-along is the JSON output's row key.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum GrammarAuditTag {
    Json,
    CssL4,
    Sheets,
    Custom(&'static str),
}

impl GrammarAuditTag {
    /// Stable key used in the JSON output.
    pub fn key(&self) -> &'static str {
        match self {
            GrammarAuditTag::Json => "json",
            GrammarAuditTag::CssL4 => "css_l4",
            GrammarAuditTag::Sheets => "sheets",
            GrammarAuditTag::Custom(s) => s,
        }
    }
}

/// Per-marker status — what the substrate says about the typed `->`
/// site at runtime.
#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[serde(rename_all = "lowercase")]
pub enum MarkerStatus {
    /// Enclosing rule has a `StructLayout` and the layout admits the
    /// marker's type.
    Mapped,
    /// Enclosing rule has no registry entry yet (registry closure
    /// has not run on this grammar). W0-baseline default.
    Pending,
    /// Enclosing rule has a registry entry but the layout fails to
    /// cover the marker's type — the red signal.
    Missing,
}

/// A single `Pending` marker — preserves enough context to point
/// operators at the rule whose registry entry is awaited.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct PendingMarker {
    /// Enclosing rule id (stable within the GrammarIR).
    pub rule_id: RuleId,
    /// Enclosing rule name (resolved from `ir.strings`).
    pub rule_name: String,
    /// `FnId` of the typed-leaf function descriptor.
    pub fn_id: FnId,
    /// Resolved typed-leaf shape (from the FnDescriptor + TypeMap).
    /// `None` when the FnDescriptor encodes a flag (`BoxWrap`,
    /// `EnumWrap`) without an explicit return type — the enclosing
    /// rule's projected type is the source of truth in that case.
    pub typed_leaf: Option<TypeDesc>,
    /// FnDescriptor variant tag, for diagnostic display.
    pub fn_kind: String,
}

/// A single `Missing` marker — the red signal. Same shape as
/// `PendingMarker`, separated by status so JSON consumers can pick
/// out the must-fix population.
pub type MissingMarker = PendingMarker;

/// Per-grammar coverage row.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct GrammarCoverage {
    /// Stable grammar key (`json`, `css_l4`, `sheets`, ...).
    pub grammar: String,
    /// Total typed `->` markers in the grammar — denominator.
    pub total_markers: usize,
    /// Markers reachable from a registered `StructLayout` whose
    /// shape covers the marker's type — numerator.
    pub mapped_markers: usize,
    /// Markers awaiting registry closure (W0 baseline state).
    pub pending_markers: usize,
    /// Markers whose enclosing rule has a registry entry that fails
    /// to cover the marker's type. The red population.
    pub missing_markers: usize,
    /// Typed markers per enclosing rule, descending by count.
    pub markers_per_rule: Vec<(String, usize)>,
    /// Pending marker list — enumerated when total > mapped.
    pub pending: Vec<PendingMarker>,
    /// Missing marker list — the red signal.
    pub missing: Vec<MissingMarker>,
}

impl GrammarCoverage {
    /// Coverage ratio in `[0.0, 1.0]`. Returns `1.0` on a grammar
    /// with zero typed markers (vacuously closed) so empty
    /// grammars do not red-flag the gate.
    pub fn ratio(&self) -> f64 {
        if self.total_markers == 0 {
            return 1.0;
        }
        self.mapped_markers as f64 / self.total_markers as f64
    }

    /// True when no marker is `Missing`. `Pending` markers are
    /// permitted at W0 (substrate has not landed); `Missing`
    /// markers are red regardless of phase.
    pub fn is_clean(&self) -> bool {
        self.missing_markers == 0
    }
}

/// Top-level audit report — one row per grammar plus a roll-up.
#[derive(Serialize, Deserialize, Clone, Debug, Default, PartialEq)]
pub struct AuditCoverageReport {
    /// Per-grammar coverage rows, keyed by `GrammarAuditTag::key()`.
    pub grammars: HashMap<String, GrammarCoverage>,
    /// Sum of `total_markers` across every grammar.
    pub total_markers: usize,
    /// Sum of `mapped_markers` across every grammar.
    pub mapped_markers: usize,
    /// Sum of `pending_markers` across every grammar.
    pub pending_markers: usize,
    /// Sum of `missing_markers` across every grammar.
    pub missing_markers: usize,
}

impl AuditCoverageReport {
    /// Create an empty report.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a per-grammar coverage row to the report.
    pub fn push(&mut self, coverage: GrammarCoverage) {
        self.total_markers += coverage.total_markers;
        self.mapped_markers += coverage.mapped_markers;
        self.pending_markers += coverage.pending_markers;
        self.missing_markers += coverage.missing_markers;
        self.grammars.insert(coverage.grammar.clone(), coverage);
    }

    /// Returns the coverage row for `tag`, if present.
    pub fn get(&self, tag: &GrammarAuditTag) -> Option<&GrammarCoverage> {
        self.grammars.get(tag.key())
    }

    /// True iff every grammar's `is_clean()` returns true. `Pending`
    /// is admitted; `Missing` is the red signal.
    pub fn is_clean(&self) -> bool {
        self.missing_markers == 0
    }
}

// ── Probe trait — registry presence projection ──────────────────────────

/// Pluggable projection of "does this rule have a `StructLayout`?".
///
/// `StructRegistry` is W1's substrate; the audit pass cannot depend
/// on it concretely from W0. This trait abstracts the question so:
///
/// 1. W0 wires [`AbsentRegistryProbe`] (every rule unmapped → every
///    marker `Pending`). The audit reports the baseline denominator.
/// 2. W1 onward wires a probe over the real `StructRegistry`; rules
///    whose layout admits the marker's type project as `Mapped`,
///    rules with no entry stay `Pending`, rules whose layout fails to
///    cover the type project as `Missing`.
/// 3. Until then, [`PayloadLayoutsProbe`] can stand in as a
///    present-day proxy whose entries are populated by
///    `compute_payload_layouts` in `finalize_compile`.
pub trait StructRegistryProbe {
    /// Return the rule's registered layout, if any.
    fn layout(&self, rule_id: RuleId) -> Option<&PayloadLayout>;

    /// Decide whether `layout` admits the typed leaf — true iff the
    /// emitter would write `typed` directly into the layout for the
    /// marker site under audit. Implementations default to "any
    /// scalar payload covers any scalar typed leaf"; the W1
    /// implementation tightens this to enum-variant identity.
    fn layout_covers(&self, layout: &PayloadLayout, typed: Option<&TypeDesc>) -> bool {
        match typed {
            None => !layout.fields.is_empty(),
            Some(t) if t.is_scalar_payload() => layout
                .fields
                .iter()
                .any(|f| f.ty == *t || f.ty.is_scalar_payload()),
            Some(_) => !layout.fields.is_empty(),
        }
    }
}

/// Probe that declares no rule mapped. Every typed marker projects
/// as `Pending`. The W0 baseline state.
pub struct AbsentRegistryProbe;

impl StructRegistryProbe for AbsentRegistryProbe {
    fn layout(&self, _rule_id: RuleId) -> Option<&PayloadLayout> {
        None
    }
}

/// Probe over `GrammarIR::payload_layouts` — the present-day
/// substrate populated by `compute_payload_layouts` in
/// `finalize_compile`. A useful interim probe before W1 lands the
/// proper `StructRegistry`: rules whose `compute_payload_layouts`
/// admits show up as `Mapped`, every other rule stays `Pending`.
pub struct PayloadLayoutsProbe<'ir> {
    layouts: &'ir HashMap<RuleId, PayloadLayout>,
}

impl<'ir> PayloadLayoutsProbe<'ir> {
    pub fn new(ir: &'ir GrammarIR) -> Self {
        Self {
            layouts: &ir.payload_layouts,
        }
    }
}

impl<'ir> StructRegistryProbe for PayloadLayoutsProbe<'ir> {
    fn layout(&self, rule_id: RuleId) -> Option<&PayloadLayout> {
        self.layouts.get(&rule_id)
    }
}

// ── Audit pass entry point ──────────────────────────────────────────────

/// Walk a single grammar IR and report typed-`->` coverage against
/// `probe`. The caller supplies `tag` so the row attributes correctly
/// in the multi-grammar JSON output.
pub fn audit_payload_coverage<P: StructRegistryProbe>(
    ir: &GrammarIR,
    tag: GrammarAuditTag,
    probe: &P,
) -> GrammarCoverage {
    let mut markers_per_rule: HashMap<RuleId, usize> = HashMap::new();
    let mut pending: Vec<PendingMarker> = Vec::new();
    let mut missing: Vec<MissingMarker> = Vec::new();
    let mut total_markers = 0usize;
    let mut mapped_markers = 0usize;
    let mut pending_markers = 0usize;
    let mut missing_markers = 0usize;

    for rule in &ir.rules {
        walk_typed_arrows(&rule.body, &mut |fn_id| {
            total_markers += 1;
            *markers_per_rule.entry(rule.id).or_insert(0) += 1;
            let descriptor = &ir.fns[fn_id as usize];
            let typed_leaf = typed_leaf_from_descriptor(descriptor);
            let fn_kind = descriptor_kind(descriptor);
            let status = classify_marker(probe, rule, typed_leaf.as_ref());
            match status {
                MarkerStatus::Mapped => mapped_markers += 1,
                MarkerStatus::Pending => {
                    pending_markers += 1;
                    pending.push(PendingMarker {
                        rule_id: rule.id,
                        rule_name: ir.get_string(rule.name).to_string(),
                        fn_id,
                        typed_leaf: typed_leaf.clone(),
                        fn_kind: fn_kind.to_string(),
                    });
                }
                MarkerStatus::Missing => {
                    missing_markers += 1;
                    missing.push(PendingMarker {
                        rule_id: rule.id,
                        rule_name: ir.get_string(rule.name).to_string(),
                        fn_id,
                        typed_leaf: typed_leaf.clone(),
                        fn_kind: fn_kind.to_string(),
                    });
                }
            }
        });
    }

    let mut markers_per_rule_sorted: Vec<(String, usize)> = markers_per_rule
        .into_iter()
        .map(|(rid, n)| (ir.get_string(ir.get_rule(rid).name).to_string(), n))
        .collect();
    markers_per_rule_sorted.sort_by(|a, b| b.1.cmp(&a.1).then_with(|| a.0.cmp(&b.0)));

    GrammarCoverage {
        grammar: tag.key().to_string(),
        total_markers,
        mapped_markers,
        pending_markers,
        missing_markers,
        markers_per_rule: markers_per_rule_sorted,
        pending,
        missing,
    }
}

/// Resolve a marker to its three-way status by consulting the probe
/// and the per-rule layout coverage check.
fn classify_marker<P: StructRegistryProbe>(
    probe: &P,
    rule: &IrRule,
    typed_leaf: Option<&TypeDesc>,
) -> MarkerStatus {
    match probe.layout(rule.id) {
        None => MarkerStatus::Pending,
        Some(layout) => {
            if probe.layout_covers(layout, typed_leaf) {
                MarkerStatus::Mapped
            } else {
                MarkerStatus::Missing
            }
        }
    }
}

// ── Marker classifier ───────────────────────────────────────────────────

/// True when this `FnDescriptor` originates from a grammar-author
/// `->` typed-leaf site (or a compiler-internal specialisation of
/// one). All variants represent a typed payload commit by the
/// emitter; the audit treats them as a single coverage population.
pub fn is_typed_arrow_fn(fd: &FnDescriptor) -> bool {
    match fd {
        FnDescriptor::EnumWrap { .. } => true,
        FnDescriptor::BoxWrap => true,
        FnDescriptor::NumberConvert { .. } => true,
        FnDescriptor::HexConvert { .. } => true,
        FnDescriptor::SpanCapture => true,
        FnDescriptor::Expr { .. } => true,
    }
}

/// Resolve the typed leaf carried by this FnDescriptor, if any.
/// `EnumWrap` / `BoxWrap` carry no explicit type — the enclosing
/// rule's projected type carries the shape; we return `None` and the
/// classifier consults the rule type via the registry probe.
fn typed_leaf_from_descriptor(fd: &FnDescriptor) -> Option<TypeDesc> {
    match fd {
        FnDescriptor::Expr {
            return_type: Some(t),
            ..
        } => Some(t.clone()),
        FnDescriptor::NumberConvert { .. } => Some(TypeDesc::F64),
        FnDescriptor::HexConvert { .. } => Some(TypeDesc::U32),
        FnDescriptor::SpanCapture => Some(TypeDesc::Span),
        FnDescriptor::Expr {
            return_type: None, ..
        }
        | FnDescriptor::EnumWrap { .. }
        | FnDescriptor::BoxWrap => None,
    }
}

/// Diagnostic kind tag for the JSON output.
fn descriptor_kind(fd: &FnDescriptor) -> &'static str {
    match fd {
        FnDescriptor::EnumWrap { .. } => "enum_wrap",
        FnDescriptor::BoxWrap => "box_wrap",
        FnDescriptor::NumberConvert { .. } => "number_convert",
        FnDescriptor::HexConvert { .. } => "hex_convert",
        FnDescriptor::SpanCapture => "span_capture",
        FnDescriptor::Expr { .. } => "expr",
    }
}

// ── Walker ───────────────────────────────────────────────────────────────

/// Visit every `IrNode::Map` in a rule body and call `f` with the
/// `FnId`. Exhaustive over [`IrNode`] variants; an added variant
/// without an arm fails to compile.
fn walk_typed_arrows(node: &IrNode, f: &mut dyn FnMut(FnId)) {
    match node {
        IrNode::Map { inner, fn_id } => {
            f(*fn_id);
            walk_typed_arrows(inner, f);
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
        IrNode::Seq(children) => {
            for c in children {
                walk_typed_arrows(c, f);
            }
        }
        IrNode::Alt(branches, _) => {
            for b in branches {
                walk_typed_arrows(&b.node, f);
            }
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner) => walk_typed_arrows(inner, f),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            walk_typed_arrows(a, f);
            walk_typed_arrows(b, f);
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            walk_typed_arrows(token, f);
            for arm in arms {
                if let Some(map_fn) = arm.map_fn {
                    f(map_fn);
                }
                walk_typed_arrows(&arm.continuation, f);
            }
            walk_typed_arrows(fallback, f);
        }
    }
}

// ── JSON sink ───────────────────────────────────────────────────────────

/// Serialise the report as pretty JSON to `path`. Creates parent
/// directories on demand. Returns the byte length written.
pub fn write_coverage_report(
    report: &AuditCoverageReport,
    path: &Path,
) -> std::io::Result<usize> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    let bytes = serde_json::to_vec_pretty(report)
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?;
    let len = bytes.len();
    std::fs::write(path, bytes)?;
    Ok(len)
}
