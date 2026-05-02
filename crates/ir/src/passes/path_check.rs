//! IR pass: source-rule-name path resolver.
//!
//! Runs **after** `project_types` so the [`StructRegistry`] is fully
//! populated and every post-pipeline transformation (alias
//! canonicalization, acyclic inlining, single-use fusion, prefix
//! factoring, prune-unreachable) is visible. Consumes the
//! [`InlineTrace`] sidecar populated during the structural normalizer
//! loop and produces a [`PathCheckResolver`] that maps every
//! user-written source-rule name to a post-pipeline `RuleId` whose
//! [`StructLayout`] still describes the rule's body.
//!
//! ## Why a sidecar pass and not a one-pass closure
//!
//! `project_types` clears + repopulates the `StructRegistry` from the
//! *post-pipeline* IR shape. Source rules that `inline_acyclic` or
//! `fuse_single_use` substituted into other rules' bodies have no
//! top-level layout entry — `prune_unreachable` removed them after the
//! substitution. The W2 invariant 8 ("Path resolution uses source rule
//! names") demands that paths the user wrote against the source name
//! continue to resolve. The mechanism: walk the inline trace, follow
//! the substitution chain back to a rule the registry knows, and bind
//! the source name to the absorber's layout in a sidecar resolver.
//!
//! The resolver is an additive sidecar — it does not change the
//! registry. The W2.4 `path!` proc-macro asks the resolver first when a
//! path's entry rule name is not directly present in the registry; only
//! if both the registry and the resolver reject a name does the macro
//! emit a `proc_macro2::Span`-anchored compile error.
//!
//! ## Determinism
//!
//! Both inputs (the trace and the registry) are deterministic given the
//! same grammar. The resolver iterates the trace events in insertion
//! order and folds every source rule into the first absorber whose
//! layout the registry has — that gives a single, stable
//! `source_name → absorber_rule_id` map per compile.
//!
//! ## Triumvirate trigger discipline
//!
//! Per the W2 dispatch rules, this pass must not require a second IR
//! pipeline pass beyond the inline-trace sidecar. The implementation
//! holds to that: one walk of the trace, one fold against the registry,
//! one sidecar map written back to the IR. Anything beyond that — for
//! example chasing trace events through nested absorber rewrites — is a
//! triumvirate-trigger condition because it would require recording
//! state that the bare passes do not produce.

use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

use crate::passes::inline_trace::InlineTrace;
use crate::registry::StructRegistry;
use crate::types::{GrammarIR, RuleId};

/// Source-rule-name → post-pipeline rule resolver.
///
/// Populated by [`run_path_check`] after `project_types`. Maps every
/// rule name the user wrote in the grammar source to a post-pipeline
/// `RuleId` whose [`StructLayout`] is the path-resolution target. The
/// W2.4 `path!` proc-macro consults this resolver first; the resolver
/// returns the same id the registry holds for rules that survived the
/// pipeline, and the absorber's id for rules that were inlined / fused
/// out of the registry.
///
/// Iteration order is `BTreeMap`-stable — the W2.2 golden test consumes
/// this stability when it dumps the resolver to JSON for byte-identical
/// reproducibility across compile runs.
#[derive(Serialize, Deserialize, Clone, Debug, Default, PartialEq, Eq)]
pub struct PathCheckResolver {
    /// Source rule name → resolved post-pipeline `RuleId`.
    ///
    /// A rule whose registry entry survives the pipeline maps to its
    /// own id. A rule that was inlined / fused into another maps to
    /// the absorber's id; the absorber's `StructLayout` covers the
    /// substituted body.
    bindings: BTreeMap<String, RuleId>,
}

impl PathCheckResolver {
    /// Construct an empty resolver.
    pub fn new() -> Self {
        Self::default()
    }

    /// Insert / overwrite a binding. Idempotent: re-running
    /// [`run_path_check`] produces an identical resolver against the
    /// same `(trace, registry)` pair.
    pub fn bind(&mut self, source_name: impl Into<String>, target: RuleId) {
        self.bindings.insert(source_name.into(), target);
    }

    /// Resolve a source-rule name to its post-pipeline target. Returns
    /// `None` when the name is neither a surviving registry entry nor
    /// reachable through the inline trace — the W2.4 macro turns that
    /// into a `proc_macro2::Span`-anchored compile error.
    pub fn resolve(&self, source_name: &str) -> Option<RuleId> {
        self.bindings.get(source_name).copied()
    }

    /// True when `source_name` has a binding (regardless of whether
    /// the target is the rule itself or an absorber).
    pub fn contains(&self, source_name: &str) -> bool {
        self.bindings.contains_key(source_name)
    }

    /// Number of bindings.
    pub fn len(&self) -> usize {
        self.bindings.len()
    }

    /// True when the resolver has no bindings.
    pub fn is_empty(&self) -> bool {
        self.bindings.is_empty()
    }

    /// Iterate `(source_name, target)` pairs in `BTreeMap`-stable
    /// order. The golden W2.2 test consumes this iteration to produce
    /// a byte-identical JSON snapshot across runs.
    pub fn iter(&self) -> impl Iterator<Item = (&String, &RuleId)> {
        self.bindings.iter()
    }
}

/// Build a [`PathCheckResolver`] from the post-pipeline IR + the
/// recorded inline trace.
///
/// Resolution rules per source rule name (the user-written name in the
/// grammar):
///
/// 1. If the registry has a layout for `name` directly, the resolver
///    binds `name → that_rule_id`. The rule survived the pipeline.
/// 2. Else, the inline trace is scanned for a substitution event whose
///    `source_rule_name == name`. The first event's
///    `absorber_rule_id` is a rule that absorbed the source's body;
///    if the registry has a layout for that absorber, the resolver
///    binds `name → absorber_rule_id`.
/// 3. Else, the trace is followed transitively — the absorber may
///    itself have been inlined further. Each hop walks one event; a
///    cycle terminates the walk (we record only the source's first
///    surviving absorber).
/// 4. Else, the name has no resolution and the resolver omits the
///    binding. The W2.4 macro produces a compile error if a `path!`
///    literal references such a name; the diagnostic surface lives in
///    `crates/core/src/path/error.rs::PathErrorReason::UnregisteredRule`.
///
/// The function returns the resolver; `run_path_check` writes it back
/// to the IR. The two split is for testability — the W2.2 fixture
/// tests build a resolver in isolation against a synthetic
/// `(registry, trace)` pair without running the full pipeline.
///
/// # Determinism
///
/// Iteration is by `(source_name, RuleId)` ascending; the trace
/// iteration follows the recorded insertion order. The same
/// `(registry, trace)` pair always produces the same resolver.
pub fn build_resolver(
    registry: &StructRegistry,
    trace: &InlineTrace,
    rules: &[crate::types::IrRule],
    strings: &[String],
) -> PathCheckResolver {
    let mut resolver = PathCheckResolver::new();

    // Phase 1 — every registered layout binds its own name to its own
    // id. Surviving rules dominate; the trace only fills gaps.
    for layout in registry.iter() {
        resolver.bind(layout.rule_name.clone(), layout.rule_id);
    }

    // Phase 2 — every rule the IR still carries (regardless of whether
    // its layout survives) binds its name to itself when the registry
    // also covers it. This catches cases where the registry retains a
    // layout for a rule that was not inlined; redundant with phase 1
    // for those rules but cheap and idempotent.
    for rule in rules {
        let name = strings.get(rule.name as usize).cloned().unwrap_or_default();
        if name.is_empty() {
            continue;
        }
        if registry.contains(rule.id) && !resolver.contains(&name) {
            resolver.bind(name, rule.id);
        }
    }

    // Phase 3 — for every substitution event, if the source name has
    // no binding yet (i.e. the registry lost the source rule's
    // layout), follow the absorber chain until we find a rule the
    // registry knows. The first surviving absorber wins.
    //
    // We walk events in insertion order so multiple events for the
    // same source land on the first absorber the trace recorded —
    // that determinism is what the golden test exercises.
    for event in trace.iter() {
        if resolver.contains(&event.source_rule_name) {
            continue;
        }

        let resolved =
            resolve_through_trace(&event.source_rule_name, trace, registry, &mut Vec::new());

        if let Some(target_id) = resolved {
            resolver.bind(event.source_rule_name.clone(), target_id);
        }
    }

    resolver
}

/// Walk the trace from `source_name` to the first absorber whose
/// layout the registry holds. Cycles short-circuit (a source can't
/// chain into itself); the visited set bounds the walk to one pass.
fn resolve_through_trace(
    source_name: &str,
    trace: &InlineTrace,
    registry: &StructRegistry,
    visited: &mut Vec<String>,
) -> Option<RuleId> {
    if visited.iter().any(|n| n == source_name) {
        return None;
    }
    visited.push(source_name.to_owned());

    let absorber = trace
        .iter()
        .find(|e| e.source_rule_name == source_name)?;

    if registry.contains(absorber.absorber_rule_id) {
        return Some(absorber.absorber_rule_id);
    }

    // Absorber was itself inlined further; chase its name through the
    // trace.
    resolve_through_trace(&absorber.absorber_rule_name, trace, registry, visited)
}

/// Pipeline entry point: compute and return the [`PathCheckResolver`]
/// for the given `(ir, trace)` pair. Caller writes the resolver back
/// to the IR if it wants to retain it (`pipeline::compile` does so).
///
/// This entry point is the IR-pass surface the
/// `crates/core/src/pipeline/compile.rs` wires after `project_types`.
/// It must run after `project_types` so `ir.struct_registry` is
/// populated; the function asserts that the registry is non-empty when
/// the IR has at least one rule, which catches accidental ordering
/// drift.
pub fn run_path_check(ir: &GrammarIR, trace: &InlineTrace) -> PathCheckResolver {
    debug_assert!(
        ir.rules.is_empty() || !ir.struct_registry.is_empty(),
        "run_path_check called before project_types populated the StructRegistry",
    );

    build_resolver(&ir.struct_registry, trace, &ir.rules, &ir.strings)
}
