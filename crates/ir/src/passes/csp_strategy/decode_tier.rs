//! Tranche AF.5 — `decode_emission_tier` pass.
//!
//! Post-AG.5 this pass is a **fallback populator** for any
//! rule the cross-rule CSP solver elided from its tier solve.
//! After `solve_grammar_components` writes the CSP-solved tier
//! decisions into `ir.emission_tier` during the pipeline's
//! `solve_grammar_components` span, this pass walks every rule
//! and writes a tier **only when one is missing**.
//!
//! In practice the CSP owns the per-rule decision for every
//! contributing rule — any rule that enters the strategy solve
//! receives a tier variable and a decoded `Site::Tier` write.
//! The fallback still runs because degenerate components
//! (single rules that emit no decision sites at all) bypass the
//! CSP short-circuit and reach this pass untouched; same for
//! rules whose body pointer is missing from the DAG lookup.
//!
//! # Decision rule
//!
//! For each rule, the decoder consults the rule's body
//! materialization class via `ir.materialization` (looked up
//! through `ir.dag.node_for(&rule.body)`) and assigns the tier
//! per `TierFollowsMaterialization`'s upper bound — same lattice
//! used by the AF.3 cross-rule constraint:
//!
//! - `MustTape` → [`EmissionTier::Tape`]
//! - `TapeSpanOnly` → [`EmissionTier::Tape`]
//! - `TransparentElide` → [`EmissionTier::Direct`] when the
//!   rule's body is a pure conversion leaf (closure-free `Map`
//!   over a leaf, or a `FnDescriptor::NumberConvert` /
//!   `HexConvert` / `Constant`); otherwise [`EmissionTier::Tape`]
//!
//! Pure conversion eligibility is the AF.6 Tier B precondition:
//! the rule's body must compile to a self-contained `Option<T>`
//! shim that doesn't push a tape record. Anything that crosses
//! into a sub-rule reference, holds a closure environment, or
//! depends on the surrounding parse state stays Tape until
//! AF.6's emit pass can prove the conversion safely.
//!
//! # Idempotence
//!
//! `decode_emission_tier` is idempotent. Calling it twice
//! produces the same map; calling it before the AF.1 classifier
//! has run leaves the map empty (defaults to
//! `EmissionTier::Tape` at every consumer). The pass is
//! invariant under any pipeline ordering that places it AFTER
//! `classify_materialization` and BEFORE `prepare_grammar`'s
//! emitter consumption.
//!
//! Post-AG.5, the pass uses `entry().or_insert_with()` semantics
//! so existing CSP decisions are preserved and only missing
//! entries receive the fallback value.
//!
//! # AF.6 hand-off
//!
//! AF.6 (Tier B emitter) reads `ir.emission_tier[rule.id]` in
//! `emit_rule_function_impl` and dispatches:
//!
//! - `Tape` — emit only the existing `__<rule>` tape function.
//! - `Direct` — emit the existing tape function PLUS
//!   `__<rule>_direct` shim returning the typed value, and PLUS
//!   a private `__<rule>_inner` helper sharing the parse logic.
//! - `Lazy` — emit the tape function plus the view-layer
//!   `DirectSlot<'p>` field on `<Rule>View<'p>`.
//!
//! AF.6 is a substantial multi-file backend change deferred to
//! its own sub-tranche; AF.5 ships the decision substrate so the
//! emitter has a stable input when it lands.

use crate::passes::materialization::{EmissionTier, MaterializationClass};
use crate::types::FnDescriptor;
use crate::{GrammarIR, IrNode};

/// Decode the per-rule emission tier for every rule in `ir`,
/// writing the result into `ir.emission_tier`.
///
/// Idempotent. Safe to call multiple times. Reads
/// `ir.materialization` (populated by
/// `classify_materialization`) and `ir.dag` (populated by
/// `build_durable_dag`); does not consult the CSP solver output
/// directly. Tier B candidacy is decided structurally — the
/// decoder is the AF.5 bridge between the AF.1 classifier and
/// the AF.6 emitter, not a fresh CSP variable.
pub fn decode_emission_tier(ir: &mut GrammarIR) {
    // Snapshot rule ids first so the iteration doesn't borrow
    // `ir` while we mutate `ir.emission_tier` below.
    let rule_ids: Vec<_> = ir.rules.iter().map(|r| r.id).collect();

    for rule_id in rule_ids {
        // AG.5 — fallback semantics: only fill entries the CSP
        // solver left empty. Most rules receive their tier from
        // `solve_grammar_components`; this branch only fires for
        // rules elided from the strategy solve (e.g., because
        // their body had no DAG NodeId lookup).
        if ir.emission_tier.contains_key(&rule_id) {
            continue;
        }
        let tier = decide_rule_tier(ir, rule_id);
        ir.emission_tier.insert(rule_id, tier);
    }
}

/// Decide the emission tier for a single rule.
///
/// The decision combines two facts:
///
/// 1. **Upper bound from materialization class** — the
///    `TierFollowsMaterialization` discipline says a rule with
///    a `MustTape` body cannot emit any tier other than `Tape`,
///    a `TapeSpanOnly` body is bounded above by `Tape` (no
///    Tier B because the span has variable byte width), and a
///    `TransparentElide` body is the only Tier B candidate.
///
/// 2. **Tier B structural eligibility** — for a
///    `TransparentElide` rule, check whether the body is a
///    pure-conversion shape (`Map` with a non-closure
///    `FnDescriptor`, or a leaf that projects to `Span`).
///    Closure-typed `Map` bodies, refs into other rules, and
///    compound bodies stay Tape until the AF.6 emit path can
///    handle them.
fn decide_rule_tier(ir: &GrammarIR, rule_id: u32) -> EmissionTier {
    let Some(rule) = ir.rules.get(rule_id as usize) else {
        return EmissionTier::Tape;
    };
    // Pinned rules (entry, preserve_identity, @pretty, @debug)
    // always stay Tape — same discipline as AF.0's pin sweep.
    if rule_id == ir.entry || rule.meta.preserve_identity {
        return EmissionTier::Tape;
    }
    if rule.meta.directives.pretty.is_some() || rule.meta.directives.debug || ir.debug_all {
        return EmissionTier::Tape;
    }
    // Materialization class is the upper bound. `MustTape` /
    // `TapeSpanOnly` exit immediately at Tape; only
    // `TransparentElide` reaches the structural check below.
    let class = lookup_materialization_class(ir, &rule.body);
    match class {
        MaterializationClass::MustTape | MaterializationClass::TapeSpanOnly => EmissionTier::Tape,
        MaterializationClass::TransparentElide => decide_tier_b_eligibility(ir, &rule.body),
    }
}

/// Look up the materialization class for a rule body via the
/// DAG. Returns `MustTape` (the safe top of the lattice) when
/// the DAG is missing or the body has no entry.
fn lookup_materialization_class(ir: &GrammarIR, body: &IrNode) -> MaterializationClass {
    let Some(dag) = ir.dag.as_ref() else {
        return MaterializationClass::MustTape;
    };
    let Some(node_id) = dag.node_for(body) else {
        return MaterializationClass::MustTape;
    };
    ir.materialization
        .get(&node_id)
        .copied()
        .unwrap_or(MaterializationClass::MustTape)
}

/// Decide Tier B vs Tape for a `TransparentElide` rule body.
///
/// Tier B is admissible only when the body compiles to a
/// self-contained `Option<T>` shim — no tape pushes, no
/// captured closure state, no cross-rule call boundaries. The
/// Tier B vocabulary covers:
///
/// - A `Map` whose `FnDescriptor` is non-closure (every variant
///   except the user-facing `Expr` that may carry a closure),
///   where the inner is a leaf or a transparent `Ref`.
/// - A leaf node (`Literal` / `Regex` / `Epsilon`) that
///   projects to `Span` — the view layer can return the span
///   directly without walking the tape.
/// - A `Seq` where every child is a leaf, transparent `Ref`, or
///   `OptionalWhitespace` over a leaf — typed Seq projections.
/// - A single-branch `Alt` — collapses to the child's tier.
/// - A bounded `Repeat` over a leaf or transparent `Ref` —
///   fixed-size shape that doesn't need tape records.
fn decide_tier_b_eligibility(ir: &GrammarIR, body: &IrNode) -> EmissionTier {
    match body {
        IrNode::Map { inner, fn_id } => {
            // Closure-typed maps stay Tape — the closure
            // environment can't be reconstructed at the call
            // site without the parent's parse state.
            let Some(desc) = ir.fns.get(*fn_id as usize) else {
                return EmissionTier::Tape;
            };
            if matches!(desc, FnDescriptor::Expr { .. }) {
                return EmissionTier::Tape;
            }
            // A Map over a Ref is Direct only when the target
            // rule is TransparentElide — otherwise the sub-rule's
            // tape record must remain available.
            if let IrNode::Ref(target) = inner.as_ref() {
                return if is_transparent_ref(ir, *target) {
                    EmissionTier::Direct
                } else {
                    EmissionTier::Tape
                };
            }
            EmissionTier::Direct
        }
        // Bare leaves that project to Span — the view layer
        // returns the span directly. Treat as Tier B candidates.
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => EmissionTier::Direct,

        // Typed Seq projections — all children must be leaves,
        // transparent refs, or OptionalWhitespace over a leaf.
        IrNode::Seq(children) => {
            let all_direct = children.iter().all(|c| match c {
                IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => true,
                IrNode::OptionalWhitespace(inner) => matches!(
                    inner.as_ref(),
                    IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon
                ),
                IrNode::Ref(rid) => is_transparent_ref(ir, *rid),
                _ => false,
            });
            if all_direct {
                EmissionTier::Direct
            } else {
                EmissionTier::Tape
            }
        }

        // Single-branch Alt collapses to the child's tier.
        IrNode::Alt(branches, _) if branches.len() == 1 => {
            decide_tier_b_eligibility(ir, &branches[0].node)
        }

        // Bounded Repeat over a leaf or transparent Ref.
        // Unbounded repeats (hi == u32::MAX) are variable-size
        // and stay Tape.
        IrNode::Repeat { inner, hi, .. } if *hi != u32::MAX => {
            match inner.as_ref() {
                IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => EmissionTier::Direct,
                IrNode::Ref(rid) if is_transparent_ref(ir, *rid) => EmissionTier::Direct,
                _ => EmissionTier::Tape,
            }
        }

        _ => EmissionTier::Tape,
    }
}

/// Check whether a `Ref(target)` points to a rule whose body is
/// `TransparentElide` in the materialization lattice.
fn is_transparent_ref(ir: &GrammarIR, target: u32) -> bool {
    let Some(rule) = ir.rules.get(target as usize) else {
        return false;
    };
    matches!(
        lookup_materialization_class(ir, &rule.body),
        MaterializationClass::TransparentElide
    )
}
