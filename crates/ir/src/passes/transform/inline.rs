//! Pass: Acyclic rule inlining.
//!
//! Replaces `Ref(id)` with the rule body when the target rule is small,
//! acyclic, and not the entry point. Reduces call overhead in the interpreter
//! and exposes further optimization opportunities (literal merging, dispatch).

use crate::passes::inline_trace::{InlinePass, InlineSubstitution, InlineTrace};
use crate::{GrammarIR, IrNode, RuleId};

/// Maximum node count for a rule to be considered inlinable.
/// Increased from 3 to 4 to enable more inlining opportunities,
/// exposing further literal merging and dispatch optimizations.
const INLINE_THRESHOLD: usize = 4;

/// Inline small acyclic rules at their call sites.
///
/// A rule is inlinable when:
/// 1. It is not cyclic
/// 2. It is not the grammar entry point
/// 3. Its body has at most `INLINE_THRESHOLD` nodes
///
/// After inlining, the original rule remains (it may be referenced externally).
/// Run `prune_unreachable` afterward to clean up dead rules.
pub fn inline_acyclic(ir: &mut GrammarIR) {
    inline_acyclic_inner(ir, None);
}

/// Same as [`inline_acyclic`] but appends an [`InlineSubstitution`]
/// event to `trace` for every `Ref(source) → body` rewrite that
/// actually fires. The events name the source rule (whose body got
/// inlined) and the absorber rule (whose body grew to contain the
/// inlined body). Used by AZ-IV.W2.2's `path_check` IR pass to
/// re-resolve `path!` literals against post-pipeline rule names.
///
/// The call observes the same body-rewrite rule the bare
/// [`inline_acyclic`] does — semantics are byte-identical; only the
/// recording channel differs.
pub fn inline_acyclic_with_trace(ir: &mut GrammarIR, trace: &mut InlineTrace) {
    inline_acyclic_inner(ir, Some(trace));
}

fn inline_acyclic_inner(ir: &mut GrammarIR, mut trace: Option<&mut InlineTrace>) {
    // Identify inlinable rules.
    //
    // AU.2.5: rules whose body is a `Seq` of two or more children are
    // candidates for aggregate payload layouts (e.g. `length =
    // number, lengthUnit` projects to `(f64, u8)`). Inlining such a
    // rule merges its Seq children into the caller's context, so the
    // rule's function body — and its aggregate epilogue — never
    // emit. Typed-materialisation cohesion demands these rules stay
    // callable; we preserve them here by excluding composite Seq
    // bodies from the inlinable set. Single-element Seqs (unlikely
    // outside lowering artefacts) remain eligible.
    let inlinable: Vec<(RuleId, IrNode)> = ir
        .rules
        .iter()
        .filter(|r| {
            // AW-I.W4α: the `scc_id.is_none()` guard retired. Cyclic
            // rules now participate in acyclic inlining too — the
            // `is_composite_seq`, `body_has_map`, `is_consumer_pinned`
            // gates continue to preserve typed-materialisation +
            // directive invariants (W2.5 source fix).
            r.id != ir.entry
                && !r.meta.is_cyclic
                && !r.meta.preserve_identity
                && node_count(&r.body) <= INLINE_THRESHOLD
                && !is_composite_seq(&r.body)
                && !body_has_map(&r.body)
                && !is_consumer_pinned(r, ir.debug_all)
        })
        .map(|r| (r.id, r.body.clone()))
        .collect();

    if inlinable.is_empty() {
        return;
    }

    // Build a lookup: rule_id → body (for inlinable rules only).
    let max_id = inlinable.iter().map(|(id, _)| *id).max().unwrap_or(0) as usize;
    let mut bodies: Vec<Option<IrNode>> = vec![None; max_id + 1];
    for (id, body) in &inlinable {
        bodies[*id as usize] = Some(body.clone());
    }

    // Pre-compute the source-rule-id → name table so the recording
    // wrapper can build owned-string events without re-borrowing from
    // `ir.strings` mid-rewrite. The trace wants the user-written name
    // captured at substitution time (post-`prune_unreachable` the rule
    // entry vanishes, but the trace event survives).
    let inlinable_names: Vec<(RuleId, String)> = inlinable
        .iter()
        .map(|(id, _)| (*id, ir.get_string(ir.rules[*id as usize].name).to_string()))
        .collect();

    // Rewrite all rule bodies.
    //
    // The recording channel notes a substitution per `(absorber, source)`
    // pair the moment a `Ref(source)` is encountered inside the
    // absorber's body. Multiple refs to the same source within a single
    // absorber produce a single event — the trace records the
    // substitution relationship, not per-occurrence counts.
    for rule_index in 0..ir.rules.len() {
        let absorber_id = ir.rules[rule_index].id;
        let absorber_name = ir.get_string(ir.rules[rule_index].name).to_string();

        // Track which inlinable sources are referenced by this absorber's
        // body before the rewrite so the trace channel records exactly
        // one event per (source, absorber) pair.
        if let Some(trace) = trace.as_deref_mut() {
            let mut refs_seen = vec![false; bodies.len()];
            collect_refs_to(&ir.rules[rule_index].body, &bodies, &mut refs_seen);
            for (source_id, source_name) in &inlinable_names {
                if refs_seen.get(*source_id as usize).copied().unwrap_or(false) {
                    trace.record(InlineSubstitution::new(
                        *source_id,
                        source_name.clone(),
                        absorber_id,
                        absorber_name.clone(),
                        InlinePass::InlineAcyclic,
                    ));
                }
            }
        }

        let body = std::mem::replace(&mut ir.rules[rule_index].body, IrNode::Epsilon);
        ir.rules[rule_index].body = inline_refs(body, &bodies);
    }
}

/// Mark every `Ref(id)` in `node` whose `id` is an inlinable source
/// (i.e. `bodies[id].is_some()`). Used by the recording wrapper to
/// detect which sources a single absorber will inline before the
/// rewrite collapses the `Ref` nodes.
fn collect_refs_to(node: &IrNode, bodies: &[Option<IrNode>], refs_seen: &mut [bool]) {
    match node {
        IrNode::Ref(id) => {
            if let Some(slot) = refs_seen.get_mut(*id as usize) {
                if bodies
                    .get(*id as usize)
                    .map(|b| b.is_some())
                    .unwrap_or(false)
                {
                    *slot = true;
                }
            }
        }
        IrNode::Seq(children) => {
            for c in children {
                collect_refs_to(c, bodies, refs_seen);
            }
        }
        IrNode::Alt(branches, _) => {
            for b in branches {
                // Mirrors `inline_refs`: bare `Ref(_)` Alt branches are
                // not inlined (codegen needs identity for variant
                // wrapping); recursive `Ref` collection follows the
                // same shape so the trace records what the rewrite
                // actually performs.
                if !matches!(&b.node, IrNode::Ref(_)) {
                    collect_refs_to(&b.node, bodies, refs_seen);
                }
            }
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => collect_refs_to(inner, bodies, refs_seen),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            collect_refs_to(a, bodies, refs_seen);
            collect_refs_to(b, bodies, refs_seen);
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            collect_refs_to(token, bodies, refs_seen);
            for a in arms {
                collect_refs_to(&a.continuation, bodies, refs_seen);
            }
            collect_refs_to(fallback, bodies, refs_seen);
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => {}
    }
}

/// True when the node is a `Seq` with two or more children — a shape
/// that can compose typed sub-rules into an aggregate payload layout
/// (e.g. `Seq(Ref(number), Ref(lengthUnit))` → `Tuple([F64, U8])`).
///
/// Used by [`inline_acyclic`] to preserve these rules as callable
/// functions so their aggregate epilogues fire. Without this check the
/// IR-level inline pass merges the Seq's children into the caller's
/// body and the layout computation loses its anchor rule.
fn is_composite_seq(node: &IrNode) -> bool {
    matches!(node, IrNode::Seq(children) if children.len() >= 2)
}

/// True when a rule is pinned by a consumer-visible directive —
/// mirror of `bbnf_ir::passes::transform::fuse::is_consumer_pinned`.
/// `@pretty` / `@debug` / grammar-wide `debug_all` require the rule
/// to retain its identity so downstream instrumentation can attach
/// to it by name.
fn is_consumer_pinned(rule: &crate::IrRule, debug_all: bool) -> bool {
    rule.meta.directives.pretty.is_some() || rule.meta.directives.debug || debug_all
}

/// True when the rule body contains any `IrNode::Map` anywhere — the
/// grammar's typed-materialisation signature. See the sibling
/// `bbnf_ir::passes::transform::fuse::body_has_map` for the full
/// rationale. Both passes run inside the structural normaliser loop
/// before `compute_payload_layouts`, so we use a recursive
/// body-shape scan — `factor_common_prefixes` can re-group an Alt so
/// that typed Map nodes migrate under inner Seq nodes, bypassing a
/// top-level-only check.
fn body_has_map(node: &IrNode) -> bool {
    match node {
        IrNode::Map { .. } => true,
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => false,
        IrNode::Seq(children) => children.iter().any(body_has_map),
        IrNode::Alt(branches, _) => branches.iter().any(|b| body_has_map(&b.node)),
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner) => body_has_map(inner),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            body_has_map(a) || body_has_map(b)
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            body_has_map(token)
                || arms.iter().any(|a| body_has_map(&a.continuation))
                || body_has_map(fallback)
        }
    }
}

/// Count the number of nodes in an IR tree (for threshold check).
fn node_count(node: &IrNode) -> usize {
    match node {
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => 1,
        IrNode::Seq(children) => 1 + children.iter().map(node_count).sum::<usize>(),
        IrNode::Alt(branches, _) => 1 + branches.iter().map(|b| node_count(&b.node)).sum::<usize>(),
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => 1 + node_count(inner),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            1 + node_count(a) + node_count(b)
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            1 + node_count(token)
                + arms
                    .iter()
                    .map(|a| node_count(&a.continuation))
                    .sum::<usize>()
                + node_count(fallback)
        }
    }
}

/// Recursively replace `Ref(id)` with the inlined body where applicable.
fn inline_refs(node: IrNode, bodies: &[Option<IrNode>]) -> IrNode {
    match node {
        IrNode::Ref(id) => {
            if let Some(Some(body)) = bodies.get(id as usize) {
                body.clone()
            } else {
                IrNode::Ref(id)
            }
        }
        IrNode::Seq(children) => IrNode::Seq(
            children
                .into_iter()
                .map(|c| inline_refs(c, bodies))
                .collect(),
        ),
        IrNode::Alt(branches, dispatch) => {
            let branches = branches
                .into_iter()
                .map(|mut b| {
                    // Don't inline bare Refs in Alt branches — codegen needs rule
                    // identity for proper enum variant wrapping. Refs inside Map
                    // wrappers or deeper sub-expressions are fine to inline.
                    if !matches!(&b.node, IrNode::Ref(_)) {
                        b.node = inline_refs(b.node, bodies);
                    }
                    b
                })
                .collect();
            IrNode::Alt(branches, dispatch)
        }
        IrNode::Repeat { inner, lo, hi } => IrNode::Repeat {
            inner: Box::new(inline_refs(*inner, bodies)),
            lo,
            hi,
        },
        IrNode::Skip(a, b) => IrNode::Skip(
            Box::new(inline_refs(*a, bodies)),
            Box::new(inline_refs(*b, bodies)),
        ),
        IrNode::Next(a, b) => IrNode::Next(
            Box::new(inline_refs(*a, bodies)),
            Box::new(inline_refs(*b, bodies)),
        ),
        IrNode::Minus(a, b) => IrNode::Minus(
            Box::new(inline_refs(*a, bodies)),
            Box::new(inline_refs(*b, bodies)),
        ),
        IrNode::Negate(inner) => IrNode::Negate(Box::new(inline_refs(*inner, bodies))),
        IrNode::OptionalWhitespace(inner) => {
            IrNode::OptionalWhitespace(Box::new(inline_refs(*inner, bodies)))
        }
        IrNode::Map { inner, fn_id } => IrNode::Map {
            inner: Box::new(inline_refs(*inner, bodies)),
            fn_id,
        },
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => IrNode::TokenDispatch {
            token: Box::new(inline_refs(*token, bodies)),
            arms: arms
                .into_iter()
                .map(|mut a| {
                    a.continuation = inline_refs(a.continuation, bodies);
                    a
                })
                .collect(),
            fallback: Box::new(inline_refs(*fallback, bodies)),
        },
        other => other,
    }
}
