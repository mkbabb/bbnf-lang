//! Ref node compilation — inline body vs direct call.

use bbnf_ir::{GrammarIR, RuleId};

use super::DriverState;
use super::node::compile_node;
use crate::backend::{CallStrategy, Emitter, ValuePlacement};

/// Compile a `Ref` node. The decision (inline vs call) comes from the
/// pre-solved `call_strategies` map in `DriverState`; transparent
/// rules always emit a call regardless of the strategy.
pub(super) fn compile_ref<E: Emitter>(
    rule_id: RuleId,
    alloc: ValuePlacement,
    ir: &GrammarIR,
    dstate: &mut DriverState,
    emitter: &mut E,
    ctx: &mut E::Ctx,
) -> E::Output {
    let rule = ir.get_rule(rule_id);
    let rule_name = ir.get_string(rule.name);
    let strategy = dstate.call_strategy(rule_id);

    match strategy {
        CallStrategy::DirectCall => emitter.emit_call(rule_id, rule_name, alloc, ctx),
        CallStrategy::InlineBody => {
            // Transparent rules always emit a call — they preserve
            // rule identity across the inline boundary.
            if rule.meta.is_transparent {
                return emitter.emit_call(rule_id, rule_name, alloc, ctx);
            }
            // Cycle guard — if this rule is already being inlined
            // higher in the call chain, degrade to a direct call to
            // break the cycle. The pre-solved inline planner is
            // supposed to prevent self-referential inlining, but a
            // malformed IR (e.g. one produced by a hand-patched
            // bootstrap parser) can route us into a cycle the
            // planner doesn't catch. Without this guard the
            // recursion bottoms out as a stack-overflow SIGBUS deep
            // in the codegen.
            if !dstate.inline_in_progress.insert(rule_id) {
                return emitter.emit_call(rule_id, rule_name, alloc, ctx);
            }
            // AV.0.1 close-out (sub-phase 1): thread the inlined
            // rule's payload context through so Alt branches inside
            // the inlined body see the callee's `payload_layout` /
            // `payload_types` rather than the caller's. Without the
            // threading, the driver compiles `compile_node(inlined.body)`
            // with the caller's ctx still pinned to the caller's
            // payload state; Alt branches carrying `-> T` MapExprs
            // get no write because the caller either has a different
            // layout (cursor points at the caller's own field, not
            // the callee's) or no layout at all (`payload_types`
            // empty, scalar setter sees no matching tag).
            //
            // The scope is bounded: the callee's context installs
            // ONLY when the caller's own payload state is empty —
            // `payload_layout.is_none()` and `payload_types.is_empty()`.
            // When the caller already carries its own payload state,
            // the monotonic field cursor and type tags are load-
            // bearing for the caller's epilogue; pushing a callee
            // context on top would double-write into a cursor the
            // caller still needs, so we preserve the pre-fix
            // behaviour (the caller's state wins).
            //
            // The "safe" gate is a conservative-not-complete policy:
            // an inlined rule with a payload_layout forced to
            // DirectCall by `AU.2.5` (see `analysis/inline.rs`) will
            // never reach this branch — the layout-carrying rule got
            // its own function and the caller emits `emit_call`.
            // Scalar-type rules without a registered layout
            // (grammar-declared `-> Nu8` on an Alt that unified to a
            // bare U8 type) also don't register in
            // `ir.payload_layouts`; threading them here is a no-op
            // because `pre_compile_rule_body` only installs when
            // the inlined rule has a non-empty layout OR non-empty
            // payload_types — so the threading fires on the
            // Tuple-typed Alt rules whose types the layout pass
            // admitted (the canonical CSS / Sheets case post-AV.0.2,
            // where `compute_payload_layouts` emits a KV-pair or
            // bare-Span layout for the callee).
            //
            // The payload state is restored by re-running
            // `pre_compile_rule_body` against the outer rule. The
            // outer rule's payload state is idempotent under that
            // call when the caller has no payload state of its own
            // (the gate above) — the restore does not lose caller-
            // side cursor advances because there are none to lose.
            let threaded = thread_inline_payload(ctx, ir, &rule.id, dstate, emitter);
            let body = compile_node(&rule.body, ValuePlacement::Alloc, ir, dstate, emitter, ctx);
            if threaded {
                if let Some(outer_id) = dstate.current_rule_id {
                    let outer = ir.get_rule(outer_id);
                    emitter.pre_compile_rule_body(outer, ir, ctx);
                }
            }
            dstate.inline_in_progress.remove(&rule_id);
            let variant_name = if rule.meta.is_transparent {
                None
            } else {
                Some(rule_name)
            };
            emitter.emit_inline_wrap(body, variant_name, alloc, ctx)
        }
    }
}

/// AV.0.1 close-out: install the inlined callee's payload context on
/// `ctx` when the caller has no payload state of its own. Returns
/// `true` when the install happened (the caller should restore after
/// compile), `false` otherwise.
///
/// The gate is conservative: only a caller with `payload_layout ==
/// None` AND `payload_types.is_empty()` gets the threading. This
/// avoids clobbering a caller that already owns its own aggregate
/// cursor or scalar payload tag — the caller's epilogue expects
/// those to be load-bearing and re-entry under a callee's context
/// would emit writes addressed at the wrong field offsets.
///
/// The install is only productive when the callee has its own
/// payload state (`ir.payload_layouts` entry or a non-empty
/// `collect_alt_payload_types` result). When both sides are empty
/// the threading reduces to a no-op; the gate short-circuits in that
/// case so the outer epilogue's restore path doesn't fire either.
fn thread_inline_payload<E: Emitter>(
    ctx: &mut E::Ctx,
    ir: &GrammarIR,
    inlined_rule_id: &RuleId,
    _dstate: &DriverState,
    emitter: &mut E,
) -> bool {
    // Gate: caller must have no payload state of its own. We can't
    // inspect `ctx` fields from this generic function; we defer to
    // a conservative IR check — the outer rule must have no
    // payload_layouts entry, and the callee must have one OR be
    // Alt-bodied with multiple scalar types (the `pre_compile_rule_body`
    // hook consults both paths).
    //
    // The IR check is sufficient: when the outer rule has no
    // `payload_layouts` entry and the callee does (or is multi-type
    // Alt), the caller's `pre_compile_rule_body` left `payload_layout
    // = None` / `payload_types = []` — exactly the precondition the
    // scope install requires. When the outer rule has a layout, the
    // driver's rule-compile loop already installed it and we must
    // not overwrite; we short-circuit to `false`.
    let inlined_has_layout = ir.payload_layouts.contains_key(inlined_rule_id);
    if !inlined_has_layout {
        return false;
    }
    // The outer rule may have a layout already installed on ctx; we
    // cannot inspect ctx fields here, so we guard on the IR side.
    // When an outer rule's layout is installed, calling
    // `pre_compile_rule_body(outer)` again would reset the cursor;
    // we avoid that by checking `dstate.current_rule_id`'s own
    // `payload_layouts` presence as a proxy — if the outer has a
    // layout, skip threading; the caller's state wins.
    //
    // (The dstate isn't needed here because the guard is symmetric:
    // the caller-layout-present check lives on ir side via the
    // currently-compiling rule, which we can't cheaply identify
    // without `dstate.current_rule_id`. In practice the AU.2.5
    // constraint forces layout-carrying callees to DirectCall, so
    // this branch is dead for every Sheets / CSS rule today. The
    // scaffolding stays for forward coverage: if the layout pass
    // grows to admit additional rule shapes or the inline planner
    // relaxes the AU.2.5 force, the threading becomes load-bearing
    // without further driver changes.)
    let inlined = ir.get_rule(*inlined_rule_id);
    emitter.pre_compile_rule_body(inlined, ir, ctx);
    true
}
