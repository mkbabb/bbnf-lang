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
            // Non-transparent inline: body compiles with Alloc so
            // downstream Refs produce boxed values.
            let body = compile_node(&rule.body, ValuePlacement::Alloc, ir, dstate, emitter, ctx);
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
