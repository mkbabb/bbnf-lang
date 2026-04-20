//! Prettify compilation driver — walks IR and delegates emission to [`Emitter`].
//!
//! Mirrors `driver.rs` but calls prettify emit methods. The driver makes all
//! structural decisions (dispatch strategy, inlining, checkpoint optimization)
//! and the emitter only produces target syntax.

use bbnf_ir::{GrammarIR, IrNode, RuleId};

use crate::backend::prettify::{
    PrettyRulePlan, SeparatorPolicy, SilentPosition, analysis, build_rule_plans,
    split_inner_for_sep,
};
use crate::backend::Emitter;

/// Compile prettify grammar: build plans, compile each rule, assemble.
pub fn compile_prettify_grammar<E: Emitter>(
    ir: &GrammarIR,
    emitter: &mut E,
    ctx: &mut E::Ctx,
) -> E::Output {
    let plans = build_rule_plans(ir);
    let mut rule_functions = Vec::new();

    for rule in &ir.rules {
        let plan = &plans[rule.id as usize];
        let body = compile_prettify_node(&rule.body, rule.id, &plans, ir, emitter, ctx);
        let rule_fn = emitter.emit_prettify_rule_function(rule, body, &plan.policy, ir, ctx);
        rule_functions.push(rule_fn);
    }

    emitter.emit_prettify_grammar(rule_functions, ir, ctx)
}

/// Compile a single IR node for prettify, making structural decisions.
fn compile_prettify_node<E: Emitter>(
    node: &IrNode,
    current_rule: RuleId,
    plans: &[PrettyRulePlan],
    ir: &GrammarIR,
    emitter: &mut E,
    ctx: &mut E::Ctx,
) -> E::Output {
    match node {
        IrNode::Literal(sid) => {
            let value = ir.get_string(*sid);
            emitter.emit_prettify_literal(value, ctx)
        }

        IrNode::Regex(sid) => {
            let pattern = ir.get_string(*sid);
            // Register regex for hoisting (reuse existing registry).
            emitter.emit_prettify_regex(pattern, 0, ir, ctx)
        }

        IrNode::Epsilon => {
            // Epsilon: no output, no state change.
            emitter.emit_prettify_seq(Vec::new(), ctx)
        }

        IrNode::Ref(rule_id) => {
            let rule_name = ir.get_string(ir.rules[*rule_id as usize].name);
            let plan = &plans[*rule_id as usize];
            if plan.inline {
                // Inline: compile the rule body directly at this call site.
                compile_prettify_node(
                    &ir.rules[*rule_id as usize].body,
                    *rule_id,
                    plans,
                    ir,
                    emitter,
                    ctx,
                )
            } else {
                emitter.emit_prettify_ref(*rule_id, rule_name, plan, ir, ctx)
            }
        }

        IrNode::Seq(children) => {
            let child_outputs: Vec<_> = children
                .iter()
                .map(|c| compile_prettify_node(c, current_rule, plans, ir, emitter, ctx))
                .collect();
            emitter.emit_prettify_seq(child_outputs, ctx)
        }

        IrNode::Alt(branches, dispatch) => {
            if branches.len() == 1 {
                return compile_prettify_node(
                    &branches[0].node, current_rule, plans, ir, emitter, ctx,
                );
            }
            if let Some(table) = dispatch {
                let branch_outputs: Vec<_> = branches
                    .iter()
                    .map(|b| compile_prettify_node(&b.node, current_rule, plans, ir, emitter, ctx))
                    .collect();
                let fallback_idx = table.fallback_idx.map(|i| i as usize);
                let fallback = fallback_idx.map(|_| {
                    // The fallback is included in branch_outputs; emitter extracts it.
                    emitter.emit_prettify_seq(Vec::new(), ctx)
                });
                emitter.emit_prettify_alt_dispatch(table, branch_outputs, fallback, ctx)
            } else {
                // Sequential trial with checkpoint.
                let branch_data: Vec<_> = branches
                    .iter()
                    .map(|b| {
                        let body = compile_prettify_node(
                            &b.node, current_rule, plans, ir, emitter, ctx,
                        );
                        let is_atomic = analysis::emits_only_on_success(&b.node, plans, ir);
                        (body, is_atomic)
                    })
                    .collect();
                emitter.emit_prettify_alt_sequential(branch_data, ctx)
            }
        }

        IrNode::Repeat { inner, lo, hi } => {
            let plan = &plans[current_rule as usize];
            // For iterated Repeats whose rule carries `@pretty sep(X)`, split
            // the body-emitted separator token out of `inner` and compile it
            // under a silent wrapper so the separator parses but emits no
            // ops — `sep(X)` then remains the sole emitter of inter-
            // iteration separator bytes. Optional Repeats `?` and non-sep
            // policies flow through the normal compile path.
            let iterated = !(*lo == 0 && *hi == 1);
            if iterated {
                if let SeparatorPolicy::Custom(sep) = &plan.policy.separator {
                    if let Some((main, silent, pos)) = split_inner_for_sep(inner, sep, ir) {
                        let main_out = compile_prettify_node(
                            &main, current_rule, plans, ir, emitter, ctx,
                        );
                        let silent_inner = compile_prettify_node(
                            &silent, current_rule, plans, ir, emitter, ctx,
                        );
                        let silent_out = emitter.emit_prettify_silent(silent_inner, ctx);
                        match pos {
                            SilentPosition::After => {
                                // `main << sep` — main emits first, sep-
                                // parse is silent, outer Repeat emits the
                                // pprint separator *between* iterations.
                                let combined = emitter.emit_prettify_seq(
                                    vec![main_out, silent_out],
                                    ctx,
                                );
                                return emitter.emit_prettify_repeat(
                                    combined, *lo, *hi, &plan.policy, ctx,
                                );
                            }
                            SilentPosition::Before => {
                                // `sep , main` — sep leads each iteration.
                                // Emit the pprint separator inline at the
                                // start of every iteration (including
                                // iter 1), then silent-parse the matching
                                // separator token, then emit main. The
                                // outer Repeat runs with `None` separator
                                // policy to avoid double-emitting.
                                let sep_out =
                                    emitter.emit_prettify_sep_inline(sep, ctx);
                                let combined = emitter.emit_prettify_seq(
                                    vec![sep_out, silent_out, main_out],
                                    ctx,
                                );
                                let mut inline_policy = plan.policy.clone();
                                inline_policy.separator = SeparatorPolicy::None;
                                return emitter.emit_prettify_repeat(
                                    combined, *lo, *hi, &inline_policy, ctx,
                                );
                            }
                        }
                    }
                }
            }
            let body = compile_prettify_node(inner, current_rule, plans, ir, emitter, ctx);
            emitter.emit_prettify_repeat(body, *lo, *hi, &plan.policy, ctx)
        }

        IrNode::Skip(left, right) => {
            let left_out = compile_prettify_node(left, current_rule, plans, ir, emitter, ctx);
            let right_out = compile_prettify_node(right, current_rule, plans, ir, emitter, ctx);
            emitter.emit_prettify_skip(left_out, right_out, ctx)
        }

        IrNode::Next(left, right) => {
            let left_out = compile_prettify_node(left, current_rule, plans, ir, emitter, ctx);
            let right_out = compile_prettify_node(right, current_rule, plans, ir, emitter, ctx);
            emitter.emit_prettify_next(left_out, right_out, ctx)
        }

        IrNode::OptionalWhitespace(inner) => {
            let is_atomic = analysis::emits_only_on_success(inner, plans, ir);
            let inner_out = compile_prettify_node(inner, current_rule, plans, ir, emitter, ctx);
            emitter.emit_prettify_optional_ws(inner_out, is_atomic, ctx)
        }

        IrNode::Map { inner, .. } => {
            // Map is transparent for prettify (formatting ignores value mapping).
            compile_prettify_node(inner, current_rule, plans, ir, emitter, ctx)
        }

        IrNode::Minus(left, right) => {
            // Minus: try right (excluded set), reject if match, else try left.
            let right_out = compile_prettify_node(right, current_rule, plans, ir, emitter, ctx);
            let left_out = compile_prettify_node(left, current_rule, plans, ir, emitter, ctx);
            // Wrap in checkpoint: try right, rollback, then left.
            let right_attempt = emitter.emit_prettify_attempt(right_out, true, false, ctx);
            emitter.emit_prettify_seq(vec![right_attempt, left_out], ctx)
        }

        IrNode::Negate(inner) => {
            let inner_out = compile_prettify_node(inner, current_rule, plans, ir, emitter, ctx);
            emitter.emit_prettify_attempt(inner_out, true, false, ctx)
        }

        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            // TokenDispatch: parse token, match on text, dispatch to continuation.
            // For prettify, emit token text + continuation.
            let token_out = compile_prettify_node(token, current_rule, plans, ir, emitter, ctx);
            let arm_outputs: Vec<_> = arms
                .iter()
                .map(|arm| {
                    compile_prettify_node(&arm.continuation, current_rule, plans, ir, emitter, ctx)
                })
                .collect();
            let fallback_out = compile_prettify_node(fallback, current_rule, plans, ir, emitter, ctx);
            // Combine as sequential: token + dispatch arms (emitter handles structure).
            emitter.emit_prettify_seq(
                std::iter::once(token_out)
                    .chain(arm_outputs)
                    .chain(std::iter::once(fallback_out))
                    .collect(),
                ctx,
            )
        }
    }
}
