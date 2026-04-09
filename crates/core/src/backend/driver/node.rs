//! Per-node dispatcher — maps `IrNode` variants to the appropriate
//! per-kind compile function.

use bbnf_ir::{GrammarIR, IrNode};

use super::DriverState;
use super::alt::compile_alt;
use super::map::compile_map;
use super::reference::compile_ref;
use super::repeat::compile_repeat;
use super::seq::compile_seq;
use super::wrap::compile_wrap;
use crate::backend::{Emitter, TokenDispatchArmCompiled, ValuePlacement};

/// Compile a single IR node. Target-agnostic decisions are made here
/// and in the per-kind helpers; emission is delegated to the
/// [`Emitter`] trait.
///
/// `alloc` controls whether the result is heap-allocated or returned
/// inline (corresponds to the old `elide_box` inverted).
pub(crate) fn compile_node<E: Emitter>(
    node: &IrNode,
    alloc: ValuePlacement,
    ir: &GrammarIR,
    dstate: &mut DriverState,
    emitter: &mut E,
    ctx: &mut E::Ctx,
) -> E::Output {
    match node {
        // Leaves.
        IrNode::Literal(sid) => {
            let raw = ir.get_string(*sid);
            let guaranteed = check_guaranteed_byte(raw, dstate);
            emitter.emit_literal_match(raw, guaranteed, ctx)
        }

        IrNode::Regex(sid) => {
            let pattern = ir.get_string(*sid);
            let regex_id = dstate.register_regex(pattern);
            emitter.emit_regex_match(pattern, regex_id, ir, ctx)
        }

        IrNode::Epsilon => emitter.emit_epsilon(ctx),

        // Structural.
        IrNode::Seq(_) => compile_seq(node, alloc, ir, dstate, emitter, ctx),

        IrNode::Alt(branches, dispatch) => {
            compile_alt(node, branches, dispatch.as_ref(), alloc, ir, dstate, emitter, ctx)
        }

        IrNode::Repeat { inner, lo, hi } => {
            compile_repeat(inner, *lo, *hi, alloc, ir, dstate, emitter, ctx)
        }

        IrNode::Ref(rule_id) => compile_ref(*rule_id, alloc, ir, dstate, emitter, ctx),

        // Binary operators. `Skip(Next(open, middle), close)` and
        // `Next(open, Skip(middle, close))` match the Wrap pattern.
        IrNode::Skip(left, right) => {
            if let IrNode::Next(open, middle) = left.as_ref() {
                compile_wrap(open, middle, right, alloc, ir, dstate, emitter, ctx)
            } else {
                let kept = compile_node(left, alloc, ir, dstate, emitter, ctx);
                let discarded =
                    compile_node(right, ValuePlacement::Inline, ir, dstate, emitter, ctx);
                emitter.emit_skip(kept, discarded, ctx)
            }
        }

        IrNode::Next(left, right) => {
            if let IrNode::Skip(middle, close) = right.as_ref() {
                compile_wrap(left, middle, close, alloc, ir, dstate, emitter, ctx)
            } else {
                let discarded =
                    compile_node(left, ValuePlacement::Inline, ir, dstate, emitter, ctx);
                let kept = compile_node(right, alloc, ir, dstate, emitter, ctx);
                emitter.emit_next(discarded, kept, ctx)
            }
        }

        IrNode::Minus(left, right) => {
            // Checkpoint/restore: try right (excluded); if it matches, reject.
            let rhs = compile_node(right, ValuePlacement::Inline, ir, dstate, emitter, ctx);
            let lhs = compile_node(left, alloc, ir, dstate, emitter, ctx);
            emitter.emit_minus(lhs, rhs, ctx)
        }

        IrNode::Negate(inner) => {
            let inner_out = compile_node(inner, ValuePlacement::Inline, ir, dstate, emitter, ctx);
            emitter.emit_negate(inner_out, ctx)
        }

        // Host integration.
        IrNode::Map { inner, fn_id } => {
            compile_map(inner, *fn_id, alloc, ir, dstate, emitter, ctx)
        }

        // Whitespace trim.
        IrNode::OptionalWhitespace(inner) => {
            let ws_pattern = ir.ws_pattern.map(|sid| ir.get_string(sid));
            let inner_out = compile_node(inner, alloc, ir, dstate, emitter, ctx);
            emitter.emit_with_ws_trim(inner_out, ws_pattern, ctx)
        }

        // Lexer-parser fusion.
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            if arms.is_empty() {
                let token_out =
                    compile_node(token, ValuePlacement::Inline, ir, dstate, emitter, ctx);
                let fallback_out = compile_node(fallback, alloc, ir, dstate, emitter, ctx);
                return emitter.emit_next(token_out, fallback_out, ctx);
            }
            let token_out =
                compile_node(token, ValuePlacement::Inline, ir, dstate, emitter, ctx);
            let compiled_arms: Vec<TokenDispatchArmCompiled<E::Output>> = arms
                .iter()
                .map(|arm| {
                    let patterns = arm
                        .patterns
                        .iter()
                        .map(|&sid| ir.get_string(sid).to_string().into_bytes())
                        .collect();
                    let continuation =
                        compile_node(&arm.continuation, alloc, ir, dstate, emitter, ctx);
                    TokenDispatchArmCompiled {
                        patterns,
                        guard_byte: arm.guard_byte,
                        continuation,
                    }
                })
                .collect();
            let fallback_out = compile_node(fallback, alloc, ir, dstate, emitter, ctx);
            emitter.emit_token_dispatch(token_out, compiled_arms, fallback_out, ctx)
        }
    }
}

/// Check if a literal can use the dispatch-guaranteed-byte
/// optimization. If the literal is a single byte matching the
/// guaranteed byte, consumes the guarantee and returns `Some(byte)`.
fn check_guaranteed_byte(raw_literal: &str, dstate: &mut DriverState) -> Option<u8> {
    let bytes = raw_literal.as_bytes();
    if bytes.len() == 1 {
        if let Some(guaranteed) = dstate.dispatch_guaranteed_byte {
            if guaranteed == bytes[0] {
                dstate.dispatch_guaranteed_byte = None;
                return Some(guaranteed);
            }
        }
    }
    None
}
