//! Alt node compilation.
//!
//! Decisions (shared across all backends):
//! - all-literal fast path
//! - dispatch table (O(1) byte lookup)
//! - key dispatch (keyword-keyed alternations)
//! - checkpoint chain fallback

use bbnf_ir::{AltBranch, AltDispatch, FnDescriptor, GrammarIR, IrNode, TypeDesc};

use super::DriverState;
use super::node::compile_node;
use crate::backend::strategy::alt_strategy::AltStrategy;
use crate::backend::{AltBranchInfo, Emitter, KeyDispatchBranch, ValuePlacement};

/// Compile an Alt node, dispatching on the pre-solved `AltStrategy`
/// (if any) and falling back to inline detection for the not-yet-
/// solved cases.
pub(super) fn compile_alt<E: Emitter>(
    alt_node: &IrNode,
    branches: &[AltBranch],
    dispatch: Option<&AltDispatch>,
    alloc: ValuePlacement,
    ir: &GrammarIR,
    dstate: &mut DriverState,
    emitter: &mut E,
    ctx: &mut E::Ctx,
) -> E::Output {
    // Look up the pre-solved strategy. Cloned to avoid borrowing
    // conflicts with the subsequent &mut DriverState passes.
    let solved_strategy = dstate.alt_strategy(alt_node, ir).cloned();

    // Classify branch types. In Inline context (Vec elements), map
    // BoxedEnum → Enum to match the TypeMap's Vec projection.
    let branch_infos: Vec<AltBranchInfo> = branches
        .iter()
        .map(|b| {
            let mut ty = ir.node_type(&b.node).cloned().unwrap_or(TypeDesc::Span);
            if alloc == ValuePlacement::Inline && ty == TypeDesc::BoxedEnum {
                ty = TypeDesc::Enum;
            }
            AltBranchInfo {
                ty,
                coercion_variant: None,
            }
        })
        .collect();

    // All-literal fast path: pre-solved strategy short-circuits the
    // inline classifier when it already marked the Alt.
    let all_literal_like = match &solved_strategy {
        Some(AltStrategy::AllLiteral) => alloc == ValuePlacement::Inline,
        Some(_) => false,
        None => {
            alloc == ValuePlacement::Inline
                && branches.iter().all(|b| {
                    matches!(b.node, IrNode::Literal(_))
                        || matches!(&b.node, IrNode::Map { inner, fn_id } if {
                            matches!(inner.as_ref(), IrNode::Literal(_))
                                && matches!(&ir.fns[*fn_id as usize], FnDescriptor::Expr { expr, .. } if expr.is_constant())
                        })
                })
        }
    };

    if all_literal_like {
        let literals: Vec<_> = branches
            .iter()
            .map(|b| {
                let (lit_sid, node_to_compile) = match &b.node {
                    IrNode::Literal(sid) => (*sid, &b.node),
                    IrNode::Map { inner, .. } if matches!(inner.as_ref(), IrNode::Literal(_)) => {
                        let IrNode::Literal(sid) = inner.as_ref() else {
                            unreachable!()
                        };
                        (*sid, &b.node)
                    }
                    _ => unreachable!(),
                };
                let value = ir.get_string(lit_sid).to_string();
                let output = compile_node(node_to_compile, alloc, ir, dstate, emitter, ctx);
                (value, output)
            })
            .collect();
        return emitter.emit_alt_all_literal(literals, alloc, ctx);
    }

    // Dispatch table: O(1) byte lookup.
    if let Some(table) = dispatch {
        let mut branch_outputs = Vec::with_capacity(branches.len());
        let mut fallback = None;

        for (i, (branch, info)) in branches.iter().zip(branch_infos.into_iter()).enumerate() {
            let byte_patterns: Vec<u8> = table
                .table
                .iter()
                .enumerate()
                .filter(|&(_, &b)| b as usize == i)
                .map(|(bv, _)| bv as u8)
                .collect();
            if byte_patterns.len() == 1 {
                dstate.dispatch_guaranteed_byte = Some(byte_patterns[0]);
            }
            let output = compile_node(&branch.node, alloc, ir, dstate, emitter, ctx);
            dstate.dispatch_guaranteed_byte = None;
            if table.fallback_idx == Some(i as u8) {
                fallback = Some((info, output));
            } else {
                branch_outputs.push((info, output));
            }
        }

        return emitter.emit_alt_dispatch(table, branch_outputs, fallback, alloc, ctx);
    }

    // Key dispatch. Skip the detection call when the solver already
    // classified this Alt as non-key-dispatch — `try_detect` is one of
    // the more expensive detection passes.
    let try_key_dispatch = !matches!(
        solved_strategy,
        Some(AltStrategy::Checkpoint)
            | Some(AltStrategy::AllLiteral)
            | Some(AltStrategy::DispatchTable),
    );
    if let Some((mut config, detected, fallback_idx)) = if try_key_dispatch {
        crate::backend::patterns::key_dispatch::try_detect(branches, ir)
    } else {
        None
    } {
        let pattern =
            crate::backend::patterns::key_dispatch::key_class_regex_pattern(&config.key_class);
        config.key_scanner_regex_id = Some(dstate.register_regex(pattern));

        let mut kd_branches = Vec::with_capacity(detected.len());
        for det in &detected {
            let branch = &branches[det.branch_idx];
            let info = AltBranchInfo {
                ty: ir.node_type(&branch.node).cloned().unwrap_or(TypeDesc::Span),
                coercion_variant: None,
            };
            let body = compile_node(&branch.node, alloc, ir, dstate, emitter, ctx);
            let key_bytes = det
                .key_literals
                .iter()
                .map(|k| k.as_bytes().to_vec())
                .collect();
            kd_branches.push(KeyDispatchBranch {
                key_bytes,
                body,
                info,
            });
        }
        let fallback = fallback_idx.map(|fi| {
            let branch = &branches[fi];
            let info = AltBranchInfo {
                ty: ir.node_type(&branch.node).cloned().unwrap_or(TypeDesc::Span),
                coercion_variant: None,
            };
            let body = compile_node(&branch.node, alloc, ir, dstate, emitter, ctx);
            (info, body)
        });
        return emitter.emit_key_dispatch(&config, kd_branches, fallback, alloc, ctx);
    }

    // Fallback: checkpoint chain.
    let branch_outputs: Vec<_> = branches
        .iter()
        .zip(branch_infos)
        .map(|(branch, info)| {
            let output = compile_node(&branch.node, alloc, ir, dstate, emitter, ctx);
            (info, output)
        })
        .collect();

    emitter.emit_alt_checkpoint(branch_outputs, alloc, ctx)
}
