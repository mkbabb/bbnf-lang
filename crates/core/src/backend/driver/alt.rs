//! Alt node compilation.
//!
//! Decisions (shared across all backends):
//! - all-literal fast path
//! - dispatch table (O(1) byte lookup)
//! - key dispatch (keyword-keyed alternations)
//! - checkpoint chain fallback
//!
//! # `AltBranchInfo.ty` (Tranche AF.1)
//!
//! Under tape-first emission (Tranche AC.2) every Alt branch body
//! projects to the same shape — `Option<()>` — because the owning
//! rule's epilogue carries the variant discriminator. Consequently
//! `AltBranchInfo.ty` is destructured as `_info` / `_` in every
//! backend emitter and contributes zero to the generated code.
//!
//! Before AF.1 the driver cloned the per-branch `TypeDesc` out of
//! `TypeMap` into `AltBranchInfo` at every Alt site, paying for the
//! clone (including recursive `TypeDesc::Tuple(Vec<_>)` allocation
//! for operator-chain Alts) only to feed a dead field. AF.1 replaces
//! the clones with [`PLACEHOLDER_TY`] — a `static` unit-variant
//! `TypeDesc::Span` — eliminating the allocation without touching
//! the shared struct definition.

use bbnf_ir::{AltBranch, AltDispatch, FnDescriptor, GrammarIR, IrNode, TypeDesc};

use super::DriverState;
use super::node::compile_node;
use crate::backend::strategy::alt_strategy::AltStrategy;
use crate::backend::{AltBranchInfo, Emitter, KeyDispatchBranch, ValuePlacement};

/// Placeholder `TypeDesc` used for the structurally-required-but-dead
/// `AltBranchInfo.ty` field. See the module-level note on
/// `AltBranchInfo.ty` for the rationale. Using a single `static`
/// unit variant means every branch info constructor is a `.clone()`
/// of a `TypeDesc::Span` — amortized to a stack copy under tape-first
/// (the variant carries no heap payload).
static PLACEHOLDER_TY: TypeDesc = TypeDesc::Span;

/// Build the dead-field-only `AltBranchInfo`. Every backend discards
/// it; this helper makes that contract explicit at the construction
/// site so future readers don't mistake the placeholder for a
/// load-bearing value.
#[inline]
fn placeholder_branch_info() -> AltBranchInfo {
    AltBranchInfo {
        ty: PLACEHOLDER_TY.clone(),
        coercion_variant: None,
    }
}

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

        for (i, branch) in branches.iter().enumerate() {
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
                fallback = Some((placeholder_branch_info(), output));
            } else {
                branch_outputs.push((placeholder_branch_info(), output));
            }
        }

        return emitter.emit_alt_dispatch(table, branch_outputs, fallback, alloc, ctx);
    }

    // Key dispatch. Pre-solved by `solve_key_dispatch_configs` in
    // `prepare_grammar` and cached in `DriverState`; skip the
    // lookup when a non-key-dispatch strategy already claimed the
    // Alt (the cache entry is irrelevant in that case).
    let try_key_dispatch = !matches!(
        solved_strategy,
        Some(AltStrategy::Checkpoint)
            | Some(AltStrategy::AllLiteral)
            | Some(AltStrategy::DispatchTable),
    );
    let cached_key_dispatch = if try_key_dispatch {
        dstate.key_dispatch_config(alt_node, ir).cloned()
    } else {
        None
    };
    if let Some((mut config, detected, fallback_idx)) = cached_key_dispatch {
        let pattern = bbnf_ir::key_class_regex_pattern(&config.key_class);
        config.key_scanner_regex_id = Some(dstate.register_regex(pattern));

        let mut kd_branches = Vec::with_capacity(detected.len());
        for det in &detected {
            let branch = &branches[det.branch_idx];
            let body = compile_node(&branch.node, alloc, ir, dstate, emitter, ctx);
            let key_bytes = det
                .key_literals
                .iter()
                .map(|k| k.as_bytes().to_vec())
                .collect();
            kd_branches.push(KeyDispatchBranch {
                key_bytes,
                body,
                info: placeholder_branch_info(),
            });
        }
        let fallback = fallback_idx.map(|fi| {
            let branch = &branches[fi];
            let body = compile_node(&branch.node, alloc, ir, dstate, emitter, ctx);
            (placeholder_branch_info(), body)
        });
        return emitter.emit_key_dispatch(&config, kd_branches, fallback, alloc, ctx);
    }

    // Fallback: checkpoint chain.
    let branch_outputs: Vec<_> = branches
        .iter()
        .map(|branch| {
            let output = compile_node(&branch.node, alloc, ir, dstate, emitter, ctx);
            (placeholder_branch_info(), output)
        })
        .collect();

    emitter.emit_alt_checkpoint(branch_outputs, alloc, ctx)
}
