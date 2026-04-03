//! Specialized emission for precedence-style operator chains.
//!
//! Matches the hot-path shape `Seq(head, Repeat(Seq(op, rhs)))` and emits a
//! direct iterative loop instead of routing through the generic Seq+Repeat
//! scaffolding for every link in the chain.

use bbnf_ir::{IrNode, RuleId, TypeDesc};

use proc_macro2::TokenStream;
use quote::quote;

use super::ir_types::IrCodegenCtx;
use super::loop_emit::{RestoringLoop, emit_restoring_loop};
use super::{MonoCtx, emit_mono_expr};

struct OperatorChainSpec<'a> {
    head: &'a IrNode,
    link: &'a IrNode,
    op: &'a IrNode,
    rhs: &'a IrNode,
}

pub(super) fn emit_operator_chain_rule(
    rule_id: RuleId,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> Option<TokenStream> {
    if !ctx.operator_chain_rules.contains(&rule_id) {
        return None;
    }

    let rule = &ctx.ir.rules[rule_id as usize];
    let spec = detect_operator_chain(&rule.body)?;

    let Some(TypeDesc::Tuple(result_elems)) = ctx.seq_result_type(match &rule.body {
        IrNode::Seq(children) => children,
        _ => return None,
    }) else {
        return None;
    };
    if result_elems.len() != 2 || !matches!(result_elems[1], TypeDesc::Vec(_)) {
        return None;
    }

    let elem_ty = ctx.vec_elem_type(spec.link);
    let TypeDesc::Tuple(link_elem_types) = &elem_ty else {
        return None;
    };
    if link_elem_types.len() != 2 {
        return None;
    }
    if result_elems[0] == TypeDesc::Span {
        return None;
    }

    let head_expr = emit_projected_child(spec.head, &result_elems[0], ctx, mctx);
    let op_expr = emit_projected_child(spec.op, &link_elem_types[0], ctx, mctx);
    let rhs_expr = emit_projected_child(spec.rhs, &link_elem_types[1], ctx, mctx);

    let depth_var = mctx.fresh("chain_depth");
    let head_var = mctx.fresh("chain_head");
    let prev_var = mctx.fresh("chain_prev");
    let op_var = mctx.fresh("chain_op");
    let rhs_var = mctx.fresh("chain_rhs");
    let init_code = ctx.emit_scratch_init(&elem_ty, &depth_var);
    let push_code = ctx.emit_scratch_push(&elem_ty, &quote! { (#op_var, #rhs_var) });
    let collect_code = ctx.emit_scratch_collect(&elem_ty, &depth_var);

    let step = quote! {
        (|| {
            let #op_var = #op_expr?;
            let #rhs_var = #rhs_expr?;
            Some((#op_var, #rhs_var))
        })()
    };
    let on_success = quote! {
        let (#op_var, #rhs_var) = __value;
        #push_code;
        if state.offset == #prev_var {
            break;
        }
    };
    let loop_code = emit_restoring_loop(RestoringLoop {
        init: init_code,
        prev_var: &prev_var,
        step,
        on_success,
        on_failure: quote! { break; },
        finish: quote! { Some((#head_var, #collect_code)) },
    });

    Some(quote! {
        {
            let #head_var = #head_expr?;
            #loop_code
        }
    })
}

fn detect_operator_chain(node: &IrNode) -> Option<OperatorChainSpec<'_>> {
    let IrNode::Seq(children) = node else {
        return None;
    };
    if children.len() != 2 {
        return None;
    }

    let IrNode::Repeat { inner, lo, hi } = &children[1] else {
        return None;
    };
    if *lo != 0 || *hi != u32::MAX {
        return None;
    }

    let IrNode::Seq(link_children) = inner.as_ref() else {
        return None;
    };
    if link_children.len() != 2 {
        return None;
    }

    Some(OperatorChainSpec {
        head: &children[0],
        link: inner.as_ref(),
        op: &link_children[0],
        rhs: &link_children[1],
    })
}

fn emit_projected_child(
    child: &IrNode,
    projected_ty: &TypeDesc,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    if *projected_ty == TypeDesc::Span {
        if let IrNode::Ref(rule_id) = child {
            let fn_ident = super::mono_fn_ident(ctx.resolve_rule_name(*rule_id));
            return quote! {
                {
                    let __sp_b1 = state.offset;
                    Self::#fn_ident(state).map(|_| {
                        ::parse_that::Span::new(__sp_b1, state.offset, state.src)
                    })
                }
            };
        }
    }

    emit_mono_expr(child, ctx, mctx, false)
}
