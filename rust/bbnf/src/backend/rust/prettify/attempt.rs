//! Shared prettify trial/rollback helpers.

use bbnf_ir::IrNode;

use proc_macro2::TokenStream;
use quote::quote;

use super::policy::{PrettifyCtx, WrapperPolicy};
use super::super::MonoCtx;

pub(super) fn emits_only_on_success(node: &IrNode, pctx: &PrettifyCtx<'_>) -> bool {
    match node {
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => true,
        IrNode::Ref(rule_id) => {
            let plan = pctx.plan(*rule_id);
            plan.inline && emits_only_on_success(&pctx.ir.rules[*rule_id as usize].body, pctx)
        }
        IrNode::Map { inner, .. } => emits_only_on_success(inner, pctx),
        IrNode::OptionalWhitespace(_) => false,
        IrNode::Seq(_) => false,
        IrNode::Alt(_, _)
        | IrNode::Repeat { .. }
        | IrNode::Skip(_, _)
        | IrNode::Next(_, _)
        | IrNode::Minus(_, _)
        | IrNode::Negate(_)
        | IrNode::TokenDispatch { .. } => false,
    }
}

/// Returns true if the expression might open groups (via non-inlined rule calls
/// whose wrappers use Group or GroupIndent). When false, light_checkpoint is
/// safe because no stale group_stack entries can be left behind on failure.
pub(super) fn may_open_groups(node: &IrNode, pctx: &PrettifyCtx<'_>) -> bool {
    match node {
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => false,
        IrNode::Ref(rule_id) => {
            let plan = pctx.plan(*rule_id);
            if plan.inline {
                may_open_groups(&pctx.ir.rules[*rule_id as usize].body, pctx)
            } else {
                // Non-inlined: only Group/GroupIndent wrappers open groups.
                // Block/BlockIndent/Off/None don't use group_open/group_close.
                matches!(plan.policy.wrapper, WrapperPolicy::Group | WrapperPolicy::GroupIndent)
            }
        }
        IrNode::Seq(children) => children.iter().any(|c| may_open_groups(c, pctx)),
        IrNode::Alt(branches, _) => branches.iter().any(|b| may_open_groups(&b.node, pctx)),
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => may_open_groups(inner, pctx),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            may_open_groups(a, pctx) || may_open_groups(b, pctx)
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            may_open_groups(token, pctx)
                || arms
                    .iter()
                    .any(|a| may_open_groups(&a.continuation, pctx))
                || may_open_groups(fallback, pctx)
        }
    }
}

/// Wrap an expression in checkpoint/restore logic.
///
/// When `node` is provided, uses `may_open_groups` analysis to choose between
/// light_checkpoint (no group_stack save) and full checkpoint.
pub(super) fn emit_prettify_attempt(
    expr: TokenStream,
    rollback_builder: bool,
    node: Option<(&IrNode, &PrettifyCtx<'_>)>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    let state_cp = mctx.fresh("pretty_cp");
    if rollback_builder {
        let use_light = node.map_or(false, |(n, p)| !may_open_groups(n, p));
        let builder_cp = mctx.fresh("pretty_bcp");
        if use_light {
            quote! {{
                let #state_cp = state.offset;
                let #builder_cp = __builder.light_checkpoint();
                let __ok = (|| -> bool { #expr; true })();
                if !__ok {
                    state.offset = #state_cp;
                    __builder.light_restore(#builder_cp);
                }
                __ok
            }}
        } else {
            quote! {{
                let #state_cp = state.offset;
                let #builder_cp = __builder.checkpoint();
                let __ok = (|| -> bool { #expr; true })();
                if !__ok {
                    state.offset = #state_cp;
                    __builder.restore(#builder_cp);
                }
                __ok
            }}
        }
    } else {
        quote! {{
            let #state_cp = state.offset;
            let __ok = (|| -> bool { #expr; true })();
            if !__ok {
                state.offset = #state_cp;
            }
            __ok
        }}
    }
}
