//! Fused parse+format monolithic code generation.
//!
//! Emits `fn __rule_prettify(state, builder) -> bool` for every rule.

mod alt;
mod attempt;
mod entry;
mod expr;
mod policy;
mod repeat;
mod seq;

pub(crate) use entry::generate_monolithic_prettify;

use bbnf_ir::IrNode;

use proc_macro2::TokenStream;
use quote::quote;

use super::MonoCtx;
use self::policy::{PrettyRulePlan, PrettifyCtx};

/// Function name for a prettify rule: `__rule_prettify`.
fn prettify_fn_ident(name: &str) -> syn::Ident {
    syn::Ident::new(&format!("__{}_prettify", name), proc_macro2::Span::call_site())
}

fn new_prettify_ctx<'a>(
    ir: &'a bbnf_ir::GrammarIR,
    ctx: &'a super::ir_types::IrCodegenCtx<'a>,
    plans: &'a [PrettyRulePlan],
) -> PrettifyCtx<'a> {
    PrettifyCtx { ir, ctx, plans }
}

fn new_prettify_mctx() -> MonoCtx {
    MonoCtx::new(Vec::new())
}

/// Emit a prettify expression — dispatches by IrNode type.
fn emit_prettify_expr(
    node: &IrNode,
    pctx: &PrettifyCtx<'_>,
    current_rule: bbnf_ir::RuleId,
    mctx: &mut MonoCtx,
) -> TokenStream {
    match node {
        IrNode::Literal(sid) => {
            let raw = pctx.ir.get_string(*sid);
            let unescaped = super::unescape_literal(raw);
            let bytes = unescaped.as_bytes();
            if bytes.len() == 1 {
                let b = bytes[0];
                let lit = proc_macro2::Literal::byte_character(b);
                quote! { {
                    if state.src_bytes.get(state.offset).copied() != Some(#lit) {
                        return false;
                    }
                    state.offset += 1;
                    __builder.char(#lit);
                } }
            } else {
                let s = proc_macro2::Literal::string(&unescaped);
                let len = unescaped.len();
                quote! { {
                    let __s = #s;
                    let __bytes = __s.as_bytes();
                    let __slc = match state.src_bytes.get(state.offset..) {
                        Some(s) if s.len() >= #len => s,
                        _ => return false,
                    };
                    if &__slc[..#len] != __bytes {
                        return false;
                    }
                    __builder.text(&state.src[state.offset..state.offset + #len]);
                    state.offset += #len;
                } }
            }
        }

        IrNode::Regex(sid) => {
            let pattern = pctx.ir.get_string(*sid);

            // Emit matched text as-is. Whitespace nullification happens only
            // at the OptionalWhitespace level (for @ws patterns), not here.
            let emit_text = quote! {
                let __matched = &state.src[__start..state.offset];
                if !__matched.is_empty() {
                    __builder.text(__matched);
                }
            };

            {
                let opts = crate::generate::regex::EmitOpts::new(
                    &crate::generate::regex::CostModel::DEFAULT,
                );
                let code = crate::generate::regex::emit_regex(pattern, &opts);
                quote! { {
                    let __start = state.offset;
                    if #code.is_none() { return false; };
                    #emit_text
                } }
            }
        }

        IrNode::Epsilon => quote! { {} },

        IrNode::Ref(rule_id) => expr::emit_prettify_ref(*rule_id, pctx, mctx),

        IrNode::Seq(children) => seq::emit_prettify_seq(children, pctx, current_rule, mctx),

        IrNode::Alt(branches, dispatch) => {
            alt::emit_prettify_alt(branches, dispatch.as_ref(), pctx, current_rule, mctx)
        }

        IrNode::Repeat { inner, lo, hi } => {
            repeat::emit_prettify_repeat(inner, *lo as usize, *hi as usize, pctx, current_rule, mctx)
        }

        IrNode::Skip(left, right) => expr::emit_prettify_skip(left, right, pctx, current_rule, mctx),
        IrNode::Next(left, right) => expr::emit_prettify_next(left, right, pctx, current_rule, mctx),

        IrNode::OptionalWhitespace(inner) => {
            // ?w whitespace: parse and emit inline spaces (e.g., after ":" in
            // declarations). Newlines/indentation are suppressed — only single-line
            // whitespace (spaces/tabs) is emitted. The @ws rule handles structural
            // whitespace nullification separately.
            let ws_trim = super::emit_ws_trim(pctx.ctx, mctx);
            let inner_expr = emit_prettify_expr(inner, pctx, current_rule, mctx);

            if attempt::emits_only_on_success(inner, pctx) {
                // Deferred pattern: scan leading ws, try inner, then emit ws
                // only after inner succeeds. No checkpoint needed because the
                // inner expression fails atomically (before emitting any ops).
                let ws1 = mctx.fresh("ows");
                let ws2 = mctx.fresh("ows");
                let ws3 = mctx.fresh("ows");
                quote! { {
                    let #ws1 = state.offset;
                    #ws_trim
                    let #ws2 = state.offset;
                    #inner_expr;
                    __builder.text_inline_ws(&state.src[#ws1..#ws2]);
                    let #ws3 = state.offset;
                    #ws_trim
                    __builder.text_inline_ws(&state.src[#ws3..state.offset]);
                } }
            } else {
                // Non-atomic inner: must checkpoint because the inner expression
                // might emit ops before failing. Leading ws is emitted eagerly.
                let ws_start1 = mctx.fresh("ows");
                let ws_start2 = mctx.fresh("ows");
                let ws_emit1 = policy::emit_whitespace_segment(&ws_start1);
                let ws_emit2 = policy::emit_whitespace_segment(&ws_start2);
                let body = quote! {{
                    let #ws_start1 = state.offset;
                    #ws_trim
                    #ws_emit1
                    #inner_expr;
                    let #ws_start2 = state.offset;
                    #ws_trim
                    #ws_emit2
                }};
                let body_try = attempt::emit_prettify_attempt(body, true, Some((inner, pctx)), mctx);
                quote! { {
                    if !#body_try {
                        return false;
                    }
                } }
            }
        }

        IrNode::Map { inner, .. } => {
            // For prettify, Map is transparent — just emit the inner expression.
            // The mapping function is irrelevant for formatting.
            emit_prettify_expr(inner, pctx, current_rule, mctx)
        }

        IrNode::Minus(left, right) => {
            let right_expr = emit_prettify_expr(right, pctx, current_rule, mctx);
            let left_expr = emit_prettify_expr(left, pctx, current_rule, mctx);
            let cp_var = mctx.fresh("minus_cp");
            // Full checkpoint: the excluded expression may call rules with
            // group wrappers that leave group_stack entries on failure.
            quote! { {
                let __save = state.offset;
                let #cp_var = __builder.checkpoint();
                let __excluded = (|| -> bool { #right_expr; true })();
                state.offset = __save;
                __builder.restore(#cp_var);
                if __excluded {
                    return false;
                }
                #left_expr
            } }
        }

        IrNode::Negate(inner) => {
            let inner_expr = emit_prettify_expr(inner, pctx, current_rule, mctx);
            let cp_var = mctx.fresh("neg_cp");
            quote! { {
                let __save = state.offset;
                let #cp_var = __builder.checkpoint();
                let __inner_ok = (|| -> bool { #inner_expr; true })();
                state.offset = __save;
                __builder.restore(#cp_var);
                if __inner_ok {
                    return false;
                }
            } }
        }

        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            // For prettify, TokenDispatch: parse the token, then dispatch
            // on the matched text to select the continuation.
            let token_expr = emit_prettify_expr(token, pctx, current_rule, mctx);
            let fallback_expr = emit_prettify_expr(fallback, pctx, current_rule, mctx);
            let arm_exprs: Vec<TokenStream> = arms
                .iter()
                .map(|arm| {
                    let patterns: Vec<TokenStream> = arm
                        .patterns
                        .iter()
                        .map(|sid| {
                            let key_str = pctx.ir.get_string(*sid);
                            let key_lit = proc_macro2::Literal::string(key_str);
                            quote! { #key_lit }
                        })
                        .collect();
                    let cont_expr = emit_prettify_expr(&arm.continuation, pctx, current_rule, mctx);
                    quote! {
                        #(#patterns)|* => { #cont_expr; }
                    }
                })
                .collect();
            quote! { {
                let __key_start = state.offset;
                #token_expr;
                let __key = &state.src[__key_start..state.offset];
                match __key {
                    #(#arm_exprs)*
                    _ => { #fallback_expr; }
                }
            } }
        }
    }
}
