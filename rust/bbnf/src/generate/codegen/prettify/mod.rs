//! Fused parse+format monolithic code generation.
//!
//! Emits `fn __rule_prettify(state, builder) -> bool` for every rule — direct
//! recursive functions that parse input AND construct FmtOp instructions
//! simultaneously. No intermediate AST. The parser IS the formatter.
//!
//! Activated by `#[parser(prettify)]` on grammars with `@pretty` directives.
//! Uses the same dispatch tables, SIMD scanners, and `find_first_of`
//! infrastructure as the arena/span paths.

mod alt;
mod expr;
mod repeat;
mod seq;

use bbnf_ir::{GrammarIR, IrNode};

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::MonoCtx;
use super::ir_types::IrCodegenCtx;

/// Function name for a prettify rule: `__rule_prettify`.
pub(in crate::generate) fn prettify_fn_ident(name: &str) -> syn::Ident {
    format_ident!("__{}_prettify", name)
}

/// Generate all fused parse+format monolithic methods.
pub fn generate_monolithic_prettify(ir: &GrammarIR, ctx: &IrCodegenCtx<'_>) -> TokenStream {
    let mut methods: Vec<TokenStream> = Vec::new();

    let fusion_eligible: Vec<bool> = ir
        .rules
        .iter()
        .map(|rule| rule.meta.is_token || (!rule.meta.is_cyclic && rule.meta.recover.is_none()))
        .collect();

    let single_site_inline = super::compute_single_site_inline(ir);

    for rule in &ir.rules {
        let name = ir.get_string(rule.name);
        let fn_ident = prettify_fn_ident(name);
        let pub_ident = format_ident!("{}_prettify", name);

        let mut mctx = MonoCtx::new(fusion_eligible.clone(), single_site_inline.clone());
        mctx.current_rule_name = Some(name.to_string());
        mctx.current_pretty_hints = rule.meta.pretty.clone();

        // Determine @pretty hints for this rule.
        let ph = rule.meta.pretty.as_ref();
        let has_group = ph.is_some_and(|p| p.group);
        let has_block = ph.is_some_and(|p| p.block);
        let has_indent = ph.is_some_and(|p| p.indent);
        let is_off = ph.is_some_and(|p| p.off);

        // Detect whitespace-only rules (body is a Regex matching the @ws pattern).
        // These are parsed for side effects but their text is NOT emitted —
        // the @pretty hints handle spacing, and emitting source whitespace
        // would break idempotency.
        let is_ws_rule =
            if let (IrNode::Regex(body_sid), Some(ws_sid)) = (&rule.body, ir.ws_pattern) {
                *body_sid == ws_sid
            } else {
                false
            };

        // Generate the body expression.
        let body_expr = emit_prettify_expr(&rule.body, ir, ctx, &mut mctx);
        let hoisted = mctx.hoisted.clone();

        // Wrap body in structural ops based on @pretty hints.
        // Uses explicit open/close instead of closures to avoid
        // `return false;` escaping closure scope issues.
        let wrapped_body = if is_ws_rule {
            // Whitespace rule: parse for side effects (advance offset) but
            // don't emit FmtOps. Checkpoint/restore discards any ops.
            let ws_cp = mctx.fresh("ws_cp");
            quote! {
                #(#hoisted)*
                let #ws_cp = __builder.checkpoint();
                #body_expr;
                __builder.restore(#ws_cp);
                true
            }
        } else if is_off {
            // @pretty off: emit body without any structural wrapping.
            quote! {
                #(#hoisted)*
                #body_expr;
                true
            }
        } else if has_block && has_indent {
            quote! {
                #(#hoisted)*
                __builder.indent_open();
                __builder.hardline();
                #body_expr;
                __builder.indent_close();
                __builder.hardline();
                true
            }
        } else if has_block {
            quote! {
                #(#hoisted)*
                #body_expr;
                true
            }
        } else if has_group && has_indent {
            quote! {
                #(#hoisted)*
                __builder.group_open();
                __builder.indent_open();
                #body_expr;
                __builder.indent_close();
                __builder.group_close();
                true
            }
        } else if has_group {
            quote! {
                #(#hoisted)*
                __builder.group_open();
                #body_expr;
                __builder.group_close();
                true
            }
        } else {
            quote! {
                #(#hoisted)*
                #body_expr;
                true
            }
        };

        // Internal function: parses + appends FmtOps. Returns bool (success).
        // On failure, both builder and parser state are restored.
        methods.push(quote! {
            #[allow(non_snake_case)]
            fn #fn_ident<'a>(
                state: &mut ::parse_that::ParserState<'a>,
                __builder: &mut ::pprint::FmtBuilder<'a>,
            ) -> bool {
                let __bcp = __builder.checkpoint();
                let __scp = state.offset;
                let __result = (|| -> bool { #wrapped_body })();
                if !__result {
                    __builder.restore(__bcp);
                    state.offset = __scp;
                }
                __result
            }
        });

        // Public method: returns Parser<Vec<FmtOp>>.
        methods.push(quote! {
            pub fn #pub_ident<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
                Parser::new(move |state: &mut ::parse_that::ParserState<'a>| {
                    let mut __builder = ::pprint::FmtBuilder::with_capacity(state.src.len() * 2);
                    if Self::#fn_ident(state, &mut __builder) {
                        Some(__builder.finish())
                    } else {
                        None
                    }
                })
            }
        });
    }

    quote! { #(#methods)* }
}

/// Emit a prettify expression — dispatches by IrNode type.
pub(super) fn emit_prettify_expr(
    node: &IrNode,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    match node {
        IrNode::Literal(sid) => {
            let raw = ir.get_string(*sid);
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
            let pattern = ir.get_string(*sid);

            // Emit matched text as-is. Whitespace nullification happens only
            // at the OptionalWhitespace level (for @ws patterns), not here.
            let emit_text = quote! {
                let __matched = &state.src[__start..state.offset];
                if !__matched.is_empty() {
                    __builder.text(__matched);
                }
            };

            if let Some(direct) =
                crate::generate::regex_ir::fast_paths::emit_regex_direct_call(pattern)
            {
                quote! { {
                    let __start = state.offset;
                    if #direct.is_none() { return false; }
                    #emit_text
                } }
            } else if let Some(inline) = crate::generate::regex_ir::try_emit_regex_inline(pattern) {
                quote! { {
                    let __start = state.offset;
                    if #inline.is_none() { return false; }
                    #emit_text
                } }
            } else if let Some(dfa_code) = crate::generate::regex_ir::try_emit_dfa_inline(pattern) {
                quote! { {
                    let __start = state.offset;
                    if #dfa_code.is_none() { return false; }
                    #emit_text
                } }
            } else {
                let err = crate::generate::regex_ir::emit_regex_unsupported(pattern);
                quote! { {
                    let __start = state.offset;
                    if #err.is_none() { return false; }
                    #emit_text
                } }
            }
        }

        IrNode::Epsilon => quote! { {} },

        IrNode::Ref(rule_id) => expr::emit_prettify_ref(*rule_id, ir, ctx, mctx),

        IrNode::Seq(children) => seq::emit_prettify_seq(children, ir, ctx, mctx),

        IrNode::Alt(branches, dispatch) => {
            alt::emit_prettify_alt(branches, dispatch.as_ref(), ir, ctx, mctx)
        }

        IrNode::Repeat { inner, lo, hi } => {
            repeat::emit_prettify_repeat(inner, *lo as usize, *hi as usize, ir, ctx, mctx)
        }

        IrNode::Skip(left, right) => expr::emit_prettify_skip(left, right, ir, ctx, mctx),
        IrNode::Next(left, right) => expr::emit_prettify_next(left, right, ir, ctx, mctx),

        IrNode::OptionalWhitespace(inner) => {
            // ?w whitespace: parse and emit inline spaces (e.g., after ":" in
            // declarations). Newlines/indentation are suppressed — only single-line
            // whitespace (spaces/tabs) is emitted. The @ws rule handles structural
            // whitespace nullification separately.
            let ws_trim = super::emit_ws_trim(ctx, mctx);
            let inner_expr = emit_prettify_expr(inner, ir, ctx, mctx);
            let ws_start1 = mctx.fresh("ows");
            let ws_start2 = mctx.fresh("ows");
            quote! { {
                let #ws_start1 = state.offset;
                #ws_trim
                {
                    let __ws = &state.src[#ws_start1..state.offset];
                    // Emit inline spaces (no newlines) to preserve source formatting.
                    if !__ws.is_empty() && !__ws.contains('\n') {
                        __builder.text(__ws);
                    }
                }
                #inner_expr;
                let #ws_start2 = state.offset;
                #ws_trim
                {
                    let __ws = &state.src[#ws_start2..state.offset];
                    if !__ws.is_empty() && !__ws.contains('\n') {
                        __builder.text(__ws);
                    }
                }
            } }
        }

        IrNode::Map { inner, .. } => {
            // For prettify, Map is transparent — just emit the inner expression.
            // The mapping function is irrelevant for formatting.
            emit_prettify_expr(inner, ir, ctx, mctx)
        }

        IrNode::Minus(left, right) => {
            let right_expr = emit_prettify_expr(right, ir, ctx, mctx);
            let left_expr = emit_prettify_expr(left, ir, ctx, mctx);
            let cp_var = mctx.fresh("minus_cp");
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
            let inner_expr = emit_prettify_expr(inner, ir, ctx, mctx);
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
            let token_expr = emit_prettify_expr(token, ir, ctx, mctx);
            let fallback_expr = emit_prettify_expr(fallback, ir, ctx, mctx);
            let arm_exprs: Vec<TokenStream> = arms
                .iter()
                .map(|arm| {
                    let patterns: Vec<TokenStream> = arm
                        .patterns
                        .iter()
                        .map(|sid| {
                            let key_str = ir.get_string(*sid);
                            let key_lit = proc_macro2::Literal::string(key_str);
                            quote! { #key_lit }
                        })
                        .collect();
                    let cont_expr = emit_prettify_expr(&arm.continuation, ir, ctx, mctx);
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
