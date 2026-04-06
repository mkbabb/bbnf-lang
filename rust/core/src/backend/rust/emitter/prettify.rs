//! Prettify trait implementation for the Rust emitter.
//!
//! Implements all `emit_prettify_*` methods on [`RustEmitter`] as inherent
//! `_impl` methods. The trait `impl Emitter` in `mod.rs` delegates to these.

use bbnf_ir::{AltDispatch, GrammarIR, IrRule, RuleId};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::backend::prettify::{PrettyPolicy, PrettyRulePlan, SeparatorPolicy, WrapperPolicy};

use super::{RustEmitCtx, RustEmitter};

// ─── Helper: prettify function identifier ─────────────────────────────────

fn prettify_fn_ident(name: &str) -> syn::Ident {
    syn::Ident::new(
        &format!("__{}_prettify", name),
        proc_macro2::Span::call_site(),
    )
}

// ─── Helper: emit separator ops ───────────────────────────────────────────

fn emit_separator_ops(policy: &PrettyPolicy) -> TokenStream {
    match &policy.separator {
        SeparatorPolicy::None => quote! {},
        SeparatorPolicy::Space => quote! { __builder.text(" "); },
        SeparatorPolicy::Softline => quote! { __builder.softline(); },
        SeparatorPolicy::Hardline => quote! { __builder.hardline(); },
        SeparatorPolicy::Blankline => quote! {
            __builder.hardline();
            __builder.hardline();
        },
        SeparatorPolicy::Custom(sep_str) => {
            let sep_lit = proc_macro2::Literal::string(sep_str);
            // Sep with empty brk: flat -> emit flat bytes, break -> newline.
            quote! { __builder.sep(#sep_lit, ""); }
        }
    }
}

// ─── Helper: emit rule wrapper (group/indent/block around body) ───────────

fn emit_rule_wrapper(policy: &PrettyPolicy, body: TokenStream) -> TokenStream {
    match policy.wrapper {
        WrapperPolicy::GroupIndent => quote! {
            __builder.group_open();
            __builder.indent_open();
            let __pretty_ok = { #body };
            __builder.indent_close();
            __builder.group_close();
            __pretty_ok
        },
        WrapperPolicy::Group => quote! {
            __builder.group_open();
            let __pretty_ok = { #body };
            __builder.group_close();
            __pretty_ok
        },
        WrapperPolicy::BlockIndent => quote! {
            __builder.indent_open();
            __builder.hardline();
            let __pretty_ok = { #body };
            __builder.indent_close();
            __builder.hardline();
            __pretty_ok
        },
        WrapperPolicy::Block => quote! { #body },
        WrapperPolicy::Off | WrapperPolicy::None => quote! { #body },
    }
}

// ─── Helper: emit inline-ws text segment ──────────────────────────────────

fn emit_whitespace_segment(start_var: &syn::Ident) -> TokenStream {
    quote! {
        __builder.text_inline_ws(&state.src[#start_var..state.offset]);
    }
}

// ─── Helper: split compile error ──────────────────────────────────────────

fn split_compile_error(rule_name: &str, split: &str) -> TokenStream {
    let msg = format!(
        "`split(\"{split}\")` is not supported by prettify codegen yet on rule `{rule_name}`"
    );
    syn::Error::new(proc_macro2::Span::call_site(), msg).to_compile_error()
}

// ─── Inherent impl: prettify methods ──────────────────────────────────────

impl RustEmitter {
    pub(super) fn emit_prettify_literal_impl(
        &mut self,
        value: &str,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let unescaped = crate::backend::unescape_literal(value);
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

    pub(super) fn emit_prettify_regex_impl(
        &mut self,
        pattern: &str,
        _regex_id: usize,
        _ir: &GrammarIR,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let opts = crate::generate::regex::EmitOpts::new(
            &crate::generate::regex::CostModel::DEFAULT,
        );
        let code = crate::generate::regex::emit_regex(pattern, &opts);
        quote! { {
            let __start = state.offset;
            if #code.is_none() { return false; };
            let __matched = &state.src[__start..state.offset];
            if !__matched.is_empty() {
                __builder.text(__matched);
            }
        } }
    }

    pub(super) fn emit_prettify_ref_impl(
        &mut self,
        _rule_id: RuleId,
        rule_name: &str,
        plan: &PrettyRulePlan,
        _ir: &GrammarIR,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // For the emitter-trait path, the driver handles inlining by compiling
        // the rule body directly and not calling this method for inline rules.
        // This method generates function calls for non-inlined rules and
        // ws-rule references.
        //
        // Ws-rule refs: the generated __ws_prettify already does the ws scan +
        // discard + text_inline_ws pattern, so a direct function call is correct.
        // Inline refs: the driver compiles the body and returns it directly
        // without reaching this method.

        if plan.inline {
            // The driver should have compiled the body inline. If we reach here,
            // generate the function call as a safety net.
        }

        let fn_ident = prettify_fn_ident(rule_name);
        quote! {
            if !Self::#fn_ident(state, __builder) {
                return false;
            }
        }
    }

    pub(super) fn emit_prettify_seq_impl(
        &mut self,
        children: Vec<TokenStream>,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        if children.is_empty() {
            return quote! {};
        }
        if children.len() == 1 {
            return children.into_iter().next().unwrap();
        }
        quote! { { #(#children;)* } }
    }

    pub(super) fn emit_prettify_alt_dispatch_impl(
        &mut self,
        table: &AltDispatch,
        branches: Vec<TokenStream>,
        _fallback: Option<TokenStream>,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let mut match_arms: Vec<TokenStream> = Vec::new();
        let mut used = vec![false; branches.len()];

        for (idx, branch) in branches.iter().enumerate() {
            if used[idx] {
                continue;
            }
            used[idx] = true;
            let bytes: Vec<u8> = (0u8..128)
                .filter(|&c| table.table.get(c as usize).copied() == Some(idx as u8))
                .collect();
            if bytes.is_empty() {
                continue;
            }
            let byte_patterns: Vec<TokenStream> = bytes
                .iter()
                .map(|&b| {
                    let b_lit = proc_macro2::Literal::byte_character(b);
                    quote! { #b_lit }
                })
                .collect();
            let branch_expr = branch;
            match_arms.push(quote! { #(#byte_patterns)|* => { #branch_expr; } });
        }

        // Find nullable branch for default + EOF handling.
        let nullable_idx = table.fallback_idx.map(|i| i as usize);

        let default_arm = if let Some(nul_idx) = nullable_idx {
            let nul_expr = &branches[nul_idx];
            quote! { _ => { #nul_expr; } }
        } else {
            quote! { _ => { return false; } }
        };
        match_arms.push(default_arm);

        let eof_handler = if let Some(nul_idx) = nullable_idx {
            let nul_expr = &branches[nul_idx];
            quote! {
                let Some(&__byte) = state.src_bytes.get(state.offset) else {
                    #nul_expr;
                    return true;
                };
            }
        } else {
            quote! {
                let __byte = match state.src_bytes.get(state.offset) {
                    Some(&b) => b,
                    None => return false,
                };
            }
        };

        quote! { {
            #eof_handler
            match __byte {
                #(#match_arms)*
            }
        } }
    }

    pub(super) fn emit_prettify_alt_sequential_impl(
        &mut self,
        branches: Vec<(TokenStream, bool)>,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        if branches.len() == 2 {
            let (first_body, first_atomic) = &branches[0];
            let first_try = self.emit_prettify_attempt_impl(
                first_body.clone(),
                !first_atomic,
                false, // conservative: full checkpoint
                ctx,
            );
            let second_body = &branches[1].0;
            return quote! { {
                if !#first_try {
                    #second_body;
                }
            } };
        }

        // General case: try each branch in order, restore builder on failure.
        let mut result = quote! { return false; };
        for (branch_body, is_atomic) in branches.iter().rev() {
            let branch_try = self.emit_prettify_attempt_impl(
                branch_body.clone(),
                !is_atomic,
                false,
                ctx,
            );
            result = quote! {
                {
                    if !#branch_try {
                        #result
                    }
                }
            };
        }
        result
    }

    pub(super) fn emit_prettify_repeat_impl(
        &mut self,
        body: TokenStream,
        lo: u32,
        hi: u32,
        policy: &PrettyPolicy,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let lo = lo as usize;
        let hi = hi as usize;

        // Optional: lo=0, hi=1
        if lo == 0 && hi == 1 {
            let inner_try = self.emit_prettify_attempt_impl(body, true, false, ctx);
            return quote! { {
                let _ = #inner_try;
                true
            } };
        }

        let sep_expr = emit_separator_ops(policy);
        let has_separator = !matches!(policy.separator, SeparatorPolicy::None);

        let inner_try = if has_separator {
            // With separator: always use full checkpoint to undo sep on fail.
            self.emit_prettify_attempt_impl(body, false, false, ctx)
        } else {
            self.emit_prettify_attempt_impl(body, true, false, ctx)
        };

        let count_var = ctx.fresh("rep_count");
        let cp_var = ctx.fresh("rep_cp");
        let loop_start_state = if lo > 0 {
            Some(ctx.fresh("rep_start"))
        } else {
            None
        };
        let loop_start_builder = if lo > 0 {
            Some(ctx.fresh("rep_bcp"))
        } else {
            None
        };

        let lo_check = if lo > 0 {
            let lo_lit = proc_macro2::Literal::usize_unsuffixed(lo);
            if let (Some(start_state), Some(start_builder)) =
                (&loop_start_state, &loop_start_builder)
            {
                quote! {
                    if #count_var < #lo_lit {
                        state.offset = #start_state;
                        __builder.restore(#start_builder);
                        return false;
                    }
                }
            } else {
                quote! {
                    if #count_var < #lo_lit {
                        return false;
                    }
                }
            }
        } else {
            quote! {}
        };

        let hi_check = if hi < usize::MAX {
            let hi_lit = proc_macro2::Literal::usize_unsuffixed(hi);
            quote! { #count_var < #hi_lit }
        } else {
            quote! { true }
        };

        let loop_cp = if let (Some(start_state), Some(start_builder)) =
            (&loop_start_state, &loop_start_builder)
        {
            quote! {
                let #start_state = state.offset;
                let #start_builder = __builder.checkpoint();
            }
        } else {
            quote! {}
        };

        if has_separator {
            // With separator: checkpoint covers sep + inner so we can undo the
            // separator if the inner expression fails on the next iteration.
            quote! { {
                #loop_cp
                let mut #count_var = 0usize;
                while #hi_check {
                    let #cp_var = state.offset;
                    let __iter_cp = if #count_var > 0 {
                        Some(__builder.checkpoint())
                    } else {
                        None
                    };
                    if #count_var > 0 {
                        #sep_expr
                    };
                    if !#inner_try {
                        state.offset = #cp_var;
                        if let Some(__bcp) = __iter_cp {
                            __builder.restore(__bcp);
                        }
                        break;
                    }
                    if state.offset == #cp_var {
                        break;
                    }
                    #count_var += 1;
                }
                #lo_check
            } }
        } else {
            // No separator: simplified loop with just state checkpoint.
            quote! { {
                #loop_cp
                let mut #count_var = 0usize;
                while #hi_check {
                    let #cp_var = state.offset;
                    if !#inner_try {
                        state.offset = #cp_var;
                        break;
                    }
                    if state.offset == #cp_var {
                        break;
                    }
                    #count_var += 1;
                }
                #lo_check
            } }
        }
    }

    pub(super) fn emit_prettify_skip_impl(
        &mut self,
        left: TokenStream,
        right: TokenStream,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // Both sides emitted for their effects (parse + format).
        quote! { {
            #left;
            #right;
        } }
    }

    pub(super) fn emit_prettify_next_impl(
        &mut self,
        left: TokenStream,
        right: TokenStream,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // Both sides emitted for their effects (parse + format).
        quote! { {
            #left;
            #right;
        } }
    }

    pub(super) fn emit_prettify_optional_ws_impl(
        &mut self,
        inner: TokenStream,
        is_atomic: bool,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // Emit ws trim code. In prettify context, OptionalWhitespace nodes
        // contain the ws pattern baked into the IR via the @ws directive lowering.
        // The actual trim comes from the standard whitespace trimmer.
        let ws_trim = quote! {
            ::parse_that::trim_leading_whitespace_mut(state);
        };

        if is_atomic {
            // Deferred pattern: scan leading ws, try inner, then emit ws only
            // after inner succeeds. No checkpoint needed because the inner
            // expression fails atomically (before emitting any ops).
            let ws1 = ctx.fresh("ows");
            let ws2 = ctx.fresh("ows");
            let ws3 = ctx.fresh("ows");
            quote! { {
                let #ws1 = state.offset;
                #ws_trim
                let #ws2 = state.offset;
                #inner;
                __builder.text_inline_ws(&state.src[#ws1..#ws2]);
                let #ws3 = state.offset;
                #ws_trim
                __builder.text_inline_ws(&state.src[#ws3..state.offset]);
            } }
        } else {
            // Non-atomic inner: must checkpoint because the inner expression
            // might emit ops before failing. Leading ws is emitted eagerly.
            let ws_start1 = ctx.fresh("ows");
            let ws_start2 = ctx.fresh("ows");
            let ws_emit1 = emit_whitespace_segment(&ws_start1);
            let ws_emit2 = emit_whitespace_segment(&ws_start2);
            let body = quote! {{
                let #ws_start1 = state.offset;
                #ws_trim
                #ws_emit1
                #inner;
                let #ws_start2 = state.offset;
                #ws_trim
                #ws_emit2
            }};
            let body_try = self.emit_prettify_attempt_impl(body, true, false, ctx);
            quote! { {
                if !#body_try {
                    return false;
                }
            } }
        }
    }

    pub(super) fn emit_prettify_attempt_impl(
        &mut self,
        expr: TokenStream,
        rollback_builder: bool,
        use_light: bool,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let state_cp = ctx.fresh("pretty_cp");
        if rollback_builder {
            let builder_cp = ctx.fresh("pretty_bcp");
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

    pub(super) fn emit_prettify_rule_function_impl(
        &mut self,
        rule: &IrRule,
        body: TokenStream,
        policy: &PrettyPolicy,
        ir: &GrammarIR,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let name = ir.get_string(rule.name);
        let fn_ident = prettify_fn_ident(name);
        let pub_ident = format_ident!("{}_prettify", name);

        let fn_body = if let Some(split) = policy.split.as_ref() {
            split_compile_error(name, split)
        } else if policy.is_ws_rule {
            // Ws rule: run body under light checkpoint to discard builder ops,
            // then re-emit the consumed span as text_inline_ws.
            let ws_start = ctx.fresh("ws_start");
            let ws_cp = ctx.fresh("ws_cp");
            let ws_emit = emit_whitespace_segment(&ws_start);
            quote! {{
                let #ws_start = state.offset;
                let #ws_cp = __builder.light_checkpoint();
                let __ok = (|| -> bool { #body; true })();
                __builder.light_restore(#ws_cp);
                if !__ok {
                    return false;
                }
                #ws_emit
                true
            }}
        } else {
            emit_rule_wrapper(
                policy,
                quote! {{
                    #body;
                    true
                }},
            )
        };

        let mut methods = TokenStream::new();

        methods.extend(quote! {
            #[allow(non_snake_case)]
            fn #fn_ident<'a>(
                state: &mut ::parse_that::ParserState<'a>,
                __builder: &mut ::pprint::FmtBuilder<'a>,
            ) -> bool {
                #fn_body
            }
        });

        methods.extend(quote! {
            pub fn #pub_ident<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
                Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                    let mut __builder =
                        ::pprint::FmtBuilder::with_capacity(state.src.len().saturating_mul(2));
                    if !Self::#fn_ident(state, &mut __builder) {
                        return None;
                    }
                    Some(__builder.finish())
                })
            }
        });

        methods
    }

    pub(super) fn emit_prettify_grammar_impl(
        &mut self,
        rule_functions: Vec<TokenStream>,
        _ir: &GrammarIR,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        quote! { #(#rule_functions)* }
    }
}
