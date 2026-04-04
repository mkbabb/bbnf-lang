//! Emitter trait implementation for the Rust backend.

mod alt;
mod dispatch;
mod repeat;
mod ws;

use bbnf_ir::{GrammarIR, IrRule, RuleId, TypeDesc};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::backend::analysis::BackendAnalysis;
use crate::backend::key_dispatch::KeyDispatchConfig;
use crate::backend::{
    AllocStrategy, AltBranchInfo, DelimScanConfig, Emitter, FlattenStrategy, KeyDispatchBranch,
    SepByConfig, SeqChildGroup, TokenDispatchArmCompiled,
};

pub use super::emitter_types::{RustEmitCtx, RustEmitter};

// ─── Emitter Implementation ────────────────────────────────────────────────

impl Emitter for RustEmitter {
    type Output = TokenStream;
    type Ctx = RustEmitCtx;

    // ── Leaves ──────────────────────────────────────────────────────────

    fn emit_literal_match(
        &mut self,
        value: &str,
        guaranteed_byte: Option<u8>,
        _ctx: &mut Self::Ctx,
    ) -> TokenStream {
        let unescaped = crate::backend::rust::unescape_literal(value);
        let bytes = unescaped.as_bytes();

        if let Some(_byte) = guaranteed_byte {
            // Dispatch already proved this byte — just advance.
            return quote! {
                {
                    let __start = state.offset;
                    state.offset += 1;
                    Some(::parse_that::Span::new(__start, state.offset, state.src))
                }
            };
        }

        if bytes.len() == 1 {
            let byte = bytes[0];
            quote! {
                if state.offset < state.src.len()
                    && state.src.as_bytes()[state.offset] == #byte
                {
                    let __start = state.offset;
                    state.offset += 1;
                    Some(::parse_that::Span::new(__start, state.offset, state.src))
                } else {
                    None
                }
            }
        } else {
            let lit = proc_macro2::Literal::string(&unescaped);
            let len = bytes.len();
            quote! {
                if state.src[state.offset..].starts_with(#lit) {
                    let __start = state.offset;
                    state.offset += #len;
                    Some(::parse_that::Span::new(__start, state.offset, state.src))
                } else {
                    None
                }
            }
        }
    }

    fn emit_regex_match(
        &mut self,
        pattern: &str,
        _regex_id: usize,
        _ir: &GrammarIR,
        _ctx: &mut Self::Ctx,
    ) -> TokenStream {
        let opts =
            crate::generate::regex::EmitOpts::new(&crate::generate::regex::CostModel::DEFAULT)
                .with_fuse(!self.effective_prettify);
        crate::generate::regex::emit_regex(pattern, &opts)
    }

    fn emit_epsilon(&mut self, _ctx: &mut Self::Ctx) -> TokenStream {
        quote! { Some(::parse_that::Span::new(state.offset, state.offset, state.src)) }
    }

    // ── Sequences ───────────────────────────────────────────────────────

    fn emit_seq_all_span(
        &mut self,
        child_outputs: Vec<TokenStream>,
        _ctx: &mut Self::Ctx,
    ) -> TokenStream {
        // All children are Span — emit for side effects, return combined Span.
        quote! {
            (|| {
                let __sp_start = state.offset;
                #( #child_outputs?; )*
                Some(::parse_that::Span::new(__sp_start, state.offset, state.src))
            })()
        }
    }

    fn emit_seq_grouped(
        &mut self,
        groups: Vec<SeqChildGroup<TokenStream>>,
        result_type: &TypeDesc,
        flatten: Option<FlattenStrategy>,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        let mut stmts = Vec::new();
        let mut result_vars = Vec::new();
        let mut result_types = Vec::new();

        for group in groups {
            match group {
                SeqChildGroup::Single { output, ty } => {
                    let var = ctx.fresh("v");
                    stmts.push(quote! { let #var = #output?; });
                    result_vars.push(var);
                    result_types.push(ty);
                }
                SeqChildGroup::SpanCompressed { outputs } => {
                    let var = ctx.fresh("sp");
                    stmts.push(quote! {
                        let __sp_start = state.offset;
                        #( #outputs?; )*
                        let #var = ::parse_that::Span::new(__sp_start, state.offset, state.src);
                    });
                    result_vars.push(var);
                    result_types.push(TypeDesc::Span);
                }
            }
        }

        // Handle Vec flattening: (T, Vec<T>) → Vec<T> via scratch.
        if let Some(flatten_strat) = flatten {
            if let TypeDesc::Vec(elem_td) = result_type {
                let ir_ctx = ctx.ir_ctx();
                let depth_var = ctx.fresh("depth");
                let init = ir_ctx.emit_scratch_init(elem_td, &depth_var);
                let collect = ir_ctx.emit_scratch_collect(elem_td, &depth_var);

                match flatten_strat {
                    FlattenStrategy::HeadThenVec => {
                        // (head, &[T]) → push head, extend from slice
                        if result_vars.len() == 2 {
                            let head = &result_vars[0];
                            let tail = &result_vars[1];
                            let push = ir_ctx.emit_scratch_push(elem_td, &quote! { #head });
                            let extend = ir_ctx.emit_scratch_extend_slice(elem_td, &quote! { #tail });
                            return quote! {
                                (|| {
                                    #init
                                    #( #stmts )*
                                    #push;
                                    #extend;
                                    Some(#collect)
                                })()
                            };
                        }
                    }
                    FlattenStrategy::VecThenTail => {
                        // (&[T], tail) → extend from slice, push tail
                        if result_vars.len() == 2 {
                            let vec_part = &result_vars[0];
                            let tail = &result_vars[1];
                            let extend = ir_ctx.emit_scratch_extend_slice(elem_td, &quote! { #vec_part });
                            let push = ir_ctx.emit_scratch_push(elem_td, &quote! { #tail });
                            return quote! {
                                (|| {
                                    #init
                                    #( #stmts )*
                                    #extend;
                                    #push;
                                    Some(#collect)
                                })()
                            };
                        }
                    }
                }
            }
        }

        // Assemble result.
        let result_expr = if result_vars.len() == 1 {
            let v = &result_vars[0];
            quote! { #v }
        } else {
            quote! { ( #( #result_vars ),* ) }
        };

        quote! {
            (|| {
                #( #stmts )*
                Some(#result_expr)
            })()
        }
    }

    // ── Alternations (delegated) ────────────────────────────────────────

    fn emit_alt_dispatch(
        &mut self,
        table: &bbnf_ir::AltDispatch,
        branches: Vec<(AltBranchInfo, TokenStream)>,
        fallback: Option<(AltBranchInfo, TokenStream)>,
        alloc: AllocStrategy,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_alt_dispatch_impl(table, branches, fallback, alloc, ctx)
    }

    fn emit_alt_checkpoint(
        &mut self,
        branches: Vec<(AltBranchInfo, TokenStream)>,
        alloc: AllocStrategy,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_alt_checkpoint_impl(branches, alloc, ctx)
    }

    fn emit_alt_all_literal(
        &mut self,
        literals: Vec<(String, TokenStream)>,
        alloc: AllocStrategy,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_alt_all_literal_impl(literals, alloc, ctx)
    }

    // ── Repetition (delegated) ──────────────────────────────────────────

    fn emit_repeat_many(
        &mut self,
        body: TokenStream,
        lo: u32,
        hi: u32,
        elem_type: &TypeDesc,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_repeat_many_impl(body, lo, hi, elem_type, ctx)
    }

    fn emit_repeat_optional(
        &mut self,
        body: TokenStream,
        inner_type: &TypeDesc,
        alloc: AllocStrategy,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_repeat_optional_impl(body, inner_type, alloc, ctx)
    }

    fn emit_sep_by(
        &mut self,
        element: TokenStream,
        separator: TokenStream,
        config: &SepByConfig,
        elem_type: &TypeDesc,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_sep_by_impl(element, separator, config, elem_type, ctx)
    }

    // ── References ──────────────────────────────────────────────────────

    fn emit_call(
        &mut self,
        _rule_id: RuleId,
        rule_name: &str,
        alloc: AllocStrategy,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        let fn_ident = format_ident!("__{}", rule_name);
        if alloc == AllocStrategy::Alloc {
            let ir_ctx = ctx.ir_ctx();
            let val = quote! { __v };
            let alloc_expr = ir_ctx.emit_alloc(&val);
            quote! { Self::#fn_ident(state).map(|__v| #alloc_expr) }
        } else {
            quote! { Self::#fn_ident(state) }
        }
    }

    fn emit_inline_wrap(
        &mut self,
        body: TokenStream,
        variant_name: Option<&str>,
        alloc: AllocStrategy,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        if let Some(name) = variant_name {
            let enum_ident = &self.enum_ident;
            let variant = format_ident!("{}", name);
            if alloc == AllocStrategy::Alloc {
                let ir_ctx = ctx.ir_ctx();
                let val = quote! { __v };
                let alloc_expr = ir_ctx.emit_alloc(&val);
                quote! {
                    #body.map(|__inner| {
                        let __v = #enum_ident::#variant(__inner);
                        #alloc_expr
                    })
                }
            } else {
                quote! {
                    #body.map(|__v| #enum_ident::#variant(__v))
                }
            }
        } else if alloc == AllocStrategy::Alloc {
            let ir_ctx = ctx.ir_ctx();
            let val = quote! { __v };
            let alloc_expr = ir_ctx.emit_alloc(&val);
            quote! { #body.map(|__v| #alloc_expr) }
        } else {
            body
        }
    }

    // ── Operator chains ──────────────────────────────────────────────────

    fn emit_operator_chain(
        &mut self,
        head: TokenStream,
        op: TokenStream,
        rhs: TokenStream,
        _ctx: &mut Self::Ctx,
    ) -> TokenStream {
        quote! {
            (|| {
                let __head = #head?;
                let __sp_start = state.offset;
                loop {
                    let __cp = state.offset;
                    let __op = #op;
                    if __op.is_none() { state.offset = __cp; break; }
                    let __rhs = #rhs;
                    if __rhs.is_none() { state.offset = __cp; break; }
                }
                Some(::parse_that::Span::new(__sp_start, state.offset, state.src))
            })()
        }
    }

    // ── Binary operators ────────────────────────────────────────────────

    fn emit_skip(
        &mut self,
        kept: TokenStream,
        discarded: TokenStream,
        _ctx: &mut Self::Ctx,
    ) -> TokenStream {
        quote! {
            (|| {
                let __kept = #kept?;
                #discarded?;
                Some(__kept)
            })()
        }
    }

    fn emit_next(
        &mut self,
        discarded: TokenStream,
        kept: TokenStream,
        _ctx: &mut Self::Ctx,
    ) -> TokenStream {
        quote! {
            (|| {
                #discarded?;
                #kept
            })()
        }
    }

    fn emit_minus(
        &mut self,
        lhs: TokenStream,
        rhs: TokenStream,
        _ctx: &mut Self::Ctx,
    ) -> TokenStream {
        quote! {
            {
                let __save_minus = state.offset;
                let __excluded = #rhs;
                state.offset = __save_minus;
                if __excluded.is_some() {
                    None
                } else {
                    #lhs
                }
            }
        }
    }

    fn emit_negate(
        &mut self,
        inner: TokenStream,
        _ctx: &mut Self::Ctx,
    ) -> TokenStream {
        quote! {
            {
                let __save_neg = state.offset;
                let __inner = #inner;
                state.offset = __save_neg;
                if __inner.is_some() {
                    None
                } else {
                    Some(())
                }
            }
        }
    }

    // ── Value manipulation ──────────────────────────────────────────────

    fn emit_enum_wrap(
        &mut self,
        inner: TokenStream,
        variant_name: &str,
        alloc: AllocStrategy,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        let enum_ident = &self.enum_ident;
        let variant = format_ident!("{}", variant_name);
        if alloc == AllocStrategy::Alloc {
            let ir_ctx = ctx.ir_ctx();
            let val = quote! { __v };
            let alloc_expr = ir_ctx.emit_alloc(&val);
            quote! {
                #inner.map(|__inner| {
                    let __v = #enum_ident::#variant(__inner);
                    #alloc_expr
                })
            }
        } else {
            quote! {
                #inner.map(|__v| #enum_ident::#variant(__v))
            }
        }
    }

    fn emit_number_convert(&mut self, _ctx: &mut Self::Ctx) -> TokenStream {
        quote! {
            ::parse_that::css_number_scan_f64(state)
        }
    }

    fn emit_constant(
        &mut self,
        discard_inner: TokenStream,
        value: &str,
        _ctx: &mut Self::Ctx,
    ) -> TokenStream {
        let value_tokens: TokenStream = value.parse().unwrap_or_else(|_| quote! { () });
        quote! {
            {
                #discard_inner?;
                Some(#value_tokens)
            }
        }
    }

    // ── Whitespace (delegated) ──────────────────────────────────────────

    fn emit_ws_trim(
        &mut self,
        ws_pattern: Option<&str>,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_ws_trim_impl(ws_pattern, ctx)
    }

    fn emit_with_ws_trim(
        &mut self,
        inner: TokenStream,
        ws_pattern: Option<&str>,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_with_ws_trim_impl(inner, ws_pattern, ctx)
    }

    // ── Key dispatch (delegated) ────────────────────────────────────────

    fn emit_key_dispatch(
        &mut self,
        config: &KeyDispatchConfig,
        branches: Vec<KeyDispatchBranch<TokenStream>>,
        fallback: Option<(AltBranchInfo, TokenStream)>,
        alloc: AllocStrategy,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_key_dispatch_impl(config, branches, fallback, alloc, ctx)
    }

    // ── Token dispatch (delegated) ─────────────────────────────────────

    fn emit_token_dispatch(
        &mut self,
        token: TokenStream,
        arms: Vec<TokenDispatchArmCompiled<TokenStream>>,
        fallback: TokenStream,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_token_dispatch_impl(token, arms, fallback, ctx)
    }

    // ── Delimiter scan (delegated) ─────────────────────────────────────

    fn emit_delim_scan(
        &mut self,
        config: &DelimScanConfig,
        ctx: &mut Self::Ctx,
    ) -> Option<TokenStream> {
        self.emit_delim_scan_impl(config, ctx)
    }

    // ── Rule-level emission ─────────────────────────────────────────────

    fn emit_rule_body_override(
        &mut self,
        rule: &IrRule,
        ir: &GrammarIR,
        _ctx: &mut Self::Ctx,
    ) -> Option<TokenStream> {
        let name = ir.get_string(rule.name);

        // Fused number: bare JSON number regex → number_scan_convert → (Span, f64).
        if self.fused_number_rules.contains(&rule.id) && !rule.meta.is_transparent {
            return Some(quote! {
                ::parse_that::number_scan_convert(state)
            });
        }

        // Operator chain hot path: Seq(head, Repeat(Seq(op, rhs))).
        // TODO: Port operator_chain::emit_operator_chain_rule when wiring to bbnf-derive.
        // For now, fall through to the driver's generic compile_node.
        let _ = name;

        None
    }

    fn emit_rule_function(
        &mut self,
        rule: &IrRule,
        body: TokenStream,
        ir: &GrammarIR,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        let ir_ctx = ctx.ir_ctx();
        let name = ir.get_string(rule.name);
        let fn_ident = format_ident!("__{}", name);
        let pub_ident = ir_ctx.method_ident_for_name(name);
        let return_type = ir_ctx.rule_return_type(rule.id);
        let enum_ident = &self.enum_ident;
        let enum_type = &ir_ctx.enum_type;

        let hoisted = std::mem::take(&mut ctx.hoisted);

        // Body is already compiled by the driver (compile_node).
        // For Rust backend: __rule returns Option<Enum>, so the body must
        // produce Option<Enum>. The driver handles variant wrapping via
        // compile_ref → emit_call (which returns Self::__rule = Option<Enum>).
        // For non-Ref leaf bodies (Literal/Regex/Seq/Alt), the driver produces
        // the raw inner type. We wrap those in the variant here.
        //
        // Heuristic: if the rule is non-transparent, wrap body in variant.
        // This is correct for leaf/Seq/Alt bodies. For Ref bodies (rule = alias),
        // the body already returns Option<Enum> from __rule call — wrapping again
        // would type-mismatch. But in practice, alias rules are transparent.
        let body_expr = if rule.meta.is_transparent {
            quote! { #(#hoisted)* #body }
        } else {
            let variant = format_ident!("{}", name);
            quote! {
                #(#hoisted)*
                (#body).map(|__x| #enum_ident::#variant(__x))
            }
        };

        // ── Debug instrumentation ───────────────────────────────────────
        let rule_debug = ir.debug_all || rule.meta.directives.debug;
        let fn_body = if rule_debug {
            let trace_entry = crate::backend::rust::trace::emit_trace_entry(name);
            let result_ident = syn::Ident::new("__trace_result", proc_macro2::Span::call_site());
            let trace_exit = crate::backend::rust::trace::emit_trace_exit(name, &result_ident);
            quote! {
                #trace_entry
                let #result_ident = (|| -> Option<#enum_type> { #body_expr })();
                #trace_exit
                #result_ident
            }
        } else {
            body_expr
        };

        let mut methods = Vec::new();

        // ── Internal function ───────────────────────────────────────────
        methods.push(quote! {
            #[allow(non_snake_case)]
            fn #fn_ident<'a>(
                state: &mut ::parse_that::ParserState<'a>,
            ) -> Option<#enum_type> {
                #fn_body
            }
        });

        // ── Sync function for @recover ──────────────────────────────────
        let has_recover = rule.meta.directives.recover.is_some()
            && !ir_ctx.parser_attrs.skip_recover;

        // Note: sync function body compilation is deferred — the driver compiled it
        // as part of the rule body or the grammar will need a separate pass.
        // For now, emit a stub that syncs on the recovery expression if present.
        // TODO: Full sync compilation requires a second compile_node pass for
        // the recovery expression. This will be wired when replacing generate_all().

        // ── Public method(s) ────────────────────────────────────────────
        if rule.meta.is_transparent {
            let alloc_code = ir_ctx.emit_alloc(&quote! { __v });
            let mut pub_parser = quote! {
                Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                    let __v = Self::#fn_ident(state)?;
                    Some(#alloc_code)
                })
            };

            if has_recover {
                let sync_ident = format_ident!("__sync_{}", name);
                let sentinel = ir_ctx.recover_sentinel(rule.id);
                pub_parser = quote! {
                    #pub_parser.recover(Parser::new(Self::#sync_ident), #sentinel)
                };
            }

            methods.push(quote! {
                pub fn #pub_ident<'a>() -> Parser<'a, #return_type> {
                    #pub_parser
                }
            });

            // Unboxed variant.
            let unboxed_ident = ir_ctx.unboxed_method_ident_for_name(name);
            methods.push(quote! {
                #[inline(always)]
                pub fn #unboxed_ident<'a>() -> Parser<'a, #enum_type> {
                    Parser::new(Self::#fn_ident)
                }
            });
        } else {
            let mut pub_parser = quote! { Parser::new(Self::#fn_ident) };

            if has_recover {
                let sync_ident = format_ident!("__sync_{}", name);
                let sentinel = ir_ctx.recover_sentinel(rule.id);
                pub_parser = quote! {
                    #pub_parser.recover(Parser::new(Self::#sync_ident), #sentinel)
                };
            }

            methods.push(quote! {
                pub fn #pub_ident<'a>() -> Parser<'a, #return_type> {
                    #pub_parser
                }
            });
        }

        quote! { #(#methods)* }
    }

    fn emit_type_definitions(
        &mut self,
        _ir: &GrammarIR,
        _analysis: &BackendAnalysis,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        let ir_ctx = ctx.ir_ctx();
        crate::backend::rust::ir_enums::generate_enum(ir_ctx)
    }

    fn emit_grammar(
        &mut self,
        type_defs: TokenStream,
        rule_functions: Vec<TokenStream>,
        ir: &GrammarIR,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        let ir_ctx = ctx.ir_ctx();
        let ident = ir_ctx.ident;
        let parser_attrs = ir_ctx.parser_attrs;

        // Grammar string array.
        let grammar_arr = crate::backend::rust::ir_enums::generate_grammar_arr(parser_attrs, ident);

        // Slab context struct + helper.
        let (alloc_ctx_struct, alloc_ctx_helper) = ir_ctx.generate_alloc_ctx();

        // Recovered static (if any rule has @recover).
        let has_recovers = ir
            .rules
            .iter()
            .any(|r| r.meta.directives.recover.is_some())
            && !parser_attrs.skip_recover;
        let enum_ident = &self.enum_ident;
        let recovered_static = if has_recovers {
            let recovered_ident = ir_ctx.recovered_static_ident();
            quote! {
                static #recovered_ident: #enum_ident<'static> = #enum_ident::Recovered;
            }
        } else {
            quote! {}
        };

        // Debug trace depth counter.
        let has_debug = ir.debug_all || ir.rules.iter().any(|r| r.meta.directives.debug);
        let depth_counter = if has_debug {
            crate::backend::rust::trace::emit_depth_counter()
        } else {
            quote! {}
        };

        quote! {
            use ::parse_that::*;

            #grammar_arr

            #type_defs
            #alloc_ctx_struct
            #alloc_ctx_helper
            #recovered_static

            impl #ident {
                #depth_counter
                #( #rule_functions )*
            }
        }
    }
}
