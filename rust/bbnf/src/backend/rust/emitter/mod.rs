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
        _flatten: Option<FlattenStrategy>,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        let mut stmts = Vec::new();
        let mut result_vars = Vec::new();

        for group in groups {
            match group {
                SeqChildGroup::Single { output, ty: _ } => {
                    let var = ctx.fresh("v");
                    stmts.push(quote! { let #var = #output?; });
                    result_vars.push(var);
                }
                SeqChildGroup::SpanCompressed { outputs } => {
                    let var = ctx.fresh("sp");
                    stmts.push(quote! {
                        let __sp_start = state.offset;
                        #( #outputs?; )*
                        let #var = ::parse_that::Span::new(__sp_start, state.offset, state.src);
                    });
                    result_vars.push(var);
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

        let _ = result_type;

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
        _alloc: AllocStrategy,
        _ctx: &mut Self::Ctx,
    ) -> TokenStream {
        let fn_ident = format_ident!("__{}", rule_name);
        quote! { Self::#fn_ident(state) }
    }

    fn emit_inline_wrap(
        &mut self,
        body: TokenStream,
        variant_name: Option<&str>,
        _alloc: AllocStrategy,
        _ctx: &mut Self::Ctx,
    ) -> TokenStream {
        if let Some(name) = variant_name {
            let enum_ident = &self.enum_ident;
            let variant = format_ident!("{}", name);
            quote! {
                #body.map(|__v| #enum_ident::#variant(__v))
            }
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
        _alloc: AllocStrategy,
        _ctx: &mut Self::Ctx,
    ) -> TokenStream {
        let enum_ident = &self.enum_ident;
        let variant = format_ident!("{}", variant_name);
        quote! {
            #inner.map(|__v| #enum_ident::#variant(__v))
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

    fn emit_rule_function(
        &mut self,
        rule: &IrRule,
        body: TokenStream,
        ir: &GrammarIR,
        _ctx: &mut Self::Ctx,
    ) -> TokenStream {
        let rule_name = ir.get_string(rule.name);
        let fn_ident = format_ident!("__{}", rule_name);
        let enum_ident = &self.enum_ident;

        if rule.meta.is_transparent {
            quote! {
                fn #fn_ident<'a>(state: &mut ::parse_that::ParserState<'a>) -> Option<#enum_ident<'a>> {
                    #body
                }
            }
        } else {
            let variant = format_ident!("{}", rule_name);
            quote! {
                fn #fn_ident<'a>(state: &mut ::parse_that::ParserState<'a>) -> Option<#enum_ident<'a>> {
                    let __result = #body;
                    __result.map(|__v| #enum_ident::#variant(__v))
                }
            }
        }
    }

    fn emit_type_definitions(
        &mut self,
        ir: &GrammarIR,
        _analysis: &BackendAnalysis,
        _ctx: &mut Self::Ctx,
    ) -> TokenStream {
        // Minimal enum generation — the full version uses ir_enums::generate_enum.
        let enum_ident = &self.enum_ident;
        let variants: Vec<_> = ir
            .rules
            .iter()
            .filter(|r| !r.meta.is_transparent)
            .map(|r| {
                let name = format_ident!("{}", ir.get_string(r.name));
                // Simplified: all variants hold Span for now.
                quote! { #name(::parse_that::Span<'a>) }
            })
            .collect();

        quote! {
            #[derive(Debug, Clone)]
            pub enum #enum_ident<'a> {
                #( #variants ),*
            }
        }
    }

    fn emit_grammar(
        &mut self,
        type_defs: TokenStream,
        rule_functions: Vec<TokenStream>,
        _ir: &GrammarIR,
        _ctx: &mut Self::Ctx,
    ) -> TokenStream {
        quote! {
            use ::parse_that::*;

            #type_defs

            #( #rule_functions )*
        }
    }
}
