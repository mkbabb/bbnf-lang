//! Rust backend: implements [`Emitter`] to produce `proc_macro2::TokenStream`.
//!
//! This module bridges the shared compilation driver with the existing Rust codegen
//! infrastructure. Each trait method produces Rust code via `quote!`.

use std::collections::HashSet;

use bbnf_ir::{AltDispatch, GrammarIR, IrRule, RuleId, TypeDesc};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::backend::analysis::BackendAnalysis;
use crate::backend::{
    AllocStrategy, AltBranchInfo, Emitter, FlattenStrategy, SepByConfig, SeqChildGroup,
};

// ─── Rust Emitter ───────────────────────────────────────────────────────────

/// Rust code emitter implementing the [`Emitter`] trait.
///
/// Produces `proc_macro2::TokenStream` for monolithic recursive descent parsers
/// with slab allocation.
pub struct RustEmitter {
    /// Enum name (e.g., `JsonParserEnum`).
    pub enum_ident: syn::Ident,
    /// Whether prettify codegen is active.
    pub effective_prettify: bool,
    /// Rules with fused number scan+convert.
    pub fused_number_rules: HashSet<RuleId>,
}

/// Mutable context for Rust emission — backend-specific state.
pub struct RustEmitCtx {
    /// Hoisted let-bindings emitted before rule bodies.
    pub hoisted: Vec<TokenStream>,
    /// Counter for generating unique variable names.
    pub counter: usize,
}

impl RustEmitCtx {
    pub fn new() -> Self {
        Self {
            hoisted: Vec::new(),
            counter: 0,
        }
    }

    /// Generate a fresh unique identifier.
    pub fn fresh(&mut self, prefix: &str) -> syn::Ident {
        let id = self.counter;
        self.counter += 1;
        format_ident!("__{}{}", prefix, id)
    }
}

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

        // Handle Vec result type (flatten would go here).
        let _ = result_type; // Used for future flatten/scratch logic

        quote! {
            (|| {
                #( #stmts )*
                Some(#result_expr)
            })()
        }
    }

    // ── Alternations ────────────────────────────────────────────────────

    fn emit_alt_dispatch(
        &mut self,
        table: &AltDispatch,
        branches: Vec<(AltBranchInfo, TokenStream)>,
        fallback: Option<(AltBranchInfo, TokenStream)>,
        _alloc: AllocStrategy,
        _ctx: &mut Self::Ctx,
    ) -> TokenStream {
        // Build match arms from dispatch table.
        let mut arms = Vec::new();

        for (branch_idx, (_info, body)) in branches.iter().enumerate() {
            // Collect all bytes that map to this branch.
            let byte_patterns: Vec<u8> = table
                .table
                .iter()
                .enumerate()
                .filter(|&(_, &b)| b as usize == branch_idx)
                .map(|(byte_val, _)| byte_val as u8)
                .collect();

            if byte_patterns.is_empty() {
                continue;
            }

            let patterns: Vec<_> = byte_patterns.iter().map(|b| quote! { #b }).collect();
            arms.push(quote! {
                #( #patterns )|* => { #body }
            });
        }

        // Fallback arm.
        let fallback_expr = if let Some((_info, fb_body)) = fallback {
            quote! { _ => { #fb_body } }
        } else {
            quote! { _ => None }
        };
        arms.push(fallback_expr);

        quote! {
            if state.offset < state.src.len() {
                match state.src.as_bytes()[state.offset] {
                    #( #arms ),*
                }
            } else {
                None
            }
        }
    }

    fn emit_alt_checkpoint(
        &mut self,
        branches: Vec<(AltBranchInfo, TokenStream)>,
        _alloc: AllocStrategy,
        _ctx: &mut Self::Ctx,
    ) -> TokenStream {
        if branches.len() == 1 {
            let (_, body) = &branches[0];
            return body.clone();
        }

        let mut chain = Vec::new();
        for (_info, body) in &branches {
            chain.push(quote! {
                {
                    let __cp = state.offset;
                    let __result = #body;
                    if __result.is_some() {
                        return __result;
                    }
                    state.offset = __cp;
                }
            });
        }

        quote! {
            (|| {
                #( #chain )*
                None
            })()
        }
    }

    fn emit_alt_all_literal(
        &mut self,
        literals: Vec<(String, TokenStream)>,
        _alloc: AllocStrategy,
        _ctx: &mut Self::Ctx,
    ) -> TokenStream {
        // Sequential literal matching — try each in order.
        let mut arms = Vec::new();
        for (value, _body) in &literals {
            let len = value.len();
            let lit = proc_macro2::Literal::string(value);
            arms.push(quote! {
                if state.src[state.offset..].starts_with(#lit) {
                    state.offset += #len;
                    return Some(::parse_that::Span::new(state.offset - #len, state.offset, state.src));
                }
            });
        }
        // Fallback to body outputs for Map(Literal, Constant) patterns.
        let _ = arms;

        // Simple approach: checkpoint/restore chain.
        self.emit_alt_checkpoint(
            literals
                .into_iter()
                .map(|(_, body)| {
                    (
                        AltBranchInfo {
                            ty: TypeDesc::Span,
                            coercion_variant: None,
                        },
                        body,
                    )
                })
                .collect(),
            AllocStrategy::Elide,
            _ctx,
        )
    }

    // ── Repetition ──────────────────────────────────────────────────────

    fn emit_repeat_many(
        &mut self,
        body: TokenStream,
        lo: u32,
        _hi: u32,
        _elem_type: &TypeDesc,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        let count_var = ctx.fresh("count");
        let lo_lit = lo as usize;
        quote! {
            (|| {
                let __sp_start = state.offset;
                let mut #count_var: usize = 0;
                loop {
                    let __prev = state.offset;
                    match #body {
                        Some(_) => {
                            #count_var += 1;
                            if state.offset == __prev { break; }
                        }
                        None => break,
                    }
                }
                if #count_var >= #lo_lit {
                    Some(::parse_that::Span::new(__sp_start, state.offset, state.src))
                } else {
                    None
                }
            })()
        }
    }

    fn emit_repeat_optional(
        &mut self,
        body: TokenStream,
        _inner_type: &TypeDesc,
        _alloc: AllocStrategy,
        _ctx: &mut Self::Ctx,
    ) -> TokenStream {
        quote! {
            {
                let __cp = state.offset;
                match #body {
                    Some(__v) => Some(Some(__v)),
                    None => {
                        state.offset = __cp;
                        Some(None)
                    }
                }
            }
        }
    }

    fn emit_sep_by(
        &mut self,
        element: TokenStream,
        separator: TokenStream,
        config: &SepByConfig,
        _elem_type: &TypeDesc,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        let count_var = ctx.fresh("count");
        let lo_lit = config.lo as usize;
        quote! {
            (|| {
                let __sp_start = state.offset;
                let mut #count_var: usize = 0;

                // First element.
                match #element {
                    Some(_) => { #count_var += 1; }
                    None => {
                        return if #count_var >= #lo_lit {
                            Some(::parse_that::Span::new(__sp_start, state.offset, state.src))
                        } else {
                            None
                        };
                    }
                }

                // Separator + element loop.
                loop {
                    let __cp = state.offset;
                    match #separator {
                        Some(_) => {}
                        None => break,
                    }
                    match #element {
                        Some(_) => { #count_var += 1; }
                        None => {
                            state.offset = __cp;
                            break;
                        }
                    }
                }

                if #count_var >= #lo_lit {
                    Some(::parse_that::Span::new(__sp_start, state.offset, state.src))
                } else {
                    None
                }
            })()
        }
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

    fn emit_ws_trim(
        &mut self,
        ws_pattern: Option<&str>,
        _ctx: &mut Self::Ctx,
    ) -> TokenStream {
        if let Some(pattern) = ws_pattern {
            let opts =
                crate::generate::regex::EmitOpts::new(&crate::generate::regex::CostModel::DEFAULT);
            let code = crate::generate::regex::emit_regex(pattern, &opts);
            quote! { { #code; Some(()) } }
        } else {
            quote! { { ::parse_that::trim_leading_whitespace_mut(state); Some(()) } }
        }
    }

    fn emit_with_ws_trim(
        &mut self,
        inner: TokenStream,
        ws_pattern: Option<&str>,
        _ctx: &mut Self::Ctx,
    ) -> TokenStream {
        let trim = if let Some(pattern) = ws_pattern {
            let opts =
                crate::generate::regex::EmitOpts::new(&crate::generate::regex::CostModel::DEFAULT);
            let code = crate::generate::regex::emit_regex(pattern, &opts);
            quote! { #code; }
        } else {
            quote! { ::parse_that::trim_leading_whitespace_mut(state); }
        };
        quote! {
            {
                #trim
                let __ws_inner = #inner;
                #trim
                __ws_inner
            }
        }
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
