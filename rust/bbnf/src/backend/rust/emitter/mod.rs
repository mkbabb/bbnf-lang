//! Emitter trait implementation for the Rust backend.

mod alt;
mod dispatch;
mod repeat;
mod ws;

use bbnf_ir::{FnDescriptor, GrammarIR, IrRule, MapExpr, RuleId, TypeDesc};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::backend::analysis::BackendAnalysis;
use crate::backend::key_dispatch::KeyDispatchConfig;
use crate::backend::{
    ValuePlacement, AltBranchInfo, DelimScanConfig, Emitter, FlattenStrategy, KeyDispatchBranch,
    SepByConfig, SeqChildGroup, TokenDispatchArmCompiled,
};

pub use super::emitter_types::{RustEmitCtx, RustEmitter};

// ─── MapExpr → TokenStream ─────────────────────────────────────────────────

impl RustEmitter {
    /// Compile a `MapExpr` tree to a Rust `TokenStream` expression.
    ///
    /// `return_type` carries the type annotation from `FnDescriptor::Expr` —
    /// used to emit correctly-suffixed Rust literals (e.g., `0u8` not `0i64`).
    /// The variable `__input` is in scope and holds the parse result.
    fn compile_map_expr_to_tokens(
        &self,
        expr: &MapExpr,
        return_type: Option<&TypeDesc>,
        ir: &GrammarIR,
    ) -> TokenStream {
        match expr {
            MapExpr::IntLit(n) => {
                // Use return_type to emit correctly-suffixed literal.
                if let Some(TypeDesc::Named(sid)) = return_type {
                    let type_name = ir.get_string(*sid);
                    let suffixed = format!("{}{}", n, type_name);
                    suffixed.parse::<TokenStream>().unwrap_or_else(|_| {
                        let lit = proc_macro2::Literal::i64_unsuffixed(*n);
                        quote! { #lit }
                    })
                } else {
                    let lit = proc_macro2::Literal::i64_unsuffixed(*n);
                    quote! { #lit }
                }
            }
            MapExpr::FloatLit(f) => {
                if let Some(TypeDesc::Named(sid)) = return_type {
                    let type_name = ir.get_string(*sid);
                    let suffixed = format!("{}{}", f, type_name);
                    suffixed.parse::<TokenStream>().unwrap_or_else(|_| {
                        let lit = proc_macro2::Literal::f64_unsuffixed(*f);
                        quote! { #lit }
                    })
                } else {
                    let lit = proc_macro2::Literal::f64_unsuffixed(*f);
                    quote! { #lit }
                }
            }
            MapExpr::BoolLit(b) => {
                if *b { quote! { true } } else { quote! { false } }
            }
            MapExpr::StringLit(sid) => {
                let s = ir.get_string(*sid);
                quote! { #s }
            }
            MapExpr::Input => quote! { __input },
            MapExpr::InputProp { prop } => {
                let prop_name = ir.get_string(*prop);
                let prop_ident = format_ident!("{}", prop_name);
                quote! { __input.#prop_ident() }
            }
            MapExpr::FnCall { name, args } => {
                let fn_name_str = ir.get_string(*name);
                if let Ok(fn_path) = fn_name_str.parse::<TokenStream>() {
                    let compiled_args: Vec<TokenStream> = args
                        .iter()
                        .map(|a| self.compile_map_expr_to_tokens(a, None, ir))
                        .collect();
                    if compiled_args.is_empty() {
                        quote! { (#fn_path)(__input) }
                    } else {
                        quote! { #fn_path(#(#compiled_args),*) }
                    }
                } else {
                    quote! { todo!("invalid fn path") }
                }
            }
            MapExpr::BinOp { op, lhs, rhs } => {
                let l = self.compile_map_expr_to_tokens(lhs, None, ir);
                let r = self.compile_map_expr_to_tokens(rhs, None, ir);
                let op_token = match op {
                    bbnf_ir::MapBinOp::Add => quote! { + },
                    bbnf_ir::MapBinOp::Sub => quote! { - },
                    bbnf_ir::MapBinOp::Mul => quote! { * },
                    bbnf_ir::MapBinOp::Div => quote! { / },
                    bbnf_ir::MapBinOp::Mod => quote! { % },
                    bbnf_ir::MapBinOp::Eq => quote! { == },
                    bbnf_ir::MapBinOp::Ne => quote! { != },
                    bbnf_ir::MapBinOp::Lt => quote! { < },
                    bbnf_ir::MapBinOp::Gt => quote! { > },
                    bbnf_ir::MapBinOp::Le => quote! { <= },
                    bbnf_ir::MapBinOp::Ge => quote! { >= },
                    bbnf_ir::MapBinOp::And => quote! { && },
                    bbnf_ir::MapBinOp::Or => quote! { || },
                    bbnf_ir::MapBinOp::BitAnd => quote! { & },
                    bbnf_ir::MapBinOp::BitOr => quote! { | },
                    bbnf_ir::MapBinOp::Shl => quote! { << },
                    bbnf_ir::MapBinOp::Shr => quote! { >> },
                };
                quote! { (#l #op_token #r) }
            }
            MapExpr::UnaryOp { op, inner } => {
                let i = self.compile_map_expr_to_tokens(inner, None, ir);
                match op {
                    bbnf_ir::MapUnaryOp::Neg => quote! { (-#i) },
                    bbnf_ir::MapUnaryOp::Not => quote! { (!#i) },
                }
            }
        }
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
        alloc: ValuePlacement,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_alt_dispatch_impl(table, branches, fallback, alloc, ctx)
    }

    fn emit_alt_checkpoint(
        &mut self,
        branches: Vec<(AltBranchInfo, TokenStream)>,
        alloc: ValuePlacement,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_alt_checkpoint_impl(branches, alloc, ctx)
    }

    fn emit_alt_all_literal(
        &mut self,
        literals: Vec<(String, TokenStream)>,
        alloc: ValuePlacement,
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
        alloc: ValuePlacement,
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
        alloc: ValuePlacement,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        let fn_ident = format_ident!("__{}", rule_name);
        if alloc == ValuePlacement::Alloc {
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
        alloc: ValuePlacement,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        if let Some(name) = variant_name {
            let enum_ident = &self.enum_ident;
            let variant = format_ident!("{}", name);
            if alloc == ValuePlacement::Alloc {
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
        } else if alloc == ValuePlacement::Alloc {
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
        head_type: &TypeDesc,
        link_elem_type: &TypeDesc,
        _ir: &GrammarIR,
        ctx: &mut Self::Ctx,
    ) -> Option<TokenStream> {
        let ir_ctx = ctx.ir_ctx();

        // Only handle typed chains (not all-Span).
        if *head_type == TypeDesc::Span {
            return None;
        }

        let depth_var = ctx.fresh("chain_depth");
        let head_var = ctx.fresh("chain_head");
        let prev_var = ctx.fresh("chain_prev");
        let op_var = ctx.fresh("chain_op");
        let rhs_var = ctx.fresh("chain_rhs");

        let init_code = ir_ctx.emit_scratch_init(link_elem_type, &depth_var);
        let push_code = ir_ctx.emit_scratch_push(link_elem_type, &quote! { (#op_var, #rhs_var) });
        let collect_code = ir_ctx.emit_scratch_collect(link_elem_type, &depth_var);

        Some(quote! {
            {
                let #head_var = #head?;
                #init_code
                loop {
                    let #prev_var = state.offset;
                    match (|| {
                        let #op_var = #op?;
                        let #rhs_var = #rhs?;
                        Some((#op_var, #rhs_var))
                    })() {
                        Some(__value) => {
                            let (#op_var, #rhs_var) = __value;
                            #push_code;
                            if state.offset == #prev_var {
                                break;
                            }
                        }
                        None => {
                            state.offset = #prev_var;
                            break;
                        }
                    }
                }
                Some((#head_var, #collect_code))
            }
        })
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
        alloc: ValuePlacement,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        let enum_ident = &self.enum_ident;
        let variant = format_ident!("{}", variant_name);
        if alloc == ValuePlacement::Alloc {
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

    fn emit_map_expr(
        &mut self,
        inner: TokenStream,
        expr: &MapExpr,
        return_type: Option<&TypeDesc>,
        _alloc: ValuePlacement,
        ir: &GrammarIR,
        _ctx: &mut Self::Ctx,
    ) -> TokenStream {
        // Check if this is a constant expression (no input dependency).
        if expr.is_constant() {
            let value_tokens = self.compile_map_expr_to_tokens(expr, return_type, ir);
            return quote! {
                {
                    #inner?;
                    Some(#value_tokens)
                }
            };
        }

        // General case: map parse result through the expression.
        let body_tokens = self.compile_map_expr_to_tokens(expr, return_type, ir);
        quote! {
            #inner.map(|__input| #body_tokens)
        }
    }

    fn emit_span_capture(
        &mut self,
        inner: TokenStream,
        _ctx: &mut Self::Ctx,
    ) -> TokenStream {
        quote! {
            {
                let __start = state.offset();
                #inner?;
                let __end = state.offset();
                Some(::parse_that::Span::new(state.input(), __start, __end))
            }
        }
    }

    fn emit_hex_convert(
        &mut self,
        inner: TokenStream,
        fn_path: &str,
        _ctx: &mut Self::Ctx,
    ) -> TokenStream {
        let fn_path_tokens: TokenStream = fn_path.parse().unwrap_or_else(|_| quote! { todo!() });
        quote! {
            #inner.map(|__s| #fn_path_tokens(__s.as_str()))
        }
    }

    fn emit_fused_map(
        &mut self,
        inner: TokenStream,
        inner_fd: &FnDescriptor,
        outer_fd: &FnDescriptor,
        alloc: ValuePlacement,
        ir: &GrammarIR,
        ctx: &mut Self::Ctx,
    ) -> Option<TokenStream> {
        let enum_ident = &self.enum_ident;
        match (inner_fd, outer_fd) {
            // EnumWrap + BoxWrap → variant wrap + alloc
            (FnDescriptor::EnumWrap { variant }, FnDescriptor::BoxWrap) => {
                let vname = ir.get_string(*variant);
                let vident = format_ident!("{}", vname);
                let ir_ctx = ctx.ir_ctx();
                if alloc == ValuePlacement::Alloc {
                    let alloc_code = ir_ctx.emit_alloc_let(&quote! { #enum_ident::#vident(__x) });
                    Some(quote! {
                        #inner.map(|__x| {
                            #alloc_code
                        })
                    })
                } else {
                    Some(quote! {
                        #inner.map(|__x| #enum_ident::#vident(__x))
                    })
                }
            }
            // BoxWrap + EnumWrap → alloc + variant wrap
            (FnDescriptor::BoxWrap, FnDescriptor::EnumWrap { variant }) => {
                let vname = ir.get_string(*variant);
                let vident = format_ident!("{}", vname);
                let ir_ctx = ctx.ir_ctx();
                let alloc_code = ir_ctx.emit_alloc(&quote! { __x });
                Some(quote! {
                    #inner.map(|__x| {
                        #enum_ident::#vident(#alloc_code)
                    })
                })
            }
            // NumberConvert + EnumWrap → fused number + variant
            (FnDescriptor::NumberConvert, FnDescriptor::EnumWrap { variant }) => {
                let vname = ir.get_string(*variant);
                let vident = format_ident!("{}", vname);
                Some(quote! {
                    ::parse_that::css_number_scan_f64(state).map(|__v| #enum_ident::#vident(__v))
                })
            }
            // HexConvert + EnumWrap → fused hex + variant
            (FnDescriptor::HexConvert { fn_path }, FnDescriptor::EnumWrap { variant }) => {
                let vname = ir.get_string(*variant);
                let vident = format_ident!("{}", vname);
                let path_str = ir.get_string(*fn_path);
                let path_tokens: TokenStream = path_str.parse().unwrap_or_else(|_| quote! { todo!() });
                Some(quote! {
                    #inner.map(|__s| #enum_ident::#vident(#path_tokens(__s.as_str())))
                })
            }
            // Expr + EnumWrap → fused expr + variant
            (FnDescriptor::Expr { expr, return_type }, FnDescriptor::EnumWrap { variant }) => {
                let vname = ir.get_string(*variant);
                let vident = format_ident!("{}", vname);
                if expr.is_constant() {
                    let value_tokens = self.compile_map_expr_to_tokens(expr, return_type.as_ref(), ir);
                    Some(quote! {
                        {
                            #inner?;
                            Some(#enum_ident::#vident(#value_tokens))
                        }
                    })
                } else {
                    let body = self.compile_map_expr_to_tokens(expr, return_type.as_ref(), ir);
                    Some(quote! {
                        #inner.map(|__input| #enum_ident::#vident(#body))
                    })
                }
            }
            _ => None,
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
        alloc: ValuePlacement,
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

    fn emit_fused_number_rule(
        &mut self,
        rule: &IrRule,
        _ir: &GrammarIR,
        _ctx: &mut Self::Ctx,
    ) -> Option<TokenStream> {
        if !rule.meta.is_transparent {
            Some(quote! {
                ::parse_that::number_scan_convert(state)
            })
        } else {
            None
        }
    }

    fn emit_operator_chain_rule(
        &mut self,
        rule: &IrRule,
        ir: &GrammarIR,
        ctx: &mut Self::Ctx,
    ) -> Option<TokenStream> {
        // Fallback: use monolithic operator_chain module for grammars where the
        // Seq-level emit_operator_chain doesn't fire (missing TypeMap entries).
        let ir_ctx = ctx.ir_ctx();
        let call_strategies = crate::pipeline::compute_call_strategies(ir);
        let call_modes: Vec<_> = call_strategies.iter().map(|s| match s {
            crate::backend::CallStrategy::DirectCall => {
                crate::backend::rust::analysis::inline::CallMode::DirectCall
            }
            crate::backend::CallStrategy::InlineBody | crate::backend::CallStrategy::InlineFusion => {
                crate::backend::rust::analysis::inline::CallMode::InlineBody
            }
        }).collect();
        let mut mctx = crate::backend::rust::MonoCtx::new(call_modes);
        mctx.current_rule_id = Some(rule.id);
        mctx.current_rule_name = Some(ir.get_string(rule.name).to_string());
        if let Some(chain_expr) =
            crate::backend::rust::operator_chain::emit_operator_chain_rule(
                rule.id, ir_ctx, &mut mctx,
            )
        {
            ctx.hoisted.extend(mctx.hoisted);
            return Some(chain_expr);
        }
        None
    }

    fn emit_rule_function(
        &mut self,
        rule: &IrRule,
        body: TokenStream,
        sync_body: Option<TokenStream>,
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
            // Check if the variant expects BoxedEnum (&'a Enum).
            // Match the enum generation logic: explicit BoxedEnum in ir.types,
            // OR missing from ir.types entirely (fallback to boxed_enum_type).
            let rule_type_entry = ir_ctx.ir.types.iter()
                .find(|(id, _)| *id == rule.id);
            let rule_inner_is_boxed = match rule_type_entry {
                Some((_, TypeDesc::BoxedEnum)) => true,
                None => true, // fallback: enum gen uses boxed_enum_type
                _ => false,
            };
            if rule_inner_is_boxed {
                let alloc_expr = ir_ctx.emit_alloc(&quote! { __x });
                quote! {
                    #(#hoisted)*
                    (#body).map(|__x| #enum_ident::#variant(#alloc_expr))
                }
            } else {
                quote! {
                    #(#hoisted)*
                    (#body).map(|__x| #enum_ident::#variant(__x))
                }
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

        if has_recover {
            if let Some(sync_expr) = sync_body {
                let sync_ident = format_ident!("__sync_{}", name);
                methods.push(quote! {
                    #[allow(non_snake_case)]
                    fn #sync_ident<'a>(
                        state: &mut ::parse_that::ParserState<'a>,
                    ) -> Option<()> {
                        (#sync_expr).map(|_| ())
                    }
                });
            }
        }

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

        let extra = &self.extra_impl_methods;

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
                #extra
            }
        }
    }
}
