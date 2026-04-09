//! Value-construction emission for the Rust backend:
//! enum-wrap, number convert, constant, map expression, span capture, hex
//! convert, and the fused-map specializations. Owns the
//! `compile_map_expr_to_tokens` helper that lowers a `MapExpr` tree into
//! a Rust `TokenStream`.

use bbnf_ir::{FnDescriptor, GrammarIR, MapExpr, TypeDesc};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::backend::ValuePlacement;

use super::{RustEmitCtx, RustEmitter};

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

    pub(super) fn emit_enum_wrap_impl(
        &mut self,
        inner: TokenStream,
        variant_name: &str,
        alloc: ValuePlacement,
        ctx: &mut RustEmitCtx,
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

    pub(super) fn emit_number_convert_impl(&mut self, _ctx: &mut RustEmitCtx) -> TokenStream {
        quote! {
            ::parse_that::scan_number_f64(state)
        }
    }

    pub(super) fn emit_constant_impl(
        &mut self,
        discard_inner: TokenStream,
        value: &str,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let value_tokens: TokenStream = value.parse().unwrap_or_else(|_| quote! { () });
        quote! {
            #discard_inner.map(|_| #value_tokens)
        }
    }

    pub(super) fn emit_map_expr_impl(
        &mut self,
        inner: TokenStream,
        expr: &MapExpr,
        return_type: Option<&TypeDesc>,
        ir: &GrammarIR,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // Constant expression: discard parse result, return constant.
        // Uses .map(|_| val) to keep failure propagation compositional —
        // bare `?` would escape to the enclosing closure scope.
        if expr.is_constant() {
            let value_tokens = self.compile_map_expr_to_tokens(expr, return_type, ir);
            return quote! {
                #inner.map(|_| #value_tokens)
            };
        }

        // General case: map parse result through the expression.
        let body_tokens = self.compile_map_expr_to_tokens(expr, return_type, ir);
        quote! {
            #inner.map(|__input| #body_tokens)
        }
    }

    pub(super) fn emit_span_capture_impl(
        &mut self,
        inner: TokenStream,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // Capture start before inner runs, end after. Uses IIFE to scope
        // the start binding while keeping failure propagation compositional.
        quote! {
            (|| {
                let __start = state.offset;
                #inner?;
                Some(::parse_that::Span::new(__start, state.offset, state.src))
            })()
        }
    }

    pub(super) fn emit_hex_convert_impl(
        &mut self,
        inner: TokenStream,
        fn_path: &str,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let fn_path_tokens: TokenStream = fn_path.parse().unwrap_or_else(|_| quote! { todo!() });
        quote! {
            #inner.map(|__s| #fn_path_tokens(__s.as_str()))
        }
    }

    pub(super) fn emit_fused_map_impl(
        &mut self,
        inner: TokenStream,
        inner_fd: &FnDescriptor,
        outer_fd: &FnDescriptor,
        alloc: ValuePlacement,
        ir: &GrammarIR,
        ctx: &mut RustEmitCtx,
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
                    ::parse_that::scan_number_f64(state).map(|__v| #enum_ident::#vident(__v))
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
                        #inner.map(|_| #enum_ident::#vident(#value_tokens))
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
}
