//! Rule-level + grammar-level emission for the Rust backend:
//! `emit_fused_number_rule`, `emit_rule_function`, `emit_type_definitions`,
//! and `emit_grammar`. These produce the per-rule `__rule` function bodies
//! and the grammar-wide `impl` block surrounding them.

use bbnf_ir::{GrammarIR, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::backend::driver::analysis::BackendAnalysis;

use super::{RustEmitCtx, RustEmitter};

impl RustEmitter {
    pub(super) fn emit_fused_number_rule_impl(
        &mut self,
        rule: &IrRule,
        _ir: &GrammarIR,
        _ctx: &mut RustEmitCtx,
    ) -> Option<TokenStream> {
        if !rule.meta.is_transparent {
            Some(quote! {
                ::parse_that::number_fused_scan_convert(state)
            })
        } else {
            None
        }
    }

    pub(super) fn emit_rule_function_impl(
        &mut self,
        rule: &IrRule,
        body: TokenStream,
        sync_body: Option<TokenStream>,
        ir: &GrammarIR,
        ctx: &mut RustEmitCtx,
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
        // For all non-transparent rules, wrap body in variant.
        // The body produces the inner type; wrapping maps it into Enum::Variant(inner).
        // For BoxedEnum rules, body_alloc=Alloc makes inner refs return &Enum,
        // and the variant wrapping produces Enum::Variant(&Enum) = Enum.
        let body_expr = if rule.meta.is_transparent {
            quote! { #(#hoisted)* #body }
        } else {
            let variant = format_ident!("{}", name);
            quote! {
                #(#hoisted)*
                (|| { #body })().map(|__x| #enum_ident::#variant(__x))
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

        let has_recover = rule.meta.directives.recover.is_some()
            && !ir_ctx.parser_attrs.skip_recover;


        // ── Internal function (non-BoxedEnum or transparent rules) ──────
        methods.push(quote! {
            #[allow(non_snake_case)]
            fn #fn_ident<'a>(
                state: &mut ::parse_that::ParserState<'a>,
            ) -> Option<#enum_type> {
                #fn_body
            }
        });

        // ── Sync function for @recover ──────────────────────────────────

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

    pub(super) fn emit_type_definitions_impl(
        &mut self,
        _ir: &GrammarIR,
        _analysis: &BackendAnalysis,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // CST helpers (children/span_text/walk_children/visitor) are emitted
        // by the frontend `grammar::schema::emit::rust` track. The backend
        // only emits the enum type definition.
        let ir_ctx = ctx.ir_ctx();
        crate::backend::rust::ir_enums::generate_enum(ir_ctx)
    }

    pub(super) fn emit_grammar_impl(
        &mut self,
        type_defs: TokenStream,
        rule_functions: Vec<TokenStream>,
        ir: &GrammarIR,
        ctx: &mut RustEmitCtx,
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
