//! Rule-level + grammar-level emission for the Rust backend under
//! Tranche AC.2 tape-first.
//!
//! `emit_rule_function_impl` wraps a pre-compiled body expression
//! in the rule's prelude + epilogue, dispatched on the rule's
//! materialization class:
//!
//! - `MustTape` — `mark_children` prelude + `push_compound`
//!   epilogue.
//! - `TapeSpanOnly` — `__span_lo` prelude + `push_leaf` epilogue.
//! - `TransparentElide` — no function is emitted. The driver
//!   inlines transparent bodies at every call site; this method
//!   returns an empty token stream for them.
//!
//! `emit_grammar_impl` assembles the grammar-wide `impl` block:
//! the grammar string array, the view types (from
//! [`crate::backend::rust::view::generate_views`]), all rule
//! functions, and a single public `parse(input)` entry point that
//! constructs a [`::bbnf::runtime::Parsed`] from a finished tape.

use bbnf_ir::{GrammarIR, IrRule};
use bbnf_ir::passes::MaterializationClass;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::backend::driver::analysis::BackendAnalysis;

use super::tape_prelude::{
    emit_must_tape_epilogue, emit_must_tape_prelude, emit_rule_signature,
    emit_tape_span_only_epilogue, emit_tape_span_only_prelude,
};
use super::{RustEmitCtx, RustEmitter};

impl RustEmitter {
    pub(super) fn emit_fused_number_rule_impl(
        &mut self,
        rule: &IrRule,
        _ir: &GrammarIR,
        _ctx: &mut RustEmitCtx,
    ) -> Option<TokenStream> {
        // Fused number rules evaluate to `Option<()>` — the
        // scanner advances `state.offset`; the f64 projection is
        // a view-layer concern.
        if !rule.meta.is_transparent {
            Some(quote! {
                (::parse_that::scan_number_f64(state)).map(|_| ())
            })
        } else {
            None
        }
    }

    /// Look up the materialization class for a rule's body node.
    fn materialization_for_rule(
        ir: &GrammarIR,
        rule: &IrRule,
    ) -> MaterializationClass {
        // `ir.materialization` is keyed by `NodeId` via `ir.dag`.
        // A rule without a dag-mapped body defaults to `MustTape`
        // — the safest choice because it preserves every child.
        if let Some(dag) = ir.dag.as_ref() {
            if let Some(node_id) = dag.node_for(&rule.body) {
                if let Some(class) = ir.materialization.get(&node_id) {
                    return *class;
                }
            }
        }
        MaterializationClass::MustTape
    }

    pub(super) fn emit_rule_function_impl(
        &mut self,
        rule: &IrRule,
        body: TokenStream,
        sync_body: Option<TokenStream>,
        ir: &GrammarIR,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let name = ir.get_string(rule.name).to_string();
        let class = Self::materialization_for_rule(ir, rule);

        // TransparentElide rules do not emit a function at all —
        // the driver inlines their body at every call site.
        if class == MaterializationClass::TransparentElide {
            return quote! {};
        }

        // Variant discriminator: the rule's index in ir.rules,
        // capped at u8::MAX. The view layer uses this to branch on
        // which rule produced a compound / leaf record.
        let rule_idx_u8: u8 = {
            let idx = rule.id as usize;
            debug_assert!(idx <= u8::MAX as usize, "rule id overflows u8 variant_idx");
            (idx & 0xFF) as u8
        };

        let (prelude, epilogue) = match class {
            MaterializationClass::MustTape => (
                emit_must_tape_prelude(),
                emit_must_tape_epilogue(rule_idx_u8),
            ),
            MaterializationClass::TapeSpanOnly => (
                emit_tape_span_only_prelude(),
                emit_tape_span_only_epilogue(rule_idx_u8),
            ),
            MaterializationClass::TransparentElide => unreachable!(),
        };

        let signature = emit_rule_signature(&name);

        // The body is a pre-compiled sub-parse expression
        // evaluating to either `Option<()>` or
        // `Option<TapeOffset>`. We `match` on it to make both
        // shapes compose under one uniform failure path.
        let rule_debug = ir.debug_all || rule.meta.directives.debug;
        let body_block = if rule_debug {
            let trace_entry = crate::backend::rust::trace::emit_trace_entry(&name);
            let trace_ident = syn::Ident::new("__trace_result", proc_macro2::Span::call_site());
            let trace_exit = crate::backend::rust::trace::emit_trace_exit(&name, &trace_ident);
            quote! {
                'rule_blk: {
                    #prelude
                    #trace_entry
                    let #trace_ident: ::core::option::Option<::bbnf::runtime::tape::TapeOffset> =
                        'trace_blk: {
                            match ({ #body }) {
                                Some(_) => (),
                                None => break 'trace_blk None,
                            }
                            #epilogue
                        };
                    #trace_exit
                    break 'rule_blk #trace_ident;
                }
            }
        } else {
            quote! {
                'rule_blk: {
                    #prelude
                    match ({ #body }) {
                        Some(_) => (),
                        None => break 'rule_blk None,
                    }
                    #epilogue
                }
            }
        };

        let mut methods = Vec::new();

        methods.push(quote! {
            #signature {
                #body_block
            }
        });

        // @recover sync function. Emits Option<()>; the sync
        // expression is side-effecting only.
        let has_recover = rule.meta.directives.recover.is_some()
            && !ctx.ir_ctx().parser_attrs.skip_recover;
        if has_recover {
            if let Some(sync_expr) = sync_body {
                let sync_ident = format_ident!("__sync_{}", name);
                methods.push(quote! {
                    #[allow(non_snake_case)]
                    fn #sync_ident<'a>(
                        state: &mut ::parse_that::ParserState<'a>,
                    ) -> ::core::option::Option<()> {
                        match (#sync_expr) {
                            Some(_) => Some(()),
                            None => None,
                        }
                    }
                });
            }
        }

        quote! { #(#methods)* }
    }

    pub(super) fn emit_type_definitions_impl(
        &mut self,
        ir: &GrammarIR,
        _analysis: &BackendAnalysis,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // Tranche AC.2 — view types replace the allocating enum.
        // `generate_views` emits one `<Rule>View<'tape>` per non-
        // transparent rule plus the `Root` trait binding.
        let ir_ctx = ctx.ir_ctx();
        crate::backend::rust::view::generate_views(ir, ir_ctx)
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

        // Grammar string const array.
        let grammar_arr =
            crate::backend::rust::ir_enums::generate_grammar_arr(parser_attrs, ident);

        // Root rule — the entry point for `parse(input)`. Pulled
        // from `ir.entry`, which is set at lowering time and
        // preserved through every IR pass. Fall back to the first
        // non-transparent rule only as a defensive guard.
        let root_rule_name = ir
            .rules
            .iter()
            .find(|r| r.id == ir.entry && !r.meta.is_transparent)
            .or_else(|| ir.rules.iter().find(|r| !r.meta.is_transparent))
            .map(|r| ir.get_string(r.name))
            .unwrap_or_else(|| {
                let names: Vec<String> = ir
                    .rules
                    .iter()
                    .map(|r| format!("{}{}", ir.get_string(r.name),
                        if r.meta.is_transparent { "(T)" } else { "" }))
                    .collect();
                panic!(
                    "tape-first emitter requires at least one non-transparent rule. \
                     ir.entry={}, rule count={}, rules=[{}]",
                    ir.entry,
                    ir.rules.len(),
                    names.join(", "),
                )
            });
        let root_fn_ident = format_ident!("__{}", root_rule_name);

        // Debug trace depth counter (only emitted if any rule
        // uses @debug).
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

            impl #ident {
                #depth_counter
                #( #rule_functions )*
                #extra

                /// Parse an input string and return an owning
                /// `Parsed<Self>` bound to the grammar's root view
                /// type via the [`::bbnf::runtime::Root`] trait.
                pub fn parse(
                    input: &str,
                ) -> ::core::result::Result<
                    ::bbnf::runtime::Parsed<Self>,
                    ::bbnf::runtime::ParseErr,
                > {
                    let owned: ::std::string::String = input.to_owned();
                    let mut state = ::parse_that::ParserState::new(&owned);
                    let mut builder =
                        ::bbnf::runtime::tape::TapeBuilder::with_capacity(
                            owned.len() / 8,
                        );
                    let root_off = Self::#root_fn_ident(&mut state, &mut builder)
                        .ok_or(::bbnf::runtime::ParseErr::Syntax {
                            offset: state.offset as u32,
                            rule: None,
                        })?;
                    if state.offset < owned.len() {
                        return ::core::result::Result::Err(
                            ::bbnf::runtime::ParseErr::Syntax {
                                offset: state.offset as u32,
                                rule: None,
                            },
                        );
                    }
                    let tape = builder
                        .finish()
                        .map_err(::bbnf::runtime::ParseErr::Tape)?;
                    ::core::result::Result::Ok(
                        ::bbnf::runtime::Parsed::new(tape, owned, root_off),
                    )
                }
            }
        }
    }
}
