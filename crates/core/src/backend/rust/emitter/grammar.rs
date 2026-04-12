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

use bbnf_ir::{GrammarIR, IrNode, IrRule};
use bbnf_ir::passes::{EmissionTier, MaterializationClass};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::backend::driver::analysis::BackendAnalysis;

use super::tape_prelude::{
    emit_direct_inner_signature, emit_direct_shim_signature, emit_must_tape_epilogue,
    emit_must_tape_prelude, emit_rule_signature, emit_tape_span_only_epilogue,
    emit_tape_span_only_prelude,
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
    ///
    /// Identity-bearing rules — the grammar entry and any rule with
    /// `preserve_identity` set — always resolve to `MustTape`
    /// regardless of what the bottom-up classifier assigned: the
    /// generated `parse()` helper dispatches through the entry's
    /// `__<name>` function by name, and `preserve_identity` rules
    /// are structural-mode guarantees that each named rule has a
    /// standalone callable. Without this override the emitter would
    /// skip their function bodies and downstream references would
    /// dangle.
    fn materialization_for_rule(
        ir: &GrammarIR,
        rule: &IrRule,
    ) -> MaterializationClass {
        // `preserve_identity` rules must always push a compound.
        // The entry rule is NOT forced — its body classification
        // determines whether it uses push_leaf (TapeSpanOnly) or
        // push_compound (MustTape). Both produce a TapeOffset
        // valid for `Parsed::root_offset`. The view layer reads
        // variant_idx from flags, which both paths store.
        if rule.meta.preserve_identity {
            return MaterializationClass::MustTape;
        }
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

        // TransparentElide rules do not emit a function at all ��
        // the driver inlines their body at every call site.
        if class == MaterializationClass::TransparentElide {
            return quote! {};
        }

        let tier = ir
            .emission_tier
            .get(&rule.id)
            .copied()
            .unwrap_or(EmissionTier::Tape);

        // Direct-tier rules emit a three-function triad:
        // inner (shared parse body) + tape wrapper + direct shim.
        if tier == EmissionTier::Direct {
            return self.emit_direct_tier_rule(&name, rule, body, sync_body, ir, ctx, class);
        }

        // Tape / Lazy — existing behaviour.
        self.emit_tape_tier_rule(&name, rule, body, sync_body, ir, ctx, class)
    }

    /// Emit a Tape-tier (or Lazy-tier) rule: the standard prelude +
    /// body + epilogue pattern from Tranche AC.2.
    ///
    /// Tranche AK.1: for Alt-bodied rules, the epilogue uses a
    /// `__branch_idx` variable (set per-arm by the Alt emitter)
    /// instead of the rule's global ID. This gives the view layer
    /// correct branch discrimination.
    fn emit_tape_tier_rule(
        &mut self,
        name: &str,
        rule: &IrRule,
        body: TokenStream,
        sync_body: Option<TokenStream>,
        ir: &GrammarIR,
        ctx: &mut RustEmitCtx,
        class: MaterializationClass,
    ) -> TokenStream {
        let is_alt_body = matches!(&rule.body, IrNode::Alt(_, _));

        let (prelude, epilogue) = if is_alt_body {
            // Alt-bodied rules use __branch_idx for the variant
            // discriminator. The Alt emitter sets __branch_idx per arm.
            match class {
                MaterializationClass::MustTape => (
                    quote! {
                        let __span_lo = state.offset as u32;
                        let __children = ::bbnf::runtime::tape::TapeBuilder::mark_children(tape);
                        let mut __branch_idx: u8 = 0;
                    },
                    quote! {
                        Some(::bbnf::runtime::tape::TapeBuilder::push_compound(
                            tape,
                            ::bbnf::runtime::tape::TapeKind::Rule,
                            __children,
                            __span_lo,
                            state.offset as u32,
                            __branch_idx,
                        ))
                    },
                ),
                MaterializationClass::TapeSpanOnly => (
                    quote! {
                        let __span_lo = state.offset as u32;
                        let mut __branch_idx: u8 = 0;
                    },
                    quote! {
                        Some(::bbnf::runtime::tape::TapeBuilder::push_leaf(
                            tape,
                            ::bbnf::runtime::tape::TapeKind::Span,
                            __span_lo,
                            state.offset as u32,
                            __branch_idx,
                        ))
                    },
                ),
                MaterializationClass::TransparentElide => unreachable!(),
            }
        } else {
            let rule_idx_u8 = Self::variant_idx(rule);
            match class {
                MaterializationClass::MustTape => (
                    emit_must_tape_prelude(),
                    emit_must_tape_epilogue(rule_idx_u8),
                ),
                MaterializationClass::TapeSpanOnly => (
                    emit_tape_span_only_prelude(),
                    emit_tape_span_only_epilogue(rule_idx_u8),
                ),
                MaterializationClass::TransparentElide => unreachable!(),
            }
        };

        let signature = emit_rule_signature(name);
        let rule_debug = ir.debug_all || rule.meta.directives.debug;
        let body_block = Self::wrap_body_in_rule_block(body, &prelude, &epilogue, rule_debug, name);

        let mut methods = Vec::new();
        methods.push(quote! {
            #signature {
                #body_block
            }
        });

        Self::maybe_emit_recover_fn(&mut methods, name, sync_body, ctx);
        quote! { #(#methods)* }
    }

    /// Emit a Direct-tier rule: `_inner` + tape wrapper + `_direct`.
    ///
    /// The inner function owns the parse body (state-only, no tape).
    /// The tape wrapper calls inner + pushes the tape record. The
    /// direct shim calls inner with no tape side-effects — used when
    /// a Direct-tier caller invokes a Direct-tier callee.
    fn emit_direct_tier_rule(
        &mut self,
        name: &str,
        rule: &IrRule,
        body: TokenStream,
        sync_body: Option<TokenStream>,
        _ir: &GrammarIR,
        ctx: &mut RustEmitCtx,
        class: MaterializationClass,
    ) -> TokenStream {
        let rule_idx_u8 = Self::variant_idx(rule);

        let inner_sig = emit_direct_inner_signature(name);
        let tape_sig = emit_rule_signature(name);
        let direct_sig = emit_direct_shim_signature(name);
        let inner_ident = format_ident!("__{}_inner", name);

        // 1. Inner: the raw parse body, state-only.
        let inner_fn = quote! {
            #inner_sig {
                match ({ #body }) {
                    Some(_) => Some(()),
                    None => None,
                }
            }
        };

        // 2. Tape wrapper: calls inner, then tape prelude/epilogue.
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

        let tape_fn = quote! {
            #tape_sig {
                'rule_blk: {
                    #prelude
                    match Self::#inner_ident(state) {
                        Some(()) => (),
                        None => break 'rule_blk None,
                    }
                    #epilogue
                }
            }
        };

        // 3. Direct shim: calls inner, no tape.
        let direct_fn = quote! {
            #direct_sig {
                Self::#inner_ident(state)
            }
        };

        let mut methods = vec![inner_fn, tape_fn, direct_fn];
        Self::maybe_emit_recover_fn(&mut methods, name, sync_body, ctx);
        quote! { #(#methods)* }
    }

    /// Variant discriminator: the rule's index in ir.rules,
    /// capped at u8::MAX.
    fn variant_idx(rule: &IrRule) -> u8 {
        let idx = rule.id as usize;
        debug_assert!(idx <= u8::MAX as usize, "rule id overflows u8 variant_idx");
        (idx & 0xFF) as u8
    }

    /// Wrap a body expression in the standard `'rule_blk` block with
    /// optional debug tracing.
    fn wrap_body_in_rule_block(
        body: TokenStream,
        prelude: &TokenStream,
        epilogue: &TokenStream,
        rule_debug: bool,
        name: &str,
    ) -> TokenStream {
        if rule_debug {
            let trace_entry = crate::backend::rust::trace::emit_trace_entry(name);
            let trace_ident = syn::Ident::new("__trace_result", proc_macro2::Span::call_site());
            let trace_exit = crate::backend::rust::trace::emit_trace_exit(name, &trace_ident);
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
        }
    }

    /// Emit the `@recover` sync function if applicable.
    fn maybe_emit_recover_fn(
        methods: &mut Vec<TokenStream>,
        name: &str,
        sync_body: Option<TokenStream>,
        ctx: &mut RustEmitCtx,
    ) {
        let has_recover = ctx.ir_ctx().parser_attrs.skip_recover;
        if has_recover {
            return;
        }
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

                /// Parse an input string and return a zero-copy
                /// `Parsed<'_, Self>` that borrows the input directly.
                pub fn parse(
                    input: &str,
                ) -> ::core::result::Result<
                    ::bbnf::runtime::Parsed<'_, Self>,
                    ::bbnf::runtime::ParseErr,
                > {
                    let mut state = ::parse_that::ParserState::new(input);
                    let mut builder =
                        ::bbnf::runtime::tape::TapeBuilder::with_capacity(
                            input.len() / 4,
                        );
                    let root_off = Self::#root_fn_ident(&mut state, &mut builder)
                        .ok_or(::bbnf::runtime::ParseErr::Syntax {
                            offset: state.offset as u32,
                            rule: None,
                        })?;
                    // Skip trailing ASCII whitespace before the EOF
                    // check so inputs with a final newline (common in
                    // files read via read_to_string) are accepted.
                    while state.offset < input.len()
                        && input.as_bytes()[state.offset].is_ascii_whitespace()
                    {
                        state.offset += 1;
                    }
                    if state.offset < input.len() {
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
                        ::bbnf::runtime::Parsed::new(tape, input, root_off),
                    )
                }
            }
        }
    }
}
