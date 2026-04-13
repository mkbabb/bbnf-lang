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
use bbnf_ir::passes::MaterializationClass;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::backend::driver::analysis::BackendAnalysis;

use super::tape_prelude::{
    emit_must_tape_epilogue,
    emit_must_tape_prelude, emit_rule_signature, emit_tape_span_only_epilogue,
    emit_tape_span_only_prelude, emit_tape_span_only_f64_prelude,
    emit_tape_span_only_f64_epilogue,
};
use super::{RustEmitCtx, RustEmitter};

impl RustEmitter {
    pub(super) fn emit_fused_number_rule_impl(
        &mut self,
        rule: &IrRule,
        _ir: &GrammarIR,
        ctx: &mut RustEmitCtx,
    ) -> Option<TokenStream> {
        if rule.meta.is_transparent {
            return None;
        }
        // AN Phase 0: when payload_kind is F64, capture the scanned
        // value into `__payload_f64` so the epilogue can store it
        // via `push_leaf_with_f64`. Otherwise discard as before.
        //
        // `fused_number_rules` is exclusively `RegexClass::JsonNumber`,
        // so unconditionally use `scan_json_number_f64`.
        if matches!(ctx.payload_kind, Some(crate::backend::rust::emitter_types::PayloadKind::F64 { .. })) {
            Some(quote! {
                match ::parse_that::scan_json_number_f64(state) {
                    Some(__v) => { __payload_f64 = __v; __has_payload = true; Some(()) }
                    None => None,
                }
            })
        } else {
            Some(quote! {
                (::parse_that::scan_json_number_f64(state)).map(|_| ())
            })
        }
    }

    /// AN Phase 0: detect if a rule body is a number-pattern regex
    /// eligible for f64 payload storage. IR passes strip Map nodes,
    /// so we detect by classifying the regex pattern directly.
    ///
    /// Returns `Some(json)` when eligible — `json = true` for
    /// `RegexClass::JsonNumber`, `false` for `RegexClass::Numeric`.
    /// The caller threads this into `PayloadKind::F64 { json }` so
    /// emission sites choose the correct scanner function.
    pub(in crate::backend::rust) fn is_f64_payload_eligible(
        rule: &IrRule,
        ir: &GrammarIR,
    ) -> Option<bool> {
        use parse_that::regex::classify::{RegexClass, classify_regex};
        match &rule.body {
            IrNode::Regex(sid) => {
                let pattern = ir.get_string(*sid);
                match classify_regex(pattern) {
                    RegexClass::JsonNumber => Some(true),
                    RegexClass::Numeric { .. } => Some(false),
                    _ => None,
                }
            }
            IrNode::Map { fn_id, .. } => {
                // NumberConvert is always JSON-class in practice
                // (lowered from `-> f64` on a JSON number regex).
                if matches!(ir.fns[*fn_id as usize], bbnf_ir::FnDescriptor::NumberConvert) {
                    Some(true)
                } else {
                    None
                }
            }
            _ => None,
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
    ///
    /// Public via [`Self::materialization_for_rule_pub`] for the
    /// `pre_compile_rule_body` hook in `mod.rs`.
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

    /// Public accessor for `materialization_for_rule`, used by
    /// `pre_compile_rule_body` in `mod.rs` for AM.3 tape surgery
    /// context setup.
    pub(in crate::backend::rust) fn materialization_for_rule_pub(
        ir: &GrammarIR,
        rule: &IrRule,
    ) -> MaterializationClass {
        Self::materialization_for_rule(ir, rule)
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

        self.emit_tape_tier_rule(&name, rule, body, sync_body, ir, ctx, class)
    }

    /// Emit a Tape-tier (or Lazy-tier) rule: the standard prelude +
    /// body + epilogue pattern from Tranche AC.2.
    ///
    /// Tranche AK.1: for Alt-bodied rules, the epilogue uses a
    /// `__branch_idx` variable (set per-arm by the Alt emitter)
    /// instead of the rule's global ID. This gives the view layer
    /// correct branch discrimination.
    ///
    /// Tranche AM.3: for Alt-bodied `MustTape` rules, per-branch
    /// tape surgery emits `push_leaf` or `mark_children` +
    /// `push_compound` inside each branch arm. The shared epilogue
    /// becomes a pass-through of the `Option<TapeOffset>` returned
    /// by the branch body. Leaf branches (literals, regex, pure maps)
    /// skip `mark_children` entirely.
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

        // AN Phase 0: check if any Alt branch has a payload-eligible
        // body (NumberConvert, Constant). If so, the prelude declares
        // `__payload_f64` and the epilogue uses `push_leaf_with_f64`
        // for the non-compound, payload-bearing case.
        let alt_has_f64_payload = if let IrNode::Alt(branches, _) = &rule.body {
            branches.iter().any(|b| {
                if let IrNode::Ref(rid) = &b.node {
                    let ref_rule = &ir.rules[*rid as usize];
                    Self::is_f64_payload_eligible(ref_rule, ir).is_some()
                } else {
                    false
                }
            })
        } else {
            false
        };

        let (prelude, epilogue) = if is_alt_body {
            // Alt-bodied rules use __branch_idx for the variant
            // discriminator. The Alt emitter sets __branch_idx per arm.
            match class {
                MaterializationClass::MustTape => {
                    // AM.3: per-branch mark_children. The prelude
                    // declares __has_children + __children; compound
                    // branches set them via the Alt emitter. The
                    // epilogue checks __has_children at runtime.
                    //
                    // AN Phase 0: payload variables are always
                    // declared in MustTape Alt preludes. The epilogue
                    // checks __has_payload at runtime to decide between
                    // push_leaf_with_f64 and plain push_leaf. Unused
                    // variables compile to no-ops.
                    (
                        quote! {
                            let __span_lo = state.offset as u32;
                            let mut __branch_idx: u8 = 0;
                            let mut __has_children = false;
                            let mut __children = ::bbnf::runtime::tape::TapeOffset::NONE;
                            let mut __payload_f64: f64 = 0.0;
                            let mut __has_payload = false;
                        },
                        quote! {
                            if __has_children {
                                Some(::bbnf::runtime::tape::TapeBuilder::push_compound(
                                    tape,
                                    ::bbnf::runtime::tape::TapeKind::Rule,
                                    __children,
                                    __span_lo,
                                    state.offset as u32,
                                    __branch_idx,
                                ))
                            } else if __has_payload {
                                Some(::bbnf::runtime::tape::TapeBuilder::push_leaf_with_f64(
                                    tape,
                                    ::bbnf::runtime::tape::TapeKind::Span,
                                    __span_lo,
                                    state.offset as u32,
                                    __branch_idx,
                                    __payload_f64,
                                ))
                            } else {
                                Some(::bbnf::runtime::tape::TapeBuilder::push_leaf(
                                    tape,
                                    ::bbnf::runtime::tape::TapeKind::Span,
                                    __span_lo,
                                    state.offset as u32,
                                    __branch_idx,
                                ))
                            }
                        },
                    )
                }
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
                MaterializationClass::TapeSpanOnly => {
                    // AN Phase 0: payload-bearing leaf rules use
                    // push_leaf_with_f64/bool/u8 instead of push_leaf.
                    use crate::backend::rust::emitter_types::PayloadKind;
                    match ctx.payload_kind {
                        Some(PayloadKind::F64 { .. }) => (
                            emit_tape_span_only_f64_prelude(),
                            emit_tape_span_only_f64_epilogue(rule_idx_u8),
                        ),
                        _ => (
                            emit_tape_span_only_prelude(),
                            emit_tape_span_only_epilogue(rule_idx_u8),
                        ),
                    }
                }
                MaterializationClass::TransparentElide => unreachable!(),
            }
        };

        let signature = emit_rule_signature(name);
        let rule_debug = ir.debug_all || rule.meta.directives.debug;
        let body_block = Self::wrap_body_in_rule_block(
            body, &prelude, &epilogue, rule_debug, name,
        );

        let mut methods = Vec::new();
        methods.push(quote! {
            #signature {
                #body_block
            }
        });

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
        // The root rule is always ir.entry — the grammar's declared
        // entry point. Even if it's transparent (e.g. JSON's `value`
        // is an Alt of Refs), it must have a function because parse()
        // calls it by name. compute_call_strategies forces DirectCall.
        let root_rule_name = ir
            .rules
            .iter()
            .find(|r| r.id == ir.entry)
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

        // AO Phase 0.4: structural pre-scan code for parse().
        let structural_prescan = if self.structural_mode {
            if let Some(ref bytes) = ir.structural_bytes {
                let byte_lits: Vec<proc_macro2::Literal> = bytes
                    .iter()
                    .map(|b| proc_macro2::Literal::u8_suffixed(*b))
                    .collect();
                quote! {
                    let __structural_bytes: &[u8] = &[#(#byte_lits),*];
                    let mut __structural_positions = ::parse_that::scan_structural(
                        input.as_bytes(), __structural_bytes,
                    );
                    ::parse_that::filter_quote_parity(
                        input.as_bytes(), &mut __structural_positions,
                    );
                    state.structural_index = __structural_positions.as_ptr();
                    state.structural_len = __structural_positions.len() as u32;
                    state.structural_cursor = 0;
                }
            } else {
                quote! {}
            }
        } else {
            quote! {}
        };

        // AP.ws: trailing whitespace before EOF — use comment-aware
        // kernel when the grammar declares a WsBlockComment @ws
        // pattern, otherwise fall back to bare is_ascii_whitespace.
        let trailing_ws = {
            use parse_that::regex::classify::{RegexClass, classify_regex};
            let ws_is_comment_aware = ir
                .ws_pattern
                .map(|sid| ir.get_string(sid))
                .is_some_and(|pat| matches!(classify_regex(pat), RegexClass::WsBlockComment));
            if ws_is_comment_aware {
                quote! {
                    let _ = ::parse_that::scan_ws_block_comments(&mut state);
                }
            } else {
                quote! {
                    while state.offset < input.len()
                        && input.as_bytes()[state.offset].is_ascii_whitespace()
                    {
                        state.offset += 1;
                    }
                }
            }
        };

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
                    #structural_prescan
                    let mut builder =
                        ::bbnf::runtime::tape::TapeBuilder::with_capacity(
                            input.len() / 4,
                        );
                    let root_off = Self::#root_fn_ident(&mut state, &mut builder)
                        .ok_or(::bbnf::runtime::ParseErr::Syntax {
                            offset: state.offset as u32,
                            rule: None,
                        })?;
                    // Skip trailing whitespace before the EOF check
                    // so inputs with a final newline (common in files
                    // read via read_to_string) are accepted.
                    //
                    // AP.ws: when a custom @ws pattern is active and
                    // classifies as WsBlockComment, use the comment-
                    // aware kernel so trailing `/* ... */` comments
                    // are consumed before the EOF gate.
                    #trailing_ws
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
