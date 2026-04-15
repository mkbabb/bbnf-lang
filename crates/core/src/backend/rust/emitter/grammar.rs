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

use bbnf_ir::{GrammarIR, IrNode, IrRule, TypeDesc};
use bbnf_ir::passes::{MaterializationClass, PayloadLayout, is_kv_pair_shape};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::backend::driver::analysis::BackendAnalysis;

use super::tape_prelude::{
    emit_must_tape_aggregate_epilogue, emit_must_tape_aggregate_prelude,
    emit_must_tape_epilogue,
    emit_must_tape_prelude, emit_rule_signature, emit_tape_span_only_aggregate_epilogue,
    emit_tape_span_only_aggregate_prelude, emit_tape_span_only_epilogue,
    emit_tape_span_only_prelude, emit_tape_span_only_scalar_prelude,
    emit_tape_span_only_scalar_epilogue,
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
        // AQ.6.A: when payload_type is F64, capture the scanned
        // value into `__payload_f64` so the epilogue can store it
        // via `push_leaf_with_f64`. Otherwise discard as before.
        //
        // `fused_number_rules` is exclusively the strict numeric
        // shape (`reject_leading_zero: true`), so unconditionally use
        // `scan_number_strict_f64`.
        if ctx.has_payload_type(&TypeDesc::F64) {
            let tag_set = ctx.payload_tag(&TypeDesc::F64).map(|tag| {
                quote! { __payload_tag = #tag; }
            });
            Some(quote! {
                match ::parse_that::scan_number_strict_f64(state) {
                    Some(__v) => { __payload_f64 = __v; #tag_set __has_payload = true; Some(()) }
                    None => None,
                }
            })
        } else {
            // AU.6.5 no-value-discard: return the scanner's
            // `Option<f64>` directly; enclosing callers match via
            // `Some(_)` which is payload-agnostic.
            Some(quote! {
                ::parse_that::scan_number_strict_f64(state)
            })
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

        // TransparentElide rules do not emit a function at all
        // the driver inlines their body at every call site.
        if class == MaterializationClass::TransparentElide {
            return quote! {};
        }

        self.emit_tape_tier_rule(&name, rule, body, sync_body, ir, ctx, class)
    }

    /// Emit a Tape-tier (or Lazy-tier) rule: the standard prelude +
    /// body + epilogue pattern from Tranche AC.2.
    ///
    /// AR.1.1: ALL rules stamp `rule.id as u8` into `variant_idx`
    /// (rule identity). Alt-bodied rules additionally stamp
    /// `__branch_idx` into `meta_idx` (branch identity); non-Alt
    /// rules pass `0u8` as `meta_idx`.
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
        let rule_idx_u8 = Self::variant_idx(rule);

        // AU.2.2: aggregate payload layout takes precedence over
        // per-type payload locals. When `ctx.payload_layout.is_some()`
        // the prelude reserves `__aggregate_buf` / `__has_payload`
        // regardless of Alt-body shape; the Alt-aware variants layer
        // the per-branch `__branch_idx` / `__has_children` locals on
        // top so per-branch tape surgery still fires.
        let (prelude, epilogue) = if let Some(layout) = ctx.payload_layout.as_ref() {
            let kv_pair = ir
                .types
                .iter()
                .find_map(|(rid, td)| (*rid == rule.id).then_some(td))
                .is_some_and(|td| match td {
                    TypeDesc::Tuple(fields) => is_kv_pair_shape(fields),
                    _ => false,
                });
            if is_alt_body {
                match class {
                    MaterializationClass::MustTape => emit_alt_mustape_aggregate_prelude_epilogue(
                        rule_idx_u8,
                        layout,
                        kv_pair,
                    ),
                    MaterializationClass::TapeSpanOnly => {
                        emit_alt_span_only_aggregate_prelude_epilogue(
                            rule_idx_u8,
                            layout,
                            kv_pair,
                        )
                    }
                    MaterializationClass::TransparentElide => unreachable!(),
                }
            } else {
                match class {
                    MaterializationClass::MustTape => (
                        emit_must_tape_aggregate_prelude(layout),
                        emit_must_tape_aggregate_epilogue(layout, rule_idx_u8, kv_pair),
                    ),
                    MaterializationClass::TapeSpanOnly => (
                        emit_tape_span_only_aggregate_prelude(layout),
                        emit_tape_span_only_aggregate_epilogue(layout, rule_idx_u8, kv_pair),
                    ),
                    MaterializationClass::TransparentElide => unreachable!(),
                }
            }
        } else if is_alt_body {
            // AT.1: Alt-bodied rules use multi-type payload support.
            // The prelude declares a `__payload_<T>` local for each
            // distinct scalar type across branches, plus a
            // `__payload_tag` discriminator for multi-type Alts.
            match class {
                MaterializationClass::MustTape => {
                    emit_alt_mustape_prelude_epilogue(rule_idx_u8, &ctx.payload_types)
                }
                MaterializationClass::TapeSpanOnly => {
                    emit_alt_span_only_prelude_epilogue(rule_idx_u8, &ctx.payload_types)
                }
                MaterializationClass::TransparentElide => unreachable!(),
            }
        } else {
            match class {
                MaterializationClass::MustTape => (
                    emit_must_tape_prelude(),
                    emit_must_tape_epilogue(rule_idx_u8),
                ),
                MaterializationClass::TapeSpanOnly => {
                    // Non-Alt rules have at most one payload type.
                    match ctx.payload_types.first() {
                        Some(td) if td.is_scalar_payload() => (
                            emit_tape_span_only_scalar_prelude(td),
                            emit_tape_span_only_scalar_epilogue(td, rule_idx_u8),
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

        // AU.6.2: derive a grammar-specific `TapeBuilder::with_capacity`
        // divisor from the push-site fingerprint recorded by
        // [`bbnf_ir::passes::compute_push_fingerprint`]. The ratio
        // avoids the `_mi_heap_realloc_zero` / `RawVec::grow_one`
        // path on first parse — under-allocating Sheets or CSS
        // bootstrap triggers 10–22% of `parse_simple` samples on
        // the realloc chain. No runtime branching: the numerator
        // and denominator are picked at codegen time.
        let with_capacity_expr = {
            let (numer, denom) = ir
                .push_fingerprint
                .as_ref()
                .map(|fp| fp.capacity_ratio())
                .unwrap_or((1u32, 2u32));
            if numer == 1 && denom == 1 {
                quote! { input.len() + 2 }
            } else if numer == 1 {
                let denom_lit = denom as usize;
                quote! { input.len() / #denom_lit + 2 }
            } else {
                let numer_lit = numer as usize;
                let denom_lit = denom as usize;
                quote! { input.len() * #numer_lit / #denom_lit + 2 }
            }
        };

        // AP.ws: trailing whitespace before EOF — use comment-aware
        // kernel when the grammar declares a WhitespaceWithBlockComment
        // @ws pattern, otherwise fall back to bare is_ascii_whitespace.
        let trailing_ws = {
            use parse_that::regex::classify::{RegexClass, classify_regex};
            let ws_is_comment_aware = ir
                .ws_pattern
                .map(|sid| ir.get_string(sid))
                .is_some_and(|pat| {
                    matches!(classify_regex(pat), RegexClass::WhitespaceWithBlockComment)
                });
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
                    let mut builder =
                        ::bbnf::runtime::tape::TapeBuilder::with_capacity(
                            #with_capacity_expr,
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
                    // classifies as WhitespaceWithBlockComment, use
                    // the comment-aware kernel so trailing
                    // `/* ... */` comments are consumed before the
                    // EOF gate.
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

/// AQ.6.A: emit the prelude + epilogue pair for an Alt-bodied
/// `MustTape` rule, parameterized by the projected scalar payload
/// type.
///
/// Per-branch tape surgery (AM.3) is layered on top of this: the Alt
/// emitter sets `__has_children` / `__children` per arm; this pair
/// just picks the right `push_leaf_with_<T>` for the leaf-branch
// ── AT.1: Multi-type payload prelude/epilogue helpers ──────────────
//
// Supports zero, one, or many scalar payload types per Alt rule.
// Single-type: direct `push_leaf_with_<T>` with `__has_payload` guard.
// Multi-type: `__payload_tag` discriminator selects the right push in
// the epilogue via a generated match arm per type.

/// Emit `let mut __payload_<T>: T = init;` declarations for each type.
fn emit_payload_declarations(types: &[TypeDesc]) -> TokenStream {
    let mut decls = TokenStream::new();
    for td in types {
        if matches!(td, TypeDesc::Span) {
            decls.extend(quote! {
                let mut __payload_lo: u32 = 0;
                let mut __payload_hi: u32 = 0;
            });
        } else {
            let rust_ident = td.rust_ident().expect("scalar TypeDesc");
            let payload_local = format_ident!("__payload_{}", rust_ident);
            let payload_ty = format_ident!("{}", rust_ident);
            let init = scalar_zero_init_token(td);
            decls.extend(quote! {
                let mut #payload_local: #payload_ty = #init;
            });
        }
    }
    if !types.is_empty() {
        decls.extend(quote! { let mut __has_payload = false; });
    }
    if types.len() > 1 {
        decls.extend(quote! { let mut __payload_tag: u8 = 0; });
    }
    decls
}

/// Emit the payload push expression for a single type.
///
/// AU.6.7: every scalar/Span push routes through `push_leaf_with` with
/// a `PayloadData` constructed per the declared type. Inline-packed
/// scalars (<= 4 bytes) use `PayloadData::InlineScalar`; 8-byte
/// scalars (f64/i64/u64/Span) use `PayloadData::WideScalar` with the
/// value's little-endian bits.
fn emit_push_leaf_with(td: &TypeDesc) -> TokenStream {
    let payload_expr = emit_scalar_payload_data(td);
    quote! {
        ::bbnf::runtime::tape::TapeBuilder::push_leaf_with(
            tape,
            ::bbnf::runtime::tape::TapeKind::Span,
            __span_lo,
            state.offset as u32,
            __variant_idx,
            __branch_idx,
            #payload_expr,
        )
    }
}

/// Emit the `PayloadData` expression for a scalar `TypeDesc`.
///
/// Inline scalars (<= 4 bytes) extend their bytes to a `u32` and use
/// `PayloadData::InlineScalar`; wide scalars (f64/i64/u64/Span) use
/// `PayloadData::WideScalar` with a u64 representation.
pub(super) fn emit_scalar_payload_data(td: &TypeDesc) -> TokenStream {
    if matches!(td, TypeDesc::Span) {
        return quote! {
            ::bbnf::runtime::tape::PayloadData::WideScalar(
                (__payload_lo as u64) | ((__payload_hi as u64) << 32),
            )
        };
    }
    let rust_ident = td.rust_ident().expect("scalar TypeDesc");
    let payload_local = format_ident!("__payload_{}", rust_ident);
    match td {
        TypeDesc::F64 => quote! {
            ::bbnf::runtime::tape::PayloadData::WideScalar(#payload_local.to_bits())
        },
        TypeDesc::U64 => quote! {
            ::bbnf::runtime::tape::PayloadData::WideScalar(#payload_local)
        },
        TypeDesc::I64 => quote! {
            ::bbnf::runtime::tape::PayloadData::WideScalar(#payload_local as u64)
        },
        TypeDesc::Bool => quote! {
            ::bbnf::runtime::tape::PayloadData::InlineScalar(#payload_local as u32)
        },
        TypeDesc::I8 => quote! {
            ::bbnf::runtime::tape::PayloadData::InlineScalar(
                u32::from_le_bytes([#payload_local as u8, 0, 0, 0]),
            )
        },
        TypeDesc::U8 => quote! {
            ::bbnf::runtime::tape::PayloadData::InlineScalar(#payload_local as u32)
        },
        TypeDesc::I16 => quote! {
            ::bbnf::runtime::tape::PayloadData::InlineScalar({
                let __b = (#payload_local as i16).to_le_bytes();
                u32::from_le_bytes([__b[0], __b[1], 0, 0])
            })
        },
        TypeDesc::U16 => quote! {
            ::bbnf::runtime::tape::PayloadData::InlineScalar(#payload_local as u32)
        },
        TypeDesc::I32 => quote! {
            ::bbnf::runtime::tape::PayloadData::InlineScalar(#payload_local as u32)
        },
        TypeDesc::U32 => quote! {
            ::bbnf::runtime::tape::PayloadData::InlineScalar(#payload_local)
        },
        _ => unreachable!("emit_scalar_payload_data: non-scalar TypeDesc {:?}", td),
    }
}

/// Emit the payload epilogue: either direct push (single type) or
/// match on `__payload_tag` (multi-type).
fn emit_payload_epilogue(types: &[TypeDesc]) -> TokenStream {
    let push_leaf = quote! {
        ::bbnf::runtime::tape::TapeBuilder::push_leaf(
            tape,
            ::bbnf::runtime::tape::TapeKind::Span,
            __span_lo,
            state.offset as u32,
            __variant_idx,
            __branch_idx,
        )
    };
    match types.len() {
        0 => quote! { Some(#push_leaf) },
        1 => {
            let push_with = emit_push_leaf_with(&types[0]);
            quote! {
                if __has_payload {
                    Some(#push_with)
                } else {
                    Some(#push_leaf)
                }
            }
        }
        _ => {
            // Multi-type: match on __payload_tag.
            let arms: Vec<TokenStream> = types
                .iter()
                .enumerate()
                .map(|(i, td)| {
                    let tag = (i + 1) as u8;
                    let push_with = emit_push_leaf_with(td);
                    quote! { #tag => Some(#push_with), }
                })
                .collect();
            quote! {
                match __payload_tag {
                    #(#arms)*
                    _ => Some(#push_leaf),
                }
            }
        }
    }
}

/// AT.1: emit prelude + epilogue for an Alt-bodied `MustTape` rule.
fn emit_alt_mustape_prelude_epilogue(
    rule_idx_u8: u8,
    payload_types: &[TypeDesc],
) -> (TokenStream, TokenStream) {
    let variant_lit = rule_idx_u8;
    let payload_decls = emit_payload_declarations(payload_types);
    let payload_push = emit_payload_epilogue(payload_types);
    (
        quote! {
            let __span_lo = state.offset as u32;
            let __variant_idx: u8 = #variant_lit;
            let mut __branch_idx: u8 = 0;
            let mut __has_children = false;
            let mut __children = ::bbnf::runtime::tape::TapeOffset::NONE;
            #payload_decls
        },
        quote! {
            if __has_children {
                Some(::bbnf::runtime::tape::TapeBuilder::push_compound(
                    tape,
                    ::bbnf::runtime::tape::TapeKind::Rule,
                    __children,
                    __span_lo,
                    state.offset as u32,
                    __variant_idx,
                    __branch_idx,
                ))
            } else {
                #payload_push
            }
        },
    )
}

/// AT.1: emit prelude + epilogue for an Alt-bodied `TapeSpanOnly` rule.
fn emit_alt_span_only_prelude_epilogue(
    rule_idx_u8: u8,
    payload_types: &[TypeDesc],
) -> (TokenStream, TokenStream) {
    let variant_lit = rule_idx_u8;
    let payload_decls = emit_payload_declarations(payload_types);
    let payload_push = emit_payload_epilogue(payload_types);
    (
        quote! {
            let __span_lo = state.offset as u32;
            let __variant_idx: u8 = #variant_lit;
            let mut __branch_idx: u8 = 0;
            #payload_decls
        },
        payload_push,
    )
}

/// AU.2.2: emit prelude + epilogue for an Alt-bodied `MustTape` rule
/// whose projected type has an aggregate payload layout.
///
/// Reconciles the Alt per-branch tape surgery (`__has_children`,
/// `__children`, `__branch_idx`) with the aggregate path the non-Alt
/// emitter already lays down (`__aggregate_buf`, `__has_payload`).
/// When a branch writes into `__aggregate_buf` via
/// `aggregate_constant_setter`, the epilogue emits
/// `push_leaf_with_aggregate`. When a branch's children mark the
/// record as compound (e.g. a composite-bodied branch), the epilogue
/// falls back to `push_compound`. When neither fires, the epilogue
/// emits a plain `push_leaf` spanning the Alt's byte range.
fn emit_alt_mustape_aggregate_prelude_epilogue(
    rule_idx_u8: u8,
    layout: &PayloadLayout,
    kv_pair: bool,
) -> (TokenStream, TokenStream) {
    let variant_lit = rule_idx_u8;
    let total_bytes = layout.total_bytes as usize;
    let tape_kind_aggregate = if kv_pair {
        quote! { ::bbnf::runtime::tape::TapeKind::KvPair }
    } else {
        quote! { ::bbnf::runtime::tape::TapeKind::Span }
    };
    let prelude = quote! {
        let __span_lo = state.offset as u32;
        let __variant_idx: u8 = #variant_lit;
        let mut __branch_idx: u8 = 0;
        let mut __has_children = false;
        let mut __children = ::bbnf::runtime::tape::TapeOffset::NONE;
        let mut __aggregate_buf: [u8; 16] = [0u8; 16];
        let mut __has_payload = false;
    };
    let epilogue = quote! {
        if __has_children {
            Some(::bbnf::runtime::tape::TapeBuilder::push_compound(
                tape,
                ::bbnf::runtime::tape::TapeKind::Rule,
                __children,
                __span_lo,
                state.offset as u32,
                __variant_idx,
                __branch_idx,
            ))
        } else if __has_payload {
            Some(::bbnf::runtime::tape::TapeBuilder::push_leaf_with(
                tape,
                #tape_kind_aggregate,
                __span_lo,
                state.offset as u32,
                __variant_idx,
                __branch_idx,
                ::bbnf::runtime::tape::PayloadData::Aggregate(
                    &__aggregate_buf[..#total_bytes],
                ),
            ))
        } else {
            Some(::bbnf::runtime::tape::TapeBuilder::push_leaf(
                tape,
                ::bbnf::runtime::tape::TapeKind::Span,
                __span_lo,
                state.offset as u32,
                __variant_idx,
                __branch_idx,
            ))
        }
    };
    (prelude, epilogue)
}

/// AU.2.2: emit prelude + epilogue for an Alt-bodied `TapeSpanOnly`
/// rule whose projected type has an aggregate payload layout.
///
/// Like the `MustTape` variant but without the children run — Alt
/// branches that need to push compound records do so independently,
/// and this prelude reserves only the aggregate-buffer locals plus
/// the Alt's `__branch_idx` discriminator.
fn emit_alt_span_only_aggregate_prelude_epilogue(
    rule_idx_u8: u8,
    layout: &PayloadLayout,
    kv_pair: bool,
) -> (TokenStream, TokenStream) {
    let variant_lit = rule_idx_u8;
    let total_bytes = layout.total_bytes as usize;
    let tape_kind_aggregate = if kv_pair {
        quote! { ::bbnf::runtime::tape::TapeKind::KvPair }
    } else {
        quote! { ::bbnf::runtime::tape::TapeKind::Span }
    };
    let prelude = quote! {
        let __span_lo = state.offset as u32;
        let __variant_idx: u8 = #variant_lit;
        let mut __branch_idx: u8 = 0;
        let mut __aggregate_buf: [u8; 16] = [0u8; 16];
        let mut __has_payload = false;
    };
    let epilogue = quote! {
        if __has_payload {
            Some(::bbnf::runtime::tape::TapeBuilder::push_leaf_with(
                tape,
                #tape_kind_aggregate,
                __span_lo,
                state.offset as u32,
                __variant_idx,
                __branch_idx,
                ::bbnf::runtime::tape::PayloadData::Aggregate(
                    &__aggregate_buf[..#total_bytes],
                ),
            ))
        } else {
            Some(::bbnf::runtime::tape::TapeBuilder::push_leaf(
                tape,
                ::bbnf::runtime::tape::TapeKind::Span,
                __span_lo,
                state.offset as u32,
                __variant_idx,
                __branch_idx,
            ))
        }
    };
    (prelude, epilogue)
}

fn scalar_zero_init_token(td: &TypeDesc) -> TokenStream {
    match td {
        TypeDesc::F64 => quote! { 0.0 },
        TypeDesc::Bool => quote! { false },
        _ => quote! { 0 },
    }
}
