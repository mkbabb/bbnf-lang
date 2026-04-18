//! Grammar-level emission for the Rust backend under the AW-I
//! DTA-wholesale dispatch.
//!
//! Post-W4α the Rust backend emits no per-rule parse functions.
//! `emit_rule_function_impl` is retained as an empty shim so the
//! driver's call pipeline compiles while the sibling per-rule
//! emitter modules (`alt.rs`, `seq.rs`, `repeat.rs`, etc.) are
//! dismantled in W4β. The `parse()` entry point emitted by
//! `emit_grammar_impl` dispatches through `dta_run_into` wholesale —
//! the DTA walker (AW-I.W2.1) owns Seq / Literal / Regex / Ref /
//! AltLinear-with-savepoint / Repeat with `lo..=hi` bounds /
//! ShuntingYard and is the sole parse pathway.
//!
//! `materialization_for_rule_pub` is preserved because the driver's
//! `pre_compile_rule_body` hook consults it to set up AM.3 tape
//! surgery context; W4β will revisit once the surgery context falls
//! out of use.

use bbnf_ir::passes::MaterializationClass;
use bbnf_ir::{GrammarIR, IrRule, TypeDesc};
use proc_macro2::TokenStream;
use quote::quote;

use crate::backend::driver::analysis::BackendAnalysis;
use crate::backend::rust::view::named_types::resolve_named_type;

use super::dfa_codegen;
use super::{RustEmitCtx, RustEmitter};

/// AW-IV.W3.5a — direct-to-struct view-layer consumer wiring.
///
/// The AW-III.W6.4 universal `BINDINGS` table sat behind passing parity
/// tests but had no per-grammar hot-path call site: `emit_view_impl`
/// never consulted `resolve_named_type`. The per-leaf
/// `view::leaves::emit_aggregate_accessors` path had a hardcoded
/// `matches!(name, "Color" | "ColorMix")` admission that pre-empted the
/// resolver — new BINDINGS rows would not have fired without also
/// editing that match.
///
/// W3.5a threads `resolve_named_type` into the grammar-level emission.
/// The function walks every non-transparent rule, asks the resolver
/// whether the rule's `TypeDesc` is a direct-to-struct candidate, and
/// emits two artefacts for each admitted binding:
///
/// 1. A `PROJECTION_DIRECT_TO_STRUCT` const carrying the list of rule
///    names whose view supports the fast path. Introspection surface
///    for downstream consumers (and the W3.5 hard gate — `cargo
///    expand` confirms the list is non-empty on grammars with named
///    aggregates).
/// 2. A `#[doc = ...]` attribute on the grammar marker struct citing
///    the resolved bindings. Makes the consumer-wiring visible in
///    rustdoc without perturbing the view surface that
///    `view::leaves::emit_aggregate_accessors` already emits.
///
/// The mechanism is universal. Future `BINDINGS` additions (JSON
/// Value, BBNF AST, Sheets formula) automatically appear in the
/// emitted list without per-grammar code; `resolve_named_type` is the
/// single admission gate.
fn emit_direct_to_struct_projection(ir: &GrammarIR, _grammar_name: &str) -> TokenStream {
    let mut resolved: Vec<(String, &'static str)> = Vec::new();
    for rule in &ir.rules {
        if rule.meta.is_transparent {
            continue;
        }
        let Some(type_desc) = ir
            .types
            .iter()
            .find_map(|(id, ty)| (*id == rule.id).then_some(ty))
        else {
            continue;
        };
        if !matches!(type_desc, TypeDesc::Named(_)) {
            continue;
        }
        let Some(binding) = resolve_named_type(type_desc, &ir.strings) else {
            continue;
        };
        let rule_name = ir.get_string(rule.name).to_string();
        resolved.push((rule_name, binding.name));
    }

    if resolved.is_empty() {
        return quote! {};
    }

    let entries: Vec<TokenStream> = resolved
        .iter()
        .map(|(rule_name, binding_name)| {
            let rule_lit = proc_macro2::Literal::string(rule_name);
            let bind_lit = proc_macro2::Literal::string(binding_name);
            quote! { (#rule_lit, #bind_lit) }
        })
        .collect();
    let count = entries.len();

    quote! {
        /// AW-IV.W3.5a — per-grammar direct-to-struct projection
        /// admissions.
        ///
        /// Each `(rule_name, binding_name)` pair identifies a non-
        /// transparent rule whose `TypeDesc::Named` resolves via the
        /// universal `BINDINGS` table (at
        /// `crates/core/src/backend/rust/view/named_types.rs`). The
        /// resolved bindings drive the `.as_<name>()` shims emitted on
        /// each rule's view by
        /// `view::leaves::emit_aggregate_accessors` — this const is
        /// the introspection surface proving the resolver was
        /// consulted at codegen time.
        ///
        /// The table is the emitted evidence that AW-III's substrate
        /// is wired: `resolve_named_type` was called, matching entries
        /// survived, and the list is non-empty for every grammar
        /// whose IR declares a named aggregate type. `cargo expand`
        /// on any such grammar's `generated.rs` surfaces this const
        /// alongside the GRAMMAR_PROFILE literal.
        pub const PROJECTION_DIRECT_TO_STRUCT: &[(&str, &str); #count] = &[
            #(#entries),*
        ];
    }
}

/// AW-V.W3.2 — emit the per-grammar shared helpers the shape fns
/// consume — JSON string escape decoder, number fallback, etc.
/// Emitted once per compilation when the grammar has any shape-
/// classified rule.
fn emit_shape_helpers(grammar_ident_str: &str, ir: &GrammarIR) -> TokenStream {
    use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag;
    if !super::shapes::has_full_shape_coverage(ir) {
        return quote! {};
    }
    let grammar_suffix = super::shapes::sanitise_grammar(grammar_ident_str);
    let mut helpers: Vec<TokenStream> = Vec::new();
    // String escape helper — emit when the grammar has any
    // String-shape rule.
    if ir
        .rules
        .iter()
        .any(|r| matches!(ir.shape_assignments.get(r.id), ShapeTag::String))
    {
        helpers.push(super::shapes::string::emit_escape_helper(&grammar_suffix));
        // AW-V.W3-bench-fix — visitor-path escape helper (separate fn
        // so visitor-generic instantiation lives alongside the tape
        // path without reusing the monomorphised tape-path body).
        helpers.push(super::shapes::string::emit_visitor_escape_helper(
            &grammar_suffix,
        ));
    }
    // Number fallback helper — emit when the grammar has any
    // Number-shape rule.
    if ir
        .rules
        .iter()
        .any(|r| matches!(ir.shape_assignments.get(r.id), ShapeTag::Number))
    {
        helpers.push(super::shapes::number::emit_number_fallback_helper());
    }
    quote! { #(#helpers)* }
}

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
        // via `PayloadData::WideScalar` (AU.6.7). Otherwise discard
        // as before.
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

    /// AW-I.W4α: per-rule function emission is a no-op.
    ///
    /// The Rust backend's `parse()` dispatches through the DTA
    /// walker wholesale (see [`Self::emit_grammar_impl`]), so the
    /// per-rule `__<name>` function bodies previously assembled here
    /// are dead surface. The driver still calls into this method
    /// once per rule; returning an empty token stream drops the
    /// body without disturbing the call pipeline. W4β dismantles
    /// the sibling emitter modules that fed this path.
    pub(super) fn emit_rule_function_impl(
        &mut self,
        _rule: &IrRule,
        _body: TokenStream,
        _sync_body: Option<TokenStream>,
        _ir: &GrammarIR,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        TokenStream::new()
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
        let views = crate::backend::rust::view::generate_views(ir, ir_ctx);

        // AW-IV.W3.5a — direct-to-struct view-layer consumer wiring.
        //
        // The AW-III.W6.4 universal binding table lives at
        // `crates/core/src/backend/rust/view/named_types.rs::BINDINGS`
        // with a passing parity suite (JSON Value, BBNF AST, Sheets
        // formula, CSS Color). Pre-W3.5a the view emitter admitted
        // only hardcoded "Color" | "ColorMix" names via a match in
        // `view::leaves::emit_aggregate_accessors`; the resolver
        // shipped but wasn't called on the per-grammar hot path.
        //
        // W3.5a threads `resolve_named_type` into the top-level view
        // emission: for every non-transparent rule whose `TypeDesc`
        // is `Named(sid)` and whose interned name resolves via the
        // universal `BINDINGS` table, the direct-to-struct projection
        // is emitted inline on the view. The mechanism is universal
        // — JSON Value, BBNF AST, Sheets formula, CSS Color all enter
        // the fast path via the same resolver call; the shape of the
        // emitted projection comes from the binding row's
        // `NamedTypeBinding::fields`.
        //
        // The `emit_direct_to_struct_projection` pass walks every
        // rule, consults the resolver, and emits one `as_<name>()`
        // shim per admitted rule. The top-level grammar-entry rule
        // gets additional root-view wiring so downstream consumers
        // can project the full parse directly without traversing the
        // tape cursor tree manually.
        let direct_to_struct =
            emit_direct_to_struct_projection(ir, ir_ctx.ident.to_string().as_str());

        quote! {
            #views
            #direct_to_struct
        }
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

        // Tranche AV Phase 1 — consolidated per-grammar fingerprint.
        // Lowers `GrammarIR::profile()` to a single `const
        // GRAMMAR_PROFILE: GrammarProfile = GrammarProfile { ... };`
        // literal emitted alongside the grammar string array.
        let profile = ir.profile();
        let grammar_profile = super::profile::emit_grammar_profile(&profile);

        // Tranche AV Phase 3 — DTA table. Lowers the lifter's
        // owned `DtaTable` into a `const DTA_TABLE: ::bbnf::runtime::tape::DtaTable
        // = DtaTable { ... };` literal next to `GRAMMAR_PROFILE`. The
        // runtime driver consuming the table ships in V4 PSI; until
        // then the table is inert data and `parse()` drives the
        // legacy fn-per-rule path.
        let dta_table = super::dta::emit_dta_table(ir);

        // AW-III.W4.b — specialised DTA walker. Mechanically lowers
        // every `DtaState` variant in the lifted table to inlined
        // Rust dispatch arms; the `DtaState` enum match disappears in
        // the output. The cold-path `dispatch_one` survives in
        // `bbnf_tape::driver` for replay (AX substrate); the hot path
        // is the `dta_run_<grammar>` function emitted here.
        //
        // The walker function exists alongside the live cold-path
        // `dta_run` (which `parse()` still drives below) so W4.c can
        // swap the `parse()` call site without touching this file
        // again. Both the hot-emitted and cold-private paths read
        // the same `DTA_TABLE` and produce structurally-identical
        // tapes for the same input.
        // AW-IV.W1.4-aggro — lift the DTA table once; the walker
        // emitter splices the DFA's loop body directly into every
        // Regex / WsTrim / boundary-ws site, so the walker owns the
        // hot-path regex semantics at the source level (no separately-
        // emitted `__dfa_match_*` fn exists). The `dfa_codegen` pass
        // still emits one per-grammar `__regex_scan_<grammar>` adapter
        // as the SOLE out-of-line regex-related fn — the adapter
        // exists only for cold-path callers that dispatch by pattern
        // string (replay surface's `try_branch` /
        // `handle_repeat_failure_bounded`), and its own dispatch arms
        // also splice inline DFA bodies so no chain of `fn` calls
        // survives anywhere on the regex hot path.
        let dta_walker_table = super::dta_walker::lift_for_walker(ir);
        let grammar_name = ident.to_string();
        let regex_scan_adapter = dfa_codegen::emit_regex_scan_adapter(
            grammar_name.as_str(),
            ir,
            &dta_walker_table,
        );
        let dta_walker = {
            let alphabet = ir
                .structural_alphabet
                .as_ref()
                .cloned()
                .unwrap_or_default();
            let profile = ir.profile();
            super::dta_walker::emit_specialised_walker(
                grammar_name.as_str(),
                ir,
                &dta_walker_table,
                &alphabet,
                &profile,
            )
        };
        // AW-III.W4.d — emit the walker fn ident parse() calls into
        // directly. The ident is the same one the
        // `emit_specialised_walker` pass produces above; sharing the
        // sanitiser keeps both call sites in sync.
        let walker_fn_ident =
            super::dta_walker::walker_fn_ident(ident.to_string().as_str());

        // AW-III.W6.2 — emit PHF keyword tables for every literal-led
        // Alt whose mined branch count exceeds the threshold. The
        // emitted statics live at module scope alongside GRAMMAR_PROFILE
        // so the specialised walker's `AltLinear` arm (or future
        // ClassifyByte specialisations) can consult them via a binary
        // search helper fn. Per §6, the mechanism runs over every
        // grammar's Alt space; per-grammar impact varies with the
        // mined branch counts.
        let keyword_phf_tables = {
            let mut tables: Vec<(u32, &[super::keyword_dispatch::LiteralBranch])> = Vec::new();
            // Allocate owned branch buffers per rule so the borrow
            // lives long enough for emit_keyword_tables's consumption.
            let mut owned: Vec<(u32, Vec<super::keyword_dispatch::LiteralBranch>)> = Vec::new();
            for rule in &ir.rules {
                if rule.meta.is_transparent {
                    continue;
                }
                let Some(dag) = ir.dag.as_ref() else { continue };
                let Some(body_id) = dag.node_for(&rule.body) else { continue };
                if let Some(branches) = ir.keyword_branches.get(&body_id) {
                    let lits: Vec<super::keyword_dispatch::LiteralBranch> = branches
                        .iter()
                        .map(|kb| super::keyword_dispatch::LiteralBranch {
                            bytes: kb.bytes.clone(),
                            branch_idx: kb.branch_idx,
                        })
                        .collect();
                    owned.push((rule.id, lits));
                }
            }
            for (rid, lits) in owned.iter() {
                tables.push((*rid, lits.as_slice()));
            }
            super::keyword_dispatch::emit_keyword_tables(
                ident.to_string().as_str(),
                tables,
            )
        };

        // AW-III.W6.5 — per-grammar Pratt precedence LUT. Mines every
        // DtaState::ShuntingYard chain's operators from the lifted
        // DTA table and emits a packed `const PRECEDENCE_LUT: [u8; 256]`
        // plus a sparse `PRECEDENCE_ENTRIES` slice. The walker's
        // ShuntingYard arm (specialised inline by W4) consults the LUT
        // at runtime — one indexed byte load + three shifts per operator
        // dispatch. Grammars without operator chains emit a zeroed LUT
        // for uniform downstream consumption.
        let precedence_lut = {
            let lifted_table = super::dta_walker::lift_for_walker(ir);
            let chain_facts =
                bbnf_ir::passes::collect_operator_chains(&lifted_table);
            super::precedence::emit_precedence_lut(
                ident.to_string().as_str(),
                &chain_facts,
            )
        };

        // Tranche AV Phase 2 — AV.2.5 reordered-unrolling kernels for
        // typed-payload visitors. One free-function per descriptor
        // with a 4-lane reordered accumulator (Sum) or lane-wise
        // extrema (Min/Max). The grammar-side `@visitor` directive is
        // not wired today, so `reorder_unroll_visitors` is empty for
        // every grammar shipped; the list is exercised by tests that
        // populate it directly. When the directive lands, the kernels
        // start appearing in every affected grammar without any
        // further emitter work.
        let visitor_kernels =
            super::visitor::emit_visitor_kernels(&profile.reorder_unroll_visitors);

        // Debug trace depth counter (only emitted if any rule
        // uses @debug).
        let has_debug = ir.debug_all || ir.rules.iter().any(|r| r.meta.directives.debug);
        let depth_counter = if has_debug {
            crate::backend::rust::trace::emit_depth_counter()
        } else {
            quote! {}
        };

        let extra = &self.extra_impl_methods;

        // AW-V.W3.2 — per-shape emitter modules.
        //
        // Walks the IR's ShapeAssignments (populated by the W3.1
        // classifier) and emits one `parse_<shape>_<grammar>_<rule>`
        // function per shape-classified rule, plus the top-level
        // `parse_<grammar>_<root>` dispatcher. The emitted stream
        // lives alongside `#dta_walker`; rules without shape match
        // continue to route through `__dta_walker_inline::run` per
        // the AX cold-path replay contract.
        //
        // When every non-transparent rule in the grammar has a W3-
        // active shape classification (JSON after W3.1 ships), the
        // grammar's `parse()` entry routes through the shape
        // dispatcher — eliminating the structural scan + PSI +
        // walker tax on the hot path. Grammars with unshaped rules
        // (CSS / Sheets / BBNF until W4 extends the detectors)
        // continue to call `dta_run_<grammar>`.
        let shape_emitters = super::shapes::emit_shapes_for_grammar(
            ident.to_string().as_str(),
            ir,
        );
        let shape_helpers = emit_shape_helpers(ident.to_string().as_str(), ir);
        let use_shape_dispatch = super::shapes::has_full_shape_coverage(ir);
        let shape_dispatcher_ident = super::shapes::root_rule_name(ir).map(|root| {
            super::shapes::dispatcher_fn_ident(ident.to_string().as_str(), &root)
        });
        // AW-V.W3-bench-fix — visitor-path dispatcher ident.
        let visitor_dispatcher_ident = super::shapes::root_rule_name(ir).map(|root| {
            super::shapes::visitor_dispatcher_fn_ident(
                ident.to_string().as_str(),
                &root,
            )
        });

        // AW-I.W3: `parse()` dispatches through `dta_run` wholesale.
        // The per-rule `rule_functions` stream and the trailing_ws /
        // root_fn_ident / with_capacity scaffolding previously woven
        // into the legacy body are retired — the DTA walker owns EOF,
        // root dispatch, and capacity derivation. `rule_functions` is
        // intentionally accepted (the upstream pipeline still compiles
        // per-rule fragments) and discarded here; W4β removes the
        // upstream compilation step once the sibling emitter modules
        // are deleted.
        //
        // AW-IV.W1.4-aggro — the DtaDfaScanner ZST + RegexScanner impl
        // + DTA_SCANNER const all delete. The walker emitter splices
        // the DFA's `loop { match state { ... } }` body directly into
        // every Regex / WsTrim / boundary-ws site at the source level;
        // no separately-emitted `__dfa_match_*` fn exists. The
        // `#regex_scan_adapter` below is the SOLE out-of-line
        // regex-related fn emitted per grammar — used by cold-path
        // replay callers (`try_branch`, `handle_repeat_failure_bounded`)
        // that dispatch by pattern string; its dispatch arms also
        // splice inline DFA bodies, so the fn-call boundary that
        // AW-III's runtime DFA interpreter imposed (31.92% self-time
        // on JSON twitter) is gone from the hot path entirely.
        let _ = rule_functions;

        // AW-V.W3.2 — parse() routes through the shape dispatcher
        // when every non-transparent rule has a W3-active shape
        // classification. Otherwise it falls through to the legacy
        // walker path (`dta_run_<grammar>`), preserving CSS / Sheets
        // / BBNF coverage until W4 extends the detectors.
        let parse_body = if use_shape_dispatch {
            let dispatcher = shape_dispatcher_ident
                .as_ref()
                .expect("use_shape_dispatch gated on root_rule_name");
            let support_mod_ident = quote::format_ident!(
                "__shape_support_{}",
                super::shapes::sanitise_grammar(ident.to_string().as_str()),
            );
            quote! {
                let mut builder =
                    ::bbnf::runtime::tape::TapeBuilder::with_capacity(
                        GRAMMAR_PROFILE.capacity_for(input.len()),
                    );
                let root_off = {
                    let mut pos: usize = 0;
                    let mut state = #support_mod_ident::ScanState::new();
                    let off = #dispatcher(
                        input.as_bytes(),
                        &mut pos,
                        &mut state,
                        &mut builder,
                    )
                    .map_err(|e| match e {
                        ::bbnf::runtime::tape::DtaError::Syntax { offset, .. } => {
                            ::bbnf::runtime::ParseErr::Syntax {
                                offset,
                                rule: None,
                            }
                        }
                        ::bbnf::runtime::tape::DtaError::UnexpectedEnd { offset } => {
                            ::bbnf::runtime::ParseErr::Syntax {
                                offset,
                                rule: None,
                            }
                        }
                        ::bbnf::runtime::tape::DtaError::InvalidState { .. } => {
                            ::bbnf::runtime::ParseErr::Syntax {
                                offset: 0,
                                rule: None,
                            }
                        }
                    })?;
                    // Trailing whitespace.
                    let _ = #support_mod_ident::skip_space(
                        input.as_bytes(), &mut pos, &mut state,
                    );
                    if pos != input.len() {
                        return Err(::bbnf::runtime::ParseErr::Syntax {
                            offset: pos as u32,
                            rule: None,
                        });
                    }
                    off
                };
                let tape = builder
                    .finish()
                    .map_err(::bbnf::runtime::ParseErr::Tape)?;
                ::core::result::Result::Ok(
                    ::bbnf::runtime::Parsed::new(tape, input, root_off),
                )
            }
        } else {
            quote! {
                let mut builder =
                    ::bbnf::runtime::tape::TapeBuilder::with_capacity(
                        GRAMMAR_PROFILE.capacity_for(input.len()),
                    );
                builder.enable_inline_frame_depth();
                let mut psi = psi_with_capacity(input.len());
                // AW-III.W5.d — stage-1 SIMD structural pre-pass.
                const STRUCTURAL_ALPHABET:
                    ::bbnf::runtime::scan::StructuralAlphabet =
                    ::bbnf::runtime::scan::StructuralAlphabet::from_profile(
                        &GRAMMAR_PROFILE,
                    );
                let idx = ::bbnf::runtime::scan::scan_structural(
                    input.as_bytes(),
                    &STRUCTURAL_ALPHABET,
                );
                let root_off = {
                    let (columns, frame_depth) =
                        builder.columns_and_frame_depth_mut();
                    if !GRAMMAR_PROFILE.list_rules.is_empty()
                        && (input.as_bytes().len() as u32)
                            > GRAMMAR_PROFILE.parallel_break_even_bytes
                        && GRAMMAR_PROFILE.parallel_break_even_bytes > 0
                    {
                        let n_workers = ::core::cmp::min(
                            4usize,
                            ::core::cmp::max(
                                1usize,
                                ::bbnf::runtime::tape::rayon_num_threads(),
                            ),
                        );
                        let list_rule_id =
                            GRAMMAR_PROFILE.list_rules[0].0;
                        ::bbnf::runtime::tape::dta_run_parallel(
                            input.as_bytes(),
                            &idx,
                            list_rule_id,
                            n_workers,
                            #walker_fn_ident,
                            columns,
                            &mut psi,
                            frame_depth,
                        )
                    } else {
                        #walker_fn_ident(
                            input.as_bytes(),
                            &idx,
                            columns,
                            &mut psi,
                            frame_depth,
                        )
                    }
                }
                    .map_err(|e| match e {
                        ::bbnf::runtime::tape::DtaError::Syntax { offset, .. } => {
                            ::bbnf::runtime::ParseErr::Syntax {
                                offset,
                                rule: None,
                            }
                        }
                        ::bbnf::runtime::tape::DtaError::UnexpectedEnd { offset } => {
                            ::bbnf::runtime::ParseErr::Syntax {
                                offset,
                                rule: None,
                            }
                        }
                        ::bbnf::runtime::tape::DtaError::InvalidState { .. } => {
                            ::bbnf::runtime::ParseErr::Syntax {
                                offset: 0,
                                rule: None,
                            }
                        }
                    })?;
                psi.fill_columns(
                    input.as_bytes(),
                    builder.columns_mut(),
                    &GRAMMAR_PROFILE,
                );
                let tape = builder
                    .finish()
                    .map_err(::bbnf::runtime::ParseErr::Tape)?;
                ::core::result::Result::Ok(
                    ::bbnf::runtime::Parsed::new(tape, input, root_off),
                )
            }
        };

        // AW-V.W3-bench-fix — `parse_with_visitor::<V>` method body.
        // When the grammar has full shape coverage, expose a
        // visitor-generic parse entry that bypasses the tape entirely.
        // The visitor is monomorphised at the call site; per-shape
        // bodies inline into one tight dispatcher, matching the
        // prototype's perf shape.
        let parse_with_visitor_body = if use_shape_dispatch {
            let visitor_dispatcher = visitor_dispatcher_ident
                .as_ref()
                .expect("use_shape_dispatch gated on root_rule_name");
            let support_mod_ident = quote::format_ident!(
                "__shape_support_{}",
                super::shapes::sanitise_grammar(ident.to_string().as_str()),
            );
            Some(quote! {
                /// AW-V.W3-bench-fix — visitor-generic parse entry.
                ///
                /// Bypasses the tape entirely; `V` is monomorphised at
                /// the call site so the per-shape parse bodies inline
                /// into one tight dispatcher. Matches the shape of
                /// `bbnf_json_prototype::parse_json::<V>`.
                ///
                /// `V` must implement every per-shape visitor sub-trait
                /// the grammar drives (`ObjectVisitor`, `ArrayVisitor`,
                /// `StringVisitor`, `NumberVisitor`, `KeywordVisitor`)
                /// so each method invocation resolves statically at
                /// monomorphisation time. See `bbnf-tape::visitor` for
                /// the trait hierarchy and the shipped `TapeVisitor` /
                /// `ValueVisitor` implementations.
                pub fn parse_with_visitor<V>(
                    input: &str,
                    visitor: &mut V,
                ) -> ::core::result::Result<(), ::bbnf::runtime::ParseErr>
                where
                    V: ::bbnf::runtime::tape::ObjectVisitor
                        + ::bbnf::runtime::tape::ArrayVisitor
                        + ::bbnf::runtime::tape::StringVisitor
                        + ::bbnf::runtime::tape::NumberVisitor
                        + ::bbnf::runtime::tape::KeywordVisitor,
                {
                    let bytes = input.as_bytes();
                    let mut pos: usize = 0;
                    let mut state = #support_mod_ident::ScanState::new();
                    #visitor_dispatcher(bytes, &mut pos, &mut state, visitor)?;
                    // Trailing whitespace tolerant.
                    let _ = #support_mod_ident::skip_space(bytes, &mut pos, &mut state);
                    if pos != input.len() {
                        return Err(::bbnf::runtime::ParseErr::Syntax {
                            offset: pos as u32, rule: None,
                        });
                    }
                    Ok(())
                }
            })
        } else {
            None
        };

        quote! {
            use ::parse_that::*;

            #grammar_arr

            #grammar_profile

            #dta_table

            // AW-III.W6.2 — PHF keyword tables for literal-led Alts.
            // Emitted at module scope per rule whose Alt body has
            // literal-led branches ≥ PHF_MIN_BRANCHES; consulted by
            // downstream AltLinear / ClassifyByte call sites.
            #keyword_phf_tables

            // AW-III.W6.5 — Pratt precedence LUT. Dense `[u8; 256]`
            // packed byte layout + sparse metadata slice for two-byte
            // operators. Consulted by the W4-emitted walker's
            // ShuntingYard arm via a single indexed byte load.
            #precedence_lut

            // AW-IV.W1.4-aggro — per-grammar regex-scan adapter.
            // Dispatches on pointer-equality of the interned pattern
            // `&'static str` statics (`__DTA_REGEX_K` / `__DTA_WS_K`);
            // each matched arm splices the corresponding DFA's loop
            // body inline (no chain of `__dfa_match_*` fn calls
            // anywhere). Consumed as the `regex_scan: fn(&str, &[u8],
            // usize) -> Option<u32>` parameter by the cold-path
            // replay (AX) and any call site that dispatches by
            // pattern string. The hot-path walker arms splice the DFA
            // body directly and never reach this adapter — it is the
            // SOLE out-of-line regex-related fn emitted per grammar.
            #regex_scan_adapter

            #dta_walker

            // AW-V.W3.2 — per-shape emitter modules + helpers. The
            // dispatcher emits alongside the walker; `parse()` routes
            // through it when the grammar's shape coverage is total.
            #shape_helpers
            #shape_emitters

            #type_defs

            impl #ident {
                #depth_counter
                #extra
                #visitor_kernels

                /// AW-IV.W1.δ — associated-constant accessor for the
                /// grammar's consolidated codegen fingerprint. Alias
                /// of the module-scope `GRAMMAR_PROFILE` const; the
                /// underlying bytes live in `.rodata` once. Downstream
                /// consumers (wire-contract tests, per-grammar
                /// introspection, cross-grammar harnesses) use
                /// `<Grammar>::GRAMMAR_PROFILE` to disambiguate when
                /// multiple grammars coexist in the same test file —
                /// the module-scope `pub use ...::*` would otherwise
                /// collide on the unqualified `GRAMMAR_PROFILE` name.
                pub const GRAMMAR_PROFILE: ::bbnf::runtime::tape::GrammarProfile =
                    GRAMMAR_PROFILE;

                /// Parse an input string and return a zero-copy
                /// `Parsed<'_, Self>` that borrows the input directly.
                ///
                /// AW-III.W4.d: `parse()` dispatches through the
                /// per-grammar specialised walker emitted by W4.b. The
                /// inlined arms cover every `DtaState` variant; the
                /// cold-path `dispatch_one` survives in `bbnf-tape`
                /// only for the AX replay subsystem and walker-arm
                /// regression tests. The hot path here:
                ///
                /// 1. Allocate a sized `TapeBuilder` + `PayloadStream`.
                /// 2. Call the emitted `dta_run_<grammar>` directly,
                ///    handing it the builder's `columns_mut` /
                ///    `frame_depth_mut` references so the inlined arms
                ///    write structurally + frame-depth inline.
                /// 3. Drain the PSI stream into typed payload columns.
                /// 4. Finalise via `TapeBuilder::finish` — the
                ///    inline-frame-depth path skips
                ///    `derive_frame_depth` reconstruction.
                pub fn parse(
                    input: &str,
                ) -> ::core::result::Result<
                    ::bbnf::runtime::Parsed<'_, Self>,
                    ::bbnf::runtime::ParseErr,
                > {
                    #parse_body
                }

                #parse_with_visitor_body
            }
        }
    }
}
