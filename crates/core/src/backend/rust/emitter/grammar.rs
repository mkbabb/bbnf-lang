//! Grammar-level emission for the Rust backend — AX.W0b shape-dispatch era.
//!
//! The Rust backend emits no per-rule parse functions.
//! `emit_rule_function_impl` is retained as an empty shim so the
//! driver's call pipeline compiles; sibling per-rule emitter modules
//! were dismantled. The `parse()` entry point emitted by
//! `emit_grammar_impl` routes through the shape dispatcher
//! unconditionally.

use bbnf_ir::{GrammarIR, IrRule, TypeDesc};
use proc_macro2::TokenStream;
use quote::quote;

use crate::backend::driver::analysis::BackendAnalysis;

use super::path_plan;
use super::regex_scan_adapter;
use super::registry_emit;
use super::{RustEmitCtx, RustEmitter};

/// AW-V.W3.2 — emit the per-grammar shared helpers the active shape
/// fns consume. Visitor helpers retired with AZ-II.cutover.O5; only
/// production shape-dispatch helpers remain here.
fn emit_shape_helpers(_grammar_ident_str: &str, ir: &GrammarIR) -> TokenStream {
    use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag;
    let mut helpers: Vec<TokenStream> = Vec::new();
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

    /// Per-rule function emission is a no-op.
    ///
    /// The Rust backend's `parse()` dispatches through the shape
    /// dispatcher unconditionally (see [`Self::emit_grammar_impl`]),
    /// so the per-rule `__<name>` function bodies previously
    /// assembled here are dead surface. The driver still calls into
    /// this method once per rule; returning an empty token stream
    /// drops the body without disturbing the call pipeline.
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
        let ir_ctx = ctx.ir_ctx();
        let grammar_name_s = ir_ctx.ident.to_string();
        let strategy = bbnf_ir::registry::EmitStrategy::for_grammar(
            grammar_name_s.as_str(),
            &ir.struct_registry,
        );

        let bbnf_ir::registry::EmitStrategy::StructDirect { .. } = strategy;

        // O3.P1-G1 / O4 — StructDirect parse output is the
        // document-owned runtime surface. Do not emit the legacy
        // tape-backed node views, generated `<Grammar>Value` /
        // `ValueRoot` surface, projection materializers, or their
        // materializer/consumer metadata.
        TokenStream::new()
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
        let grammar_arr = crate::backend::rust::ir_enums::generate_grammar_arr(parser_attrs, ident);

        // Tranche AV Phase 1 / AZ-II.O5 — structural scan constants
        // emitted alongside the grammar string array. The generated
        // runtime no longer constructs the tape crate's
        // `GrammarProfile` carrier.
        let profile = ir.profile();
        let grammar_profile = super::profile::emit_grammar_profile(&profile);

        // Regex-scan adapter — cold-path pattern-keyed replay surface.
        // Shape emitters consume the adapter ident at every Regex /
        // WsTrim site whose dispatch needs to replay a regex by
        // pattern string. The adapter's dispatch arms splice the
        // minimised DFA body inline (no chain of `fn` calls). Lifts
        // grammar facts once to read the pattern set; the lift is
        // codegen-time data, NOT a runtime table.
        let grammar_name = ident.to_string();
        let regex_facts_table = bbnf_ir::passes::lift_dta(ir);
        let regex_scan_adapter_ts = regex_scan_adapter::emit_regex_scan_adapter(
            grammar_name.as_str(),
            ir,
            &regex_facts_table,
        );

        // AW-III.W6.2 — emit PHF keyword tables for every literal-led
        // Alt whose mined branch count exceeds the threshold. The
        // emitted statics live at module scope alongside the profile
        // constants
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
                let Some(body_id) = dag.node_for(&rule.body) else {
                    continue;
                };
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
            super::keyword_dispatch::emit_keyword_tables(ident.to_string().as_str(), tables)
        };

        // AW-III.W6.5 — per-grammar Pratt precedence LUT. Mines every
        // DtaState::ShuntingYard chain's operators from the lifted
        // DTA table and emits a packed `const PRECEDENCE_LUT: [u8; 256]`
        // plus a sparse `PRECEDENCE_ENTRIES` slice. Consulted inline by
        // the shape-dispatch Pratt body.
        let precedence_lut = {
            let chain_facts = bbnf_ir::passes::collect_operator_chains(ir, &regex_facts_table);
            super::precedence::emit_precedence_lut(ident.to_string().as_str(), &chain_facts)
        };

        // AZ-IV.W3.3 — codegen-emitted path plan (lazy bail-out parse).
        // Walks the grammar's `StructRegistry` (populated by the
        // `project_types` CSP solver) to produce a flat
        // `(RuleId, SegmentKind, Decision)` static array the W3.1
        // path executor consults. Grammar-general — no rule-name
        // matching; the plan derives entirely from `LayoutKind` +
        // `StructField::source` provenance facts.
        let path_plan_static = path_plan::emit_path_plan(ident.to_string().as_str(), ir);

        // AZ-IV.W5 T4 — codegen-emitted production `StructRegistry`.
        // Materialises the registry the IR pipeline projected as a
        // `LazyLock<StructRegistry>` static the `bbnf-path` proc-macro
        // (and any other in-tree consumer) reaches through the
        // per-grammar generated module. Replaces the synthetic
        // hand-authored fixture in `crates/bbnf-path/src/registry.rs` —
        // the macro's lex / lower / validate stages stay unchanged;
        // only the source of truth swaps.
        let registry_static = registry_emit::emit_registry(ir);

        let extra = &self.extra_impl_methods;

        // AW-V.W3.2 — per-shape emitter modules.
        //
        // Walks the IR's ShapeAssignments (populated by the W3.1
        // classifier) and emits one `parse_<shape>_<grammar>_<rule>`
        // function per shape-classified rule, plus the top-level
        // `parse_<grammar>_<root>` dispatcher. Every non-transparent
        // rule routes through the shape dispatcher; the dispatcher
        // owns EOF, root dispatch, and capacity derivation.
        let shape_emitters = super::shapes::emit_shapes_for_grammar(ident.to_string().as_str(), ir);
        let shape_helpers = emit_shape_helpers(ident.to_string().as_str(), ir);
        let shape_dispatcher_ident = super::shapes::root_rule_name(ir)
            .map(|root| super::shapes::dispatcher_fn_ident(ident.to_string().as_str(), &root));

        // The per-rule `rule_functions` stream is intentionally
        // accepted (the upstream pipeline still composes per-rule
        // fragments) and discarded here; the regex-scan adapter
        // (emitted just above) is the SOLE out-of-line regex-related
        // fn this module produces, used by cold-path callers
        // (`try_branch`, `handle_repeat_failure_bounded`) that
        // dispatch by pattern string. Its dispatch arms splice
        // inline DFA bodies; no `__dfa_match_*` fn-call boundary
        // survives.
        let _ = rule_functions;

        // AZ-II.cutover.O4 — codegen-time substrate selection is
        // fail-closed. `EmitStrategy::for_grammar` must return a
        // StructDirect binding for every production grammar; unknown
        // grammars and empty registries panic before emission.
        let strategy = bbnf_ir::registry::EmitStrategy::for_grammar(
            ident.to_string().as_str(),
            &ir.struct_registry,
        );
        let bbnf_ir::registry::EmitStrategy::StructDirect { rust, .. } = strategy;
        let parse_body: TokenStream = {
            let dispatcher = shape_dispatcher_ident
                .as_ref()
                .expect("shape dispatcher gated on root_rule_name");
            let support_mod_ident = quote::format_ident!(
                "__shape_support_{}",
                super::shapes::sanitise_grammar(ident.to_string().as_str()),
            );
            emit_parse_body_struct_direct(
                dispatcher,
                &support_mod_ident,
                rust.builder_path,
                rust.document_path,
            )
        };
        // Every production parser returns the grammar-specific
        // document type (`JsonDocument<'_>` for JSON). The result is
        // still wrapped in `Result<_, ParseErr>`.
        let parse_return_type: TokenStream = {
            let path: syn::Path = syn::parse_str(rust.document_path)
                .expect("EmitStrategy::StructDirect.rust.document_path must parse as a Rust path");
            quote! { #path<'_> }
        };
        let parse_docs: TokenStream = quote! {
            /// Parse an input string and return the grammar-specific
            /// document that owns the StructDirect runtime arena.
        };
        // AY-II.W0'.a — visitor-generic parse entry retired. The
        // fused parse above IS the visitor lane — every shape
        // emitter's push goes through the fused builder's atomic
        // tape + value stamping. The separate visitor-trait-bounded
        // entry duplicated the dispatcher body against an external
        // visitor trait the fused projection path supersedes;
        // retaining it would violate invariant §5 (fused pipeline is
        // real) and invariant §7 (consumer totality — every surface
        // has a production consumer). The visitor trait hierarchy
        // remains in `tape::visitor` for test fixtures that exercise
        // the trait API directly; `TapeVisitor` now emits via the
        // fused builder, so those consumers are not orphaned.

        quote! {
            use ::parse_that::*;

            #grammar_arr

            #grammar_profile

            // AW-III.W6.2 — PHF keyword tables for literal-led Alts.
            // Emitted at module scope per rule whose Alt body has
            // literal-led branches ≥ PHF_MIN_BRANCHES; consulted by
            // downstream AltLinear / ClassifyByte call sites.
            #keyword_phf_tables

            // AW-III.W6.5 — Pratt precedence LUT. Dense `[u8; 256]`
            // packed byte layout + sparse metadata slice for two-byte
            // operators. Consulted by the shape-dispatch Pratt body.
            #precedence_lut

            // AZ-IV.W3.3 — lazy bail-out parse plan. Static array of
            // `(rule_id, segment_kind, field_index, decision)`
            // entries; the W3.1 `PathCursor` consults the plan when
            // descending into a rule's parse function with a typed
            // path attached. The local `__path_plan` module carries
            // the row schema definitions; W3.1's executor cherry-pick
            // replaces them with re-exports from
            // `crate::path::path_plan` while preserving byte-stable
            // row contents.
            #path_plan_static

            // AZ-IV.W5 T4 — production `StructRegistry`. The
            // `bbnf-path` proc-macro consumes this static (instead of
            // the synthetic fixture it shipped through W2/W3/W4)
            // through the per-marker resolver; the runtime audit pass
            // and emitter consume the same registry shape from
            // `GrammarIR::struct_registry`. Closes AUDIT-F mid-tranche
            // §R1.
            #registry_static

            // Per-grammar regex-scan adapter. Dispatches on
            // pointer-equality of the interned pattern `&'static str`
            // statics (`__DTA_REGEX_K` / `__DTA_WS_K`); each matched
            // arm splices the corresponding DFA's loop body inline.
            // Consumed by shape emitters whose Regex / WsTrim arms
            // splice its dispatch in-line.
            #regex_scan_adapter_ts

            // AW-V.W3.2 — per-shape emitter modules + helpers.
            #shape_helpers
            #shape_emitters

            #type_defs

            impl #ident {
                #extra

                #parse_docs
                pub fn parse(
                    input: &str,
                ) -> ::core::result::Result<
                    #parse_return_type,
                    crate::runtime::ParseErr,
                > {
                    #parse_body
                }
            }
        }
    }
}

/// AZ-I.W2.RA — emit the struct-direct `parse()` body.
///
/// The body instantiates a concrete `StructBuilder` (e.g.
/// `JsonStructBuilder::new()`), drives the shape dispatcher against
/// `(&__input_bytes, &mut pos, &mut state, &mut builder)`, finalises
/// via `builder.finalise()`, and returns the grammar-specific
/// document type (e.g. `JsonDocument<'_>`).
///
/// `builder_path` and `document_path` are fully-qualified Rust paths
/// drawn from [`bbnf_ir::registry::EmitStrategy::StructDirect`]'s
/// [`bbnf_ir::registry::SubstrateBinding`] for the Rust backend.
/// The path strings are spliced via `syn::parse_str` so the emitter
/// rejects malformed paths at codegen time.
///
/// Wire contract: the dispatcher emitted under struct-direct mode
/// (when B/C/D/E activate per-shape struct-builder bodies in stage
/// 2) must accept `&mut <builder>` matching `builder_path`'s type.
/// The pre-stage-2 dispatcher emits tape bodies that take `&mut tape`;
/// until B/C/D/E land, this struct-direct body is exercised only
/// when the same regen and per-shape activation cherry-pick onto
/// master together. The activation gate below (`for_grammar`'s
/// JSON arm + populated registry) governs when this path is
/// selected at codegen time.
fn emit_parse_body_struct_direct(
    dispatcher: &syn::Ident,
    support_mod_ident: &syn::Ident,
    builder_path: &str,
    _document_path: &str,
) -> TokenStream {
    let builder_ty: syn::Path = syn::parse_str(builder_path)
        .expect("EmitStrategy::StructDirect.builder_path must parse as a Rust path");
    quote! {
        let __input_bytes = input.as_bytes();
        // AZ-I.W2.RA — struct-direct parse body. The builder owns a
        // typed in-flight stack of compound frames; `finalise()`
        // recovers the rooted document.
        let mut state = #support_mod_ident::ScanState::new();
        let mut builder = #builder_ty::new();
        // AZ-IV.W3.6 — eager parses construct an always-ParseFully
        // cursor against a 'static empty path. The cursor's state is
        // unused by the emit body (the cursor consult returns
        // ParseFully for every rule), but the dispatcher signature
        // carries a cursor parameter so eager and lazy lanes share
        // the surface. The `LazyLock` keeps the empty path 'static so
        // the cursor's borrow does not propagate the function-local
        // lifetime through the dispatcher's `'p`-tied builder /
        // document return type.
        static __EAGER_EMPTY_PATH: ::std::sync::LazyLock<
            crate::path::ir::TypedPath<crate::path::markers::Json, &'static str>,
        > = ::std::sync::LazyLock::new(|| {
            crate::path::ir::TypedPath::from_owned(::std::vec::Vec::new())
        });
        let mut __eager_cursor: crate::path::cursor::PathCursor<
            'static,
            crate::path::ir::TypedPath<crate::path::markers::Json, &'static str>,
        > = crate::path::cursor::PathCursor::new(
            &*__EAGER_EMPTY_PATH,
            |_rid, _kind, _idx| crate::path::cursor::Decision::ParseFully,
        );
        {
            let mut pos: usize = 0;
            #dispatcher(
                __input_bytes,
                &mut pos,
                &mut state,
                &mut builder,
                &mut __eager_cursor,
            )
            .map_err(|e| match e {
                crate::runtime::DtaError::Syntax { offset } => {
                    crate::runtime::ParseErr::Syntax {
                        offset,
                        rule: None,
                    }
                }
                crate::runtime::DtaError::UnexpectedEnd { offset } => {
                    crate::runtime::ParseErr::Syntax {
                        offset,
                        rule: None,
                    }
                }
                crate::runtime::DtaError::InvalidState { .. } => {
                    crate::runtime::ParseErr::Syntax {
                        offset: 0,
                        rule: None,
                    }
                }
            })?;
            // Trailing whitespace.
            let _ = #support_mod_ident::skip_space(
                __input_bytes, &mut pos, &mut state,
            );
            if pos != input.len() {
                return Err(crate::runtime::ParseErr::Syntax {
                    offset: pos as u32,
                    rule: None,
                });
            }
        }
        ::core::result::Result::Ok(builder.finalise(input))
    }
}
