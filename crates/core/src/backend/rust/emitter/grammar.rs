//! Grammar-level emission for the Rust backend — AX.W0b shape-dispatch era.
//!
//! Post-W0b the Rust backend emits no per-rule parse functions and
//! no walker. `emit_rule_function_impl` is retained as an empty
//! shim so the driver's call pipeline compiles; sibling per-rule
//! emitter modules were dismantled in AW-I.W4β. The `parse()` entry
//! point emitted by `emit_grammar_impl` routes through the
//! shape dispatcher unconditionally.

use bbnf_ir::{GrammarIR, IrRule, TypeDesc};
use proc_macro2::TokenStream;
use quote::quote;

use crate::backend::driver::analysis::BackendAnalysis;

use super::dfa_codegen;
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

        // AW-IV.W1.4-aggro — regex-scan adapter. The shape emitters
        // consume it for every Regex / WsTrim site; its dispatch arms
        // splice inline DFA bodies so no chain of `fn` calls survives
        // on the hot path. Lifts the DTA once to read the pattern
        // set; the table is NOT emitted as runtime data.
        let grammar_name = ident.to_string();
        let dta_walker_table = bbnf_ir::passes::lift_dta(ir);
        let regex_scan_adapter =
            dfa_codegen::emit_regex_scan_adapter(grammar_name.as_str(), ir, &dta_walker_table);

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
            let chain_facts = bbnf_ir::passes::collect_operator_chains(ir, &dta_walker_table);
            super::precedence::emit_precedence_lut(ident.to_string().as_str(), &chain_facts)
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
        let shape_emitters = super::shapes::emit_shapes_for_grammar(ident.to_string().as_str(), ir);
        let shape_helpers = emit_shape_helpers(ident.to_string().as_str(), ir);
        // AX.W0b — every grammar routes through the shape dispatcher
        // post-W0a.2.h; the gate predicates retired with the walker.
        let shape_dispatcher_ident = super::shapes::root_rule_name(ir)
            .map(|root| super::shapes::dispatcher_fn_ident(ident.to_string().as_str(), &root));

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

            // AW-IV.W1.4-aggro — per-grammar regex-scan adapter.
            // Dispatches on pointer-equality of the interned pattern
            // `&'static str` statics (`__DTA_REGEX_K` / `__DTA_WS_K`);
            // each matched arm splices the corresponding DFA's loop
            // body inline. Consumed by shape emitters whose Regex /
            // WsTrim arms splice its dispatch in-line.
            #regex_scan_adapter

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
        {
            let mut pos: usize = 0;
            #dispatcher(
                __input_bytes,
                &mut pos,
                &mut state,
                &mut builder,
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
