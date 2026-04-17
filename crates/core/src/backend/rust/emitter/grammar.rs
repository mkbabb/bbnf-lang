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
        let dta_walker = {
            let table = super::dta_walker::lift_for_walker(ir);
            let alphabet = ir
                .structural_alphabet
                .as_ref()
                .cloned()
                .unwrap_or_default();
            let profile = ir.profile();
            super::dta_walker::emit_specialised_walker(
                ident.to_string().as_str(),
                &table,
                &alphabet,
                &profile,
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
        // `DTA_SCANNER` is promoted to a module-level `const` singleton
        // so `parse()` borrows a single shared scanner instead of
        // stack-allocating one per call. The `DtaDfaScanner` struct is
        // zero-size so `const` is canonical.
        let _ = rule_functions;

        quote! {
            use ::parse_that::*;

            #grammar_arr

            #grammar_profile

            #dta_table

            #dta_walker

            #type_defs

            /// AW-III.W1.8 — DTA regex-scanner adapter that bypasses
            /// the global `cached_dfa` HashMap on the hot path.
            ///
            /// The pattern → Dfa cache lives in a per-pattern
            /// `OnceLock<&'static Dfa>` keyed by interned pattern
            /// pointer (each `&'static str` pattern emitted by the
            /// DTA literal table is unique by address). The first
            /// scan for a pattern compiles + leaks the Dfa into the
            /// static slot; subsequent scans hit a single atomic
            /// load. The leak is bounded by the grammar's regex
            /// count (≤ a few hundred per shipped grammar) and lives
            /// for the process lifetime — symmetric with the global
            /// HashMap in `parse_that::cached_dfa` that would have
            /// stored the same `Arc<Dfa>` payload, minus the
            /// per-scan `RwLock + HashMap::get + Sip13` overhead.
            struct DtaDfaScanner;

            impl ::bbnf::runtime::tape::RegexScanner for DtaDfaScanner {
                fn scan(
                    &self,
                    pattern: &str,
                    input: &[u8],
                    offset: usize,
                ) -> ::core::option::Option<u32> {
                    // AW-III.W1.8 — bypass the global `cached_dfa`
                    // HashMap on the hot path. The pattern's interned
                    // `&'static str` pointer IS the cache key; the
                    // resolved `&'static Dfa` is leaked once on first
                    // touch. Lookups use a global `RwLock<HashMap>`
                    // keyed on pointer (Sip13-free, just `usize`
                    // hashing); read-only path takes the read lock
                    // and returns a `&'static Dfa` directly. The
                    // leak is bounded by the grammar's regex count
                    // (≤ a few hundred per shipped grammar) and
                    // lives for the process lifetime — symmetric
                    // with the global HashMap in
                    // `parse_that::cached_dfa` minus the per-scan
                    // Sip13 + Arc::clone overhead.
                    use ::std::collections::HashMap;
                    use ::std::sync::{OnceLock, RwLock};
                    static SLOTS: OnceLock<RwLock<HashMap<usize, &'static ::parse_that::regex::dfa::Dfa>>> =
                        OnceLock::new();
                    let slots = SLOTS.get_or_init(|| RwLock::new(HashMap::new()));
                    let key = pattern.as_ptr() as usize;
                    let dfa: &'static ::parse_that::regex::dfa::Dfa = {
                        let map = slots.read().unwrap();
                        if let Some(d) = map.get(&key).copied() {
                            d
                        } else {
                            drop(map);
                            let mut map = slots.write().unwrap();
                            if let Some(d) = map.get(&key).copied() {
                                d
                            } else {
                                let compiled = ::parse_that::regex::dfa::Dfa::compile(pattern)
                                    .unwrap_or_else(|| {
                                        panic!("Failed to compile regex to DFA: {}", pattern)
                                    });
                                let leaked: &'static ::parse_that::regex::dfa::Dfa =
                                    ::std::boxed::Box::leak(::std::boxed::Box::new(compiled));
                                map.insert(key, leaked);
                                leaked
                            }
                        }
                    };
                    dfa.find_at(input, offset).map(|end| (end - offset) as u32)
                }
            }

            /// Module-level scanner singleton. `DtaDfaScanner` is a ZST;
            /// `const` binds the one-and-only value at compile time so
            /// every `parse()` call borrows the same instance rather
            /// than materialising a new stack local.
            const DTA_SCANNER: DtaDfaScanner = DtaDfaScanner;

            impl #ident {
                #depth_counter
                #extra
                #visitor_kernels

                /// Parse an input string and return a zero-copy
                /// `Parsed<'_, Self>` that borrows the input directly.
                ///
                /// AW-I.W3: `parse()` dispatches through the DTA driver
                /// wholesale. The per-rule fn-per-rule path retired at
                /// W3.1; the DTA walker (AW-I.W2.1) owns Seq / Literal /
                /// Regex / Ref / AltLinear-with-savepoint / Repeat with
                /// `lo..=hi` bounds / ShuntingYard. `dta_run_into`
                /// drives `DTA_TABLE` over the input bytes, writing
                /// records into the builder's columns and stamping
                /// `frame_depth` inline so `finish()` skips the
                /// `derive_frame_depth` reconstruction pass.
                pub fn parse(
                    input: &str,
                ) -> ::core::result::Result<
                    ::bbnf::runtime::Parsed<'_, Self>,
                    ::bbnf::runtime::ParseErr,
                > {
                    let mut builder =
                        ::bbnf::runtime::tape::TapeBuilder::with_capacity(
                            GRAMMAR_PROFILE.capacity_for(input.len()),
                        );
                    builder.enable_inline_frame_depth();
                    let mut psi = psi_with_capacity(input.len());
                    let root_off = builder
                        .dta_run_into(&DTA_TABLE, input.as_bytes(), &DTA_SCANNER, &mut psi)
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
            }
        }
    }
}
