//! AW-III.W4.b — Per-grammar specialised DTA walker emitter.
//!
//! # Architectural role
//!
//! The legacy DTA hot path runs through `bbnf_tape::driver::dispatch_one`
//! — a single 14-arm `match` over `DtaState` that LLVM lowers to a
//! `log₂(14) ≈ 4`-compare tree per state visit. With JSON twitter
//! visiting ~3–8 states per byte, that floor accounts for the measured
//! 24% self-time in `dispatch_one`. AW-III.W4 replaces that interpreter
//! with mechanically-specialised per-grammar code: the emitter walks
//! `DtaTable.states` and lowers every variant to an inlined arm in one
//! `loop { match cur { ... } }` skeleton. The `DtaState` enum match
//! disappears in the **output**; the table itself still ships in
//! `generated.rs` so the cold-path replay surface (AX substrate) keeps
//! consulting it verbatim.
//!
//! Per the §6 generalisation invariant the pass body never branches on
//! grammar identity. The `grammar` parameter is symbol-namespace prefix
//! only — `dta_run_<grammar>` distinguishes per-grammar functions when
//! multiple grammars coexist in the same compilation. The pass reads
//! IR-structural facts (state count, state visit frequency,
//! StructuralAlphabet, GrammarProfile) and emits the same mechanism for
//! every grammar. Per-grammar IMPACT varies because each grammar has a
//! different IR; per-grammar MECHANISM does not.
//!
//! # Submodules
//!
//! * [`hot_cold`] — hot/cold state partitioning driven by IR cardinality
//!   and the contracted `compute_state_visit_frequency` mining pass
//!   (W4.a). Below the `HOT_BUDGET` threshold the entire table inlines;
//!   above it the partition splits hot states into the outer loop and
//!   cold states into `#[cold] #[inline(never)]` siblings.
//! * [`lower_state`] — per-`DtaState` variant lowering. One free
//!   function per variant lowers the IR fact to the matching arm of the
//!   outer dispatch `match`. Every variant has a complete lowering
//!   route — `ByteDispatch` inlines its 256-entry LUT verbatim
//!   (LLVM lowers to a jump table); other variants currently route
//!   through the cold-path bridge while preserving the per-state arm
//!   structure for the W4.c collapse.
//! * [`helpers`] — the `__StepOutcome` enum + `__dispatch_via_cold`
//!   bridge function. Emitted once per grammar so the lowered code
//!   links against the same name-space without crossing the cold
//!   path's private helper boundary.
//!
//! # Hot/cold contract
//!
//! - `state_count ≤ HOT_BUDGET` → single function, all states inline in
//!   the outer `match`. Optimal for grammars whose entire DTA fits in
//!   L1 i-cache (every grammar in the corpus other than CSS L4).
//! - `state_count > HOT_BUDGET` → hot states inline in the outer loop;
//!   cold states emit as `#[cold] #[inline(never)]` siblings. The
//!   sibling's dispatch contract is the same `Result<__StepOutcome,
//!   DtaError>` shape so the outer loop dispatches uniformly.
//!
//! Both strategies are general; the choice is driven by IR cardinality,
//! not grammar name.

use bbnf_ir::passes::lift_dta;
use bbnf_ir::passes::profile::GrammarProfile;
use bbnf_ir::passes::recognizers::dta::DtaTable;
use bbnf_ir::passes::sets::StructuralAlphabet;
use bbnf_ir::GrammarIR;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

mod helpers;
mod hot_cold;
mod lower_state;

pub use hot_cold::{HotColdPartition, HOT_BUDGET};

/// AW-III.W4 — the central emitter pass.
///
/// Mechanically lowers `DtaTable.states` to inlined Rust. Returns one
/// `pub fn dta_run_<grammar>(...)` declaration plus its supporting
/// helper functions in a single `TokenStream` ready to splice into the
/// per-grammar `generated.rs` alongside `DTA_TABLE`.
///
/// The `grammar` parameter is symbol-namespace prefix only. The pass
/// body never branches on its value — every behavioural decision reads
/// from the IR-structural inputs (`table` + `alphabet` + `profile`).
/// This satisfies the §6 generalisation invariant: per-grammar IMPACT
/// varies because each grammar has different IR; per-grammar MECHANISM
/// does not.
///
/// When `table.states` is empty (the lifter saw no liftable rules for
/// this grammar) the function emits an empty stub that returns
/// `DtaError::InvalidState` — the caller's `parse()` should never
/// reach it because the surrounding pipeline routes empty-table
/// grammars through the legacy fn-per-rule path. Both branches share
/// the same callable surface so `parse()` can swap between them
/// without conditional dispatch in the hot path.
pub fn emit_specialised_walker(
    grammar: &str,
    table: &DtaTable,
    alphabet: &StructuralAlphabet,
    profile: &GrammarProfile,
) -> TokenStream {
    let _ = (alphabet, profile); // W5 / W6 consumers attach later.
    let fn_ident = walker_fn_ident(grammar);

    if table.states.is_empty() {
        return quote! {
            /// AW-III.W4.b — empty-table specialised walker.
            ///
            /// The lifter emitted no states for this grammar, so the
            /// callable surface is preserved but every invocation
            /// fails with `InvalidState`. The surrounding `parse()`
            /// should route through the legacy fn-per-rule path until
            /// the lifter expands its coverage.
            #[allow(dead_code)]
            pub fn #fn_ident(
                _table: &::bbnf::runtime::tape::DtaTable,
                _input: &[u8],
                _scanner: &dyn ::bbnf::runtime::tape::RegexScanner,
                _columns: &mut ::bbnf::runtime::tape::Columns,
                _psi: &mut ::bbnf::runtime::tape::PayloadStream,
                _frame_depth: &mut ::std::vec::Vec<u8>,
            ) -> ::core::result::Result<
                ::bbnf::runtime::tape::TapeOffset,
                ::bbnf::runtime::tape::DtaError,
            > {
                ::core::result::Result::Err(
                    ::bbnf::runtime::tape::DtaError::InvalidState {
                        state: ::bbnf::runtime::tape::DtaStateId::NONE,
                    },
                )
            }
        };
    }

    let partition = HotColdPartition::for_table(table);
    let helper_block = helpers::emit_inline_helpers();
    let dispatch_arms = lower_state::emit_state_dispatch_arms(table, &partition);
    let cold_siblings = lower_state::emit_cold_siblings(table, &partition);
    let entry_state_lookup = quote! {
        let mut cur: u16 = {
            let s = table.rule_entry_for(table.entry);
            if s == ::bbnf::runtime::tape::DtaStateId::NONE {
                return ::core::result::Result::Err(
                    ::bbnf::runtime::tape::DtaError::InvalidState {
                        state: ::bbnf::runtime::tape::DtaStateId::NONE,
                    },
                );
            }
            s.0
        };
    };

    let invalid_state_arm = quote! {
        _ => {
            return ::core::result::Result::Err(
                ::bbnf::runtime::tape::DtaError::InvalidState {
                    state: ::bbnf::runtime::tape::DtaStateId(cur),
                },
            );
        }
    };

    quote! {
        #[allow(dead_code, unused_variables, unused_assignments, unused_mut)]
        mod __dta_walker_inline {
            use super::*;

            #helper_block

            #cold_siblings

            /// AW-III.W4.b — specialised DTA walker for this grammar.
            ///
            /// Mechanically lowered from `DTA_TABLE.states`. The outer
            /// `match cur` has one arm per state-id; the dispatch is
            /// jump-table-ready, eliminating the runtime
            /// `match table.states[idx]` over the 14-variant enum
            /// that LLVM lowered to a 4-compare ladder.
            ///
            /// `ByteDispatch` arms inline their 256-entry LUT
            /// verbatim — LLVM lowers the inlined byte match to a
            /// jump table indexed by `input[pos]`. Other variants
            /// route through the cold-path bridge while preserving
            /// the per-state arm structure; W4.c collapses the
            /// bridge into per-arm inlined logic once the cold
            /// helpers are exposed.
            ///
            /// Drop-in replacement contract for `dta_run` per the W4
            /// hard gate; the cold-path `dispatch_one` survives in
            /// `bbnf_tape::driver` for replay/recovery only (AX
            /// substrate).
            #[allow(dead_code)]
            pub fn run(
                table: &::bbnf::runtime::tape::DtaTable,
                input: &[u8],
                scanner: &dyn ::bbnf::runtime::tape::RegexScanner,
                columns: &mut ::bbnf::runtime::tape::Columns,
                psi: &mut ::bbnf::runtime::tape::PayloadStream,
                frame_depth: &mut ::std::vec::Vec<u8>,
            ) -> ::core::result::Result<
                ::bbnf::runtime::tape::TapeOffset,
                ::bbnf::runtime::tape::DtaError,
            > {
                let root_rec =
                    ::bbnf::runtime::tape::TapeOffset(columns.len() as u32);
                let pos: u32 = 0;
                let _ = pos;

                #entry_state_lookup

                // Outer dispatch loop — one match arm per state id.
                // The `loop` runs until either a state arm surfaces
                // `__StepOutcome::Done` (entry rule's root frame
                // closed via the cold-path bridge) or a syntax error
                // propagates outward.
                'walk: loop {
                    let outcome = match cur {
                        #dispatch_arms
                        #invalid_state_arm
                    };
                    match outcome {
                        __StepOutcome::Next(next) => { cur = next; }
                        __StepOutcome::Done => break 'walk,
                        __StepOutcome::Syntax(state_id) => {
                            return ::core::result::Result::Err(
                                ::bbnf::runtime::tape::DtaError::Syntax {
                                    offset: 0,
                                    failing_state: state_id,
                                    failing_rule:
                                        ::bbnf::runtime::tape::DtaRuleId(
                                            u32::MAX,
                                        ),
                                },
                            );
                        }
                    }
                }

                ::core::result::Result::Ok(root_rec)
            }
        }

        /// AW-III.W4.b — public entry into the specialised DTA walker.
        ///
        /// Surfaces the per-grammar `__dta_walker_inline::run` under a
        /// stable name (`dta_run_<grammar>`) so the surrounding
        /// `parse()` can call into it without traversing the inner
        /// module. The inner module exists to scope the per-state
        /// helper functions away from the surrounding `generated.rs`
        /// namespace.
        #[allow(dead_code)]
        pub fn #fn_ident(
            table: &::bbnf::runtime::tape::DtaTable,
            input: &[u8],
            scanner: &dyn ::bbnf::runtime::tape::RegexScanner,
            columns: &mut ::bbnf::runtime::tape::Columns,
            psi: &mut ::bbnf::runtime::tape::PayloadStream,
            frame_depth: &mut ::std::vec::Vec<u8>,
        ) -> ::core::result::Result<
            ::bbnf::runtime::tape::TapeOffset,
            ::bbnf::runtime::tape::DtaError,
        > {
            __dta_walker_inline::run(
                table, input, scanner, columns, psi, frame_depth,
            )
        }
    }
}

/// Emit the symbol identifier for the per-grammar walker function. The
/// `grammar` argument is sanitised so identifiers like `bbnf-bootstrap`
/// or `css/l4` produce valid Rust idents.
pub(crate) fn walker_fn_ident(grammar: &str) -> proc_macro2::Ident {
    let mut sanitised = String::with_capacity(grammar.len() + 8);
    sanitised.push_str("dta_run_");
    for ch in grammar.chars() {
        if ch.is_ascii_alphanumeric() || ch == '_' {
            sanitised.push(ch);
        } else {
            sanitised.push('_');
        }
    }
    format_ident!("{}", sanitised)
}

/// Lift the IR's DTA table for the grammar — single source of truth.
/// Used by the grammar-level emitter to pass into
/// [`emit_specialised_walker`] alongside the table that
/// `emit_dta_table` already consumes; both sides agree on the same
/// `lift_dta(ir)` output.
pub(crate) fn lift_for_walker(ir: &GrammarIR) -> DtaTable {
    lift_dta(ir)
}
