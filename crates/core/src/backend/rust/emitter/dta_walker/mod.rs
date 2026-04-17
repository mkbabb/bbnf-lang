//! AW-III.W4 — Per-grammar specialised DTA walker emitter.
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
//!   outer dispatch `match`. W4.d completes the lowering: every variant
//!   inlines its dispatch_one semantic directly. The cold-path
//!   `dispatch_one` survives in the runtime crate as the AX replay
//!   surface; the hot path never calls it.
//! * [`helpers`] — outcome-type re-exports + the SY placeholder bridge
//!   surfaces. Emitted once per grammar so the lowered code shares
//!   uniform return shapes with the runtime helpers.
//!
//! # Hot/cold contract
//!
//! - `state_count ≤ HOT_BUDGET` → single function, all states inline in
//!   the outer `match`. Optimal for grammars whose entire DTA fits in
//!   L1 i-cache (every grammar in the corpus other than CSS L4).
//! - `state_count > HOT_BUDGET` → hot states inline in the outer loop;
//!   cold states emit as `#[cold] #[inline(never)]` siblings. The
//!   sibling's dispatch contract is the same `Result<StepResult,
//!   DtaError>` shape so the outer loop dispatches uniformly.
//!
//! Both strategies are general; the choice is driven by IR cardinality,
//! not grammar name.

use bbnf_ir::passes::lift_dta;
use bbnf_ir::passes::profile::GrammarProfile;
use bbnf_ir::passes::recognizers::dta::{DtaState, DtaTable};
use bbnf_ir::passes::sets::StructuralAlphabet;
use bbnf_ir::GrammarIR;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

mod decoders;
mod helpers;
mod hot_cold;
mod lower_state;

pub use hot_cold::{HotColdPartition, HOT_BUDGET};

/// Sanitise a grammar identifier for use as a Rust ident fragment.
///
/// Non-alphanumeric / non-underscore characters map to `_`. Used
/// consistently by every emitter-emitted symbol that embeds the
/// grammar name (`dta_run_<grammar>`, `__regex_scan_<grammar>`) so
/// cross-emitter ident compositions are guaranteed to agree
/// byte-for-byte.
fn sanitise_grammar(grammar: &str) -> String {
    let mut s = String::with_capacity(grammar.len());
    for ch in grammar.chars() {
        if ch.is_ascii_alphanumeric() || ch == '_' {
            s.push(ch);
        } else {
            s.push('_');
        }
    }
    s
}

/// Compose the per-grammar regex-scan adapter ident —
/// `__regex_scan_<grammar>`. The adapter's signature is
/// `fn(&str, &[u8], usize) -> Option<u32>`; its body dispatches on
/// pointer-equality of the interned pattern `&'static str` statics
/// and splices the inline DFA body for each matched pattern. Used by
/// the cold-path replay surface helpers (`try_branch`,
/// `handle_repeat_failure_bounded`) the cold siblings call.
///
/// AW-IV.W1.4-aggro — the hot path does NOT consult this adapter.
/// Every Regex / WsTrim / boundary-ws site splices the DFA body
/// inline at the source level; the adapter exists only as the SOLE
/// out-of-line per-grammar fn for cold-path callers that dispatch by
/// pattern string. The adapter's own dispatch arms also splice inline
/// DFA bodies so no chain of `__dfa_match_*` fn calls exists anywhere.
pub(crate) fn regex_scan_adapter_ident(grammar: &str) -> proc_macro2::Ident {
    format_ident!("__regex_scan_{}", sanitise_grammar(grammar))
}

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
    ir: &GrammarIR,
    table: &DtaTable,
    alphabet: &StructuralAlphabet,
    profile: &GrammarProfile,
) -> TokenStream {
    let _ = (alphabet, profile); // W5 / W6 consumers attach later.
    let fn_ident = walker_fn_ident(grammar);
    // AW-IV.W2.1 — body splice for the main-loop's Repeat-failure
    // absorber. Replaces the cross-crate
    // `bbnf_tape::handle_repeat_failure` call with the stack-walk +
    // iter-savepoint restore + close-compound logic at source level.
    let repeat_failure_splice = helpers::emit_handle_repeat_failure_inline();

    if table.states.is_empty() {
        return quote! {
            /// AW-III.W4 — empty-table specialised walker.
            ///
            /// The lifter emitted no states for this grammar, so the
            /// callable surface is preserved but every invocation
            /// fails with `InvalidState`. The surrounding `parse()`
            /// should route through the legacy fn-per-rule path until
            /// the lifter expands its coverage.
            ///
            /// AW-IV.W1.4-aggro — signature drops the `scanner: &dyn
            /// RegexScanner` parameter; the per-grammar walker splices
            /// the DFA's loop body inline at every Regex / WsTrim site
            /// (no trait-object scanner, no separately-emitted
            /// `__dfa_match_*` fn).
            #[allow(dead_code)]
            pub fn #fn_ident(
                _input: &[u8],
                _idx: &::bbnf::runtime::tape::stage1::StructuralIndex,
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
    let dispatch_arms = lower_state::emit_state_dispatch_arms(grammar, ir, table, &partition);
    let cold_siblings = lower_state::emit_cold_siblings(grammar, ir, table, &partition);
    let boundary_ws = emit_boundary_ws(grammar, ir, table);
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
        #[allow(dead_code, unused_variables, unused_assignments, unused_mut, clippy::needless_borrow)]
        mod __dta_walker_inline {
            use super::*;

            #helper_block

            #cold_siblings

            /// AW-III.W4.d — specialised DTA walker for this grammar.
            ///
            /// Mechanically lowered from `DTA_TABLE.states`. The outer
            /// `match cur` has one arm per state-id; the dispatch is
            /// jump-table-ready, eliminating the runtime
            /// `match table.states[idx]` over the 14-variant enum
            /// that LLVM lowered to a 4-compare ladder.
            ///
            /// Every variant's body inlines its `dispatch_one`
            /// semantic directly — `Literal` byte cmp, `Regex`
            /// scanner.scan, `Seq` reserve_compound + push, etc. The
            /// cold-path `dispatch_one` survives in
            /// `bbnf_tape::driver` solely as the AX replay surface;
            /// the hot path never calls it. Repeat-failure absorption
            /// happens at the outer loop's error handler via
            /// `handle_repeat_failure`.
            ///
            /// `DTA_TABLE` is referenced as a `pub const` from the
            /// surrounding module so LLVM can constant-fold every
            /// `DTA_TABLE.states[N]` destructure inside an arm body —
            /// the runtime variant + its slice references resolve at
            /// compile time, not via a `&'static [DtaState]` indirect
            /// load each visit.
            ///
            /// AW-IV.W1.4-aggro — the `<__S: RegexScanner>` generic +
            /// the `scanner: &__S` parameter are abrogated. The hot
            /// path's Regex / WsTrim arms splice the DFA's `loop {
            /// match state { ... } }` body inline as a labelled block;
            /// the cold sibling's helper-bound `try_branch` /
            /// `handle_repeat_failure` calls pass the per-grammar
            /// `__regex_scan_<grammar>` adapter as a concrete fn
            /// pointer (the adapter's own dispatch arms also splice
            /// inline DFA bodies — no chain of `__dfa_match_*` fn
            /// calls exists anywhere). No trait-object dispatch
            /// survives.
            #[allow(dead_code)]
            pub fn run(
                input: &[u8],
                idx: &::bbnf::runtime::tape::stage1::StructuralIndex,
                columns: &mut ::bbnf::runtime::tape::Columns,
                psi: &mut ::bbnf::runtime::tape::PayloadStream,
                frame_depth: &mut ::std::vec::Vec<u8>,
            ) -> ::core::result::Result<
                ::bbnf::runtime::tape::TapeOffset,
                ::bbnf::runtime::tape::DtaError,
            > {
                // Reference the per-grammar `DTA_TABLE` const through
                // the inner module's `use super::*;` glob — `super` is
                // ambiguous when multiple `#[derive(Parser)]` invocations
                // re-export their `DTA_TABLE` into a shared parent scope
                // (test files commonly do this). The bare ident
                // resolves unambiguously because `use super::*;` brings
                // exactly one `DTA_TABLE` into this module's scope.
                let table: &::bbnf::runtime::tape::DtaTable = &DTA_TABLE;
                let root_rec =
                    ::bbnf::runtime::tape::TapeOffset(columns.len() as u32);
                let mut stack_owned = ::bbnf::runtime::tape::FrameStack::new();
                let stack: &mut ::bbnf::runtime::tape::FrameStack = &mut stack_owned;
                let mut pos_owned: u32 = 0;
                let pos: &mut u32 = &mut pos_owned;
                // AW-III.W5.d — dual cursor's structural slot.
                // `idx` now arrives populated from the caller's
                // `bbnf_simd_scan::scan_structural` call; the
                // structural-aware arms consume slot-indexed lookups
                // instead of byte-stepping.
                let mut slot_owned: u32 = 0;
                let slot: &mut u32 = &mut slot_owned;

                // AW-IV.W1.α — boundary trim is a codegen-time
                // decision: the emitter scans `table.states` for the
                // first `DtaState::WsTrim`, emits a direct DFA match
                // loop when a pattern exists, the ASCII trim when the
                // pattern is `None`, or no code at all when the
                // grammar has no `WsTrim` state. The runtime
                // `first_ws_pattern` table scan + `trim_with_pattern`
                // scanner-trait indirection are both abrogated.
                #boundary_ws

                #entry_state_lookup

                'walk: loop {
                    let step: ::core::result::Result<
                        ::bbnf::runtime::tape::StepResult,
                        ::bbnf::runtime::tape::DtaError,
                    > = 'step: {
                        match cur {
                            #dispatch_arms
                            #invalid_state_arm
                        }
                    };
                    match step {
                        ::core::result::Result::Ok(
                            ::bbnf::runtime::tape::StepResult::Next(next),
                        ) => { cur = next.0; }
                        ::core::result::Result::Ok(
                            ::bbnf::runtime::tape::StepResult::Done,
                        ) => break 'walk,
                        ::core::result::Result::Err(
                            e @ ::bbnf::runtime::tape::DtaError::Syntax { .. },
                        ) => {
                            // AW-IV.W2.1 — body splice replaces the
                            // cross-crate `handle_repeat_failure` call.
                            // `_ = idx;` matches the runtime helper's
                            // signature threading (see
                            // `handle_repeat_failure`'s body in
                            // `bbnf-tape/src/driver.rs`).
                            let _ = idx;
                            match #repeat_failure_splice? {
                                ::bbnf::runtime::tape::RepeatAbsorbResult::Continue(next) => {
                                    cur = next.0;
                                }
                                ::bbnf::runtime::tape::RepeatAbsorbResult::Done => {
                                    break 'walk;
                                }
                                ::bbnf::runtime::tape::RepeatAbsorbResult::NotAbsorbed => {
                                    return ::core::result::Result::Err(e);
                                }
                            }
                        }
                        ::core::result::Result::Err(e) => {
                            return ::core::result::Result::Err(e);
                        }
                    }
                }

                #boundary_ws
                if (*pos as usize) < input.len() {
                    return ::core::result::Result::Err(
                        ::bbnf::runtime::tape::DtaError::UnexpectedEnd {
                            offset: *pos,
                        },
                    );
                }

                ::core::result::Result::Ok(root_rec)
            }
        }

        /// AW-III.W4 — public entry into the specialised DTA walker.
        ///
        /// Surfaces the per-grammar `__dta_walker_inline::run` under a
        /// stable name (`dta_run_<grammar>`) so the surrounding
        /// `parse()` can call into it without traversing the inner
        /// module. The inner module exists to scope the per-state
        /// helper functions away from the surrounding `generated.rs`
        /// namespace.
        ///
        /// AW-III.W4.d — `DTA_TABLE` is referenced internally as
        /// `&super::DTA_TABLE` (a `pub const`) so LLVM constant-folds
        /// every per-arm `DTA_TABLE.states[N]` destructure at compile
        /// time. The `table` parameter is gone: the walker is
        /// per-grammar, the table is per-grammar, the binding is
        /// static. Removing the parameter eliminates the dynamic
        /// indirect load that gated `match table.states[N]`
        /// const-folding.
        ///
        /// AW-IV.W1.4-aggro — the `<__S: RegexScanner>` generic + the
        /// `scanner: &__S` parameter are abrogated. Every regex
        /// semantic on the hot path is an inline DFA body spliced
        /// into the walker arm; no separately-emitted `__dfa_match_*`
        /// fn exists, and no trait-object dispatch survives.
        #[allow(dead_code)]
        #[inline]
        pub fn #fn_ident(
            input: &[u8],
            idx: &::bbnf::runtime::tape::stage1::StructuralIndex,
            columns: &mut ::bbnf::runtime::tape::Columns,
            psi: &mut ::bbnf::runtime::tape::PayloadStream,
            frame_depth: &mut ::std::vec::Vec<u8>,
        ) -> ::core::result::Result<
            ::bbnf::runtime::tape::TapeOffset,
            ::bbnf::runtime::tape::DtaError,
        > {
            __dta_walker_inline::run(
                input, idx, columns, psi, frame_depth,
            )
        }
    }
}

/// AW-IV.W1.4-aggro — codegen-time boundary whitespace trim with
/// inline DFA body splice.
///
/// Walks `table.states` once at codegen time for the first
/// `DtaState::WsTrim` variant. When found with a pattern, splice the
/// DFA's `loop { match state { ... } }` body directly into a fixed-
/// point driving loop — no separately-emitted `__dfa_match_*` function
/// exists. When found with no pattern, emit an ASCII-whitespace trim.
/// When no WsTrim state exists in the table, emit nothing.
///
/// The DFA body is spliced via [`super::dfa_codegen::emit_dfa_inline_body`];
/// the splice introduces `__dfa_`-prefixed locals that don't collide
/// with the walker's `pos` / `stack` / `idx` bindings. Each iteration
/// shadows `pos` with the current `*pos as usize` inside an inner
/// block so the DFA body's bindings see a concrete `usize`; the outer
/// `*pos += len` mutation happens outside the shadowed scope, against
/// the walker's `&mut u32`.
fn emit_boundary_ws(
    grammar: &str,
    ir: &GrammarIR,
    table: &DtaTable,
) -> TokenStream {
    for (idx, state) in table.states.iter().enumerate() {
        match state {
            DtaState::WsTrim { pattern: Some(_) } => {
                let dfa_inline_body =
                    super::dfa_codegen::emit_dfa_inline_body(grammar, ir, table, idx);
                return quote! {
                    loop {
                        let dfa_result: ::core::option::Option<u32> = {
                            let pos: usize = *pos as usize;
                            #dfa_inline_body
                        };
                        match dfa_result {
                            ::core::option::Option::Some(0)
                            | ::core::option::Option::None => break,
                            ::core::option::Option::Some(n) => {
                                *pos += n;
                            }
                        }
                    }
                };
            }
            DtaState::WsTrim { pattern: None } => {
                return quote! {
                    ::bbnf::runtime::tape::trim_ascii_ws(input, pos);
                };
            }
            _ => {}
        }
    }
    TokenStream::new()
}

/// Emit the symbol identifier for the per-grammar walker function. The
/// `grammar` argument is sanitised so identifiers like `bbnf-bootstrap`
/// or `css/l4` produce valid Rust idents.
pub fn walker_fn_ident(grammar: &str) -> proc_macro2::Ident {
    format_ident!("dta_run_{}", sanitise_grammar(grammar))
}

/// Lift the IR's DTA table for the grammar — single source of truth.
/// Used by the grammar-level emitter to pass into
/// [`emit_specialised_walker`] alongside the table that
/// `emit_dta_table` already consumes; both sides agree on the same
/// `lift_dta(ir)` output.
pub(crate) fn lift_for_walker(ir: &GrammarIR) -> DtaTable {
    lift_dta(ir)
}
