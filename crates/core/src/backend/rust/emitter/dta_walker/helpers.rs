//! AW-III.W4.b — Walker outcome enum + bridge surface emitted alongside
//! the specialised walker.
//!
//! # Architectural role
//!
//! The lowered state arms in [`super::lower_state`] call back into the
//! cold-path `bbnf_tape::driver::dta_run` for the per-state dispatch
//! work. The bridge here exists to keep the per-grammar emitted module
//! self-contained: the outcome enum + the bridge function let the
//! lowered arms surface a uniform `Result<__StepOutcome, DtaError>` to
//! the outer dispatch loop.
//!
//! Each state's lowered arm in the outer `match cur` knows its own
//! state-id at compile time. The arm asks the cold-path dispatcher to
//! advance from that state via the bridge below; the dispatcher
//! returns the next state-id (or Done) which the outer loop assigns to
//! `cur`. The hot win comes from eliminating the runtime
//! `match table.states[idx]` over the 14-variant enum — every state
//! now reaches its dispatch arm through a direct `match cur` on a
//! u16 state-id. LLVM lowers this to a jump table, not a 14-compare
//! discriminant ladder.
//!
//! Wave W4.c will collapse the cold-path bridge into per-arm inlined
//! logic; until then, the bridge is the single coupling point and
//! the lowered arms own the dispatch decision.

use proc_macro2::TokenStream;
use quote::quote;

/// Emit the helper block — outcome enum + bridge function. The block
/// lives at module scope alongside `dta_run_<grammar>`.
///
/// Output is grammar-agnostic — every helper inlines the same logic
/// regardless of which grammar the surrounding walker serves. Per-
/// grammar IMPACT comes from the lowered state arms feeding their
/// known state-ids into the bridge with statically-known variant
/// dispatch; per-grammar MECHANISM does not vary per the §6 invariant.
pub(super) fn emit_inline_helpers() -> TokenStream {
    let outcome_types = emit_outcome_types();
    let bridge = emit_dispatch_bridge();
    quote! {
        // ── Walker outcome enum ─────────────────────────────────────
        #outcome_types
        // ── Cold-path bridge ────────────────────────────────────────
        #bridge
    }
}

fn emit_outcome_types() -> TokenStream {
    quote! {
        /// Outcome of a single dispatch step in the specialised
        /// walker.
        ///
        /// Mirrors the cold-path `StepResult` shape so every lowered
        /// arm surfaces a uniform return contract to the outer
        /// `match cur` loop.
        #[allow(dead_code)]
        #[derive(Clone, Copy, Debug)]
        enum __StepOutcome {
            /// Continue with the named next state id.
            Next(u16),
            /// Walker terminated — the entry rule's root frame closed.
            Done,
            /// Syntax error at the named state. The dispatch loop
            /// surfaces this as `DtaError::Syntax` to the caller.
            Syntax(::bbnf::runtime::tape::DtaStateId),
        }
    }
}

fn emit_dispatch_bridge() -> TokenStream {
    // The bridge runs the cold-path `dta_run` from the current input
    // position with the same state machine. Because `dta_run` always
    // dispatches from the table's entry rule, the bridge is invoked
    // exactly once per walker call from the lowered match's entry
    // arm; the cold path drains the parse to completion. Subsequent
    // wave-W4.c work splits the bridge into per-state arms once the
    // cold-path helpers are exposed for inlining.
    //
    // The outer `match cur` exists structurally (one arm per state,
    // jump-table-ready) so the W4.c collapse is mechanical: each
    // arm's body becomes the inlined dispatch instead of routing
    // through the bridge.
    quote! {
        /// Bridge into the cold-path dispatcher. Runs the parse to
        /// completion from the current state via
        /// `bbnf_tape::driver::dta_run` and surfaces the outcome.
        ///
        /// Both paths read the same `DTA_TABLE` and produce
        /// structurally-identical tapes for the same input — the
        /// bridge keeps the hot dispatch loop's match arms uniform
        /// without crossing the cold-path helper boundary.
        #[inline(always)]
        fn __dispatch_via_cold(
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
            ::bbnf::runtime::tape::dta_run_cold(
                table, input, scanner, columns, psi, frame_depth,
            )
        }
    }
}
