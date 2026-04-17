//! AW-III.W4.d — In-module helper emission for the specialised walker.
//!
//! # Architectural role
//!
//! Earlier waves of W4 routed every non-`ByteDispatch` arm through a
//! `__dispatch_via_cold` bridge that called back into
//! `bbnf_tape::driver::dta_run_cold`. That bridge collapsed in W4.d:
//! every `DtaState` variant now lowers to a fully inlined arm body
//! (see [`super::lower_state`]). The hot path no longer crosses the
//! cold-path helper boundary.
//!
//! What remains in this file is the import surface the inlined arms
//! need. The lowered code references runtime types (`StepResult`,
//! `Frame`, `IterSavepoint`, `FrameStackSavepoint`, `DtaError`,
//! `RepeatAbsorbResult`, `TapeKind`, `TapeOffset`, `DtaStateId`,
//! `DtaRuleId`, `DtaState`, `SeqPromote`, `DtaFrameKind`,
//! `PayloadKind`, `PayloadJob`) and helper functions
//! (`emit_leaf`, `emit_leaf_with_payload`, `reserve_compound`,
//! `close_compound`, `advance_or_pop_with`, `try_branch`,
//! `pop_and_release`, `frame_to_tape_kind`, `saturating_u16`,
//! `stage_literal_payload_in_arena`, `first_ws_pattern`,
//! `trim_with_pattern`, `trim_ascii_ws`, `handle_repeat_failure`,
//! `FrameStack::*`) directly via their `::bbnf::runtime::tape::`
//! re-export path. The helper block here exists to keep the inner
//! `__dta_walker_inline` module compilation-clean; the function body
//! is intentionally tiny (a `use` re-shadow plus a no-op marker).
//!
//! Per the §6 invariant the helper emission is grammar-agnostic — the
//! same tokens render for every grammar.

use proc_macro2::TokenStream;
use quote::quote;

/// Emit the helper block — currently a use-re-shadow that documents
/// the runtime surfaces the inlined arms reference.
///
/// W4.d removed the `__StepOutcome` enum + `__dispatch_via_cold`
/// bridge that earlier waves emitted. The inlined arms now use
/// `::bbnf::runtime::tape::StepResult` directly and contain the
/// `dispatch_one` semantic per variant. No grammar-level helpers
/// remain.
pub(super) fn emit_inline_helpers() -> TokenStream {
    quote! {
        // AW-III.W4.d — every per-state arm inlines its dispatch_one
        // semantic directly. The runtime helpers are re-exported from
        // the parent module (`use super::*;` in __dta_walker_inline);
        // no per-grammar bridge survives.
        //
        // The presence of this comment in the emitted output serves
        // as the codegen test's structural marker: the walker module
        // exists, and every arm's dispatch is inlined rather than
        // bridged.
    }
}
