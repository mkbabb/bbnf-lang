//! AW-III.W6.3 / AW-IV.W3.3 — ClassifyByte emitter.
//!
//! # Architectural role
//!
//! A ClassifyByte state is a 256-entry lookup table keyed on the
//! current input byte: `table[b]` names the next state, or
//! [`DtaStateId::NONE`] when `b` is outside the mined alphabet.
//!
//! AW-IV.W3.3 lowers this to a single indexed load on an arm-local
//! `const CLASSIFY_TABLE_<idx>: [DtaStateId; 256]` literal — the
//! tightest dispatch form. One load + one `== NONE` branch per Alt
//! entry; no jumptable, no per-byte match cascade, no indirection
//! through the `DTA_TABLE` const's table reference. LLVM sees the
//! 256-entry array as a literal at codegen time and co-locates it
//! next to the calling arm (per-site const-folding of the dispatch
//! key).
//!
//! This supersedes `ByteDispatch` for any Alt whose branches the
//! `disjoint_first` miner admits. The lifter gate (see
//! [`bbnf_ir::passes::recognizers::dta`]) routes admitted Alts to
//! `ClassifyByte`; the less-optimised `ByteDispatch` path covers
//! only Alts that `disjoint_first` rejected (overlapping FIRSTs,
//! fallback branches, typed-branches + superset-fallback escape
//! hatch).
//!
//! # §6 generalisation
//!
//! The mining pass ([`bbnf_ir::passes::recognizers::disjoint_first`])
//! identifies every Alt with mutually-disjoint FIRST sets — the
//! mechanism is grammar-agnostic. Per-grammar impact varies because
//! CSS `compoundSelector`, BBNF `directive`, JSON escape, and Sheets
//! function-first-letter all surface distinct alphabet entries; the
//! per-grammar MECHANISM does not vary.
//!
//! # Emission
//!
//! Each ClassifyByte state emits:
//!
//! 1. A `const CLASSIFY_TABLE_<idx>: [DtaStateId; 256] = [...];`
//!    literal — 256 entries, each a `DtaStateId(u16)` or
//!    `DtaStateId::NONE` sentinel. AW-IV.W1.1 hoisting: the emitter
//!    knows every byte → state-id mapping at codegen time; the
//!    const is a literal projection of that map.
//! 2. A single indexed load: `let next =
//!    CLASSIFY_TABLE_<idx>[input[*pos as usize] as usize];`.
//! 3. A `if next == DtaStateId::NONE` branch: route to the mined
//!    `fallback` state when present, else raise `DtaError::Syntax`.
//! 4. An Alt-frame cursor stamp so the Alt's `variant_idx` captures
//!    the chosen branch id (ByteDispatch semantic parity).
//!
//! The emitted arm IS the dispatcher — one load + one branch. No
//! function call, no runtime table lookup through `DTA_TABLE`, no
//! per-byte jumptable cascade.

use bbnf_ir::passes::recognizers::dta::StateId;
use proc_macro2::{Literal, TokenStream};
use quote::{format_ident, quote};

/// Emit the inlined body of a ClassifyByte arm.
///
/// `idx` is the state's position in `DTA_TABLE.states` (used in error
/// paths so the surfaced `DtaError::Syntax` carries the correct
/// `failing_state`, and in the `__CLASSIFY_TABLE_<idx>` ident so each
/// state's LUT is uniquely named). `disp` is the 256-entry state-id
/// table; entries equal to [`StateId::NONE`] route to the fallback.
///
/// Lowered shape (AW-IV.W3.3):
///
/// ```ignore
/// const __CLASSIFY_TABLE_<idx>: [DtaStateId; 256] = [/* 256 literals */];
/// let __b = input.get(*pos as usize).copied().unwrap_or(0);
/// let __next = __CLASSIFY_TABLE_<idx>[__b as usize];
/// if __next == DtaStateId::NONE {
///     /* fallback handling — route to fallback_state or raise Syntax */
/// } else {
///     if let Some(top) = stack.top_mut() {
///         if matches!(top.kind, DtaFrameKind::Alt) {
///             top.cursor = __next.0;
///         }
///     }
///     Ok(StepResult::Next(__next))
/// }
/// ```
pub fn emit_classify_byte_arm(
    idx: usize,
    disp: &[StateId],
    fallback: Option<StateId>,
) -> TokenStream {
    let idx_lit = Literal::usize_unsuffixed(idx);
    let table_ident = format_ident!("__CLASSIFY_TABLE_{}", idx);

    // AW-IV.W1.1 hoisting: emit every (byte → state-id) mapping as
    // a literal const array. The emitter knows at codegen time the
    // full 256-entry LUT; we project it directly into a
    // `[DtaStateId; 256]` literal so LLVM can const-fold the single
    // indexed load at the call site.
    debug_assert_eq!(disp.len(), 256, "ClassifyByte table must be 256 entries");
    let entry_literals = disp.iter().map(|s| {
        if *s == StateId::NONE {
            quote! { ::bbnf::runtime::tape::DtaStateId::NONE }
        } else {
            let n = Literal::u16_unsuffixed(s.0);
            quote! { ::bbnf::runtime::tape::DtaStateId(#n) }
        }
    });

    let fallback_arm = match fallback {
        Some(s) if s != StateId::NONE => {
            let s_lit = Literal::u16_unsuffixed(s.0);
            quote! {
                ::bbnf::runtime::tape::DtaStateId(#s_lit)
            }
        }
        _ => {
            quote! {
                {
                    break 'step ::core::result::Result::Err(
                        ::bbnf::runtime::tape::DtaError::Syntax {
                            offset: *pos,
                            failing_state: ::bbnf::runtime::tape::DtaStateId(#idx_lit as u16),
                            failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                        },
                    );
                }
            }
        }
    };

    quote! {
        // AW-IV.W3.3 — ClassifyByte inline dispatch.
        //
        // Arm-local `const` literal carries the 256-entry LUT
        // directly; LLVM sees the array at codegen time and lowers
        // the `CLASSIFY_TABLE[b]` access as a single indexed load
        // from a co-located `.rodata` slot. No runtime indirection
        // through `DTA_TABLE.states[N].table`, no per-byte match
        // cascade — one load + one NONE-branch per Alt entry.
        const #table_ident: [::bbnf::runtime::tape::DtaStateId; 256] = [
            #(#entry_literals),*
        ];
        let __b = input.get(*pos as usize).copied().unwrap_or(0);
        let __next = #table_ident[__b as usize];
        let chosen = if __next == ::bbnf::runtime::tape::DtaStateId::NONE {
            #fallback_arm
        } else {
            __next
        };
        if let ::core::option::Option::Some(top) = stack.top_mut() {
            if matches!(top.kind, ::bbnf::runtime::tape::DtaFrameKind::Alt) {
                top.cursor = chosen.0;
            }
        }
        ::core::result::Result::Ok(
            ::bbnf::runtime::tape::StepResult::Next(chosen),
        )
    }
}
