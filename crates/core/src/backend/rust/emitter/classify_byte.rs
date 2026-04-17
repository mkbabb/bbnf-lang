//! AW-III.W6.3 — ClassifyByte emitter.
//!
//! # Architectural role
//!
//! A ClassifyByte state is a 256-entry lookup table keyed on the
//! current input byte: `table[b]` names the next state, or
//! [`DtaStateId::NONE`] when `b` is outside the mined alphabet. The
//! emitter lowers this to a `match input[pos]` expression with one
//! arm per mined byte-class, identical in shape to
//! [`crate::backend::rust::emitter::dta_walker::lower_state`]'s
//! `ByteDispatch` arm but gated on the disjoint-first mining pass.
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
//! 1. An inner `match b { ... }` that groups every (byte → state-id)
//!    pair by target, one arm per distinct target.
//! 2. A fallback arm: either routes to the mined `fallback` state or
//!    raises `DtaError::Syntax` when no fallback exists.
//! 3. An Alt-frame cursor stamp so the Alt's `variant_idx` captures
//!    the chosen branch id (ByteDispatch semantic parity).
//!
//! The emission shape is deliberately identical to
//! [`emit_byte_dispatch_arm`] so the cold-path walker's
//! `DtaState::ClassifyByte` arm reproduces the same semantic via the
//! same runtime helper calls. LLVM lowers both to a jumptable; the
//! ClassifyByte call site's disjoint-first provenance lets future
//! tranches hook additional specialisation (e.g. CMOV-based
//! conditional dispatch for two-target tables) without touching the
//! ByteDispatch path.

use bbnf_ir::passes::recognizers::dta::StateId;
use proc_macro2::{Literal, TokenStream};
use quote::quote;

/// Emit the inlined body of a ClassifyByte arm.
///
/// `idx` is the state's position in `DTA_TABLE.states` (used in error
/// paths so the surfaced `DtaError::Syntax` carries the correct
/// `failing_state`). `disp` is the 256-entry state-id table; entries
/// equal to [`StateId::NONE`] route to the fallback.
///
/// Lowered shape:
///
/// ```ignore
/// let b = input.get(*pos as usize).copied().unwrap_or(0);
/// let chosen = match b {
///     <bytes_for_target_A>|... => DtaStateId(<A>),
///     <bytes_for_target_B>|... => DtaStateId(<B>),
///     _ => <fallback_or_syntax>,
/// };
/// if let Some(top) = stack.top_mut() {
///     if matches!(top.kind, DtaFrameKind::Alt) {
///         top.cursor = chosen.0;
///     }
/// }
/// Ok(StepResult::Next(chosen))
/// ```
pub(crate) fn emit_classify_byte_arm(
    idx: usize,
    disp: &[StateId],
    fallback: Option<StateId>,
) -> TokenStream {
    let idx_lit = Literal::usize_unsuffixed(idx);

    // Group the 256 entries by target state so each arm is a
    // `byte_lit | byte_lit | ... => DtaStateId(<target>)`. LLVM
    // lowers the resulting match to a jumptable when the byte set is
    // dense, or a compare-chain when sparse — both outperform the
    // table indirect load.
    let mut by_target: std::collections::BTreeMap<u16, Vec<u8>> =
        std::collections::BTreeMap::new();
    for (b, &target) in disp.iter().enumerate() {
        if target != StateId::NONE {
            by_target.entry(target.0).or_default().push(b as u8);
        }
    }

    let arms = by_target.iter().map(|(target, bytes)| {
        let target_lit = Literal::u16_unsuffixed(*target);
        let byte_lits = bytes.iter().map(|b| Literal::u8_unsuffixed(*b));
        quote! {
            #(#byte_lits)|* => ::bbnf::runtime::tape::DtaStateId(#target_lit),
        }
    });

    let fallback_arm = match fallback {
        Some(s) if s != StateId::NONE => {
            let s_lit = Literal::u16_unsuffixed(s.0);
            quote! {
                _ => ::bbnf::runtime::tape::DtaStateId(#s_lit),
            }
        }
        _ => {
            quote! {
                _ => {
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
        // AW-III.W6.3 — ClassifyByte inlined dispatch.
        //
        // The outer `match b` is the mechanical lowering of the
        // mining pass's 256-entry table. Byte classes route to
        // state-ids; the fallback covers every byte outside the
        // mined alphabet (either routing to an explicit fallback
        // state or raising Syntax when the mined Alt is total).
        let b = input.get(*pos as usize).copied().unwrap_or(0);
        let chosen = match b {
            #(#arms)*
            #fallback_arm
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
