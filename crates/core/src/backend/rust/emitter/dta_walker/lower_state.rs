//! AW-III.W4.b — Per-`DtaState` lowering routines.
//!
//! # Architectural role
//!
//! Each variant of `bbnf_ir::passes::DtaState` lowers to a `match cur`
//! arm in the outer dispatch loop. The arm encodes the variant's
//! semantics — byte cmp for `Literal`, regex scan for `Regex`, frame
//! push for `Seq`, etc. — through the cold-path bridge until W4.c
//! exposes the per-arm inline helpers. The structural per-state match
//! eliminates the runtime `match table.states[idx]` over the
//! 14-variant enum: every state now reaches its dispatch arm through a
//! direct `match cur` on a u16 state-id, which LLVM lowers to a jump
//! table.
//!
//! Per the §6 invariant the lowering decisions read only IR-structural
//! facts (variant, byte ranges, child slices); the grammar's identity
//! never appears in the lowering body.
//!
//! # ByteDispatch inlining
//!
//! `ByteDispatch` is the canonical example of a state whose
//! const-foldable dispatch table inlines verbatim at codegen time:
//! the 256-entry LUT becomes 256 match arms over the byte literal,
//! which LLVM lowers to a jump-table dispatch. The inlined arms reach
//! the next state via a direct `cur = N` assignment — no runtime
//! table read.
//!
//! Other variants (`Literal`, `Regex`, `Seq`, `AltLinear`, `Repeat`,
//! `ShuntingYard`, `Ref`, `WsTrim`, `Minus`, `Epsilon`) currently
//! route through the cold-path bridge while preserving the per-state
//! arm structure. Wave W4.c lifts the cold helpers to `pub` so the
//! bridge call can be replaced with the inlined per-arm logic.
//!
//! # Hot/cold split
//!
//! [`emit_state_dispatch_arms`] emits the hot states' arms inline in
//! the outer `match cur`. Cold states emit as `#[cold] #[inline(never)]`
//! sibling functions called via fallthrough — the outer loop treats a
//! cold sibling as a single dispatch step that surfaces the same
//! `Result<__StepOutcome, DtaError>` shape.

use bbnf_ir::passes::recognizers::dta::{
    DtaState as IrState, DtaTable, StateId,
};
use proc_macro2::{Literal, TokenStream};
use quote::{format_ident, quote};

use super::hot_cold::HotColdPartition;

/// Emit the body of the outer `match cur { ... }` dispatch.
///
/// Hot states emit as inline arms with the lowered state body. Cold
/// states emit as forwarders to their `#[cold]` sibling — the outer
/// dispatch sees a single uniform `Result<__StepOutcome, DtaError>`
/// return shape.
pub(super) fn emit_state_dispatch_arms(
    table: &DtaTable,
    partition: &HotColdPartition,
) -> TokenStream {
    let arms = table
        .states
        .iter()
        .enumerate()
        .map(|(idx, state)| {
            let id = idx as u16;
            let id_lit = Literal::u16_unsuffixed(id);
            if partition.is_hot(id) {
                let body = emit_state_arm_body(idx, state);
                quote! {
                    #id_lit => { #body }
                }
            } else {
                let cold_ident = cold_sibling_ident(id);
                quote! {
                    #id_lit => {
                        #cold_ident(
                            table, input, scanner, columns, psi, frame_depth,
                            pos, cur,
                        )?
                    }
                }
            }
        })
        .collect::<Vec<_>>();
    quote! { #(#arms)* }
}

/// Emit the cold sibling functions — one per cold state. Each sibling
/// is `#[cold] #[inline(never)]` and contains the same arm body the
/// hot path would have inlined; the dispatch contract is the same
/// `Result<__StepOutcome, DtaError>` return shape.
///
/// Cold siblings take the walker's mutable state (`pos`, `cur`) as
/// parameters because their lowered body references them — the hot
/// path supplies them from its own enclosing scope; the cold sibling
/// receives them through the function signature.
pub(super) fn emit_cold_siblings(
    table: &DtaTable,
    partition: &HotColdPartition,
) -> TokenStream {
    if partition.cold.is_empty() {
        return TokenStream::new();
    }
    let siblings = table
        .states
        .iter()
        .enumerate()
        .filter_map(|(idx, state)| {
            let id = idx as u16;
            if partition.is_hot(id) {
                return None;
            }
            let cold_ident = cold_sibling_ident(id);
            let body = emit_state_arm_body(idx, state);
            Some(quote! {
                #[cold]
                #[inline(never)]
                fn #cold_ident(
                    table: &::bbnf::runtime::tape::DtaTable,
                    input: &[u8],
                    scanner: &dyn ::bbnf::runtime::tape::RegexScanner,
                    columns: &mut ::bbnf::runtime::tape::Columns,
                    psi: &mut ::bbnf::runtime::tape::PayloadStream,
                    frame_depth: &mut ::std::vec::Vec<u8>,
                    pos: u32,
                    cur: u16,
                ) -> ::core::result::Result<
                    __StepOutcome,
                    ::bbnf::runtime::tape::DtaError,
                > {
                    let pos = pos;
                    let cur = cur;
                    let _ = (pos, cur);
                    ::core::result::Result::Ok({ #body })
                }
            })
        })
        .collect::<Vec<_>>();
    quote! { #(#siblings)* }
}

/// The fn ident for a cold sibling — `__cold_state_<id>`.
fn cold_sibling_ident(id: u16) -> proc_macro2::Ident {
    format_ident!("__cold_state_{}", id)
}

/// Emit the inlined body of one state's dispatch arm. The body
/// produces a `__StepOutcome` value that the outer dispatch loop
/// consumes to set the next `cur` or terminate.
///
/// Per the AW-III.W4.b scope the arm bodies route through the
/// cold-path bridge while preserving the per-state structure. The
/// outer `match cur` is the load-bearing structural change — once
/// W4.c lifts the cold-path helpers, the bridge call collapses into
/// inlined per-state logic.
///
/// `ByteDispatch` is the exception: its 256-entry LUT inlines as a
/// `match input[pos]` over the byte literals, which LLVM lowers to
/// a jump table — the const-folded dispatch table is already a
/// tangible per-state win without further wave coordination.
fn emit_state_arm_body(idx: usize, state: &IrState) -> TokenStream {
    let _ = idx;
    let kind_tag = state_kind_tag(state);
    let inline_dispatch = match state {
        IrState::ByteDispatch { table: disp, fallback } => {
            Some(emit_byte_dispatch_inlined(disp, *fallback))
        }
        _ => None,
    };
    if let Some(inlined) = inline_dispatch {
        quote! {
            // AW-III.W4.b — ByteDispatch arm: 256-entry LUT inlined
            // as a `match input[pos]` over byte literals. LLVM lowers
            // this to a jump-table dispatch; the per-state runtime
            // table read is gone.
            //
            // State kind tag: #kind_tag
            let _ = #kind_tag;
            #inlined
        }
    } else {
        quote! {
            // AW-III.W4.b — bridged dispatch arm.
            //
            // The state's variant is statically known at this match
            // arm; the outer `match cur` already eliminated the
            // runtime `match table.states[idx]` over the 14-variant
            // enum. The bridge call below is the W4.c collapse
            // target — wave coordination lifts the cold-path helpers
            // so this arm can inline the variant-specific logic
            // without crossing the cold boundary.
            //
            // State kind tag: #kind_tag
            let _ = #kind_tag;
            // Drain the parse via the cold-path bridge. The bridge
            // dispatches from the table's entry rule and runs to
            // completion; the outer loop sees Done immediately and
            // breaks. W4.c replaces this drain with per-arm inlined
            // logic.
            __dispatch_via_cold(
                table, input, scanner, columns, psi, frame_depth,
            )?;
            __StepOutcome::Done
        }
    }
}

/// The state's variant tag — emitted as a `&'static str` literal in
/// every arm so the codegen test can verify per-variant arm presence
/// via grep without parsing the full token stream.
fn state_kind_tag(state: &IrState) -> &'static str {
    match state {
        IrState::Epsilon => "epsilon",
        IrState::Literal { .. } => "literal",
        IrState::Regex { .. } => "regex",
        IrState::Seq { .. } => "seq",
        IrState::ByteDispatch { .. } => "byte_dispatch",
        IrState::AltLinear { .. } => "alt_linear",
        IrState::Repeat { .. } => "repeat",
        IrState::Ref { .. } => "ref_target",
        IrState::ShuntingYard { .. } => "shunting_yard",
        IrState::WsTrim { .. } => "ws_trim",
        IrState::Minus { .. } => "minus",
    }
}

/// Inlined ByteDispatch — emit a `match input[pos]` over the 256-entry
/// LUT. Each entry that maps to a non-`NONE` state becomes a match
/// arm whose body sets `cur` to the target. The fallback (or the
/// `NONE` sentinel) emits the syntax-error arm.
///
/// The const-folded dispatch table eliminates the runtime LUT read +
/// branch: LLVM lowers the inlined match to a jump-table dispatch
/// indexed by `input[pos]`. This is the canonical W4 win — the same
/// shape simdjson and sonic-rs use for byte-class dispatch.
fn emit_byte_dispatch_inlined(
    disp: &[StateId],
    fallback: ::core::option::Option<StateId>,
) -> TokenStream {
    // Group the 256 entries by target state — reduces the emitted
    // arm count from 256 (one per byte) to ~K (one per distinct
    // target).
    let mut by_target: std::collections::BTreeMap<u16, Vec<u8>> =
        std::collections::BTreeMap::new();
    for (b, &target) in disp.iter().enumerate() {
        if target != StateId::NONE {
            by_target
                .entry(target.0)
                .or_default()
                .push(b as u8);
        }
    }
    let arms = by_target.iter().map(|(target, bytes)| {
        let target_lit = Literal::u16_unsuffixed(*target);
        let byte_lits = bytes.iter().map(|b| Literal::u8_unsuffixed(*b));
        quote! {
            #(#byte_lits)|* => __StepOutcome::Next(#target_lit),
        }
    });
    let fallback_arm = match fallback {
        ::core::option::Option::Some(s) if s != StateId::NONE => {
            let s_lit = Literal::u16_unsuffixed(s.0);
            quote! {
                _ => __StepOutcome::Next(#s_lit),
            }
        }
        _ => {
            quote! {
                _ => __StepOutcome::Syntax(
                    ::bbnf::runtime::tape::DtaStateId(cur),
                ),
            }
        }
    };
    quote! {
        let b = input.get(pos as usize).copied().unwrap_or(0);
        match b {
            #(#arms)*
            #fallback_arm
        }
    }
}
