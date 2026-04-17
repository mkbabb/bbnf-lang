//! AW-IV.W1.β — Per-state DFA-compiled match function emission.
//!
//! # Architectural role
//!
//! Post-AW-III the Regex arm of the hot-path walker called through a
//! `&dyn RegexScanner` trait object whose `scan` method delegated to
//! `parse_that::regex::dfa::Dfa::find_at`. That path accounted for
//! 31.92% self-time on JSON twitter: a runtime DFA interpreter walking
//! `byte_classes` + `flat_transitions` + per-state `is_accept` at every
//! byte, reached through a cross-crate indirect call, with a leaked
//! `&'static Dfa` fished out of a process-global `OnceLock<RwLock<
//! HashMap<usize, &'static Dfa>>>` on first touch.
//!
//! This module lifts that interpreter into codegen. For every
//! `DtaState::Regex { pattern, .. }` and every `DtaState::WsTrim {
//! pattern: Some(_) }` in `DTA_TABLE.states`, the emitter compiles the
//! pattern via `Dfa::compile` at codegen time and emits the resulting
//! state machine as inline Rust: an `fn __dfa_match_<grammar>_<state_idx>
//! (input, pos) -> Option<u32>` whose body is a flat `match state {
//! 0 => match b { <byte_ranges> => state = <next>, ..., _ => break },
//! ...  }` form.
//!
//! The hot-path walker emits direct named calls to these functions; the
//! trait object, HashMap, leaked Boxes, and interpreter loop all vanish
//! from the parse surface. The cold-path replay (AX) reaches the same
//! functions through a per-grammar `__regex_scan_<grammar>` adapter
//! that dispatches on pointer-equality of the interned pattern static.
//!
//! # Emission shape
//!
//! For a pattern whose minimised DFA has N states and K equivalence
//! classes, the emitted function's body is:
//!
//! ```ignore
//! #[inline]
//! fn __dfa_match_<grammar>_<state_idx>(input: &[u8], pos: usize) -> Option<u32> {
//!     let mut state: u32 = 0;
//!     let mut p = pos;
//!     let mut last_match: Option<u32> = if /* states[0].is_accept */ { Some(p as u32) } else { None };
//!     loop {
//!         let b = match input.get(p) { Some(&b) => b, None => break };
//!         match state {
//!             0 => match b {
//!                 <bytes for target A> => state = A,
//!                 <bytes for target B> => state = B,
//!                 _ => break,
//!             },
//!             1 => match b { ... },
//!             ...
//!             _ => unsafe { ::core::hint::unreachable_unchecked() },
//!         }
//!         p += 1;
//!         match state {
//!             <accepting state literals> => last_match = Some(p as u32),
//!             _ => {}
//!         }
//!     }
//!     last_match.map(|end| end - pos as u32)
//! }
//! ```
//!
//! Byte grouping follows the ByteDispatch-arm discipline in
//! `dta_walker/lower_state.rs::emit_byte_dispatch_arm`: bytes sharing a
//! target are coalesced into one `(b1 | b2 | b3)` arm, reducing the
//! per-state arm count from 256 to the distinct-target cardinality. The
//! DFA's `byte_classes` table already partitions the byte space by
//! behavioural equivalence; emission walks it directly.
//!
//! # Adapter
//!
//! `__regex_scan_<grammar>(pattern: &str, input: &[u8], pos: usize) ->
//! Option<u32>` dispatches on pointer-equality of the interned pattern
//! `&'static str` against each regex-bearing state's pattern static:
//! `if ::core::ptr::eq(pattern.as_ptr(), super::__DTA_REGEX_0.as_ptr())
//! { return __dfa_match_<grammar>_<idx>(input, pos); }` chains down
//! through every state. Unknown patterns fall through to `None`.
//!
//! The adapter is the fn pointer the cold-path replay consumes; the
//! hot-path walker arms emit direct named calls and never reach the
//! adapter.

use bbnf_ir::passes::recognizers::dta::{DtaState, DtaTable};
use bbnf_ir::GrammarIR;
use proc_macro2::{Literal, TokenStream};
use quote::{format_ident, quote};

use parse_that::regex::dfa::Dfa;

/// Sanitise a grammar name for use in a Rust identifier — same rule the
/// walker-fn ident sanitiser uses in `dta_walker/mod.rs::walker_fn_ident`.
fn sanitise_grammar(grammar: &str) -> String {
    let mut out = String::with_capacity(grammar.len());
    for ch in grammar.chars() {
        if ch.is_ascii_alphanumeric() || ch == '_' {
            out.push(ch);
        } else {
            out.push('_');
        }
    }
    out
}

/// Compute the per-state DFA match function ident for a grammar +
/// state index.
fn dfa_match_ident(grammar_sanitised: &str, state_idx: usize) -> proc_macro2::Ident {
    format_ident!("__dfa_match_{}_{}", grammar_sanitised, state_idx)
}

/// Public identifier for the per-state DFA match function — exposed
/// for the walker's Regex arm so it can emit a direct named call to the
/// correct function for that state. Keeps the identifier-construction
/// contract in one place.
pub fn dfa_match_fn_ident(grammar: &str, state_idx: usize) -> proc_macro2::Ident {
    let sanitised = sanitise_grammar(grammar);
    dfa_match_ident(&sanitised, state_idx)
}

/// Identifier for the per-grammar `__regex_scan_<grammar>` adapter.
pub fn regex_scan_adapter_ident(grammar: &str) -> proc_macro2::Ident {
    let sanitised = sanitise_grammar(grammar);
    format_ident!("__regex_scan_{}", sanitised)
}

/// Pattern-static identifier for a Regex state at index `idx`. Mirrors
/// the emission in `dta.rs::emit_state_literal`'s Regex arm.
fn dta_regex_static_ident(idx: usize) -> proc_macro2::Ident {
    format_ident!("__DTA_REGEX_{}", idx)
}

/// Pattern-static identifier for a WsTrim state at index `idx`. Mirrors
/// the emission in `dta.rs::emit_state_literal`'s WsTrim arm.
fn dta_ws_static_ident(idx: usize) -> proc_macro2::Ident {
    format_ident!("__DTA_WS_{}", idx)
}

/// Collect every state index in `table.states` whose state carries a
/// regex pattern — Regex + WsTrim-with-pattern. Returns `(state_idx,
/// resolved_pattern_string, pattern_static_ident)` triples.
///
/// Patterns are resolved against `ir`'s string table — the IR-side
/// `DtaState` carries `StringId`, the `dta.rs` emitter already resolves
/// them via `ir.get_string`, and this pass composes with that emission
/// so the static identifiers and the per-state DFA match functions
/// line up by state index.
fn collect_regex_bearing_states<'a>(
    ir: &'a GrammarIR,
    table: &'a DtaTable,
) -> Vec<(usize, &'a str, proc_macro2::Ident)> {
    let mut out = Vec::new();
    for (idx, state) in table.states.iter().enumerate() {
        match state {
            DtaState::Regex { pattern, .. } => {
                let pat = ir.get_string(*pattern);
                out.push((idx, pat, dta_regex_static_ident(idx)));
            }
            DtaState::WsTrim {
                pattern: Some(pattern),
            } => {
                let pat = ir.get_string(*pattern);
                out.push((idx, pat, dta_ws_static_ident(idx)));
            }
            _ => {}
        }
    }
    out
}

/// Emit one per-state DFA match function for `pattern` under the
/// identifier `fn_ident`.
///
/// Compiles `pattern` via `parse_that::regex::dfa::Dfa::compile` and
/// walks the minimised DFA to produce the W1.4-shape inline match. The
/// byte alphabet is compacted per `(source_state, target_state)` pair:
/// bytes sharing a target are coalesced into one `|`-joined arm.
///
/// Panics at codegen time if the pattern fails to compile — the pattern
/// comes from a `DtaState::Regex.pattern` or `DtaState::WsTrim.pattern`
/// that the lifter already validated, so a compile failure here is a
/// breach of the lifter's contract worth surfacing loudly.
fn emit_one_dfa_fn(fn_ident: &proc_macro2::Ident, pattern: &str) -> TokenStream {
    let dfa = Dfa::compile(pattern).unwrap_or_else(|| {
        panic!(
            "AW-IV.W1.β: Dfa::compile failed for pattern {:?}; the lifter's \
             DtaState::Regex/WsTrim must only carry patterns that compile \
             successfully — scanner_plan.rs classification is the guard",
            pattern,
        )
    });

    // ── Per-state dispatch arms ─────────────────────────────────────
    //
    // For each DFA state, collect the bytes that transition to each
    // distinct target state (filtering dead transitions). Emit one
    // `<byte_lits>|... => state = <target>` arm per distinct target,
    // matching the `emit_byte_dispatch_arm` compaction discipline.
    let state_count = dfa.state_count();
    let state_arms = (0..state_count).map(|sid| {
        let sid_lit = Literal::u32_unsuffixed(sid as u32);
        // Map target → bytes (bytes sharing a DFA equivalence class and
        // target collapse into one arm). `BTreeMap` keeps output
        // deterministic — important for codegen reproducibility.
        let mut by_target: std::collections::BTreeMap<u32, Vec<u8>> =
            std::collections::BTreeMap::new();
        for b in 0u16..=255 {
            let b = b as u8;
            let cls = dfa.byte_classes[b as usize] as usize;
            let tgt = dfa.states[sid].transitions[cls];
            if tgt == parse_that::regex::nfa::DEAD {
                continue;
            }
            by_target.entry(tgt).or_default().push(b);
        }
        let byte_arms = by_target.iter().map(|(target, bytes)| {
            let target_lit = Literal::u32_unsuffixed(*target);
            let byte_lits = bytes.iter().map(|b| Literal::u8_unsuffixed(*b));
            quote! {
                #(#byte_lits)|* => state = #target_lit,
            }
        });
        quote! {
            #sid_lit => match b {
                #(#byte_arms)*
                _ => break,
            },
        }
    });

    // ── Accept-state check ─────────────────────────────────────────
    //
    // After a byte is consumed, the new state is checked against the
    // set of accepting states. Greedy semantics: `last_match` is
    // updated on every accept, so the longest match wins — matching
    // `Dfa::find_at`.
    let accept_lits: Vec<Literal> = dfa
        .states
        .iter()
        .enumerate()
        .filter(|(_, s)| s.is_accept)
        .map(|(i, _)| Literal::u32_unsuffixed(i as u32))
        .collect();
    let accept_check = if accept_lits.is_empty() {
        // Pattern compiles to a DFA that accepts nothing — the walker
        // contract says `find_at` returns None in this case; preserving
        // that behaviour means never updating `last_match`.
        quote! {}
    } else {
        quote! {
            match state {
                #(#accept_lits)|* => last_match = ::core::option::Option::Some(p as u32),
                _ => {}
            }
        }
    };

    // ── Start-state accept bootstrap ────────────────────────────────
    //
    // If state 0 is accepting, the empty match at `pos` is a valid
    // (shortest) match; `last_match` begins populated. This matches
    // `Dfa::find_at`'s initial `last_accept = if states[0].is_accept
    // { Some(pos) } else { None }`.
    let start_accept = if dfa.states[0].is_accept {
        quote! { ::core::option::Option::Some(pos as u32) }
    } else {
        quote! { ::core::option::Option::None }
    };

    quote! {
        #[inline]
        fn #fn_ident(input: &[u8], pos: usize) -> ::core::option::Option<u32> {
            let mut state: u32 = 0;
            let mut p = pos;
            let mut last_match: ::core::option::Option<u32> = #start_accept;
            loop {
                let b = match input.get(p) {
                    ::core::option::Option::Some(&b) => b,
                    ::core::option::Option::None => break,
                };
                match state {
                    #(#state_arms)*
                    _ => unsafe { ::core::hint::unreachable_unchecked() },
                }
                p += 1;
                #accept_check
            }
            last_match.map(|end| end - pos as u32)
        }
    }
}

/// Emit one per-state DFA match function per regex-bearing state in
/// `table.states`. Every `DtaState::Regex { pattern, .. }` and every
/// `DtaState::WsTrim { pattern: Some(_) }` contributes one function;
/// other states contribute nothing.
///
/// The emitted functions live at module scope alongside the walker's
/// `__dta_walker_inline` module; the walker arms call them directly by
/// name at the call site, bypassing any indirection.
pub fn emit_dfa_match_fns(grammar: &str, ir: &GrammarIR, table: &DtaTable) -> TokenStream {
    let sanitised = sanitise_grammar(grammar);
    let states = collect_regex_bearing_states(ir, table);
    let fns = states.into_iter().map(|(idx, pattern, _pat_ident)| {
        let fn_ident = dfa_match_ident(&sanitised, idx);
        emit_one_dfa_fn(&fn_ident, pattern)
    });
    quote! {
        #(#fns)*
    }
}

/// Emit the per-grammar regex-scan adapter used by the cold-path replay
/// (AX) and any call site that dispatches by pattern string.
///
/// The adapter walks every regex-bearing state in `table.states` and
/// emits a pointer-equality check against the interned pattern
/// `&'static str`. Patterns are interned per-state by the `dta.rs`
/// emitter (`static __DTA_REGEX_K: &str = "..."` / `static __DTA_WS_K:
/// &str = "..."`), so pointer-equality is a safe and O(1) dispatch
/// key — two distinct states never share the same pattern pointer
/// unless the lifter has already deduplicated them, and in that case
/// the dispatch is still correct.
///
/// Unknown patterns fall through to `None`. The hot-path walker arms
/// emit direct named DFA function calls and never reach the adapter;
/// the adapter is for cold-path replay + any call site that consumes
/// a fn-pointer under the signature `fn(&str, &[u8], usize) ->
/// Option<u32>`.
pub fn emit_regex_scan_adapter(
    grammar: &str,
    ir: &GrammarIR,
    table: &DtaTable,
) -> TokenStream {
    let sanitised = sanitise_grammar(grammar);
    let adapter_ident = regex_scan_adapter_ident(grammar);
    let states = collect_regex_bearing_states(ir, table);

    let dispatch_arms = states.into_iter().map(|(idx, _pattern, pat_ident)| {
        let fn_ident = dfa_match_ident(&sanitised, idx);
        quote! {
            if ::core::ptr::eq(pattern.as_ptr(), #pat_ident.as_ptr()) {
                return #fn_ident(input, pos);
            }
        }
    });

    quote! {
        #[inline]
        fn #adapter_ident(
            pattern: &str,
            input: &[u8],
            pos: usize,
        ) -> ::core::option::Option<u32> {
            #(#dispatch_arms)*
            ::core::option::Option::None
        }
    }
}
