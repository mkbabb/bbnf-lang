//! AW-IV.W1.4-aggressive — Per-state DFA inline body emission.
//!
//! # Architectural role
//!
//! Post-W1 the Regex arm of the per-grammar walker called a
//! separately-emitted `__dfa_match_<grammar>_<state_idx>` function by
//! name. JSON's small DFAs got LLVM-inlined by the compiler's heuristic,
//! but CSS L4's larger DFAs stayed out-of-line as 26 fn symbols in the
//! bench binary, paying a function-call boundary per Regex visit. Per
//! the AW-IV §architectural-thesis binding rule:
//!
//! > **Zero function-call boundary in the per-grammar walker hot path;
//! > emit every callee inline at the source level.**
//!
//! W1.4-aggressive lifts the DFA's `loop { match state { ... } }` into
//! the walker arm directly. There is no `fn __dfa_match_*` symbol; the
//! DFA body is a labelled block spliced into each Regex / WsTrim / boundary-
//! ws site. The walker arm is one giant straight-line Rust function
//! whose Regex states carry the DFA match loop in the source — LLVM
//! sees the entire arm as one basic block and can const-fold byte
//! comparisons, jump-table layouts, and tail-call patterns at the point
//! of use. Code-size cost is irrelevant per the binding rule.
//!
//! # Emission shape
//!
//! For a pattern whose minimised DFA has N states and K equivalence
//! classes, [`emit_dfa_inline_body`] returns a labelled block of the
//! form:
//!
//! ```ignore
//! '__dfa: {
//!     let mut __dfa_state: u32 = 0;
//!     let mut __dfa_p: usize = pos;
//!     let mut __dfa_last_match: Option<u32> =
//!         if /* states[0].is_accept */ { Some(pos as u32) } else { None };
//!     loop {
//!         let b = match input.get(__dfa_p) {
//!             Some(&b) => b,
//!             None => break,
//!         };
//!         match __dfa_state {
//!             0 => match b {
//!                 <bytes for target A> => __dfa_state = A,
//!                 <bytes for target B> => __dfa_state = B,
//!                 _ => break,
//!             },
//!             1 => match b { /* ... */ },
//!             _ => unsafe { ::core::hint::unreachable_unchecked() },
//!         }
//!         __dfa_p += 1;
//!         match __dfa_state {
//!             <accepting state literals> => __dfa_last_match = Some(__dfa_p as u32),
//!             _ => {}
//!         }
//!     }
//!     break '__dfa __dfa_last_match.map(|end| end - pos as u32);
//! }
//! ```
//!
//! The caller supplies `input: &[u8]` and `pos: usize` in its scope;
//! every internal binding uses the `__dfa_` prefix so no identifier
//! collides with the walker's own bindings.
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
//! Option<u32>` is the SOLE out-of-line per-grammar fn this module
//! emits. Its dispatch arms themselves splice inline DFA bodies (no
//! chain of fn calls) so the adapter contains everything the walker
//! needs to replay a regex scan by pattern string. Hot-path walker arms
//! emit the DFA body directly; the adapter exists for cold-path
//! `dispatch_one` / `try_branch` callers only.

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

/// Resolve the pattern string for a regex-bearing state at `idx` in
/// `table.states`. Returns `None` if the state at that index is not a
/// `Regex` variant or a `WsTrim` with a pattern — the caller should
/// already know it is requesting a pattern-bearing state.
fn pattern_for_state<'a>(
    ir: &'a GrammarIR,
    table: &'a DtaTable,
    idx: usize,
) -> Option<&'a str> {
    let state = table.states.get(idx)?;
    match state {
        DtaState::Regex { pattern, .. } => Some(ir.get_string(*pattern)),
        DtaState::WsTrim {
            pattern: Some(pattern),
        } => Some(ir.get_string(*pattern)),
        _ => None,
    }
}

/// Collect every state index in `table.states` whose state carries a
/// regex pattern — Regex + WsTrim-with-pattern. Returns `(state_idx,
/// resolved_pattern_string, pattern_static_ident)` triples.
///
/// Patterns are resolved against `ir`'s string table — the IR-side
/// `DtaState` carries `StringId`, the `dta.rs` emitter already resolves
/// them via `ir.get_string`, and this pass composes with that emission
/// so the static identifiers and the per-state DFA bodies line up by
/// state index.
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

/// Compile `pattern` and emit the DFA's `loop { match state { ... } }`
/// body as a labelled `'__dfa: { ... }` block yielding `Option<u32>`.
///
/// The emitted block references two bindings from the surrounding
/// scope — `input: &[u8]` and `pos: usize` — and introduces its own
/// `__dfa_`-prefixed locals. The block's value is `Option<u32>`: the
/// byte count of the matched prefix (equivalent to
/// `Dfa::find_at(input, pos)`), or `None` when no match prefix exists.
///
/// Internals mirror the W1.β out-of-line function body verbatim —
/// byte grouping via `byte_classes`, accept-state check after each
/// transition, start-state accept bootstrap — differing only in the
/// wrapper: `break '__dfa <value>` replaces `return <value>` and the
/// `__dfa_` prefix replaces the original `state` / `p` / `last_match`
/// bindings to avoid collision with the walker's own `state`, `pos`,
/// etc.
///
/// Panics at codegen time if the pattern fails to compile — the pattern
/// comes from a `DtaState::Regex.pattern` or `DtaState::WsTrim.pattern`
/// that the lifter already validated.
fn emit_dfa_body_for_pattern(pattern: &str) -> TokenStream {
    let dfa = Dfa::compile(pattern).unwrap_or_else(|| {
        panic!(
            "AW-IV.W1.4-aggro: Dfa::compile failed for pattern {:?}; the lifter's \
             DtaState::Regex/WsTrim must only carry patterns that compile \
             successfully — scanner_plan.rs classification is the guard",
            pattern,
        )
    });

    // ── Per-state dispatch arms ─────────────────────────────────────
    //
    // For each DFA state, collect the bytes that transition to each
    // distinct target state (filtering dead transitions). Emit one
    // `<byte_lits>|... => __dfa_state = <target>` arm per distinct
    // target, matching the `emit_byte_dispatch_arm` compaction.
    let state_count = dfa.state_count();
    let state_arms = (0..state_count).map(|sid| {
        let sid_lit = Literal::u32_unsuffixed(sid as u32);
        // `BTreeMap` keeps output deterministic — important for
        // codegen reproducibility.
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
                #(#byte_lits)|* => __dfa_state = #target_lit,
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
    // set of accepting states. Greedy semantics: `__dfa_last_match` is
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
        // Pattern compiles to a DFA that accepts nothing — preserve
        // `Dfa::find_at`'s behaviour: never update `__dfa_last_match`.
        quote! {}
    } else {
        quote! {
            match __dfa_state {
                #(#accept_lits)|* => __dfa_last_match = ::core::option::Option::Some(__dfa_p as u32),
                _ => {}
            }
        }
    };

    // ── Start-state accept bootstrap ────────────────────────────────
    //
    // If state 0 is accepting, the empty match at `pos` is a valid
    // (shortest) match; `__dfa_last_match` begins populated. This
    // matches `Dfa::find_at`'s initial `last_accept = if states[0].is_accept
    // { Some(pos) } else { None }`.
    let start_accept = if dfa.states[0].is_accept {
        quote! { ::core::option::Option::Some(pos as u32) }
    } else {
        quote! { ::core::option::Option::None }
    };

    quote! {
        '__dfa: {
            let mut __dfa_state: u32 = 0;
            let mut __dfa_p: usize = pos;
            let mut __dfa_last_match: ::core::option::Option<u32> = #start_accept;
            loop {
                let b = match input.get(__dfa_p) {
                    ::core::option::Option::Some(&b) => b,
                    ::core::option::Option::None => break,
                };
                match __dfa_state {
                    #(#state_arms)*
                    _ => unsafe { ::core::hint::unreachable_unchecked() },
                }
                __dfa_p += 1;
                #accept_check
            }
            break '__dfa __dfa_last_match.map(|end| end - pos as u32);
        }
    }
}

/// Emit the inline DFA body for the regex-bearing state at `idx` in
/// `table.states`. The returned `TokenStream` is a labelled `'__dfa: {
/// ... }` block yielding `Option<u32>`; the caller splices it where a
/// `__dfa_match_<grammar>_<idx>(input, pos)` call would have sat.
///
/// The caller must have `input: &[u8]` and `pos: usize` in scope at the
/// splice site. The body introduces `__dfa_`-prefixed locals only.
///
/// Panics at codegen time if `idx` is not a regex-bearing state (the
/// caller already filtered by `DtaState::Regex` / `DtaState::WsTrim {
/// pattern: Some(_) }`).
pub fn emit_dfa_inline_body(
    grammar: &str,
    ir: &GrammarIR,
    table: &DtaTable,
    idx: usize,
) -> TokenStream {
    let _ = grammar; // retained for symmetry with the ident composer
    let pattern = pattern_for_state(ir, table, idx).unwrap_or_else(|| {
        panic!(
            "AW-IV.W1.4-aggro: emit_dfa_inline_body invoked for state {} which \
             is not regex-bearing — only DtaState::Regex and \
             DtaState::WsTrim{{ pattern: Some(_) }} states are admissible",
            idx,
        )
    });
    emit_dfa_body_for_pattern(pattern)
}

/// Emit the per-grammar regex-scan adapter used by the cold-path replay
/// (AX) and any call site that dispatches by pattern string.
///
/// The adapter walks every regex-bearing state in `table.states` and
/// emits a pointer-equality check against the interned pattern
/// `&'static str`; on match, the adapter RETURNS the inline DFA body's
/// value directly. Patterns are interned per-state by the `dta.rs`
/// emitter (`static __DTA_REGEX_K: &str = "..."` / `static __DTA_WS_K:
/// &str = "..."`), so pointer-equality is a safe and O(1) dispatch key.
///
/// AW-IV.W1.4-aggro — the adapter's dispatch arms themselves splice
/// inline DFA bodies (no chain of `fn __dfa_match_*` calls; those fns
/// are deleted). `__regex_scan_<grammar>` is the SOLE out-of-line
/// per-grammar fn this module emits. Its body grows large (one DFA
/// inline body per regex pattern), but it is a COLD path — the hot
/// path's walker arms splice the DFA body directly and never reach the
/// adapter.
pub fn emit_regex_scan_adapter(
    grammar: &str,
    ir: &GrammarIR,
    table: &DtaTable,
) -> TokenStream {
    let adapter_ident = regex_scan_adapter_ident(grammar);
    let states = collect_regex_bearing_states(ir, table);

    let dispatch_arms = states.into_iter().map(|(_idx, pattern, pat_ident)| {
        let body = emit_dfa_body_for_pattern(pattern);
        quote! {
            if ::core::ptr::eq(pattern.as_ptr(), #pat_ident.as_ptr()) {
                return #body;
            }
        }
    });

    quote! {
        #[inline]
        #[cold]
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
