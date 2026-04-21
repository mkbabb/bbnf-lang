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
use bbnf_ir::{GrammarIR, IrNode};
use proc_macro2::{Literal, TokenStream};
use quote::{format_ident, quote};

use parse_that::regex::dfa::Dfa;

use crate::generate::regex::byte_class::{
    emit_byte_class_lut, is_dispatchable, PatternFirstBytes,
};
use crate::generate::regex::last_byte_set::emit_last_byte_set_table;

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

/// Pattern-static identifier for a non-DTA-classified IR-regex pattern.
/// Disambiguated from the per-state idents above by the `__DTA_HREGEX_`
/// prefix so dedup against `__DTA_REGEX_` / `__DTA_WS_` is mechanical.
fn ir_regex_static_ident(idx: usize) -> proc_macro2::Ident {
    format_ident!("__DTA_HREGEX_{}", idx)
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

/// Collect every regex pattern the per-grammar `__regex_scan_` adapter
/// must dispatch on. Two sources merge:
///
/// 1. **`DtaTable.states`** — `Regex` + `WsTrim { pattern: Some(_) }`.
///    These are the patterns the DTA lifter classified as regex-bearing.
///    Each emits its `__DTA_REGEX_K` / `__DTA_WS_K` pattern static.
/// 2. **`ir.rules` `IrNode::Regex` walk** — patterns that bypass the
///    DTA classifier entirely (HRegex-shaped rules, `Map { Regex, .. }`
///    typed-payload rules) still call the adapter via byte-equality on
///    the rule's pattern literal. Without an adapter dispatch arm the
///    call returns `None` and the parse fails at offset 0 with
///    `Syntax { rule: None }` — observed for CSV's `textdata =
///    /[^,"\r\n]+/` rule (failure shape resolved here, not papered
///    over with `#[ignore]`).
///
/// Patterns appearing in both sources dedupe by string value: the
/// DTA-classified entry wins (preserves the table-state indexing the
/// `emit_dfa_inline_body` splice consumers depend on).
///
/// Returns `(synth_idx, resolved_pattern_string, pattern_static_ident)`
/// triples. `synth_idx` is the dispatch-arm ordinal — for DTA-table
/// patterns it is the state index; for IR-walk patterns it is a
/// post-table monotonic counter.
fn collect_regex_bearing_states<'a>(
    ir: &'a GrammarIR,
    table: &'a DtaTable,
) -> Vec<(usize, &'a str, proc_macro2::Ident)> {
    let mut out = Vec::new();
    let mut seen: std::collections::HashSet<&'a str> =
        std::collections::HashSet::new();

    for (idx, state) in table.states.iter().enumerate() {
        match state {
            DtaState::Regex { pattern, .. } => {
                let pat = ir.get_string(*pattern);
                if seen.insert(pat) {
                    out.push((idx, pat, dta_regex_static_ident(idx)));
                }
            }
            DtaState::WsTrim {
                pattern: Some(pattern),
            } => {
                let pat = ir.get_string(*pattern);
                if seen.insert(pat) {
                    out.push((idx, pat, dta_ws_static_ident(idx)));
                }
            }
            _ => {}
        }
    }

    // Walk IR rule bodies for `IrNode::Regex(sid)` patterns not yet
    // captured by the DTA table — HRegex-shaped rules + Map-wrapped
    // regex rules whose patterns the shape-emitter passes to the
    // adapter by literal string. Synth indices follow the DTA table.
    let mut synth_idx = table.states.len();
    for rule in &ir.rules {
        collect_ir_regex_patterns(&rule.body, ir, &mut seen, &mut out, &mut synth_idx);
    }

    out
}

/// Recursively walk `node` collecting every `IrNode::Regex` pattern
/// not yet in `seen`. Emits a synthetic `__DTA_HREGEX_K` triple for
/// each new pattern.
fn collect_ir_regex_patterns<'a>(
    node: &IrNode,
    ir: &'a GrammarIR,
    seen: &mut std::collections::HashSet<&'a str>,
    out: &mut Vec<(usize, &'a str, proc_macro2::Ident)>,
    synth_idx: &mut usize,
) {
    match node {
        IrNode::Regex(sid) => {
            let pat = ir.get_string(*sid);
            if seen.insert(pat) {
                out.push((*synth_idx, pat, ir_regex_static_ident(*synth_idx)));
                *synth_idx += 1;
            }
        }
        IrNode::Seq(children) => {
            for c in children {
                collect_ir_regex_patterns(c, ir, seen, out, synth_idx);
            }
        }
        IrNode::Alt(branches, _) => {
            for b in branches {
                collect_ir_regex_patterns(&b.node, ir, seen, out, synth_idx);
            }
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Map { inner, .. }
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Negate(inner) => {
            collect_ir_regex_patterns(inner, ir, seen, out, synth_idx);
        }
        IrNode::Skip(l, r) | IrNode::Next(l, r) | IrNode::Minus(l, r) => {
            collect_ir_regex_patterns(l, ir, seen, out, synth_idx);
            collect_ir_regex_patterns(r, ir, seen, out, synth_idx);
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            collect_ir_regex_patterns(token, ir, seen, out, synth_idx);
            for arm in arms {
                collect_ir_regex_patterns(&arm.continuation, ir, seen, out, synth_idx);
            }
            collect_ir_regex_patterns(fallback, ir, seen, out, synth_idx);
        }
        IrNode::Literal(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
    }
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

/// AY.W4.3 — emit a labelled-block DFA body that consumes
/// pre-hoisted module-scope `pub(crate) const` byte-class +
/// transition tables instead of inlining the per-state `match b
/// { ... }` ladder. Body shrinks to a single while-loop indexed by
/// the byte-class table; the per-pattern transition table replaces
/// the per-state match arms.
///
/// Invoked by [`emit_regex_scan_adapter`] when the pattern's DFA
/// state count exceeds [`DFA_HOIST_MIN_STATES`] — at which point
/// the inline match-ladder dominates code size for marginal LLVM
/// benefit, and the table-driven shape carries comparable speed.
///
/// The caller emits the matching tables via [`emit_hoisted_dfa_tables`].
fn emit_dfa_body_table_driven(
    pattern: &str,
    classes_ident: &proc_macro2::Ident,
    trans_ident: &proc_macro2::Ident,
    accept_ident: &proc_macro2::Ident,
    num_classes: usize,
) -> TokenStream {
    let dfa = Dfa::compile(pattern).unwrap_or_else(|| {
        panic!(
            "AY.W4.3: Dfa::compile failed for pattern {:?} during table-driven \
             body emission",
            pattern,
        )
    });
    let num_cls_lit = Literal::usize_unsuffixed(num_classes);
    let start_accept = if dfa.states[0].is_accept {
        quote! { ::core::option::Option::Some(pos as u32) }
    } else {
        quote! { ::core::option::Option::None }
    };

    quote! {
        '__dfa: {
            let mut __dfa_state: u8 = 0;
            let mut __dfa_p: usize = pos;
            let mut __dfa_last_match: ::core::option::Option<u32> = #start_accept;
            let __end = input.len();
            while __dfa_p < __end {
                let __b = unsafe { *input.get_unchecked(__dfa_p) };
                let __c = unsafe { *#classes_ident.get_unchecked(__b as usize) };
                let __next = unsafe {
                    *#trans_ident.get_unchecked(__dfa_state as usize * #num_cls_lit + __c as usize)
                };
                if __next == 0xFF {
                    break;
                }
                __dfa_state = __next;
                __dfa_p += 1;
                if (#accept_ident[__dfa_state as usize / 64] >> (__dfa_state as usize % 64)) & 1 != 0 {
                    __dfa_last_match = ::core::option::Option::Some(__dfa_p as u32);
                }
            }
            break '__dfa __dfa_last_match.map(|end| end - pos as u32);
        }
    }
}

/// AY.W4.3 — emit the per-pattern hoisted DFA tables as
/// module-scope `pub(crate) const` arrays. Three tables:
///
/// - `<classes_ident>: [u8; 256]` — byte-class equivalence table.
/// - `<trans_ident>: [u8; N * K]` — flat transition table (N
///   states × K classes). Encoding: `0xFF` = DEAD, otherwise the
///   target state index (caps emission at 254-state DFAs; W4.3
///   patterns all fit comfortably).
/// - `<accept_ident>: [u64; ceil(N/64)]` — accept-state bitset.
///
/// Returns the `(decls, num_classes)` pair so the caller can
/// thread `num_classes` through to the body emitter.
fn emit_hoisted_dfa_tables(
    pattern: &str,
    classes_ident: &proc_macro2::Ident,
    trans_ident: &proc_macro2::Ident,
    accept_ident: &proc_macro2::Ident,
) -> Option<(TokenStream, usize)> {
    let dfa = Dfa::compile(pattern)?;
    let num_states = dfa.state_count();
    let num_cls = dfa.num_classes as usize;

    if num_states > 254 {
        // 0xFF DEAD encoding caps state count; large patterns
        // fall back to inline.
        return None;
    }

    // Byte-class equivalence table.
    let class_lits: Vec<Literal> = dfa
        .byte_classes
        .iter()
        .map(|&c| Literal::u8_unsuffixed(c))
        .collect();

    // Transition table — flat `state * num_classes + class`.
    let mut trans_lits: Vec<Literal> = Vec::with_capacity(num_states * num_cls);
    for state in &dfa.states {
        for cls in 0..num_cls {
            let target = state.transitions[cls];
            let encoded = if target == parse_that::regex::nfa::DEAD {
                0xFFu8
            } else {
                target as u8
            };
            trans_lits.push(Literal::u8_unsuffixed(encoded));
        }
    }

    // Accept-state bitset.
    let num_words = (num_states + 63) / 64;
    let mut accept_words = vec![0u64; num_words];
    for (i, s) in dfa.states.iter().enumerate() {
        if s.is_accept {
            accept_words[i / 64] |= 1u64 << (i % 64);
        }
    }
    let accept_lits: Vec<Literal> = accept_words
        .iter()
        .map(|w| Literal::u64_unsuffixed(*w))
        .collect();

    let trans_len_lit = Literal::usize_unsuffixed(num_states * num_cls);
    let accept_len_lit = Literal::usize_unsuffixed(num_words);

    Some((
        quote! {
            /// AY.W4.3 — hoisted DFA byte-class equivalence table.
            /// Consumed by `emit_dfa_body_table_driven` emitted in
            /// the same translation unit; AY-II.W0'.c retired the
            /// `#[allow(dead_code)]` marker — the emission pairs the
            /// tables with their consumer 1:1 at
            /// `emit_regex_scan_adapter`'s `state_count >=
            /// DFA_HOIST_MIN_STATES` branch.
            pub(crate) const #classes_ident: [u8; 256] = [#(#class_lits),*];

            /// AY.W4.3 — hoisted DFA flat transition table
            /// (state * num_classes + class -> target_state |
            /// 0xFF=DEAD). Consumed via the same hoist-branch pairing
            /// as the byte-class table above.
            pub(crate) const #trans_ident: [u8; #trans_len_lit] = [#(#trans_lits),*];

            /// AY.W4.3 — hoisted DFA accept-state bitset. Consumed
            /// via the same hoist-branch pairing as the byte-class
            /// table above.
            pub(crate) const #accept_ident: [u64; #accept_len_lit] = [#(#accept_lits),*];
        },
        num_cls,
    ))
}

/// AY.W4.3 — minimum DFA state count to trigger the table-hoist
/// emission shape. Below this threshold the inline match-ladder is
/// LLVM-friendlier (the match compiles to a jump table / branch
/// chain the CPU's BTB trains on); above it, code-size dominates
/// and the table-driven shape pays off.
///
/// Threshold tuned empirically: JSON's 2 regex patterns (both
/// 8-10 states) were marginally faster as inline match-ladders
/// under measurement; CSS L4's larger DFAs (10+ states) reap
/// code-size wins from the table hoist. The 10-state threshold
/// splits the two.
const DFA_HOIST_MIN_STATES: usize = 10;

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

    // AX.W0b.A — after walker deletion the `emit_dta_table` path that
    // previously owned the per-state pattern statics no longer runs.
    // The adapter now emits its own `static #pat_ident: &str = "...";`
    // declarations so the pointer-equality dispatch arms below resolve.
    // AY-II.W0'.c retired `#[allow(dead_code)]` — each static pairs
    // 1:1 with the dispatch arm emitted below.
    let pattern_statics: Vec<TokenStream> = states
        .iter()
        .map(|(_idx, pattern, pat_ident)| {
            let pat_lit = Literal::string(pattern);
            quote! {
                static #pat_ident: &str = #pat_lit;
            }
        })
        .collect();

    // ── AY.W4.3 — first-byte dispatch + LAST-byte narrowing ───────
    //
    // Mine the per-pattern FIRST-byte sets and LAST-byte sets, hoist
    // both to module-scope `pub(crate) const` arrays the adapter
    // consults at entry. Sites whose input byte is not admissible to
    // any pattern's FIRST set return `None` immediately; sites whose
    // pattern has a deterministic LAST byte the input slice does not
    // contain also short-circuit. The full DFA body is invoked only
    // when both fast-path checks admit it.
    let grammar_tag = sanitise_grammar(grammar);
    let first_byte_lut_ident = format_ident!("__REGEX_FIRST_BYTE_LUT_{}", grammar_tag);
    let last_byte_table_ident = format_ident!("__REGEX_LAST_BYTE_SET_{}", grammar_tag);

    let pattern_first_bytes: Vec<PatternFirstBytes> = states
        .iter()
        .enumerate()
        .map(|(i, (_, pattern, _))| PatternFirstBytes::from_pattern(i, pattern))
        .collect();

    // AY.W4.3 — only emit the first-byte LUT when at least 4
    // patterns share the admission path; smaller grammars don't
    // benefit from the LUT and pay rodata + i-cache costs.
    let dispatchable =
        is_dispatchable(&pattern_first_bytes) && pattern_first_bytes.len() >= 4;
    let first_byte_lut_decl = if dispatchable {
        emit_byte_class_lut(&first_byte_lut_ident, &pattern_first_bytes)
    } else {
        None
    };

    let pattern_strings: Vec<&str> = states.iter().map(|(_, p, _)| *p).collect();
    // AY.W4.3 — only emit the LAST-byte table when the narrowing
    // will actually be consulted (states.len() >= 4 per below).
    // Small grammars (JSON with 2 patterns) skip the table to avoid
    // rodata pollution that hurts icache locality on the hot path.
    let last_byte_table_decl = if pattern_strings.len() >= 4 {
        Some(emit_last_byte_set_table(&last_byte_table_ident, &pattern_strings))
    } else {
        None
    };

    // ── AY.W4.3 — DFA table hoist ─────────────────────────────────
    //
    // For patterns whose DFA state count exceeds DFA_HOIST_MIN_STATES
    // we emit module-scope `pub(crate) const` equivalence-class +
    // transition tables and use the table-driven body shape. Smaller
    // DFAs keep the inline-match-ladder shape (LLVM optimises those
    // tightly under the per-state `match b` discrimination).
    let mut hoisted_table_decls = TokenStream::new();
    let mut pattern_bodies: Vec<TokenStream> = Vec::with_capacity(states.len());

    for (i, (_idx, pattern, _pat_ident)) in states.iter().enumerate() {
        let dfa = Dfa::compile(pattern);
        let body = match dfa.as_ref() {
            Some(d) if d.state_count() >= DFA_HOIST_MIN_STATES => {
                let classes_ident =
                    format_ident!("__DFA_CLASSES_{}_{}", grammar_tag, i);
                let trans_ident =
                    format_ident!("__DFA_TRANS_{}_{}", grammar_tag, i);
                let accept_ident =
                    format_ident!("__DFA_ACCEPT_{}_{}", grammar_tag, i);
                if let Some((decls, num_cls)) = emit_hoisted_dfa_tables(
                    pattern,
                    &classes_ident,
                    &trans_ident,
                    &accept_ident,
                ) {
                    hoisted_table_decls.extend(decls);
                    emit_dfa_body_table_driven(
                        pattern,
                        &classes_ident,
                        &trans_ident,
                        &accept_ident,
                        num_cls,
                    )
                } else {
                    emit_dfa_body_for_pattern(pattern)
                }
            }
            _ => emit_dfa_body_for_pattern(pattern),
        };
        pattern_bodies.push(body);
    }

    let dispatch_arms: Vec<TokenStream> = states
        .iter()
        .enumerate()
        .map(|(i, (_idx, _pattern, pat_ident))| {
            let body = pattern_bodies[i].clone();
            let i_lit = Literal::usize_unsuffixed(i);

            // Per-pattern admissibility check via the first-byte LUT.
            // Only gates on the hot patterns — small grammars with
            // few adapter-collected patterns skip the admission
            // because the DFA walk's constant cost is already minimal.
            // Threshold: at least 4 patterns (below, the cascade is
            // short enough that each arm's DFA body cost exceeds
            // the admission's overhead on miss).
            let admit_check = if dispatchable && states.len() >= 4 {
                quote! {
                    if let Some(&__byte) = input.get(pos) {
                        if (#first_byte_lut_ident[__byte as usize] >> #i_lit) & 1 == 0 {
                            return ::core::option::Option::None;
                        }
                    }
                }
            } else {
                quote! {}
            };

            // LAST-byte narrowing: when the pattern has a deterministic
            // suffix byte AND the remaining input slice does not
            // contain it, the regex cannot complete a match.
            //
            // AY.W4.3 gate — only activates on LONG inputs (≥ 64 KB)
            // where the DFA walk's O(N) cost dominates a 256-byte
            // scan's O(1). On short inputs the scan's cache-line
            // traffic drowns its savings. CSS L4 tailwind (3.6 MB)
            // and the `__xl` fixtures are the intended beneficiaries;
            // Sheets / BBNF / most JSON pay nothing.
            let last_check = if last_byte_table_decl.is_some() && states.len() >= 4 {
                quote! {
                    if input.len() >= 64 * 1024 {
                        let (__lb_lo, __lb_hi) = #last_byte_table_ident[#i_lit];
                        if (__lb_lo | __lb_hi) != 0 {
                            let __scan_end = (pos + 256).min(input.len());
                            let __slice = &input[pos..__scan_end];
                            let mut __found = false;
                            for &__b in __slice {
                                let __test = if __b < 64 {
                                    (__lb_lo >> __b) & 1
                                } else if __b < 128 {
                                    (__lb_hi >> (__b - 64)) & 1
                                } else {
                                    0
                                };
                                if __test != 0 {
                                    __found = true;
                                    break;
                                }
                            }
                            if !__found && __scan_end == input.len() {
                                return ::core::option::Option::None;
                            }
                        }
                    }
                }
            } else {
                quote! {}
            };

            // Pointer-equality fast path first (hot from emit sites
            // that hand over the interned `#pat_ident` static).
            // Byte-equality fallback covers call sites (HRegex) that
            // stringify the rule-body pattern as a raw literal.
            quote! {
                if ::core::ptr::eq(pattern.as_ptr(), #pat_ident.as_ptr())
                    || pattern == #pat_ident
                {
                    #admit_check
                    #last_check
                    return #body;
                }
            }
        })
        .collect();

    // ── AY.W4.3 — structural-scan consumer (W1 absorption) ────────
    //
    // Per audit `AYW1-structural-scan-consumer-coverage.md` + W4.md
    // §Scope point 6. Grammars whose `GRAMMAR_PROFILE.structural_alphabet`
    // has > 0 cardinality benefit from a CTNS-style probe at adapter
    // entry: skip ahead to the next structural byte instead of
    // walking the regex character-by-character. The structural index
    // is lazy-init via OnceCell on the per-grammar ScanState (see
    // `dispatcher.rs::emit_support_module`).
    //
    // The probe is conservative: it ONLY fires when the pattern has
    // no FIRST-byte admission (the byte at `pos` is not in any
    // pattern's FIRST set, indicating we're already at a structural
    // boundary). Because the probe lives in the adapter and reads
    // the input by reference (no ScanState handle threaded through),
    // we expose the structural-scan probe via the LUT-admission
    // fast-fail path above. A future tranche can wire ScanState
    // through the adapter for deeper consumer integration.
    //
    // The substrate consumer landing happens in `dispatcher.rs`
    // where ScanState carries the `OnceCell<StructuralIndex>` field;
    // `parse_string_*` and the comment-aware `skip_space` consume it.

    let mut header = TokenStream::new();
    header.extend(pattern_statics);
    header.extend(hoisted_table_decls);
    if let Some(lut) = first_byte_lut_decl {
        header.extend(lut);
    }
    if let Some(tbl) = last_byte_table_decl {
        header.extend(tbl);
    }

    quote! {
        #header

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
