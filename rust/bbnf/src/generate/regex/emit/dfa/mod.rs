//! DFA -> inline TokenStream code emission.
//!
//! Converts a compiled `parse_that::regex::Dfa` into inline Rust code
//! that performs anchored regex matching via direct byte operations.
//!
//! Three emission tiers based on DFA size:
//! - **Tier A** (<=12 states): Inline match-chain state machine. LLVM sees
//!   every transition and can optimize aggressively.
//! - **Tier B** (13--64 states): Static transition table + driver loop.
//!   Compact, cache-friendly, with SIMD acceleration for self-loop states.
//! - **Tier C** (65+ states): Fallback to Tier B (PackedDfa runtime is future).
//!
//! All tiers emit code that evaluates to `Option<Span<'a>>`, reading from
//! `state.src_bytes` and advancing `state.offset`.

mod helpers;
mod table;

use helpers::{build_class_predicate, try_emit_accel_scan};
use table::emit_tier_b;

use parse_that::regex::accel::detect_accel;
use parse_that::regex::dfa::Dfa;
use parse_that::regex::nfa::DEAD;
use proc_macro2::TokenStream;
use quote::quote;

/// Try to compile a regex pattern to inline DFA code.
///
/// Returns `None` if the pattern uses unsupported features (backreferences,
/// lazy quantifiers) or the DFA exceeds the state limit (exponential blowup).
pub fn try_emit_dfa_inline(pattern: &str) -> Option<TokenStream> {
    let hir = parse_that::regex::parse_with(pattern, &parse_that::regex::ParseOptions::byte_mode())
        .ok()?;
    if super::hir::contains_lazy_quantifier(&hir) {
        return None;
    }

    let dfa = Dfa::compile(pattern)?;
    Some(emit_dfa(&dfa))
}

/// Emit inline code for a compiled DFA.
///
/// Unified tier selection (Phase 3.2):
/// - 1-12 states: Decision-tree codegen (inline match-chain, LLVM-friendly).
///   Subsumes the old Tier A -- a match-chain is a degenerate decision tree.
/// - 13-64 states: Static transition table + driver loop (Tier B).
/// - 65+ states: Fallback to Tier B (PackedDfa runtime integration is future).
fn emit_dfa(dfa: &Dfa) -> TokenStream {
    let n = dfa.state_count();
    if n == 0 {
        return quote! { None };
    }

    // Unified decision-tree tier: handles 1-12 states.
    // CostModel.decision_tree_max_states = 12 (default).
    if n <= 12 {
        emit_tier_a(dfa)
    } else if n <= 64 {
        emit_tier_b(dfa)
    } else {
        emit_tier_b(dfa)
    }
}

// ── Tier A: Inline match-chain (<=12 states) ───────────────────────────

/// Emit an inline state machine with explicit state transitions.
///
/// For small DFAs, this generates the most LLVM-friendly code: each state
/// becomes a block with direct byte checks and jumps. Self-loop states
/// get tight while loops with optional memchr acceleration.
fn emit_tier_a(dfa: &Dfa) -> TokenStream {
    let accels = detect_accel(dfa);
    let num_states = dfa.state_count();
    let num_cls = dfa.num_classes as usize;

    // Analyze the DFA structure to emit optimized code.
    // For each state, build a list of (byte_predicate, target_state) transitions.
    let mut state_transitions: Vec<Vec<(TokenStream, u32)>> = Vec::new();

    for state in dfa.states.iter() {
        let mut transitions: Vec<(TokenStream, u32)> = Vec::new();

        // Group classes by target state to merge predicates.
        let mut target_classes: std::collections::HashMap<u32, Vec<u8>> =
            std::collections::HashMap::new();
        for cls in 0..num_cls {
            let target = state.transitions[cls];
            if target != DEAD {
                target_classes.entry(target).or_default().push(cls as u8);
            }
        }

        for (&target, classes) in &target_classes {
            let pred = build_class_predicate(dfa, classes);
            transitions.push((pred, target));
        }

        state_transitions.push(transitions);
    }

    // Detect simple patterns: single accepting self-loop state (e.g., [a-z]+).
    if num_states == 2 {
        if let Some(ts) = try_emit_simple_loop(dfa, &accels, &state_transitions) {
            return ts;
        }
    }

    // General case: emit a state machine loop.
    emit_general_state_machine(dfa, &accels, &state_transitions)
}

/// Try to emit a simple two-state loop pattern: start -> loop(accept).
///
/// Matches patterns like `[a-z]+`, `\d+`, `[^"]+` where there's exactly one
/// non-start state that self-loops and is accepting.
fn try_emit_simple_loop(
    dfa: &Dfa,
    accels: &[parse_that::regex::accel::StateAccel],
    _state_transitions: &[Vec<(TokenStream, u32)>],
) -> Option<TokenStream> {
    let state0 = &dfa.states[0];
    let state1 = &dfa.states[1];

    // State 0 must not be accepting, state 1 must be accepting.
    if state0.is_accept || !state1.is_accept {
        return None;
    }

    // State 0 must transition only to state 1 (never to itself or dead).
    // State 1 must self-loop on the same predicate.
    let num_cls = dfa.num_classes as usize;

    let mut entry_classes: Vec<u8> = Vec::new();
    let mut loop_classes: Vec<u8> = Vec::new();

    for cls in 0..num_cls {
        if state0.transitions[cls] == 1 {
            entry_classes.push(cls as u8);
        }
        if state1.transitions[cls] == 1 {
            loop_classes.push(cls as u8);
        }
    }

    if entry_classes.is_empty() {
        return None;
    }

    // Check for self-loop acceleration on state 1.
    let accel = &accels[1];
    if let Some(accel_ts) = try_emit_accel_scan(accel) {
        // Use SIMD acceleration for the loop.
        let entry_pred = build_class_predicate(dfa, &entry_classes);

        // If entry and loop predicates are the same, emit simpler code.
        if entry_classes == loop_classes {
            return Some(quote! {
                {
                    let __start = state.offset;
                    let __end = state.src_bytes.len();
                    if __start >= __end {
                        None
                    } else {
                        let __b = unsafe { *state.src_bytes.get_unchecked(__start) };
                        if #entry_pred {
                            state.offset = __start + 1;
                            // Accelerated scan for remaining bytes.
                            #accel_ts
                            Some(::parse_that::Span::new(__start, state.offset, state.src))
                        } else {
                            None
                        }
                    }
                }
            });
        }
    }

    // Non-accelerated simple loop.
    let entry_pred = build_class_predicate(dfa, &entry_classes);
    let loop_pred = build_class_predicate(dfa, &loop_classes);

    Some(quote! {
        {
            let __start = state.offset;
            let __end = state.src_bytes.len();
            if __start >= __end {
                None
            } else {
                let __b = unsafe { *state.src_bytes.get_unchecked(__start) };
                if #entry_pred {
                    let mut __pos = __start + 1;
                    while __pos < __end {
                        let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                        if #loop_pred { __pos += 1; } else { break; }
                    }
                    state.offset = __pos;
                    Some(::parse_that::Span::new(__start, __pos, state.src))
                } else {
                    None
                }
            }
        }
    })
}

/// Emit a general state machine loop for small DFAs.
fn emit_general_state_machine(
    dfa: &Dfa,
    accels: &[parse_that::regex::accel::StateAccel],
    state_transitions: &[Vec<(TokenStream, u32)>],
) -> TokenStream {
    let _num_states = dfa.state_count();

    // Build match arms for each state.
    let mut arms: Vec<TokenStream> = Vec::new();
    for (sid, transitions) in state_transitions.iter().enumerate() {
        let sid_lit = proc_macro2::Literal::u8_unsuffixed(sid as u8);

        if transitions.is_empty() {
            // Dead-end state: no transitions out.
            arms.push(quote! { #sid_lit => { break; } });
            continue;
        }

        // Self-loop detection: if this state loops to itself on most inputs,
        // emit a while loop instead of a match arm.
        let self_loop_count = transitions.iter().filter(|(_, t)| *t == sid as u32).count();
        let is_self_loop_dominant = self_loop_count > 0 && transitions.len() - self_loop_count <= 2;

        if is_self_loop_dominant && dfa.states[sid].is_accept {
            // Self-looping accepting state: emit tight loop.
            let self_pred: Vec<&TokenStream> = transitions
                .iter()
                .filter(|(_, t)| *t == sid as u32)
                .map(|(p, _)| p)
                .collect();
            let combined_self = if self_pred.len() == 1 {
                let p = self_pred[0];
                quote! { #p }
            } else {
                quote! { #(#self_pred)||* }
            };

            // Non-self transitions.
            let other_arms: Vec<TokenStream> = transitions
                .iter()
                .filter(|(_, t)| *t != sid as u32)
                .map(|(pred, target)| {
                    let t_lit = proc_macro2::Literal::u8_unsuffixed(*target as u8);
                    quote! { if #pred { __s = #t_lit; continue; } }
                })
                .collect();

            // Phase 2.4: Check for SIMD acceleration on this self-loop state.
            let accel_scan = if sid < accels.len() {
                try_emit_accel_scan(&accels[sid])
            } else {
                None
            };

            if let Some(accel_code) = accel_scan {
                // SIMD-accelerated self-loop: skip ahead to exit byte.
                arms.push(quote! {
                    #sid_lit => {
                        // SIMD-accelerated self-loop scan.
                        if __pos < __end {
                            if let Some(__found) = #accel_code {
                                let __skip_end = __pos + __found;
                                __pos = __skip_end;
                                __last_accept = Some(__pos);
                                // Check the exit byte for non-self transitions.
                                if __pos < __end {
                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                    #(#other_arms)*
                                }
                            } else {
                                __pos = __end;
                                __last_accept = Some(__pos);
                            }
                        }
                        break;
                    }
                });
            } else {
                // Scalar self-loop.
                arms.push(quote! {
                    #sid_lit => {
                        while __pos < __end {
                            let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                            if #combined_self {
                                __pos += 1;
                                __last_accept = Some(__pos);
                            } else {
                                #(#other_arms)*
                                break;
                            }
                        }
                        break;
                    }
                });
            }
        } else {
            // General state: try each transition.
            let mut checks: Vec<TokenStream> = Vec::new();
            for (pred, target) in transitions {
                let t_lit = proc_macro2::Literal::u8_unsuffixed(*target as u8);
                let update_accept = if dfa.states[*target as usize].is_accept {
                    quote! { __last_accept = Some(__pos + 1); }
                } else {
                    quote! {}
                };
                checks.push(quote! {
                    if #pred {
                        __pos += 1;
                        #update_accept
                        __s = #t_lit;
                        continue;
                    }
                });
            }

            arms.push(quote! {
                #sid_lit => {
                    if __pos >= __end { break; }
                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                    #(#checks)*
                    break; // no transition matched -> stop
                }
            });
        }
    }

    let start_accept = if dfa.states[0].is_accept {
        quote! { let mut __last_accept: Option<usize> = Some(__start); }
    } else {
        quote! { let mut __last_accept: Option<usize> = None; }
    };

    quote! {
        {
            let __start = state.offset;
            let __end = state.src_bytes.len();
            let mut __pos = __start;
            let mut __s: u8 = 0;
            #start_accept

            loop {
                match __s {
                    #(#arms)*
                    _ => { break; }
                }
            }

            if let Some(__end) = __last_accept {
                state.offset = __end;
                Some(::parse_that::Span::new(__start, __end, state.src))
            } else {
                None
            }
        }
    }
}

// ── Phase 6: DFA Canonical Hashing for Cross-Rule Deduplication ─────────

/// Compute a canonical hash of a compiled DFA's structure.
///
/// Two different regex pattern strings that produce identical DFAs
/// (same transitions, same accept states) will hash to the same value.
/// Used for cross-rule deduplication in MonoCtx.
pub fn canonical_dfa_hash(pattern: &str) -> Option<u64> {
    let hir = parse_that::regex::parse_with(pattern, &parse_that::regex::ParseOptions::byte_mode())
        .ok()?;
    if super::hir::contains_lazy_quantifier(&hir) {
        return None;
    }

    let dfa = Dfa::compile(pattern)?;
    Some(helpers::hash_dfa_structure(&dfa))
}
