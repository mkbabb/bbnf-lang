//! DFA → inline TokenStream code emission.
//!
//! Converts a compiled `parse_that::regex_engine::Dfa` into inline Rust code
//! that performs anchored regex matching via direct byte operations.
//!
//! Three emission tiers based on DFA size:
//! - **Tier A** (≤ 8 states): Inline match-chain state machine. LLVM sees
//!   every transition and can optimize aggressively.
//! - **Tier B** (9–64 states): Static transition table + driver loop.
//!   Compact, cache-friendly, with SIMD acceleration for self-loop states.
//! - **Tier C** (65+ states): Reference to a runtime `PackedDfa` (future).
//!
//! All tiers emit code that evaluates to `Option<Span<'a>>`, reading from
//! `state.src_bytes` and advancing `state.offset`.

use parse_that::regex_engine::accel::{AccelStrategy, detect_accel};
use parse_that::regex_engine::dfa::Dfa;
use parse_that::regex_engine::nfa::DEAD;
use proc_macro2::TokenStream;
use quote::quote;

/// Try to compile a regex pattern to inline DFA code.
///
/// Returns `None` if the pattern uses unsupported features (backreferences,
/// lazy quantifiers) or the DFA exceeds the state limit (exponential blowup).
pub fn try_emit_dfa_inline(pattern: &str) -> Option<TokenStream> {
    // DFA-based matching always produces longest-match semantics.
    // Patterns with lazy quantifiers need shortest-match, which requires
    // NFA simulation. Bail — the pattern is unsupported.
    let hir = regex_syntax::ParserBuilder::new()
        .utf8(false)
        .unicode(false)
        .build()
        .parse(pattern)
        .ok()?;
    if super::hir::contains_lazy_quantifier(&hir) {
        return None;
    }

    let dfa = Dfa::compile(pattern)?;
    Some(emit_dfa(&dfa))
}

/// Emit inline code for a compiled DFA.
fn emit_dfa(dfa: &Dfa) -> TokenStream {
    let n = dfa.state_count();
    if n == 0 {
        return quote! { None };
    }

    if n <= 8 {
        emit_tier_a(dfa)
    } else if n <= 64 {
        emit_tier_b(dfa)
    } else {
        // Tier C: for now, fall back to tier B for any size.
        // PackedDfa runtime integration is a future task.
        emit_tier_b(dfa)
    }
}

// ── Tier A: Inline match-chain (≤ 8 states) ────────────────────────────

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

/// Try to emit a simple two-state loop pattern: start → loop(accept).
///
/// Matches patterns like `[a-z]+`, `\d+`, `[^"]+` where there's exactly one
/// non-start state that self-loops and is accepting.
fn try_emit_simple_loop(
    dfa: &Dfa,
    accels: &[parse_that::regex_engine::accel::StateAccel],
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
    _accels: &[parse_that::regex_engine::accel::StateAccel],
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

            arms.push(quote! {
                #sid_lit => {
                    // Self-loop scan.
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
                    break; // no transition matched → stop
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

// ── Tier B: Static transition table (9–64 states) ──────────────────────

/// Emit a static transition table + driver loop.
fn emit_tier_b(dfa: &Dfa) -> TokenStream {
    let num_states = dfa.state_count();
    let num_cls = dfa.num_classes as usize;

    // Emit the equivalence class table.
    let class_bytes: Vec<proc_macro2::Literal> = dfa
        .byte_classes
        .iter()
        .map(|&c| proc_macro2::Literal::u8_unsuffixed(c))
        .collect();

    // Emit the flattened transition table: state * num_classes + class → next_state.
    let mut trans_bytes: Vec<proc_macro2::Literal> = Vec::with_capacity(num_states * num_cls);
    for state in &dfa.states {
        for cls in 0..num_cls {
            let target = state.transitions[cls];
            // Encode DEAD as 0xFF (works for ≤ 254 states).
            let encoded = if target == DEAD { 0xFF } else { target as u8 };
            trans_bytes.push(proc_macro2::Literal::u8_unsuffixed(encoded));
        }
    }

    let num_cls_lit = proc_macro2::Literal::usize_unsuffixed(num_cls);
    let trans_len_lit = proc_macro2::Literal::usize_unsuffixed(num_states * num_cls);
    let accept_mask_lit = proc_macro2::Literal::u64_unsuffixed(dfa.accept_mask);

    let start_accept = if dfa.states[0].is_accept {
        quote! { let mut __last_accept: Option<usize> = Some(__start); }
    } else {
        quote! { let mut __last_accept: Option<usize> = None; }
    };

    quote! {
        {
            static __CLASSES: [u8; 256] = [#(#class_bytes),*];
            static __TRANS: [u8; #trans_len_lit] = [#(#trans_bytes),*];
            const __ACCEPT: u64 = #accept_mask_lit;

            let __start = state.offset;
            let __end = state.src_bytes.len();
            let mut __s: u8 = 0;
            let mut __pos = __start;
            #start_accept

            while __pos < __end {
                let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                let __c = unsafe { *__CLASSES.get_unchecked(__b as usize) };
                let __next = unsafe {
                    *__TRANS.get_unchecked(__s as usize * #num_cls_lit + __c as usize)
                };
                if __next == 0xFF { break; }
                __s = __next;
                __pos += 1;
                if __ACCEPT & (1u64 << __s as u64) != 0 {
                    __last_accept = Some(__pos);
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

// ── Helpers ─────────────────────────────────────────────────────────────

/// Build a boolean predicate for a set of equivalence classes.
///
/// Expands the classes back to byte ranges and emits `__b` checks.
fn build_class_predicate(dfa: &Dfa, classes: &[u8]) -> TokenStream {
    // Collect all bytes belonging to these classes.
    let mut bytes: Vec<u8> = Vec::new();
    for (b, &cls) in dfa.byte_classes.iter().enumerate() {
        if classes.contains(&cls) {
            bytes.push(b as u8);
        }
    }

    if bytes.is_empty() {
        return quote! { false };
    }

    // Try to emit efficient predicates.
    // Check for known shorthand patterns.
    if let Some(shorthand) = detect_shorthand(&bytes) {
        return shorthand;
    }

    // Build ranges for compact emission.
    let ranges = bytes_to_ranges(&bytes);

    if ranges.len() == 1 {
        let (lo, hi) = ranges[0];
        if lo == hi {
            let lit = proc_macro2::Literal::byte_character(lo);
            return quote! { __b == #lit };
        }
        let lo_lit = proc_macro2::Literal::byte_character(lo);
        let hi_lit = proc_macro2::Literal::byte_character(hi);
        return quote! { __b >= #lo_lit && __b <= #hi_lit };
    }

    let mut conditions: Vec<TokenStream> = Vec::new();
    for (lo, hi) in &ranges {
        if lo == hi {
            let lit = proc_macro2::Literal::byte_character(*lo);
            conditions.push(quote! { __b == #lit });
        } else {
            let lo_lit = proc_macro2::Literal::byte_character(*lo);
            let hi_lit = proc_macro2::Literal::byte_character(*hi);
            conditions.push(quote! { (__b >= #lo_lit && __b <= #hi_lit) });
        }
    }

    quote! { #(#conditions)||* }
}

/// Detect well-known shorthand predicates.
fn detect_shorthand(bytes: &[u8]) -> Option<TokenStream> {
    let set: std::collections::HashSet<u8> = bytes.iter().copied().collect();

    // \d = [0-9]
    if set.len() == 10
        && (b'0'..=b'9').all(|b| set.contains(&b))
        && set.iter().all(|b| b.is_ascii_digit())
    {
        return Some(quote! { __b.is_ascii_digit() });
    }

    // \w = [0-9A-Za-z_]
    let word_chars: std::collections::HashSet<u8> = (b'0'..=b'9')
        .chain(b'A'..=b'Z')
        .chain(b'a'..=b'z')
        .chain(std::iter::once(b'_'))
        .collect();
    if set == word_chars {
        return Some(quote! { (__b.is_ascii_alphanumeric() || __b == b'_') });
    }

    // [a-zA-Z]
    let alpha: std::collections::HashSet<u8> = (b'A'..=b'Z').chain(b'a'..=b'z').collect();
    if set == alpha {
        return Some(quote! { __b.is_ascii_alphabetic() });
    }

    // \s = ASCII whitespace
    let ws: std::collections::HashSet<u8> = [b' ', b'\t', b'\n', b'\r', 0x0B, 0x0C]
        .iter()
        .copied()
        .collect();
    if set == ws {
        return Some(quote! { __b.is_ascii_whitespace() });
    }

    // [0-9a-fA-F]
    let hex: std::collections::HashSet<u8> = (b'0'..=b'9')
        .chain(b'A'..=b'F')
        .chain(b'a'..=b'f')
        .collect();
    if set == hex {
        return Some(quote! { __b.is_ascii_hexdigit() });
    }

    None
}

/// Convert a sorted list of bytes to inclusive ranges.
fn bytes_to_ranges(bytes: &[u8]) -> Vec<(u8, u8)> {
    if bytes.is_empty() {
        return Vec::new();
    }
    let mut sorted = bytes.to_vec();
    sorted.sort_unstable();
    sorted.dedup();

    let mut ranges = Vec::new();
    let mut start = sorted[0];
    let mut end = sorted[0];

    for &b in &sorted[1..] {
        if b == end + 1 {
            end = b;
        } else {
            ranges.push((start, end));
            start = b;
            end = b;
        }
    }
    ranges.push((start, end));

    ranges
}

/// Try to emit SIMD-accelerated scanning code for a self-loop state.
fn try_emit_accel_scan(accel: &parse_that::regex_engine::accel::StateAccel) -> Option<TokenStream> {
    match &accel.strategy {
        AccelStrategy::Memchr1(b) => {
            let b_lit = proc_macro2::Literal::byte_character(*b);
            Some(quote! {
                if let Some(__skip) = ::parse_that::memchr::memchr(
                    #b_lit,
                    &state.src_bytes[state.offset..]
                ) {
                    state.offset += __skip;
                } else {
                    state.offset = state.src_bytes.len();
                }
            })
        }
        AccelStrategy::Memchr2(b1, b2) => {
            let b1_lit = proc_macro2::Literal::byte_character(*b1);
            let b2_lit = proc_macro2::Literal::byte_character(*b2);
            Some(quote! {
                if let Some(__skip) = ::parse_that::memchr::memchr2(
                    #b1_lit, #b2_lit,
                    &state.src_bytes[state.offset..]
                ) {
                    state.offset += __skip;
                } else {
                    state.offset = state.src_bytes.len();
                }
            })
        }
        AccelStrategy::Memchr3(b1, b2, b3) => {
            let b1_lit = proc_macro2::Literal::byte_character(*b1);
            let b2_lit = proc_macro2::Literal::byte_character(*b2);
            let b3_lit = proc_macro2::Literal::byte_character(*b3);
            Some(quote! {
                if let Some(__skip) = ::parse_that::memchr::memchr3(
                    #b1_lit, #b2_lit, #b3_lit,
                    &state.src_bytes[state.offset..]
                ) {
                    state.offset += __skip;
                } else {
                    state.offset = state.src_bytes.len();
                }
            })
        }
        _ => None,
    }
}
