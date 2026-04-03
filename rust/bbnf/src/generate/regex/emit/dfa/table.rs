//! Tier B: Static transition table emission for larger DFAs (9--64 states).
//!
//! Emits a byte-class table, flattened transition table, and accept bitset
//! as `static` arrays, driven by a compact while-loop.

use parse_that::regex_engine::dfa::Dfa;
use parse_that::regex_engine::nfa::DEAD;
use proc_macro2::TokenStream;
use quote::quote;

/// Emit a static transition table + driver loop.
pub(super) fn emit_tier_b(dfa: &Dfa) -> TokenStream {
    let num_states = dfa.state_count();
    let num_cls = dfa.num_classes as usize;

    // Emit the equivalence class table.
    let class_bytes: Vec<proc_macro2::Literal> = dfa
        .byte_classes
        .iter()
        .map(|&c| proc_macro2::Literal::u8_unsuffixed(c))
        .collect();

    // Emit the flattened transition table: state * num_classes + class -> next_state.
    let mut trans_bytes: Vec<proc_macro2::Literal> = Vec::with_capacity(num_states * num_cls);
    for state in &dfa.states {
        for cls in 0..num_cls {
            let target = state.transitions[cls];
            // Encode DEAD as 0xFF (works for <= 254 states).
            let encoded = if target == DEAD { 0xFF } else { target as u8 };
            trans_bytes.push(proc_macro2::Literal::u8_unsuffixed(encoded));
        }
    }

    let num_cls_lit = proc_macro2::Literal::usize_unsuffixed(num_cls);
    let trans_len_lit = proc_macro2::Literal::usize_unsuffixed(num_states * num_cls);

    // Build accept bitset -- one u64 per 64 states. No arbitrary limit.
    let num_words = (num_states + 63) / 64;
    let mut accept_words = vec![0u64; num_words];
    for (i, s) in dfa.states.iter().enumerate() {
        if s.is_accept {
            accept_words[i / 64] |= 1u64 << (i % 64);
        }
    }
    let accept_lits: Vec<_> = accept_words
        .iter()
        .map(|w| proc_macro2::Literal::u64_unsuffixed(*w))
        .collect();
    let num_words_lit = proc_macro2::Literal::usize_unsuffixed(num_words);

    let start_accept = if dfa.states[0].is_accept {
        quote! { let mut __last_accept: Option<usize> = Some(__start); }
    } else {
        quote! { let mut __last_accept: Option<usize> = None; }
    };

    quote! {
        {
            static __CLASSES: [u8; 256] = [#(#class_bytes),*];
            static __TRANS: [u8; #trans_len_lit] = [#(#trans_bytes),*];
            static __ACCEPT: [u64; #num_words_lit] = [#(#accept_lits),*];

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
                if __ACCEPT[__s as usize / 64] & (1u64 << (__s as usize % 64)) != 0 {
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
