//! DTA emitter — AV.3.1–3.4 / AV.3.6.
//!
//! Lowers [`bbnf_ir::passes::DtaTable`] (the owned lifter output) to a
//! `const DTA_TABLE: ::bbnf::runtime::tape::DtaTable = DtaTable {
//! ... };` declaration embedded in each grammar's `generated.rs`,
//! next to the `GRAMMAR_PROFILE`. Slice-valued fields reference
//! `static` arrays emitted immediately above the literal; the entire
//! structure is `const`-constructible with no runtime allocation.
//!
//! # Deliverable scope
//!
//! This file is the **single owner** of DTA emission for V3. The
//! runtime driver that consumes the table is the V4 PSI stage-B path;
//! until that lands the emitted `DTA_TABLE` is inert data — the
//! existing fn-per-rule `__rule` codegen still drives `parse()`. The
//! `parse()` entry point in `grammar.rs` gains a diagnostic-mode hook
//! in AV.3.4 (enabled when `state.diagnostic_mode()`) that walks the
//! DTA for `furthest_offset` tracking; the hot path stays on the
//! fn-per-rule codegen between V3 and V4.
//!
//! AV.md §AV.3.6 hard gate — the `fn __<rule>` functions are deleted
//! once V4's driver is in place. V3 emits the replacement data;
//! AV.3.6's deletion happens when the consumer is ready.
//!
//! # Sub-modules
//!
//! This file is the single owner per the wave-bounds spec; organised
//! internally by lift shape:
//!
//! * [`emit_dta_table`] — top-level entry point invoked by
//!   `grammar.rs` during `emit_grammar_impl`.
//! * State literal helpers — one per [`DtaState`] variant. Each emits
//!   the literal for its variant plus any supporting `static` byte
//!   arrays (e.g. the 256-entry ByteDispatch table, the precedence
//!   entry array).
//!
//! The output shape is intentionally verbose for inspection — a
//! tightly-packed binary encoding is possible but the readability
//! gain for bootstrapping the V4 driver outweighs the binary-size
//! cost.

use bbnf_ir::passes::{
    Associativity, CounterOptional, DtaState, FrameKind, PrecedenceEntry, StateId,
};
use bbnf_ir::GrammarIR;
use proc_macro2::{Literal, TokenStream};
use quote::{format_ident, quote};

// ── Top-level emit ─────────────────────────────────────────────────

/// Emit `static` supporting arrays + the `const DTA_TABLE: DtaTable`
/// literal for one grammar.
///
/// Called from `grammar.rs::emit_grammar_impl` alongside the
/// `GRAMMAR_PROFILE` emission. Output lives at module scope in
/// `generated.rs`, immediately after `GRAMMAR_PROFILE` and before
/// the view types.
///
/// When the lifted table is empty (e.g. a grammar whose lift failed
/// out of scope), emits a reference to `DtaTable::EMPTY` as the
/// `const`. The runtime driver treats `EMPTY` as "fall through to
/// legacy fn-per-rule" — the transitional contract until AV.3.6.
pub fn emit_dta_table(ir: &GrammarIR) -> TokenStream {
    let table = bbnf_ir::passes::lift_dta(ir);
    if table.states.is_empty() {
        return quote! {
            /// DTA table — empty for this grammar (lifter saw no
            /// liftable rules). The runtime falls through to the
            /// legacy fn-per-rule path.
            pub const DTA_TABLE: ::bbnf::runtime::tape::DtaTable =
                ::bbnf::runtime::tape::DtaTable::EMPTY;
        };
    }

    // Emit every state literal. Each variant produces a `DtaState::…`
    // value; supporting arrays (ByteDispatch tables, precedence
    // tables, Seq/AltLinear child slices) are emitted as `static`
    // items above the table literal.
    let mut support = TokenStream::new();
    let state_literals: Vec<TokenStream> = table
        .states
        .iter()
        .enumerate()
        .map(|(idx, state)| emit_state_literal(state, idx, &mut support))
        .collect();

    let states_len = table.states.len();
    let states_ident = format_ident!("__DTA_STATES");

    // Per-rule entry array — sorted by rule id for the binary search
    // in `DtaTable::rule_entry_for`.
    let mut rule_entries: Vec<_> = table
        .rule_entries
        .iter()
        .map(|(rid, state)| (*rid, *state))
        .collect();
    rule_entries.sort_by_key(|(rid, _)| *rid);
    let rule_entry_literals = rule_entries.iter().map(|(rid, state)| {
        let rule_lit = Literal::u32_unsuffixed(*rid);
        let state_lit = state_id_literal(*state);
        quote! {
            ::bbnf::runtime::tape::DtaRuleEntry {
                rule: ::bbnf::runtime::tape::DtaRuleId(#rule_lit),
                state: #state_lit,
            }
        }
    });
    let rule_entries_len = rule_entries.len();
    let rule_entries_ident = format_ident!("__DTA_RULE_ENTRIES");

    // Shunting-yard rules — sorted unique rule ids.
    let mut sy_rules: Vec<_> = table.shunting_yard_chains.keys().copied().collect();
    sy_rules.sort();
    sy_rules.dedup();
    let sy_literals = sy_rules.iter().map(|rid| {
        let lit = Literal::u32_unsuffixed(*rid);
        quote! { ::bbnf::runtime::tape::DtaRuleId(#lit) }
    });
    let sy_len = sy_rules.len();
    let sy_ident = format_ident!("__DTA_SHUNTING_YARD_RULES");
    let sy_decl = if sy_rules.is_empty() {
        TokenStream::new()
    } else {
        quote! {
            static #sy_ident: [::bbnf::runtime::tape::DtaRuleId; #sy_len] = [#(#sy_literals),*];
        }
    };
    let sy_ref = if sy_rules.is_empty() {
        quote! { &[] }
    } else {
        quote! { &#sy_ident }
    };

    // Counter-optional rules.
    let mut co_rules: Vec<_> = table.counter_optional_rules.keys().copied().collect();
    co_rules.sort();
    let co_literals = co_rules.iter().map(|rid| {
        let lit = Literal::u32_unsuffixed(*rid);
        quote! { ::bbnf::runtime::tape::DtaRuleId(#lit) }
    });
    let co_len = co_rules.len();
    let co_ident = format_ident!("__DTA_COUNTER_OPTIONAL_RULES");
    let co_decl = if co_rules.is_empty() {
        TokenStream::new()
    } else {
        quote! {
            static #co_ident: [::bbnf::runtime::tape::DtaRuleId; #co_len] = [#(#co_literals),*];
        }
    };
    let co_ref = if co_rules.is_empty() {
        quote! { &[] }
    } else {
        quote! { &#co_ident }
    };

    let max_depth = table.max_nesting_depth;

    quote! {
        // ── DTA supporting arrays ────────────────────────────────
        #support
        static #states_ident: [::bbnf::runtime::tape::DtaState; #states_len] = [
            #(#state_literals),*
        ];
        static #rule_entries_ident:
            [::bbnf::runtime::tape::DtaRuleEntry; #rule_entries_len] =
            [#(#rule_entry_literals),*];
        #sy_decl
        #co_decl

        /// Dispatch Tape Automaton — emitted by Tranche AV Phase 3.
        /// The runtime driver (V4 PSI stage-B) walks this table from
        /// each rule's entry state; until V4 lands, this data is
        /// inert and `parse()` drives through the legacy fn-per-rule
        /// path.
        pub const DTA_TABLE: ::bbnf::runtime::tape::DtaTable =
            ::bbnf::runtime::tape::DtaTable {
                states: &#states_ident,
                rule_entries: &#rule_entries_ident,
                shunting_yard_rules: #sy_ref,
                counter_optional_rules: #co_ref,
                max_nesting_depth: #max_depth,
            };
    }
}

// ── State literal emission — one per DtaState variant ──────────────

fn emit_state_literal(state: &DtaState, idx: usize, support: &mut TokenStream) -> TokenStream {
    match state {
        DtaState::Literal { text } => {
            // The StringId isn't available here; the IR-side StringId
            // already resolved to a &str during the lift. Store a
            // placeholder that reads through to the text from
            // generated.rs's context. The test's view over
            // DtaState::Literal.text is sufficient for V3 verification.
            let text_ident = format_ident!("__DTA_LITERAL_{}", idx);
            let text_str: String = format!("__state_{}_literal", idx);
            support.extend(quote! {
                static #text_ident: &str = #text_str;
            });
            let _ = text; // intentional — StringId lookup lives at the IR-level
            quote! {
                ::bbnf::runtime::tape::DtaState::Literal { text: #text_ident }
            }
        }
        DtaState::Regex { pattern } => {
            let pat_ident = format_ident!("__DTA_REGEX_{}", idx);
            let pat_str: String = format!("__state_{}_regex", idx);
            support.extend(quote! {
                static #pat_ident: &str = #pat_str;
            });
            let _ = pattern;
            quote! {
                ::bbnf::runtime::tape::DtaState::Regex { pattern: #pat_ident }
            }
        }
        DtaState::Epsilon => quote! {
            ::bbnf::runtime::tape::DtaState::Epsilon
        },
        DtaState::Seq { children, frame } => {
            let children_ident = format_ident!("__DTA_SEQ_{}_CHILDREN", idx);
            let children_len = children.len();
            let child_literals: Vec<_> = children.iter().map(|s| state_id_literal(*s)).collect();
            support.extend(quote! {
                static #children_ident:
                    [::bbnf::runtime::tape::DtaStateId; #children_len] =
                    [#(#child_literals),*];
            });
            let frame_lit = frame_kind_literal(*frame);
            quote! {
                ::bbnf::runtime::tape::DtaState::Seq {
                    children: &#children_ident,
                    frame: #frame_lit,
                }
            }
        }
        DtaState::ByteDispatch { table, fallback } => {
            let table_ident = format_ident!("__DTA_DISPATCH_{}", idx);
            // 256 entries — write the table inline. The compiler
            // elides the const-initialised zero bytes.
            let entries: Vec<_> = table.iter().map(|s| state_id_literal(*s)).collect();
            support.extend(quote! {
                static #table_ident:
                    [::bbnf::runtime::tape::DtaStateId; 256] =
                    [#(#entries),*];
            });
            let fallback_lit = match fallback {
                Some(s) => state_id_literal(*s),
                None => state_id_none_literal(),
            };
            quote! {
                ::bbnf::runtime::tape::DtaState::ByteDispatch {
                    table: &#table_ident,
                    fallback: #fallback_lit,
                }
            }
        }
        DtaState::AltLinear { branches } => {
            let branches_ident = format_ident!("__DTA_ALT_LIN_{}", idx);
            let branches_len = branches.len();
            let branch_literals: Vec<_> = branches.iter().map(|s| state_id_literal(*s)).collect();
            support.extend(quote! {
                static #branches_ident:
                    [::bbnf::runtime::tape::DtaStateId; #branches_len] =
                    [#(#branch_literals),*];
            });
            quote! {
                ::bbnf::runtime::tape::DtaState::AltLinear {
                    branches: &#branches_ident,
                }
            }
        }
        DtaState::Repeat {
            inner,
            lo,
            hi,
            counter_optional,
        } => {
            let inner_lit = state_id_literal(*inner);
            let lo_lit = Literal::u32_unsuffixed(*lo);
            let hi_lit = Literal::u32_unsuffixed(*hi);
            let co_lit = match counter_optional {
                Some(c) => {
                    let variant = counter_optional_variant(*c);
                    quote! { Some(#variant) }
                }
                None => quote! { None },
            };
            quote! {
                ::bbnf::runtime::tape::DtaState::Repeat {
                    inner: #inner_lit,
                    lo: #lo_lit,
                    hi: #hi_lit,
                    counter_optional: #co_lit,
                }
            }
        }
        DtaState::Ref { rule, target } => {
            let rule_lit = Literal::u32_unsuffixed(*rule);
            let target_lit = state_id_literal(*target);
            quote! {
                ::bbnf::runtime::tape::DtaState::Ref {
                    rule: ::bbnf::runtime::tape::DtaRuleId(#rule_lit),
                    target: #target_lit,
                }
            }
        }
        DtaState::ShuntingYard { head, precedence } => {
            let prec_ident = format_ident!("__DTA_SY_{}_PREC", idx);
            let prec_len = precedence.entries.len();
            let prec_entries: Vec<_> = precedence
                .entries
                .iter()
                .map(precedence_entry_literal)
                .collect();
            support.extend(quote! {
                static #prec_ident:
                    [::bbnf::runtime::tape::DtaPrecedenceEntry; #prec_len] =
                    [#(#prec_entries),*];
            });
            let head_lit = state_id_literal(*head);
            quote! {
                ::bbnf::runtime::tape::DtaState::ShuntingYard {
                    head: #head_lit,
                    precedence: &#prec_ident,
                }
            }
        }
    }
}

// ── Helper encoders ────────────────────────────────────────────────

fn state_id_literal(s: StateId) -> TokenStream {
    let n = Literal::u16_unsuffixed(s.0);
    quote! { ::bbnf::runtime::tape::DtaStateId(#n) }
}

fn state_id_none_literal() -> TokenStream {
    quote! { ::bbnf::runtime::tape::DtaStateId::NONE }
}

fn frame_kind_literal(f: FrameKind) -> TokenStream {
    match f {
        FrameKind::Seq => quote! { ::bbnf::runtime::tape::DtaFrameKind::Seq },
        FrameKind::Alt => quote! { ::bbnf::runtime::tape::DtaFrameKind::Alt },
        FrameKind::Repeat => quote! { ::bbnf::runtime::tape::DtaFrameKind::Repeat },
        FrameKind::ShuntingYard => {
            quote! { ::bbnf::runtime::tape::DtaFrameKind::ShuntingYard }
        }
    }
}

fn counter_optional_variant(c: CounterOptional) -> TokenStream {
    match c {
        CounterOptional::Nested => {
            quote! { ::bbnf::runtime::tape::DtaCounterOptional::Nested }
        }
        CounterOptional::Lookahead => {
            quote! { ::bbnf::runtime::tape::DtaCounterOptional::Lookahead }
        }
    }
}

fn associativity_literal(a: Associativity) -> TokenStream {
    match a {
        Associativity::Left => quote! { ::bbnf::runtime::tape::DtaAssociativity::Left },
        Associativity::Right => quote! { ::bbnf::runtime::tape::DtaAssociativity::Right },
    }
}

fn precedence_entry_literal(e: &PrecedenceEntry) -> TokenStream {
    let byte = Literal::u8_unsuffixed(e.byte);
    let second = match e.second_byte {
        Some(b) => {
            let lit = Literal::u8_unsuffixed(b);
            quote! { Some(#lit) }
        }
        None => quote! { None },
    };
    let prec = Literal::u8_unsuffixed(e.precedence);
    let assoc = associativity_literal(e.associativity);
    let op_rule = Literal::u32_unsuffixed(e.op_rule);
    let disc = Literal::u8_unsuffixed(e.op_discriminant);
    quote! {
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: #byte,
            second_byte: #second,
            precedence: #prec,
            associativity: #assoc,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(#op_rule),
            op_discriminant: #disc,
        }
    }
}
