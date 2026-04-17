//! DTA emitter — AV.3.1–3.4 / AV.3.6 / AV.4.1.
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
//! This file is the **single owner** of DTA emission for V3 and the
//! PSI emitter integration for V4.1. The runtime driver that consumes
//! the table + PSI stream is the V4 PSI stage-B path; until that
//! lands the emitted `DTA_TABLE` is inert data — the existing
//! fn-per-rule `__rule` codegen still drives `parse()`. The `parse()`
//! entry point in `grammar.rs` gains a diagnostic-mode hook in AV.3.4
//! (enabled when `state.diagnostic_mode()`) that walks the DTA for
//! `furthest_offset` tracking; the hot path stays on the fn-per-rule
//! codegen between V3 and V4.
//!
//! AV.md §AV.3.6 hard gate — the `fn __<rule>` functions are deleted
//! once V4's driver is in place. V3 emits the replacement data;
//! AV.3.6's deletion happens when the consumer is ready.
//!
//! # PSI emission (AV.4.1)
//!
//! Alongside the DTA table the emitter produces a
//! `pub fn psi_with_capacity(input_len: usize) -> PayloadStream`
//! free function that the runtime stage-A driver calls once at parse
//! start to allocate the [`PayloadStream`](::bbnf::runtime::tape::PayloadStream).
//! Capacity comes straight from
//! `GRAMMAR_PROFILE.leaves_per_input_byte × input_len` — no
//! grammar-specific constant is needed because the profile already
//! carries the per-byte estimate. The helper exists so the V4 driver
//! has a single, grammar-bounded entry point for stream allocation;
//! it is currently inert (no caller until the legacy fn-per-rule
//! path is replaced by the DTA driver in AV.3.6).
//!
//! # Sub-modules
//!
//! This file is the single owner per the wave-bounds spec; organised
//! internally by lift shape:
//!
//! * [`emit_dta_table`] — top-level entry point invoked by
//!   `grammar.rs` during `emit_grammar_impl`. Emits both the DTA table
//!   and the PSI helper.
//! * State literal helpers — one per [`DtaState`] variant. Each emits
//!   the literal for its variant plus any supporting `static` byte
//!   arrays (e.g. the 256-entry ByteDispatch table, the precedence
//!   entry array).
//!
//! The output shape is intentionally verbose for inspection — a
//! tightly-packed binary encoding is possible but the readability
//! gain for bootstrapping the V4 driver outweighs the binary-size
//! cost.

use bbnf_ir::passes::recognizers::shape_dict::{ShapeTemplate, TemplatePiece};
use bbnf_ir::passes::recognizers::shape_dict_bbnf::{BbnfShapeKind, BbnfShapeTemplate};
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
    let bbnf_shapes = emit_bbnf_shape_dict(ir);
    let shape_dict_block = emit_shape_dict_arrays(ir);
    if table.states.is_empty() {
        let psi_helper = emit_psi_helper();
        return quote! {
            /// DTA table — empty for this grammar (lifter saw no
            /// liftable rules). The runtime falls through to the
            /// legacy fn-per-rule path.
            pub const DTA_TABLE: ::bbnf::runtime::tape::DtaTable =
                ::bbnf::runtime::tape::DtaTable::EMPTY;

            #shape_dict_block
            #psi_helper

            #bbnf_shapes
        };
    }

    // Emit every state literal. Each variant produces a `DtaState::…`
    // value; supporting arrays (ByteDispatch tables, precedence
    // tables, Seq/AltLinear child slices) are emitted as `static`
    // items above the table literal.
    //
    // AW.1.2: `Literal` / `Regex` variants resolve their `StringId`
    // payloads to the real IR-interned text so the DTA driver's
    // byte-compare and regex-scan arms have something to match
    // against. Prior waves emitted placeholder strings (`__state_N
    // _literal`) because the driver was inert; now the driver is
    // the primary parse path and needs the real bytes.
    let mut support = TokenStream::new();
    let state_literals: Vec<TokenStream> = table
        .states
        .iter()
        .enumerate()
        .map(|(idx, state)| emit_state_literal(state, idx, ir, &mut support))
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
    let entry_rule = Literal::u32_unsuffixed(table.entry);
    let psi_helper = emit_psi_helper();

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
        /// The runtime driver walks this table from the grammar's
        /// entry rule state. AW-I.W4γ — `entry` carries the
        /// authoritative `RuleId` so the walker dispatches the
        /// correct grammar entry irrespective of lift order.
        pub const DTA_TABLE: ::bbnf::runtime::tape::DtaTable =
            ::bbnf::runtime::tape::DtaTable {
                states: &#states_ident,
                rule_entries: &#rule_entries_ident,
                shunting_yard_rules: #sy_ref,
                counter_optional_rules: #co_ref,
                max_nesting_depth: #max_depth,
                entry: ::bbnf::runtime::tape::DtaRuleId(#entry_rule),
            };

        #shape_dict_block
        #psi_helper

        #bbnf_shapes
    }
}

// ── AV.4.1 — PSI stream construction helper ───────────────────────

/// Emit the per-grammar PSI allocation helper.
///
/// Stage A's runtime driver (V4) calls this once at parse start to
/// allocate a [`PayloadStream`](::bbnf::runtime::tape::PayloadStream)
/// sized from `GRAMMAR_PROFILE.leaves_per_input_byte × input_len`.
/// The helper delegates to
/// [`PayloadStream::with_capacity_for`](::bbnf::runtime::tape::PayloadStream::with_capacity_for)
/// — no grammar-specific data is needed because the profile already
/// carries the per-byte estimate.
///
/// The companion `fill_payloads` helper drives stage-B over the
/// constructed stream, dispatching the parallel fork on
/// `GRAMMAR_PROFILE.parallel_break_even_bytes`. Both helpers live at
/// module scope so the V4 stage-A driver can call them without
/// re-deriving the profile reference.
fn emit_psi_helper() -> TokenStream {
    quote! {
        /// AV.4.1 — Allocate this grammar's stage-A PSI stream sized
        /// from [`GRAMMAR_PROFILE`]'s `leaves_per_input_byte`.
        ///
        /// The runtime stage-A driver (AV.3.6 / V4) calls this once
        /// at parse start; every subsequent `PayloadJob::push` lands
        /// in pre-allocated memory.
        #[inline]
        pub fn psi_with_capacity(
            input_len: usize,
        ) -> ::bbnf::runtime::tape::PayloadStream {
            ::bbnf::runtime::tape::PayloadStream::with_capacity_for(
                &GRAMMAR_PROFILE,
                input_len,
            )
        }

        /// AV.4.2 — Drive stage-B's payload fill over the PSI stream.
        ///
        /// Single API for sequential and parallel paths; the dispatch
        /// fork lives inside
        /// [`PayloadStream::fill_columns`](::bbnf::runtime::tape::PayloadStream::fill_columns)
        /// on `GRAMMAR_PROFILE.parallel_break_even_bytes`. Inputs
        /// below the break-even gate run sequentially; above, the
        /// rayon `par_chunks` walk takes over.
        #[inline]
        pub fn fill_payloads(
            psi: &::bbnf::runtime::tape::PayloadStream,
            input: &[u8],
            columns: &mut ::bbnf::runtime::tape::Columns,
        ) -> usize {
            psi.fill_columns(input, columns, &GRAMMAR_PROFILE)
        }
    }
}

// ── State literal emission — one per DtaState variant ──────────────

fn emit_state_literal(
    state: &DtaState,
    idx: usize,
    ir: &GrammarIR,
    support: &mut TokenStream,
) -> TokenStream {
    match state {
        DtaState::Literal { text } => {
            // AW.1.2: resolve the IR-interned StringId to the actual
            // byte sequence the driver will compare against input.
            // Emitted as a `&str` literal via `proc_macro2::Literal::
            // string` so arbitrary content (UTF-8, escape sequences,
            // embedded quotes) round-trips verbatim through the token
            // stream.
            let text_ident = format_ident!("__DTA_LITERAL_{}", idx);
            let text_str = ir.get_string(*text);
            let text_literal = Literal::string(text_str);
            support.extend(quote! {
                static #text_ident: &str = #text_literal;
            });
            quote! {
                ::bbnf::runtime::tape::DtaState::Literal { text: #text_ident }
            }
        }
        DtaState::Regex { pattern } => {
            // AW.1.2: same resolution as `Literal`. The driver's regex
            // arm feeds this pattern into the caller-supplied
            // `RegexScanner` implementation.
            let pat_ident = format_ident!("__DTA_REGEX_{}", idx);
            let pat_str = ir.get_string(*pattern);
            let pat_literal = Literal::string(pat_str);
            support.extend(quote! {
                static #pat_ident: &str = #pat_literal;
            });
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
        DtaState::WsTrim { pattern } => {
            // AW-I.W4γ: emit `DtaState::WsTrim { pattern: Some(_) }`
            // when the grammar has `@ws`; `None` otherwise (walker
            // treats a pattern-less WsTrim as Epsilon).
            match pattern {
                Some(sid) => {
                    let pat_ident = format_ident!("__DTA_WS_{}", idx);
                    let pat_str = ir.get_string(*sid);
                    let pat_literal = Literal::string(pat_str);
                    support.extend(quote! {
                        static #pat_ident: &str = #pat_literal;
                    });
                    quote! {
                        ::bbnf::runtime::tape::DtaState::WsTrim {
                            pattern: ::core::option::Option::Some(#pat_ident),
                        }
                    }
                }
                None => quote! {
                    ::bbnf::runtime::tape::DtaState::WsTrim {
                        pattern: ::core::option::Option::None,
                    }
                },
            }
        }
        DtaState::Minus { primary, excluded } => {
            // AW-II.W5b: IrNode::Minus → DtaState::Minus set-difference.
            // The walker probes `excluded` with a savepoint; if it
            // matches, the Minus fails; otherwise `primary` dispatches.
            let primary_lit = state_id_literal(*primary);
            let excluded_lit = state_id_literal(*excluded);
            quote! {
                ::bbnf::runtime::tape::DtaState::Minus {
                    primary: #primary_lit,
                    excluded: #excluded_lit,
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

// ── AV.5.4 — Shape-dictionary emission ────────────────────────────

/// Emit `static SHAPE_DICT_TABLE: [ShapeEntry; N]` plus the
/// supporting per-template `child_kinds` and `leaf_payload_offsets`
/// arrays.
///
/// Walks `ir.shape_dict_selection` (the indices admitted by
/// [`bbnf_ir::passes::csp_strategy::constraints::shape_dict::solve_shape_dict_selection`])
/// and bakes the corresponding [`ShapeTemplate`]s into a
/// `static SHAPE_DICT_TABLE: [::bbnf::runtime::tape::ShapeEntry; N]`
/// array plus a `pub const SHAPE_DICT: &[ShapeEntry]` view that
/// downstream tranches will splice into `GRAMMAR_PROFILE.shape_dict`
/// once the profile-emitter wiring lands.
///
/// When the selection is empty the function emits a `pub const
/// SHAPE_DICT: &[ShapeEntry] = &[];` so downstream consumers always
/// have a stable symbol to reference.
///
/// Each `ShapeEntry` carries:
///
/// - `shape_hash` — canonical 64-bit hash from
///   [`ShapeTemplate::shape_hash`], used by the V4 DTA driver to
///   route a stage-A rule body to its dictionary entry.
/// - `rule` — the root rule id whose collapsed compound the entry
///   represents (resolved from the template's NodeId).
/// - `child_kinds` — per-position skeleton bytes
///   ([`bbnf_tape::TapeKind`] discriminants) that `TapeCursor::
///   shape_ref_children` reads at view time to synthesise children.
/// - `leaf_payload_offsets` — per-position byte offset within the
///   packed payload blob where each leaf hole's `(span_lo,
///   span_hi)` pair begins. `u16::MAX` for structural (non-hole)
///   positions.
/// - `payload_bytes` — total byte width of the per-instance packed
///   blob.
fn emit_shape_dict_arrays(ir: &GrammarIR) -> TokenStream {
    if ir.shape_dict_selection.is_empty() {
        return quote! {
            /// Shape dictionary — empty for this grammar (selection
            /// admitted no templates). Reserved symbol so downstream
            /// driver code can reference it unconditionally.
            pub const SHAPE_DICT: &[::bbnf::runtime::tape::ShapeEntry] = &[];
        };
    }

    let mut support = TokenStream::new();
    let mut entry_literals = Vec::with_capacity(ir.shape_dict_selection.len());

    for (slot, &template_idx) in ir.shape_dict_selection.iter().enumerate() {
        let (node_id, template) = &ir.shape_dict_templates[template_idx];
        let rule_id = resolve_rule_for_node(ir, *node_id);
        let entry = emit_shape_entry_literal(slot, rule_id, template, &mut support);
        entry_literals.push(entry);
    }

    let table_ident = format_ident!("__SHAPE_DICT_TABLE");
    let table_len = entry_literals.len();

    quote! {
        #support
        static #table_ident:
            [::bbnf::runtime::tape::ShapeEntry; #table_len] = [
            #(#entry_literals),*
        ];

        /// Shape dictionary — Tranche AV.5.4 emission. Each entry
        /// describes one fixed-shape compound subtree the parser
        /// collapses to a single `TapeKind::ShapeRef` record. The
        /// V4+ DTA driver consults this table by `shape_hash`
        /// during counter-DFA stage-A transitions; on match it
        /// routes through `TapeBuilder::push_shape_ref` instead of
        /// the per-position child push sites.
        pub const SHAPE_DICT: &[::bbnf::runtime::tape::ShapeEntry] =
            &#table_ident;
    }
}

/// Emit one `ShapeEntry` literal plus its supporting `child_kinds`
/// and `leaf_payload_offsets` static arrays.
fn emit_shape_entry_literal(
    slot: usize,
    rule_id: u32,
    template: &ShapeTemplate,
    support: &mut TokenStream,
) -> TokenStream {
    // Compute per-position kind discriminants and leaf-hole offsets
    // in source order. The skeleton is a flat list emitted by
    // ShapeDictMiner; each piece maps to one TapeKind + an optional
    // packed-blob offset.
    let mut child_kinds: Vec<u8> = Vec::with_capacity(template.skeleton.len());
    let mut leaf_offsets: Vec<u16> = Vec::with_capacity(template.skeleton.len());
    let mut next_hole_offset: u16 = 0;

    for piece in &template.skeleton {
        let kind_byte = template_piece_to_kind_byte(piece);
        child_kinds.push(kind_byte);
        match piece {
            TemplatePiece::LeafHole => {
                leaf_offsets.push(next_hole_offset);
                // 8 bytes for (span_lo, span_hi); the typed-payload
                // bytes follow but the offset table only needs to
                // point at the span pair — typed payloads are read
                // through the same packed-blob arena via
                // `Tape::payload_bytes` keyed by absolute offset.
                next_hole_offset = next_hole_offset.saturating_add(8);
            }
            _ => {
                leaf_offsets.push(u16::MAX);
            }
        }
    }

    let kinds_ident = format_ident!("__SHAPE_DICT_{}_KINDS", slot);
    let offsets_ident = format_ident!("__SHAPE_DICT_{}_OFFSETS", slot);
    let kinds_len = child_kinds.len();
    let offsets_len = leaf_offsets.len();
    let kind_lits = child_kinds.iter().map(|b| Literal::u8_unsuffixed(*b));
    let offset_lits = leaf_offsets.iter().map(|o| Literal::u16_unsuffixed(*o));

    support.extend(quote! {
        static #kinds_ident: [u8; #kinds_len] = [#(#kind_lits),*];
        static #offsets_ident: [u16; #offsets_len] = [#(#offset_lits),*];
    });

    let shape_hash_lit = Literal::u64_unsuffixed(template.shape_hash);
    let rule_lit = Literal::u32_unsuffixed(rule_id);
    let payload_bytes = next_hole_offset; // total packed blob width

    let payload_bytes_lit = Literal::u16_unsuffixed(payload_bytes);

    quote! {
        ::bbnf::runtime::tape::ShapeEntry {
            shape_hash: #shape_hash_lit,
            rule: ::bbnf::runtime::tape::RuleId(#rule_lit),
            child_kinds: &#kinds_ident,
            leaf_payload_offsets: &#offsets_ident,
            payload_bytes: #payload_bytes_lit,
        }
    }
}

/// Map a [`TemplatePiece`] to its corresponding
/// [`bbnf_tape::TapeKind`] discriminant byte.
fn template_piece_to_kind_byte(piece: &TemplatePiece) -> u8 {
    use bbnf_tape::TapeKind;
    match piece {
        TemplatePiece::Literal(_) => TapeKind::Literal as u8,
        TemplatePiece::LeafHole => TapeKind::Span as u8,
        TemplatePiece::Whitespace => TapeKind::Span as u8,
        TemplatePiece::Epsilon => TapeKind::Epsilon as u8,
    }
}

/// Resolve a `NodeId` back to the rule id whose body contains it.
///
/// Walks the IR's rules in order; the first rule whose body's DAG
/// position matches `node_id` wins. Returns `0` (the entry rule) as
/// the conservative fallback when the node is not directly
/// associated with a rule root — the dictionary entry still works
/// because `rule` is a hint the driver uses for diagnostics, not a
/// dispatch key.
fn resolve_rule_for_node(ir: &GrammarIR, node_id: bbnf_ir::dag::NodeId) -> u32 {
    let Some(dag) = ir.dag.as_ref() else {
        return 0;
    };
    for rule in &ir.rules {
        if let Some(body_id) = dag.node_for(&rule.body) {
            if body_id == node_id {
                return rule.id;
            }
        }
    }
    0
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

// ── AV.5.6 / AV.6.1–6.3 — BBNF shape dictionary emission ──────────
//
// Composes with V5a's `TapeKind::ShapeRef` substrate. The rule
// codegen consults `BBNF_SHAPE_DICT` at compile time (via the
// emitter — see `dispatch.rs` integration) to decide whether a
// rule's body collapses to a single `ShapeRef` push instead of the
// normal fn-per-rule emission.
//
// Each entry encodes:
// - `kind` — the structural pattern (BigComment, MappedFactorEmpty)
// - `shape_hash` — canonical 64-bit dedup key
// - `payload_bytes` — packed payload blob size (8 for big_comment's
//   Span, 0 for mapped_factor's empty branch)
//
// The dict is a `pub const` so downstream tests can verify
// per-grammar templates land. Empty for non-BBNF grammars.

/// Emit the per-grammar BBNF shape dictionary.
///
/// Produces a `static __BBNF_SHAPE_TEMPLATES` array plus a
/// `pub const BBNF_SHAPE_DICT` reference. Empty for grammars whose
/// `IR::profile().bbnf_shape_templates` is empty (every non-BBNF
/// grammar). The constant is always emitted so consumers can rely on
/// its presence.
fn emit_bbnf_shape_dict(ir: &GrammarIR) -> TokenStream {
    let templates = ir.profile().bbnf_shape_templates;
    if templates.is_empty() {
        return quote! {
            /// BBNF shape dictionary — empty for this grammar.
            /// Populated only for grammars whose rules match the
            /// BBNF-specific shape patterns (`big_comment`,
            /// `mapped_factor` empty branch). See AV.5.6 / AV.6.1–6.3.
            pub const BBNF_SHAPE_DICT: &[::bbnf::runtime::tape::BbnfShapeEntry] = &[];
        };
    }

    let entry_literals: Vec<TokenStream> =
        templates.iter().map(bbnf_shape_template_literal).collect();
    let entries_len = templates.len();
    let entries_ident = format_ident!("__BBNF_SHAPE_TEMPLATES");

    quote! {
        // ── BBNF shape dictionary (AV.5.6 / AV.6.1–6.3) ─────────────
        //
        // Each entry describes a rule body whose multi-record tape
        // skeleton collapses to a single `ShapeRef` record + an
        // optional packed payload blob. The codegen checks the entry's
        // `kind` at the rule emission site to dispatch to the
        // collapsed `push_shape_ref` path.
        static #entries_ident:
            [::bbnf::runtime::tape::BbnfShapeEntry; #entries_len] = [
            #(#entry_literals),*
        ];

        /// BBNF shape dictionary — non-empty for BBNF self-hosting.
        /// The codegen for `__big_comment` / `__mapped_factor` checks
        /// this dictionary at compile time and emits a single
        /// `push_shape_ref` per match. See AV.5.6 / AV.6.1–6.3.
        pub const BBNF_SHAPE_DICT:
            &[::bbnf::runtime::tape::BbnfShapeEntry] = &#entries_ident;
    }
}

/// Encode one BBNF shape template as a `BbnfShapeEntry` literal.
///
/// The runtime tape side (V5a) defines the `BbnfShapeEntry` struct
/// alongside `TapeKind::ShapeRef`. This function emits the literal
/// using fully-qualified paths so the compose order with V5a is
/// deterministic.
fn bbnf_shape_template_literal(t: &BbnfShapeTemplate) -> TokenStream {
    let kind = bbnf_shape_kind_literal(t.kind);
    let hash = Literal::u64_unsuffixed(t.shape_hash);
    let payload_bytes = Literal::u16_unsuffixed(t.payload_bytes);
    let rule_name = &t.rule_name;
    quote! {
        ::bbnf::runtime::tape::BbnfShapeEntry {
            rule_name: #rule_name,
            kind: #kind,
            shape_hash: #hash,
            payload_bytes: #payload_bytes,
        }
    }
}

/// Encode a `BbnfShapeKind` as a tape-side enum literal.
///
/// Maps the IR-side `BbnfShapeKind` to the runtime `BbnfShapeKind`
/// enum exposed by `bbnf::runtime::tape` (V5a substrate).
fn bbnf_shape_kind_literal(k: BbnfShapeKind) -> TokenStream {
    match k {
        BbnfShapeKind::BigComment => quote! {
            ::bbnf::runtime::tape::BbnfShapeKind::BigComment
        },
        BbnfShapeKind::MappedFactorEmpty => quote! {
            ::bbnf::runtime::tape::BbnfShapeKind::MappedFactorEmpty
        },
    }
}
