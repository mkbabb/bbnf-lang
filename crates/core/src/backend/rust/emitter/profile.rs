//! Tranche AV Phase 1 — `GRAMMAR_PROFILE` const-literal emitter.
//!
//! Lowers [`bbnf_ir::passes::GrammarProfile`] (the owned precursor)
//! to a `const GRAMMAR_PROFILE: bbnf::runtime::tape::GrammarProfile =
//! GrammarProfile { ... };` declaration embedded in each grammar's
//! `generated.rs`, next to the grammar string array.
//!
//! The emitted literal is fully `const`-evaluable. Slice fields
//! reference `static` byte/struct arrays emitted immediately above
//! the profile literal in the same module — no runtime allocation,
//! no lazy initialisation. Every per-grammar codegen decision that
//! grammars differ on (tape capacity, structural alphabet, column
//! set, list rules, keyword tables, shape dict, branch priors,
//! dedup set, reorder visitors) reads the matching field.
//!
//! ## Slot population by wave
//!
//! | Slot | Populated in |
//! |------|--------------|
//! | push counts + per-byte density | V1 |
//! | structural alphabet + digraphs | V1 |
//! | active_columns | V2 (AW-IV.W1.δ projection live; mining in W3) |
//! | list_rules | V6 (AW-IV.W1.δ projection live; mining in W4.4) |
//! | keyword_tables | V7 (AW-III.W6.2 mining; AW-IV.W1.δ projection) |
//! | shape_dict | V5 (AV.5.4 mining; AW-IV.W1.δ projection) |
//! | branch_priors | V4 (AW-IV.W1.δ projection live; mining later) |
//! | dedup_eligible_rules | V8 (AW-IV.W1.δ projection live; AW-IV.W4.3 mining active) |
//! | reorder_unroll_visitors | V2 (AV.2.5) |
//!
//! # AW-IV.W1.δ — wire-contract closure
//!
//! Pre-W1.δ every consumer slot (`active_columns`, `list_rules`,
//! `keyword_tables`, `shape_dict`, `branch_priors`,
//! `dedup_eligible_rules`) landed in the emitted literal as `&[]`
//! even when the upstream IR mining produced data. This silently
//! dropped every mined value on its way to the runtime const — the
//! bug AW-IV.W1.δ fixes. Every previously-`&[]` slot now emits a
//! supporting `static __GRAMMAR_PROFILE_<NAME>: [<Elem>; N] = [...];`
//! (when the IR field is non-empty) and a `&__GRAMMAR_PROFILE_<NAME>`
//! reference into the const literal. Empty IR fields fall back to
//! `&[]` — the projection is still live; the empty set is the
//! legitimate current state for slots whose mining lands in W3/W4.

use bbnf_ir::passes::{BranchPriorIr, GrammarProfile, KeywordTableIr, ShapeEntryIr};
use proc_macro2::{Literal, TokenStream};
use quote::quote;

/// Emit the `GRAMMAR_PROFILE` const literal + its supporting
/// `static` arrays for one grammar.
///
/// Lives at module scope in `generated.rs`, immediately after the
/// `GRAMMAR_<Ident>` string array and before the view types + rule
/// functions.
///
/// AW-IV.W1.δ — every consumer slot in the emitted literal now
/// references a `static` array (populated from the IR projection)
/// or `&[]` (genuinely empty IR-side). The wire contract from IR
/// mining → [`GrammarIR::profile`] → this emitter → the `pub const
/// GRAMMAR_PROFILE` literal → runtime consumer is closed.
pub fn emit_grammar_profile(profile: &GrammarProfile) -> TokenStream {
    let push_compound_count = profile.push_compound_count;
    let push_leaf_count = profile.push_leaf_count;
    let push_leaf_with_count = profile.push_leaf_with_count;

    let compounds_per_input_byte = f32_literal(profile.compounds_per_input_byte);
    let leaves_per_input_byte = f32_literal(profile.leaves_per_input_byte);
    let payload_bytes_per_input_byte = f32_literal(profile.payload_bytes_per_input_byte);
    let expected_ns_per_byte = f32_literal(profile.expected_ns_per_byte);
    let parallel_break_even_bytes = profile.parallel_break_even_bytes;

    // Static byte arrays for the slice-valued fields. Empty alphabets
    // reference the shared `&[]` rather than an empty `static` — the
    // compiler accepts `&[]` as a `const` in the struct literal
    // without needing a placeholder array.
    let (alphabet_decl, alphabet_ref) = if profile.structural_alphabet.is_empty() {
        (TokenStream::new(), quote! { &[] })
    } else {
        let bytes: Vec<Literal> = profile
            .structural_alphabet
            .iter()
            .map(|b| Literal::u8_unsuffixed(*b))
            .collect();
        let len = profile.structural_alphabet.len();
        (
            quote! {
                static __GRAMMAR_PROFILE_ALPHABET: [u8; #len] = [#(#bytes),*];
            },
            quote! { &__GRAMMAR_PROFILE_ALPHABET },
        )
    };

    // AW-III.W5.d — `(u8, u8)` tuple shape so the same static feeds
    // both the tape-side profile field and the SIMD scanner alphabet's
    // `digraph_pairs` without a shim layer.
    let (digraphs_decl, digraphs_ref) = if profile.structural_digraphs.is_empty() {
        (TokenStream::new(), quote! { &[] })
    } else {
        let pairs = profile.structural_digraphs.iter().map(|(a, b)| {
            let a_lit = Literal::u8_unsuffixed(*a);
            let b_lit = Literal::u8_unsuffixed(*b);
            quote! { (#a_lit, #b_lit) }
        });
        let len = profile.structural_digraphs.len();
        (
            quote! {
                static __GRAMMAR_PROFILE_DIGRAPHS: [(u8, u8); #len] = [#(#pairs),*];
            },
            quote! { &__GRAMMAR_PROFILE_DIGRAPHS },
        )
    };

    // AW-III.W5.a — pre-computed digraph first-byte bitmap. The
    // `[u64; 4]` lays out as a const-evaluable inline literal; no
    // supporting `static`.
    let digraph_mask_words = profile
        .structural_digraph_mask
        .iter()
        .map(|w| Literal::u64_unsuffixed(*w));
    let digraph_mask_ref = quote! { [#(#digraph_mask_words),*] };

    // AW-III.W5.a — quote-class bytes (sorted, ASCII range).
    let (quote_classes_decl, quote_classes_ref) = if profile.structural_quote_classes.is_empty() {
        (TokenStream::new(), quote! { &[] })
    } else {
        let bytes: Vec<Literal> = profile
            .structural_quote_classes
            .iter()
            .map(|b| Literal::u8_unsuffixed(*b))
            .collect();
        let len = profile.structural_quote_classes.len();
        (
            quote! {
                static __GRAMMAR_PROFILE_QUOTE_CLASSES: [u8; #len] = [#(#bytes),*];
            },
            quote! { &__GRAMMAR_PROFILE_QUOTE_CLASSES },
        )
    };

    // AV.2.5 — one `VisitorId(i)` per descriptor in
    // `reorder_unroll_visitors`, positional so the id matches the
    // index of the emitted kernel in the grammar `impl` block.
    let (visitors_decl, visitors_ref) = if profile.reorder_unroll_visitors.is_empty() {
        (TokenStream::new(), quote! { &[] })
    } else {
        let ids = profile
            .reorder_unroll_visitors
            .iter()
            .enumerate()
            .map(|(idx, _)| {
                let idx_lit = Literal::u16_unsuffixed(idx as u16);
                quote! { ::bbnf::runtime::tape::VisitorId(#idx_lit) }
            });
        let len = profile.reorder_unroll_visitors.len();
        (
            quote! {
                static __GRAMMAR_PROFILE_VISITORS:
                    [::bbnf::runtime::tape::VisitorId; #len] = [#(#ids),*];
            },
            quote! { &__GRAMMAR_PROFILE_VISITORS },
        )
    };

    // AW-IV.W1.δ — `active_columns` projection. The mining pass is
    // scheduled in W3 (columnar substrate); today the IR field is
    // empty so the reference lowers to `&[]`.
    let (active_columns_decl, active_columns_ref) =
        emit_active_columns(&profile.active_columns);

    // AW-IV.W1.δ — `list_rules` projection. Mining lands in W4.4
    // (document-level parallel parse); IR-side empty today.
    let (list_rules_decl, list_rules_ref) = emit_list_rules(&profile.list_rules);

    // AW-IV.W1.δ — `keyword_tables` projection. Populated from the
    // `KeywordStatsMiner` via [`GrammarIR::profile`]. Each entry
    // references a per-entry `static __GRAMMAR_PROFILE_KW_<i>: [&[u8]; N]`
    // array holding the sorted keyword byte slices.
    let (keyword_tables_decl, keyword_tables_ref) =
        emit_keyword_tables(&profile.keyword_tables);

    // AW-IV.W1.δ — `shape_dict` projection. Populated from the
    // `ShapeDictMiner` + `solve_shape_dict_selection` via
    // [`GrammarIR::profile`]. Each entry references per-entry
    // `static __GRAMMAR_PROFILE_SHAPE_<i>_KINDS: [u8; N]` +
    // `__GRAMMAR_PROFILE_SHAPE_<i>_OFFSETS: [u16; N]` arrays.
    let (shape_dict_decl, shape_dict_ref) = emit_shape_dict(&profile.shape_dict);

    // AW-IV.W1.δ — `branch_priors` projection. Mining lands in V4
    // (speculative Alt dispatch); IR-side empty today.
    let (branch_priors_decl, branch_priors_ref) = emit_branch_priors(&profile.branch_priors);

    // AW-IV.W1.δ wire + AW-IV.W4.3 mining active — the emitter now
    // lowers `GrammarProfile::dedup_eligible_rules` to the per-grammar
    // `__GRAMMAR_PROFILE_DEDUP_RULES: [RuleId; N]` static array.
    // Consumed at parse time by the walker's compound-emit arm for
    // dedup-eligible rules via `BloomDedup::try_dedup`.
    let (dedup_decl, dedup_ref) = emit_dedup_eligible_rules(&profile.dedup_eligible_rules);

    quote! {
        #alphabet_decl
        #digraphs_decl
        #quote_classes_decl
        #visitors_decl
        #active_columns_decl
        #list_rules_decl
        #keyword_tables_decl
        #shape_dict_decl
        #branch_priors_decl
        #dedup_decl

        /// Per-grammar codegen fingerprint — consolidated static
        /// profile emitted by Tranche AV Phase 1. Every downstream
        /// consumer (tape capacity, scanner dispatch, column-set
        /// selection, reorder visitors, keyword tables, shape
        /// dictionary, runtime dedup) reads the matching field.
        ///
        /// AW-IV.W1.δ — the wire contract from IR mining through this
        /// const literal to the runtime consumer is closed. Every
        /// `&[...]` slice reference below lowers to a static array
        /// emitted above when the IR-side field has mined data, or
        /// `&[]` when the upstream mining is genuinely empty.
        pub const GRAMMAR_PROFILE: ::bbnf::runtime::tape::GrammarProfile =
            ::bbnf::runtime::tape::GrammarProfile {
                push_compound_count: #push_compound_count,
                push_leaf_count: #push_leaf_count,
                push_leaf_with_count: #push_leaf_with_count,
                compounds_per_input_byte: #compounds_per_input_byte,
                leaves_per_input_byte: #leaves_per_input_byte,
                payload_bytes_per_input_byte: #payload_bytes_per_input_byte,
                expected_ns_per_byte: #expected_ns_per_byte,
                parallel_break_even_bytes: #parallel_break_even_bytes,
                structural_alphabet: #alphabet_ref,
                structural_digraphs: #digraphs_ref,
                structural_digraph_mask: #digraph_mask_ref,
                structural_quote_classes: #quote_classes_ref,
                active_columns: #active_columns_ref,
                list_rules: #list_rules_ref,
                keyword_tables: #keyword_tables_ref,
                shape_dict: #shape_dict_ref,
                branch_priors: #branch_priors_ref,
                dedup_eligible_rules: #dedup_ref,
                reorder_unroll_visitors: #visitors_ref,
            };
    }
}

// ── Per-slot emitters ─────────────────────────────────────────────────

/// AW-IV.W1.δ — emit `static __GRAMMAR_PROFILE_ACTIVE_COLUMNS:
/// [ColumnId; N] = [...]` and a `&__GRAMMAR_PROFILE_ACTIVE_COLUMNS`
/// reference, or `(empty, &[])` when the IR-side slot is empty.
fn emit_active_columns(columns: &[u16]) -> (TokenStream, TokenStream) {
    if columns.is_empty() {
        return (TokenStream::new(), quote! { &[] });
    }
    let ids = columns.iter().map(|c| {
        let lit = Literal::u16_unsuffixed(*c);
        quote! { ::bbnf::runtime::tape::ColumnId(#lit) }
    });
    let len = columns.len();
    (
        quote! {
            static __GRAMMAR_PROFILE_ACTIVE_COLUMNS:
                [::bbnf::runtime::tape::ColumnId; #len] = [#(#ids),*];
        },
        quote! { &__GRAMMAR_PROFILE_ACTIVE_COLUMNS },
    )
}

/// AW-IV.W1.δ — emit `static __GRAMMAR_PROFILE_LIST_RULES:
/// [RuleId; N] = [...]` and a `&__GRAMMAR_PROFILE_LIST_RULES`
/// reference, or `(empty, &[])` when the IR-side slot is empty.
fn emit_list_rules(rules: &[u32]) -> (TokenStream, TokenStream) {
    if rules.is_empty() {
        return (TokenStream::new(), quote! { &[] });
    }
    let ids = rules.iter().map(|r| {
        let lit = Literal::u32_unsuffixed(*r);
        quote! { ::bbnf::runtime::tape::RuleId(#lit) }
    });
    let len = rules.len();
    (
        quote! {
            static __GRAMMAR_PROFILE_LIST_RULES:
                [::bbnf::runtime::tape::RuleId; #len] = [#(#ids),*];
        },
        quote! { &__GRAMMAR_PROFILE_LIST_RULES },
    )
}

/// AW-IV.W1.δ — emit `static __GRAMMAR_PROFILE_KEYWORD_TABLES:
/// [KeywordTable; N] = [...]` plus one `static
/// __GRAMMAR_PROFILE_KW_<i>: [&[u8]; M] = [...]` per entry holding
/// the per-rule sorted keyword byte slices, and a reference into
/// the resulting table. Empty IR-side slot → `(empty, &[])`.
fn emit_keyword_tables(tables: &[KeywordTableIr]) -> (TokenStream, TokenStream) {
    if tables.is_empty() {
        return (TokenStream::new(), quote! { &[] });
    }
    let mut support = TokenStream::new();
    let mut entries: Vec<TokenStream> = Vec::with_capacity(tables.len());
    for (idx, table) in tables.iter().enumerate() {
        let kw_ident = quote::format_ident!("__GRAMMAR_PROFILE_KW_{}", idx);
        let kw_lits: Vec<TokenStream> = table
            .keywords
            .iter()
            .map(|bytes| {
                let lit = Literal::byte_string(bytes);
                quote! { #lit }
            })
            .collect();
        let kw_len = table.keywords.len();
        support.extend(quote! {
            static #kw_ident: [&[u8]; #kw_len] = [#(#kw_lits),*];
        });
        let rule_lit = Literal::u32_unsuffixed(table.rule_id);
        entries.push(quote! {
            ::bbnf::runtime::tape::KeywordTable {
                rule: ::bbnf::runtime::tape::RuleId(#rule_lit),
                keywords: &#kw_ident,
            }
        });
    }
    let table_len = entries.len();
    (
        quote! {
            #support
            static __GRAMMAR_PROFILE_KEYWORD_TABLES:
                [::bbnf::runtime::tape::KeywordTable; #table_len] = [
                #(#entries),*
            ];
        },
        quote! { &__GRAMMAR_PROFILE_KEYWORD_TABLES },
    )
}

/// AW-IV.W1.δ — emit `static __GRAMMAR_PROFILE_SHAPE_DICT:
/// [ShapeEntry; N]` plus per-entry `__GRAMMAR_PROFILE_SHAPE_<i>_KINDS`
/// and `__GRAMMAR_PROFILE_SHAPE_<i>_OFFSETS` support arrays, and a
/// reference into the table. Empty IR-side slot → `(empty, &[])`.
///
/// Mirrors the existing `emit_shape_dict_arrays` in
/// [`super::dta`] — the shape is canonical. Both emitters walk the
/// same IR source data (`shape_dict_selection` → `shape_dict_templates`)
/// so they produce identical entries; the profile's reference goes
/// through this dedicated projection (and its local statics) rather
/// than reaching cross-module for the existing `SHAPE_DICT`, to keep
/// `emit_grammar_profile` self-contained. When both emitters run for
/// the same grammar the duplicate data lives in `.rodata` without
/// runtime cost.
fn emit_shape_dict(entries: &[ShapeEntryIr]) -> (TokenStream, TokenStream) {
    if entries.is_empty() {
        return (TokenStream::new(), quote! { &[] });
    }
    let mut support = TokenStream::new();
    let mut entry_literals: Vec<TokenStream> = Vec::with_capacity(entries.len());
    for (idx, entry) in entries.iter().enumerate() {
        let kinds_ident = quote::format_ident!("__GRAMMAR_PROFILE_SHAPE_{}_KINDS", idx);
        let offsets_ident = quote::format_ident!("__GRAMMAR_PROFILE_SHAPE_{}_OFFSETS", idx);
        let kinds_len = entry.child_kinds.len();
        let offsets_len = entry.leaf_payload_offsets.len();
        let kind_lits = entry.child_kinds.iter().map(|b| Literal::u8_unsuffixed(*b));
        let offset_lits = entry
            .leaf_payload_offsets
            .iter()
            .map(|o| Literal::u16_unsuffixed(*o));
        support.extend(quote! {
            static #kinds_ident: [u8; #kinds_len] = [#(#kind_lits),*];
            static #offsets_ident: [u16; #offsets_len] = [#(#offset_lits),*];
        });
        let shape_hash_lit = Literal::u64_unsuffixed(entry.shape_hash);
        let rule_lit = Literal::u32_unsuffixed(entry.rule_id);
        let payload_bytes_lit = Literal::u16_unsuffixed(entry.payload_bytes);
        entry_literals.push(quote! {
            ::bbnf::runtime::tape::ShapeEntry {
                shape_hash: #shape_hash_lit,
                rule: ::bbnf::runtime::tape::RuleId(#rule_lit),
                child_kinds: &#kinds_ident,
                leaf_payload_offsets: &#offsets_ident,
                payload_bytes: #payload_bytes_lit,
            }
        });
    }
    let table_len = entry_literals.len();
    (
        quote! {
            #support
            static __GRAMMAR_PROFILE_SHAPE_DICT:
                [::bbnf::runtime::tape::ShapeEntry; #table_len] = [
                #(#entry_literals),*
            ];
        },
        quote! { &__GRAMMAR_PROFILE_SHAPE_DICT },
    )
}

/// AW-IV.W1.δ — emit `static __GRAMMAR_PROFILE_BRANCH_PRIORS:
/// [BranchPrior; N] = [...]` and a reference, or `(empty, &[])`
/// when the IR-side slot is empty.
fn emit_branch_priors(priors: &[BranchPriorIr]) -> (TokenStream, TokenStream) {
    if priors.is_empty() {
        return (TokenStream::new(), quote! { &[] });
    }
    let entries = priors.iter().map(|p| {
        let rule_lit = Literal::u32_unsuffixed(p.rule_id);
        let branch_lit = Literal::u16_unsuffixed(p.branch_idx);
        let weight_lit = Literal::u8_unsuffixed(p.weight_q8);
        quote! {
            ::bbnf::runtime::tape::BranchPrior {
                rule: ::bbnf::runtime::tape::RuleId(#rule_lit),
                branch_idx: #branch_lit,
                weight_q8: #weight_lit,
            }
        }
    });
    let len = priors.len();
    (
        quote! {
            static __GRAMMAR_PROFILE_BRANCH_PRIORS:
                [::bbnf::runtime::tape::BranchPrior; #len] = [#(#entries),*];
        },
        quote! { &__GRAMMAR_PROFILE_BRANCH_PRIORS },
    )
}

/// AW-IV.W1.δ — emit `static __GRAMMAR_PROFILE_DEDUP_RULES:
/// [RuleId; N] = [...]` and a reference, or `(empty, &[])` when the
/// IR-side slot is empty.
fn emit_dedup_eligible_rules(rules: &[u32]) -> (TokenStream, TokenStream) {
    if rules.is_empty() {
        return (TokenStream::new(), quote! { &[] });
    }
    let ids = rules.iter().map(|r| {
        let lit = Literal::u32_unsuffixed(*r);
        quote! { ::bbnf::runtime::tape::RuleId(#lit) }
    });
    let len = rules.len();
    (
        quote! {
            static __GRAMMAR_PROFILE_DEDUP_RULES:
                [::bbnf::runtime::tape::RuleId; #len] = [#(#ids),*];
        },
        quote! { &__GRAMMAR_PROFILE_DEDUP_RULES },
    )
}

/// Emit a `f32` literal that round-trips through `proc_macro2` into
/// generated code without losing precision. `quote!` renders
/// `f32` values as untyped floats which trip "ambiguous numeric
/// type" errors inside the const literal; an explicit typed suffix
/// pins the type.
fn f32_literal(value: f32) -> TokenStream {
    let lit = Literal::f32_suffixed(value);
    quote! { #lit }
}
