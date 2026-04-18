//! AW-V.W5.1 — BBNF bootstrap `GRAMMAR_PROFILE` wire-contract test.
//!
//! # Architectural role
//!
//! Per the AW-V.W5.1 hard gate (`docs/tranches/AW/AW-V.md` §W5.1), the
//! self-hosted BBNF parser emitted by
//! `crates/bootstrap/src/lib.rs`
//! (`#[parser(path = "...", structural)]`) must carry every
//! `GrammarProfile` slot through from IR mining to the runtime const
//! literal in `crates/core/src/grammar/generated.rs`. Pre-W5.1 the
//! pipeline gated every miner pass on `!options.structural`, which
//! silently dropped every `GrammarProfile` slot for the BBNF
//! bootstrap — `structural_alphabet`, `structural_digraphs`,
//! `keyword_tables`, `shape_dict` all emitted as `&[]` despite the
//! upstream mining (over a non-structural-mode IR of the same
//! grammar) producing non-empty sets.
//!
//! This test closes the wire contract at the end-to-end granularity
//! demanded by `docs/instructions/README.md` §architecture-invariants
//! §wire-contract-pipelines:
//!
//!     IR mining → IR pass → emitter pass → `pub const` literal → runtime consumer
//!
//! The fixture is the *actual* bootstrap-generated `GRAMMAR_PROFILE`
//! const from the checked-in `generated.rs`. A mining-side
//! assertion or an emitter-side assertion alone is insufficient —
//! the projection silently drops data when only one boundary is
//! asserted. This test reads the bootstrap const directly.
//!
//! # Companion coverage
//!
//! `crates/core/tests/grammar_profile_wire_contract.rs` exercises
//! the non-structural `#[derive(Parser)]` path for JSON / BBNF /
//! CSS L4 / Sheets fixtures. Per W5.1 the BBNF bootstrap path
//! (structural mode) now flows the same mined data — this test
//! asserts that flow at the bootstrap's own const symbol, not a
//! sibling derive of the same grammar.
//!
//! Both files assert the same invariant for the same grammar; both
//! catching a silent drop in different code paths is the point.

use bbnf::grammar::generated::BbnfBootstrap;

/// The bootstrap's `GRAMMAR_PROFILE` const — the symbol the BBNF
/// parser's `parse()` entry point consumes for capacity sizing,
/// structural-alphabet dispatch, keyword-table lookup, and
/// shape-ref dedup (at runtime, per
/// `crates/bbnf-tape/src/profile.rs`).
///
/// AW-IV.W1.δ — `BbnfBootstrap::GRAMMAR_PROFILE` is the associated-
/// constant accessor emitted by the grammar codegen, aliasing the
/// module-scope `pub const GRAMMAR_PROFILE` in
/// `generated.rs`. Both names resolve to the same bytes in
/// `.rodata`; this test accesses through the associated-constant
/// form because that is the symbol downstream consumers read when
/// multiple grammars coexist.
fn bootstrap_profile() -> &'static bbnf::runtime::tape::GrammarProfile {
    &BbnfBootstrap::GRAMMAR_PROFILE
}

// ── Per-slot wire-contract assertions ──────────────────────────────────

#[test]
fn bootstrap_structural_alphabet_carries_bbnf_delimiters() {
    let prof = bootstrap_profile();
    let alph = prof.structural_alphabet;

    // AW-V.W5.1 — pre-fix this slot was `&[]`. Post-fix the miner
    // runs in structural mode and emits the BBNF delimiter set.
    assert!(
        !alph.is_empty(),
        "BBNF bootstrap structural_alphabet must be non-empty; got {:?}",
        alph
    );

    // BBNF's canonical single-byte delimiters per the grammar:
    //   rule terminator `;`, assignment `=`, alternation `|`,
    //   concatenation `,`. These are the load-bearing structural
    //   bytes that drive scanner kernels at runtime.
    for expected in [b'=', b';', b'|', b','] {
        assert!(
            alph.contains(&expected),
            "BBNF bootstrap structural_alphabet must contain {:?}; \
             got {:?}",
            expected as char,
            alph
        );
    }
}

#[test]
fn bootstrap_structural_digraphs_carry_arrow_and_comment_opener() {
    let prof = bootstrap_profile();
    let digraphs = prof.structural_digraphs;

    // AW-V.W5.1 — pre-fix this slot was `&[]`. Post-fix the
    // structural-alphabet miner's digraph channel fires.
    assert!(
        !digraphs.is_empty(),
        "BBNF bootstrap structural_digraphs must be non-empty; got {:?}",
        digraphs
    );

    // BBNF's load-bearing digraphs per the grammar source:
    //   - `->` (map-expr arrow, every `factor -> value_expr`)
    //   - `/*` (block-comment opener, every `big_comment`)
    //   - `*/` (block-comment closer, same)
    //   - `//` (line-comment opener, every `comment`)
    //
    // The arrow is particularly load-bearing because it gates
    // every typed-materialisation decision in the lowering.
    assert!(
        digraphs.contains(&(b'-', b'>')),
        "BBNF bootstrap structural_digraphs must mine `->`; got {:?}",
        digraphs
    );
    assert!(
        digraphs.contains(&(b'/', b'*')),
        "BBNF bootstrap structural_digraphs must mine `/*`; got {:?}",
        digraphs
    );
    assert!(
        digraphs.contains(&(b'*', b'/')),
        "BBNF bootstrap structural_digraphs must mine `*/`; got {:?}",
        digraphs
    );
    assert!(
        digraphs.contains(&(b'/', b'/')),
        "BBNF bootstrap structural_digraphs must mine `//`; got {:?}",
        digraphs
    );
}

#[test]
fn bootstrap_structural_digraph_mask_is_consistent_with_digraphs() {
    let prof = bootstrap_profile();
    let digraphs = prof.structural_digraphs;
    let mask = prof.structural_digraph_mask;

    // The 256-bit bitmap packs the first-byte of every digraph
    // pair. Word `i` covers bytes `64*i .. 64*(i+1)`. Every
    // digraph's first byte must have its bit set.
    for &(first, _second) in digraphs {
        let word = (first / 64) as usize;
        let bit = first % 64;
        let set = (mask[word] & (1u64 << bit)) != 0;
        assert!(
            set,
            "BBNF bootstrap structural_digraph_mask must set bit for \
             digraph-first byte {:?} (word {}, bit {}); got mask={:?}, \
             digraphs={:?}",
            first as char, word, bit, mask, digraphs
        );
    }

    // Non-emptiness invariant — if digraphs is non-empty the mask
    // must have at least one bit set.
    if !digraphs.is_empty() {
        let any_set = mask.iter().any(|w| *w != 0);
        assert!(
            any_set,
            "BBNF bootstrap structural_digraph_mask must have at least \
             one bit set when digraphs non-empty; got mask={:?}, \
             digraphs={:?}",
            mask, digraphs
        );
    }
}

#[test]
fn bootstrap_keyword_tables_carry_directive_keywords() {
    let prof = bootstrap_profile();
    let tables = prof.keyword_tables;

    // AW-V.W5.1 — pre-fix this slot was `&[]`. Post-fix the
    // `KeywordStatsMiner` runs and surfaces the directive-prefix
    // Alt (`@import` / `@pretty` / `@ws` / `@token` / `@recover` /
    // `@debug` / `@host`) as a keyword table.
    assert!(
        !tables.is_empty(),
        "BBNF bootstrap keyword_tables must be non-empty; got len={}",
        tables.len()
    );

    // At least one table must carry the seven directive keywords.
    // The miner sorts + dedupes its output (per `GrammarIR::profile`),
    // so the match is against the sorted set.
    let expected_directives: &[&[u8]] = &[
        b"@debug",
        b"@host",
        b"@import",
        b"@pretty",
        b"@recover",
        b"@token",
        b"@ws",
    ];
    let has_directive_table = tables.iter().any(|t| {
        expected_directives
            .iter()
            .all(|kw| t.keywords.iter().any(|k| *k == *kw))
    });
    assert!(
        has_directive_table,
        "BBNF bootstrap keyword_tables must mine the directive Alt \
         branches ({:?}); got {:?}",
        expected_directives,
        tables
            .iter()
            .map(|t| (
                t.rule.0,
                t.keywords
                    .iter()
                    .map(|kw| std::str::from_utf8(kw).unwrap_or("<non-utf8>"))
                    .collect::<Vec<_>>()
            ))
            .collect::<Vec<_>>()
    );
}

#[test]
fn bootstrap_shape_dict_entries_are_structurally_sound() {
    let prof = bootstrap_profile();
    let entries = prof.shape_dict;

    // AW-V.W5.1 — pre-fix this slot was `&[]`. Post-fix the
    // `ShapeDictMiner` + `solve_shape_dict_selection` run and
    // surface the compound-collapse candidates for BBNF.
    assert!(
        !entries.is_empty(),
        "BBNF bootstrap shape_dict must be non-empty; got len={}",
        entries.len()
    );

    // Structural health: every entry must carry a non-zero
    // shape_hash, a non-empty child_kinds slice, and matching-
    // length child_kinds / leaf_payload_offsets parallel arrays.
    for (idx, entry) in entries.iter().enumerate() {
        assert_ne!(
            entry.shape_hash, 0,
            "BBNF bootstrap shape_dict[{}] must carry non-zero shape_hash",
            idx
        );
        assert!(
            !entry.child_kinds.is_empty(),
            "BBNF bootstrap shape_dict[{}] must carry non-empty child_kinds",
            idx
        );
        assert_eq!(
            entry.child_kinds.len(),
            entry.leaf_payload_offsets.len(),
            "BBNF bootstrap shape_dict[{}] child_kinds.len() must match \
             leaf_payload_offsets.len()",
            idx
        );
    }
}

#[test]
fn bootstrap_list_rules_admits_grammar_entry() {
    let prof = bootstrap_profile();
    let rules = prof.list_rules;

    // AW-IV.W4.4 — `mine_list_rules` admits the grammar's entry
    // rule when its body (after stripping transparent wrappers) is
    // a Repeat over an Alt / Ref / Seq. BBNF's `grammar` entry
    // rule IS list-shaped: `grammar = ( grammar_item ?w ) *`. This
    // slot was already populated pre-W5.1 because
    // `mine_list_rules` ran in profile() regardless of structural
    // mode; W5.1 preserves that flow and this test pins it.
    assert!(
        !rules.is_empty(),
        "BBNF bootstrap list_rules must admit the grammar entry rule; \
         got {:?}",
        rules
    );
}

#[test]
fn bootstrap_push_fingerprint_is_non_trivial() {
    let prof = bootstrap_profile();

    // `PushFingerprint` ran pre-W5.1 (in `analyze_grammar`, not
    // gated on structural mode); W5.1 preserves that and this
    // test pins the non-zero push-site count. A regression on the
    // non-slot fields would surface here.
    assert!(
        prof.total_push_sites() > 0,
        "BBNF bootstrap push fingerprint must be non-trivial; got {}",
        prof.total_push_sites()
    );
}

// ── Wire-contract end-to-end: runtime consumer reads the const ─────────

#[test]
fn bootstrap_runtime_capacity_reads_profile_density() {
    let prof = bootstrap_profile();

    // The runtime `GrammarProfile::capacity_for` consumer reads
    // `compounds_per_input_byte + leaves_per_input_byte` (per
    // `crates/bbnf-tape/src/profile.rs`). This exercise is the
    // "runtime invocation consumes the const non-trivially" side
    // of the wire contract (per `docs/instructions/README.md`
    // §wave-verification-ledger — a runtime invocation must
    // consume the const, not just read-and-discard).
    let cap_small = prof.capacity_for(128);
    let cap_large = prof.capacity_for(128 * 1024);
    assert!(
        cap_large > cap_small,
        "BBNF bootstrap capacity_for(128 KiB) > capacity_for(128 B); \
         got small={}, large={}",
        cap_small,
        cap_large
    );

    // The AR-audit floor guarantees `cap >= input_len / 2 + 2`
    // even for sparse grammars; BBNF's 53-compound push
    // fingerprint places it above that floor.
    assert!(
        cap_small >= 128 / 2 + 2,
        "BBNF bootstrap capacity_for(128) must meet AR-audit floor; \
         got {}",
        cap_small
    );
}

// ── Universal slot-reachability ────────────────────────────────────────

#[test]
fn bootstrap_every_slot_is_reachable() {
    let prof = bootstrap_profile();

    // Type-level reachability: the struct's every slot compiles
    // against the bootstrap's const symbol. If the emitter stops
    // emitting a slot or the runtime struct drifts, the compiler
    // catches it — this test is the canary.
    let _ = prof.structural_alphabet;
    let _ = prof.structural_digraphs;
    let _ = prof.structural_digraph_mask;
    let _ = prof.structural_quote_classes;
    let _ = prof.active_columns;
    let _ = prof.list_rules;
    let _ = prof.keyword_tables;
    let _ = prof.shape_dict;
    let _ = prof.branch_priors;
    let _ = prof.dedup_eligible_rules;
    let _ = prof.reorder_unroll_visitors;
    let _ = prof.push_compound_count;
    let _ = prof.push_leaf_count;
    let _ = prof.push_leaf_with_count;
    let _ = prof.compounds_per_input_byte;
    let _ = prof.leaves_per_input_byte;
    let _ = prof.payload_bytes_per_input_byte;
    let _ = prof.expected_ns_per_byte;
    let _ = prof.parallel_break_even_bytes;
}
