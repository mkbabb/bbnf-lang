//! AW-III.W6.2 — PHF keyword dispatch activation test.
//!
//! Verifies that [`bbnf::backend::rust::emitter::keyword_dispatch::
//! emit_keyword_phf`] produces a callable PHF dispatcher for a
//! literal-led Alt with branch count >= threshold.

use bbnf::backend::rust::emitter::keyword_dispatch::{
    emit_keyword_phf, emit_keyword_tables, LiteralBranch, PHF_MIN_BRANCHES,
};

/// Below-threshold: the emitter returns `None` so the caller falls
/// back to linear Alt dispatch.
#[test]
fn below_threshold_returns_none() {
    let branches = vec![
        LiteralBranch { bytes: b"true".to_vec(), branch_idx: 0 },
        LiteralBranch { bytes: b"false".to_vec(), branch_idx: 1 },
    ];
    assert!(branches.len() < PHF_MIN_BRANCHES);
    let out = emit_keyword_phf("test_grammar", 0, &branches);
    assert!(out.is_none(), "Below threshold must not emit");
}

/// Above-threshold: the emitter produces a `TokenStream` containing
/// two static tables + one dispatch fn. Token stream text contains
/// recognisable identifiers so the activation is visible downstream.
#[test]
fn threshold_met_emits_phf_table() {
    // JSON literal triad isn't enough (3 branches); synthesise 5
    // branches to cross the PHF_MIN_BRANCHES gate.
    let branches: Vec<LiteralBranch> = ["true", "false", "null", "undef", "none"]
        .iter()
        .enumerate()
        .map(|(i, s)| LiteralBranch {
            bytes: s.as_bytes().to_vec(),
            branch_idx: i as u8,
        })
        .collect();
    assert!(branches.len() >= PHF_MIN_BRANCHES);
    let out = emit_keyword_phf("test_grammar", 42, &branches);
    let tokens = out.expect("Above threshold must emit").to_string();

    // The emission declares two statics + one fn; assert all three
    // are present and carry the rule_id suffix.
    assert!(tokens.contains("__PHF_test_grammar_42_KW"),
        "Emitted stream missing keyword table static: {}", tokens);
    assert!(tokens.contains("__PHF_test_grammar_42_IDX"),
        "Emitted stream missing branch-idx table: {}", tokens);
    assert!(tokens.contains("__phf_test_grammar_dispatch_42"),
        "Emitted stream missing dispatch fn: {}", tokens);
    // The dispatch lookup is binary_search-based.
    assert!(tokens.contains("binary_search"),
        "Emitted stream missing binary_search call: {}", tokens);
}

/// `emit_keyword_tables` is the grammar-level wrapper — accumulate
/// multiple rules' PHF tables into one token stream. Rules below the
/// threshold contribute nothing.
#[test]
fn grammar_level_accumulation() {
    let rule_a: Vec<LiteralBranch> = ["a", "b", "c", "d", "e", "f", "g"]
        .iter()
        .enumerate()
        .map(|(i, s)| LiteralBranch {
            bytes: s.as_bytes().to_vec(),
            branch_idx: i as u8,
        })
        .collect();
    let rule_b: Vec<LiteralBranch> = ["h", "i"]
        .iter()
        .enumerate()
        .map(|(i, s)| LiteralBranch {
            bytes: s.as_bytes().to_vec(),
            branch_idx: i as u8,
        })
        .collect();
    let tables: Vec<(u32, &[LiteralBranch])> = vec![
        (1, rule_a.as_slice()),
        (2, rule_b.as_slice()),
    ];
    let out = emit_keyword_tables("demo", tables).to_string();

    // rule_a crosses the threshold; rule_b does not.
    assert!(out.contains("__PHF_demo_1_KW"),
        "rule_a PHF table missing: {}", out);
    assert!(!out.contains("__PHF_demo_2_KW"),
        "rule_b below threshold must not emit: {}", out);
}
