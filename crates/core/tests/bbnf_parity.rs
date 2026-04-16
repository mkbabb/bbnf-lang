//! BBNF (self-hosted bootstrap) parse-reach gates.
//!
//! The BBNF grammar (`grammar/bbnf/*.bbnf`) declares typed `->`
//! annotations on its lexical rules (identifier, literal, regex,
//! comment, big_comment, int_lit, float_lit, string_lit). AV.0.1 +
//! AV.0.2 + AV.0.3 landed the codegen emissions so every annotated
//! rule reaches the tape with its declared payload.
//!
//! This file retains the structural-reach gates that are independent
//! of concrete variant indices. The variant-index-dispatched
//! `VAR_*` ledger was fossilised against un-fused IR rule IDs;
//! post-W2.3 activation of `inline_acyclic` + `fuse_single_use`
//! renumbers surviving rules wholesale, so variant-id dispatch is no
//! longer a stable parity signal.

use bbnf::grammar::generated::BbnfBootstrap;

// ─── Self-hosted parse: `bbnf.bbnf` itself round-trips ───────────────

#[test]
fn bbnf_parses_its_own_grammar() {
    // The BBNF grammar must parse its own definition cleanly. This is
    // the canonical structural-reach gate for the BBNF family.
    let path_candidates = [
        "../../grammar/bbnf/bbnf.bbnf",
        "../grammar/bbnf/bbnf.bbnf",
        "grammar/bbnf/bbnf.bbnf",
    ];
    let input = path_candidates
        .iter()
        .find_map(|p| std::fs::read_to_string(p).ok())
        .expect("bbnf.bbnf fixture must be loadable");
    let parsed = BbnfBootstrap::parse(&input);
    assert!(
        parsed.is_ok(),
        "BBNF must parse its own grammar definition: {:?}",
        parsed.err()
    );
}

#[test]
fn bbnf_parses_expressions_grammar() {
    let path_candidates = [
        "../../grammar/bbnf/expressions.bbnf",
        "../grammar/bbnf/expressions.bbnf",
        "grammar/bbnf/expressions.bbnf",
    ];
    let input = path_candidates
        .iter()
        .find_map(|p| std::fs::read_to_string(p).ok())
        .expect("expressions.bbnf fixture must be loadable");
    let parsed = BbnfBootstrap::parse(&input);
    assert!(
        parsed.is_ok(),
        "BBNF must parse the expressions sub-grammar: {:?}",
        parsed.err()
    );
}
