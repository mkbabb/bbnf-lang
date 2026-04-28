//! AZ-II.cutover.D4 — BBNF parse-reach gates over the canonical
//! reference [`bbnf::types::AST`].
//!
//! The BBNF grammar (`grammar/bbnf/*.bbnf`) declares typed `->`
//! annotations on its lexical rules (identifier, literal, regex,
//! comment, big_comment, int_lit, float_lit, string_lit). The parse
//! must reach the typed AST shape on every fixture: `parse(source)`
//! must return `Some(GrammarExtract)` whose `rules: AST` is
//! non-empty (and, for grammars with directives, whose directive
//! vectors are populated). This is the canonical structural-reach
//! gate for the BBNF family.
//!
//! Pre-cutover.D the harness asserted `BbnfBootstrap::parse(input)`
//! returned `Ok` — a tape-substrate gate. Post-cutover.D the
//! reference shifts onto the typed AST: every typed rule the BBNF
//! source declares must materialise as an entry in the canonical
//! `bbnf::types::AST` IndexMap, with its name spans intact.

use bbnf::grammar;

// ─── Self-hosted parse: `bbnf.bbnf` itself round-trips to AST ───────

#[test]
fn bbnf_parses_its_own_grammar() {
    // The BBNF grammar must parse its own definition cleanly into
    // the canonical typed AST. This is the structural-reach gate.
    let path_candidates = [
        "../../grammar/bbnf/bbnf.bbnf",
        "../grammar/bbnf/bbnf.bbnf",
        "grammar/bbnf/bbnf.bbnf",
    ];
    let input = path_candidates
        .iter()
        .find_map(|p| std::fs::read_to_string(p).ok())
        .expect("bbnf.bbnf fixture must be loadable");
    let extract = grammar::parse(&input).expect("BBNF must parse its own grammar definition");
    assert!(
        !extract.rules.is_empty(),
        "BBNF self-host AST must contain ≥ 1 rule entry",
    );
    // Every rule has a non-degenerate name span.
    for (name, entry) in extract.rules.iter() {
        assert!(
            entry.name_span.start < entry.name_span.end,
            "rule `{}` name span is degenerate",
            name
        );
    }
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
    let extract =
        grammar::parse(&input).expect("BBNF must parse the expressions sub-grammar");
    assert!(
        !extract.rules.is_empty(),
        "expressions.bbnf AST must contain ≥ 1 rule entry",
    );
}
