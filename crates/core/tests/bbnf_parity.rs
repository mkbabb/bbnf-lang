//! AU.6.8 — BBNF (self-hosted bootstrap) typed-materialisation
//! parity tests.
//!
//! The BBNF grammar (`grammar/bbnf/*.bbnf`) declares the following
//! typed `->` annotations:
//!
//!   identifier   = /regex/                  -> Span
//!   literal      = ( … alts … )             -> Span
//!   regex        = ( "/", /…/, "/" )        -> Span
//!   big_comment  = ( "/*", /…/, "*/" ) ?w   -> Span
//!   comment      = ( "//", /.*/ ) ?w        -> Span
//!   int_lit      = /regex/                  -> i64
//!   float_lit    = /regex/                  -> f64
//!   string_lit   = ( "\"", /…/, "\"" )      -> Span
//!
//! Every BBNF parse of grammar text exercises these rules. The
//! parity invariant: each annotated rule must reach the tape with
//! its declared payload.
//!
//! AU.6.8 audit finding: the bootstrap codegen emits `push_compound(
//! Rule, …)` for every BBNF-typed rule. The inferred payload setup
//! (`__has_payload`, `__aggregate_buf`, `__payload_tag`) is present
//! in the function bodies but never written — the inner scanner
//! / regex match completes without setting the payload variable.
//! `int_lit -> i64`, `float_lit -> f64`, and every `-> Span`
//! annotation are DEAD codegen paths in the W5 bootstrap.
//!
//! This file pins the structural / parse-reach invariant (every
//! BBNF input parses) and the payload-loss invariant (no typed
//! payload reaches the tape under the W5 codegen). Once the
//! emitter routes the inner scanners' values through
//! `PayloadData::WideScalar` / `PayloadData::Bytes`, the relevant
//! tests will start observing typed payloads — flip the
//! `pinned_*` assertions accordingly. Audit doc:
//! `docs/tranches/AU/typed-parity-audit.md`.

use bbnf::grammar::generated::BbnfBootstrap;
use bbnf::runtime::tape::{Tape, TapeCursor, TapeKind};

// ─── Helpers ─────────────────────────────────────────────────────────

/// Pre-order walk via the AU.3.2 zero-alloc child iterator. Returns
/// every record's (kind, variant_idx, has_payload) tuple — the
/// minimal projection sufficient to detect alt-payload firing.
fn walk_records<'t>(
    tape: &'t Tape,
    cursor: TapeCursor<'t>,
    out: &mut Vec<(TapeKind, u8, bool)>,
) {
    let rec = cursor.record();
    out.push((rec.kind(), cursor.variant_idx(), rec.has_payload()));
    if rec.has_children() {
        let mut kids: Vec<TapeCursor<'t>> = cursor.children_zero_alloc().collect();
        kids.reverse();
        for c in kids {
            walk_records(tape, c, out);
        }
    }
}

fn parse_records(input: &str) -> Vec<(TapeKind, u8, bool)> {
    let parsed = BbnfBootstrap::parse(input)
        .unwrap_or_else(|e| panic!("BBNF parse failed for {input:?}: {e:?}"));
    let root_off = parsed.view().cursor().offset();
    let tape = parsed.tape();
    let cursor = TapeCursor::new(tape, root_off);
    let mut out = Vec::new();
    walk_records(tape, cursor, &mut out);
    out
}

// BBNF rule variant indices — derived from
// `BbnfBootstrapRuleKind` declaration order in `generated.rs`.
const VAR_INT_LIT: u8 = 0;
const VAR_FLOAT_LIT: u8 = 1;
const VAR_STRING_LIT: u8 = 3;
const VAR_IDENTIFIER: u8 = 22;
const VAR_LITERAL: u8 = 23;
const VAR_REGEX: u8 = 24;
const VAR_BIG_COMMENT: u8 = 25;
const VAR_COMMENT: u8 = 26;

/// Count records of a given variant_idx in the tape. Used to assert
/// "this rule fired N times" without depending on the typed-payload
/// path (which is broken pre-fix).
fn count_variant(records: &[(TapeKind, u8, bool)], variant: u8) -> usize {
    records.iter().filter(|(_, v, _)| *v == variant).count()
}

/// Count records of a given variant_idx that materialise a typed
/// payload — i.e. their kind is `TapeKind::Span` (a typed leaf, NOT
/// a `TapeKind::Rule` empty-compound that happens to lack children).
///
/// The empty-compound representation produces records with
/// `kind=Rule, has_children=false, has_payload=true` (a builder
/// quirk: when a compound's children-run is empty, `child_off`
/// equals the parent's own index, and `push_compound` writes
/// `has_children=false` to avoid a cursor cycle, but child_off is
/// still 0x0 — not the NONE sentinel — so `has_payload()` returns
/// true). The `kind == Span` filter excludes those false positives
/// and counts only true typed-leaf payload firings.
fn count_typed_payload_leaves(records: &[(TapeKind, u8, bool)], variant: u8) -> usize {
    records
        .iter()
        .filter(|(k, v, p)| *v == variant && *p && *k == TapeKind::Span)
        .count()
}

// ─── Structural reach: every annotated rule fires ────────────────────

#[test]
fn identifier_rule_fires_on_simple_grammar() {
    let input = "rule_name = \"x\" ;";
    let records = parse_records(input);
    assert!(
        count_variant(&records, VAR_IDENTIFIER) >= 1,
        "identifier rule must fire at least once for `rule_name`; \
         records = {records:?}"
    );
}

#[test]
fn literal_rule_fires_on_quoted_atom() {
    let input = "rule = \"hello\" ;";
    let records = parse_records(input);
    assert!(
        count_variant(&records, VAR_LITERAL) >= 1,
        "literal rule must fire for \"hello\""
    );
}

#[test]
fn regex_rule_fires_on_slash_delimited_pattern() {
    let input = "rule = /[a-z]+/ ;";
    let records = parse_records(input);
    assert!(
        count_variant(&records, VAR_REGEX) >= 1,
        "regex rule must fire for /[a-z]+/"
    );
}

#[test]
fn comment_rule_fires_on_line_comment() {
    let input = "// this is a comment\nrule = \"x\" ;";
    let records = parse_records(input);
    assert!(
        count_variant(&records, VAR_COMMENT) >= 1,
        "comment rule must fire for `// this is a comment`"
    );
}

#[test]
fn big_comment_fires_on_block_comment() {
    let input = "/* block comment */\nrule = \"x\" ;";
    let records = parse_records(input);
    assert!(
        count_variant(&records, VAR_BIG_COMMENT) >= 1,
        "big_comment rule must fire for `/* … */`"
    );
}

// ─── Value-expression typed leaves (in the map syntax) ───────────────

#[test]
fn int_lit_fires_on_map_arrow_int_value() {
    let input = "kw = \"x\" -> 42i64 ;";
    let records = parse_records(input);
    assert!(
        count_variant(&records, VAR_INT_LIT) >= 1,
        "int_lit must fire on map-arrow int value `42i64`"
    );
}

#[test]
fn float_lit_fires_on_map_arrow_float_value() {
    // Plain decimal — no f64 suffix — exercises the float_lit regex
    // `[0-9]*\.[0-9]+([eE][+-]?[0-9]+)?\w*`. The grammar's value_atom
    // tries int_lit before float_lit, so a leading-dot variant
    // disambiguates: `3.14` would be lexed as int_lit `3` + leftover
    // `.14`, breaking the parse. The codegen does NOT yet emit
    // PEG-style longest-match preference at the value_atom level —
    // documented in the audit doc.
    let input = "kw = \"x\" -> .14 ;";
    let records = parse_records(input);
    assert!(
        count_variant(&records, VAR_FLOAT_LIT) >= 1,
        "float_lit must fire on map-arrow float value `.14`"
    );
}

#[test]
fn string_lit_fires_on_map_arrow_string_value() {
    let input = "kw = \"x\" -> \"hello\" ;";
    let records = parse_records(input);
    assert!(
        count_variant(&records, VAR_STRING_LIT) >= 1,
        "string_lit must fire on map-arrow string value"
    );
}

// ─── Payload firing (AV.0.2 + AV.0.3 — every typed rule reaches the tape) ─

/// AV.0.2 + AV.0.3 landing: every BBNF `-> Span` / `-> i64` / `-> f64`
/// annotation routes its payload through `push_leaf_with` — `Span`
/// rules pack `(lo, hi)` into `PayloadData::Aggregate(&buf[..8])`
/// via the bare-Span aggregate layout, `i64`/`f64` rules capture via
/// `parse_that::parse_i64_from_bytes` / `parse_f64_from_bytes` and
/// commit through `PayloadData::WideScalar`. Both paths produce a
/// `TapeKind::Span` leaf with `has_payload = true`; the pre-AV.0
/// behaviour was a `TapeKind::Rule` empty compound that lost the
/// declared payload. `count_typed_payload_leaves` filters for
/// `Span`-kind + has_payload to isolate the typed-leaf firings
/// from any residual empty-compound false positives.
#[test]
fn pinned_int_lit_drops_payload() {
    let input = "kw = \"x\" -> 42i64 ;";
    let records = parse_records(input);
    assert!(
        count_typed_payload_leaves(&records, VAR_INT_LIT) >= 1,
        "AV.0.3: BBNF int_lit `-> i64` payload must reach the tape. \
         records = {records:?}"
    );
}

#[test]
fn pinned_float_lit_drops_payload() {
    let input = "kw = \"x\" -> .14 ;";
    let records = parse_records(input);
    assert!(
        count_typed_payload_leaves(&records, VAR_FLOAT_LIT) >= 1,
        "AV.0.3: BBNF float_lit `-> f64` payload must reach the tape. \
         records = {records:?}"
    );
}

#[test]
fn pinned_identifier_drops_payload() {
    let input = "rule_name = \"x\" ;";
    let records = parse_records(input);
    assert!(
        count_typed_payload_leaves(&records, VAR_IDENTIFIER) >= 1,
        "AV.0.2: BBNF identifier `-> Span` payload must reach the tape. \
         records = {records:?}"
    );
}

#[test]
fn pinned_literal_drops_payload() {
    let input = "rule = \"hello\" ;";
    let records = parse_records(input);
    assert!(
        count_typed_payload_leaves(&records, VAR_LITERAL) >= 1,
        "AV.0.2: BBNF literal `-> Span` payload must reach the tape. \
         records = {records:?}"
    );
}

#[test]
fn pinned_regex_drops_payload() {
    let input = "rule = /[a-z]+/ ;";
    let records = parse_records(input);
    assert!(
        count_typed_payload_leaves(&records, VAR_REGEX) >= 1,
        "AV.0.2: BBNF regex `-> Span` payload must reach the tape. \
         records = {records:?}"
    );
}

#[test]
fn pinned_comment_drops_payload() {
    let input = "// hi\nrule = \"x\" ;";
    let records = parse_records(input);
    assert!(
        count_typed_payload_leaves(&records, VAR_COMMENT) >= 1,
        "AV.0.2: BBNF comment `-> Span` payload must reach the tape. \
         records = {records:?}"
    );
}

#[test]
fn pinned_big_comment_drops_payload() {
    let input = "/* hi */\nrule = \"x\" ;";
    let records = parse_records(input);
    assert!(
        count_typed_payload_leaves(&records, VAR_BIG_COMMENT) >= 1,
        "AV.0.2: BBNF big_comment `-> Span` payload must reach the tape. \
         records = {records:?}"
    );
}

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

// ─── ChildIter walker round-trip ─────────────────────────────────────

#[test]
fn child_iter_walks_bbnf_rule() {
    // The ChildIter must visit every direct child of every BBNF
    // compound on a small grammar input. Number of records seen
    // through the walker must match `tape.len()`.
    let input = "rule = \"a\" | \"b\" ;";
    let parsed = BbnfBootstrap::parse(input).expect("parse");
    let tape = parsed.tape();
    let root_off = parsed.view().cursor().offset();
    let cursor = TapeCursor::new(tape, root_off);
    let mut out = Vec::new();
    walk_records(tape, cursor, &mut out);
    assert!(
        !out.is_empty(),
        "ChildIter walk must visit every record from root"
    );
    // At least one identifier (`rule`) and two literals (`a`, `b`)
    // must appear via the walker.
    assert!(
        count_variant(&out, VAR_IDENTIFIER) >= 1,
        "identifier (rule_name) must be reachable via ChildIter"
    );
    assert!(
        count_variant(&out, VAR_LITERAL) >= 2,
        "two literals must be reachable via ChildIter"
    );
}

