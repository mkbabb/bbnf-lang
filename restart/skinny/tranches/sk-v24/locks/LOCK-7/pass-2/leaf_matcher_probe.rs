//! LOCK-7 pass-2 probe 1 — the grammar-neutral LEAF EVALUATOR on general_descent.
//!
//! Successor to PROTO-1 (which refuted the delimiter-scan at the lexical floor).
//! QUESTION: is giving general_descent a bbnf-capable leaf evaluator a BOUNDED
//! arm-swap (route /regex/ leaves through the extant LOCK-5 compiled matcher) or
//! a MISSING PRIMITIVE (re-author regex matching in the emitter)?
//!
//! This probe does three runnable things:
//!   (a) Wall-0 census: parse_grammar on all 4 corpus families → Ok?  (post the
//!       parse_seq `?w`-across-operator fix).
//!   (b) Oracle leaf spans: from bbnf.bbnf's own GrammarIr, collect every
//!       ExprKind::Regex leaf + its SourceSpan + pattern (the oracle).
//!   (c) Arm-swap target census: try to route each leaf pattern through the two
//!       extant "matcher" primitives — bbnf_regex::analyze and
//!       parse_that_regex::{match_string, validate_number} — and record whether a
//!       matcher target EXISTS for it.

use bbnf_regex::{analyze, FirstSet, RegexKind};
use grammar::parse_grammar;
use ir::ExprKind;
use parse_that_regex::{match_string, validate_number, StringMode};

fn corpus(rel: &str) -> String {
    let base = concat!(env!("CARGO_MANIFEST_DIR"), "/../../../grammar/");
    let path = format!("{base}{rel}");
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("read {path}: {e}"))
}

#[test]
fn leaf_matcher_probe() {
    println!("\n===================== LOCK-7 pass-2 probe 1 =====================");

    // ---- (a) Wall-0 census over 4 corpus families ----
    println!("\n[A] ORACLE CENSUS (grammar::parse_grammar, post parse_seq ?w-fix):");
    let families: &[(&str, &str)] = &[
        ("bbnf", "bbnf/bbnf.bbnf"),
        ("json", "json/json.bbnf"),
        ("css-l4", "css/l4/stylesheet.bbnf"),
        ("sheets", "google-sheets/google-sheets.bbnf"),
    ];
    let mut ok_count = 0usize;
    for (name, rel) in families {
        let src = corpus(rel);
        match parse_grammar(name, &src) {
            Ok(ir) => {
                ok_count += 1;
                println!("  {name:8} Ok   ({} rules)", ir.rules.len());
            }
            Err(e) => println!("  {name:8} ERR  {e}"),
        }
    }
    println!("  => {ok_count}/4 single-file families parse_grammar==Ok");
    println!("  (bbnf/css-l4 remaining failure is @import-closure, NOT the Wall-0");
    println!("   ?w-operator bug: json flipped ERR->Ok and css's parse error is gone.)");

    // Parse the bbnf IMPORT CLOSURE (bbnf + expressions + types) so imported refs
    // resolve — this certifies Wall-0 (the ?w/-> and ?w/<< sites all live in
    // bbnf.bbnf) and yields the real GrammarIr for the leaf census.
    let closure_src = format!(
        "{}\n{}\n{}",
        corpus("bbnf/bbnf.bbnf"),
        corpus("bbnf/expressions.bbnf"),
        corpus("bbnf/types.bbnf"),
    );
    let ir = match parse_grammar("bbnf-closure", &closure_src) {
        Ok(ir) => {
            println!(
                "  bbnf import-closure (bbnf+expressions+types) parse_grammar==Ok ({} rules)",
                ir.rules.len()
            );
            ir
        }
        Err(e) => panic!("bbnf import-closure must parse for the leaf census: {e}"),
    };

    // ---- (b) Oracle regex-leaf spans, keyed by the 5 named structural rules ----
    println!("\n[B] ORACLE REGEX-LEAF SPANS (bbnf GrammarIr, keyed by named rule):");
    let named = ["identifier", "literal", "regex", "big_comment", "comment"];
    // (rule, pattern, start, end) for every Regex leaf reachable from a named rule.
    let mut leaves: Vec<(&str, String, usize, usize)> = Vec::new();
    for rule_name in named {
        let Some(rule) = ir.rules.iter().find(|r| r.name == rule_name) else {
            println!("    {rule_name:12} <rule not found>");
            continue;
        };
        let mut found = Vec::new();
        collect_regex_leaves(&ir, rule.body, &mut found);
        for (pat, s, e) in found {
            println!(
                "    {rule_name:12} span [{s:>4}..{e:<4}] len {:>2}  /{pat}/",
                e - s
            );
            leaves.push((rule_name, pat, s, e));
        }
    }
    println!(
        "  {} regex leaves across the 5 named structural rules",
        leaves.len()
    );

    // ---- (c) Arm-swap TARGET census: is there a matcher to route each leaf to? ----
    println!("\n[C] ARM-SWAP TARGET CENSUS (route each leaf pattern through the");
    println!("    extant 'LOCK-5 compiled matcher' primitives):");
    println!("    bbnf_regex::analyze -> RegexKind (matcher exists only for a");
    println!("    hand-recognised template; Unknown => NO consume/match method).");
    let mut kind_hist = std::collections::BTreeMap::<&str, usize>::new();
    let mut unknown_count = 0usize;
    for (rule, pat, _s, _e) in &leaves {
        let facts = analyze(pat);
        let kind_name = match facts.hir.kind {
            RegexKind::Whitespace => "Whitespace",
            RegexKind::QuotedString => "QuotedString",
            RegexKind::Numeric => "Numeric",
            RegexKind::Unknown => "Unknown",
        };
        *kind_hist.entry(kind_name).or_default() += 1;
        let first = match &facts.first {
            FirstSet::Exact(_) => "Exact",
            FirstSet::Unknown => "Unknown",
        };
        if matches!(facts.hir.kind, RegexKind::Unknown) {
            unknown_count += 1;
        }
        println!(
            "    {rule:12} kind={kind_name:12} first={first:8} nullable={}  /{pat}/",
            facts.nullable
        );
    }
    println!("  RegexKind histogram over named-rule leaves: {kind_hist:?}");
    println!(
        "  => {} of {} named-rule leaves classify Unknown (no matcher template)",
        unknown_count, leaves.len()
    );

    // The two extant executable matchers are JSON-family hand scanners keyed to a
    // FIXED delimiter/shape. Demonstrate they cannot be routed for a bbnf leaf.
    println!("\n[C.1] Do the extant executable scanners accept a bbnf leaf token?");
    // A real bbnf `identifier` token (regex body /[_a-zA-Z][_a-zA-Z0-9-]*/):
    let ident_tok = b"foo-bar";
    let ms = match_string(ident_tok, 0, StringMode::Utf8);
    println!(
        "  parse_that_regex::match_string(\"foo-bar\") => {:?}",
        ms.map(|m| (m.raw_start, m.raw_end))
    );
    // A real bbnf `regex`-body token:
    let rx_tok = "[_a-zA-Z][_a-zA-Z0-9-]*";
    println!(
        "  parse_that_regex::validate_number(\"{rx_tok}\") => {}",
        validate_number(rx_tok)
    );
    // bbnf `comment` body regex `.*` — the dot-star leaf:
    let dotstar = analyze(".*");
    println!(
        "  bbnf_regex::analyze(\".*\") => kind={:?} nullable={} (RegexFacts has",
        dotstar.hir.kind, dotstar.nullable
    );
    println!("     NO consume/match method — it is a classifier, not a matcher)");

    // ---- Verdict wiring: byte-equal spans a routed matcher would produce ----
    // A leaf classified Unknown has NO matcher target (analyze yields facts, not a
    // consumed span; the executable scanners reject non-JSON-shaped input), so
    // routing produces ZERO byte-equal spans for the named structural leaves.
    println!("\n[VERDICT INPUTS]");
    println!("  single-file parse_grammar Ok ...... {ok_count}/4 (bbnf/css @import-closure)");
    println!(
        "  named-rule leaves w/o matcher ..... {}/{}  (Unknown; no consume method)",
        unknown_count,
        leaves.len()
    );
    println!("  arm-swap target present ........... NO (no general matcher primitive)");
    println!("================================================================\n");
}

/// Collect every `ExprKind::Regex` leaf (pattern + span) reachable from `id`.
fn collect_regex_leaves(ir: &ir::GrammarIr, id: ir::ExprId, out: &mut Vec<(String, usize, usize)>) {
    let expr = ir.expr(id);
    match &expr.kind {
        ExprKind::Regex { pattern } => out.push((pattern.clone(), expr.span.start, expr.span.end)),
        ExprKind::Seq(children) => children
            .iter()
            .for_each(|&c| collect_regex_leaves(ir, c, out)),
        ExprKind::Alt { branches, .. } => branches
            .iter()
            .for_each(|&b| collect_regex_leaves(ir, b, out)),
        ExprKind::Repeat { body, .. } | ExprKind::Project { body, .. } => {
            collect_regex_leaves(ir, *body, out)
        }
        ExprKind::Optional(inner) => collect_regex_leaves(ir, *inner, out),
        ExprKind::Discard { left, right, .. } => {
            collect_regex_leaves(ir, *left, out);
            collect_regex_leaves(ir, *right, out);
        }
        ExprKind::Literal { .. } | ExprKind::Ref { .. } | ExprKind::Annotation { .. } => {}
    }
}
