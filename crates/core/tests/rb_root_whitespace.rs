//! sk-v27 LW3.ORC2 (ruling ORC-2, 2026-11-12a): root reads only the
//! whitespace the grammar reads. Born RED: the defect class the peggy oracle
//! (in custody, LW3.ORC2) names on `ebnf`, `bnf` and `math`, ruled in
//! crates/oracle/ruled.tsv (owner LW4.COMP-1) and in SK/grammar/DIVERGENCE.tsv
//! J-14..J-16 where the TS package is the other side.
//!
//! Root's generated parsers read `[ \t\n\r]*` where no grammar writes it:
//! before every rule (each rule's value-position dispatcher calls
//! `skip_space`), as `?w` (docs/bbnf/operators.md: "`ws` matches `\s`", which
//! reads U+000B and U+000C too), and in place of the whitespace rule an entry
//! repetition names (`ebnf`'s `grammar = ( S , rule , S ) *`, whose `S` is
//! `/[ \t\n\r\f]*/`). A model transcription that reads exactly that agrees
//! with root on every case of the three corpora
//! (restart/skinny/tranches/sk-v27/evidence/LW3.ORC/ws_model.mjs), so the
//! class is the whole of root's divergence there.
//!
//! The cure is the spine's (LW4.COMP-1, the plan-driven Rust emitter), which
//! reads whitespace by the grammar's text; root is the quarry. Each case below
//! is a minimal corpus case (the ids name the custody file it reduces).

use bbnf::grammar::generated::{bnf::BnfParser, ebnf::EbnfParser, math::MathParser};

#[test]
#[ignore = "born-RED LW3.ORC2 root whitespace class (crates/oracle/ruled.tsv; SK/grammar/DIVERGENCE.tsv J-14 J-15 J-16): root reads [ \\t\\n\\r]* where the grammar reads none; cure LW4.COMP-1 at LW4"]
fn root_reads_only_the_whitespace_the_grammar_reads() {
    let mut wrong = Vec::new();
    // math/cases/hand-15, mut-0173: `number` reads no leading whitespace,
    // and its regex matches the empty string only at offset 0.
    for t in ["  1", "  ", "\t.5"] {
        if MathParser::parse(t).is_ok() {
            wrong.push(format!("math accepted {t:?}"));
        }
    }
    // ebnf/cases/hand-15: `S` reads U+000C. A terminal's `character`
    // reads a space and a carriage return as itself.
    for t in ["\u{c}a = b ;\u{c}", "a = \" \" ;", "a = \"\r\" ;", "a = '\t' ;"] {
        if EbnfParser::parse(t).is_err() {
            wrong.push(format!("ebnf refused {t:?}"));
        }
    }
    // ebnf: `( S , rule , S ) *` reads no rule from a lone line break
    // (S is inside the repetition; zero iterations leave it unread).
    for t in ["\n", " "] {
        if EbnfParser::parse(t).is_ok() {
            wrong.push(format!("ebnf accepted {t:?}"));
        }
    }
    // bnf/cases/hand-12, hand-13, gen-0204: `?w` reads `\s`, U+000B and
    // U+000C among it.
    for t in ["<a> ::= \"x\"\u{b}<b> ::= \"y\"", "<a> ::= \"x\"\u{c}\n<b> ::= \"y\"", "\u{c}<Z0> ::=\t\"\"\n"] {
        if BnfParser::parse(t).is_err() {
            wrong.push(format!("bnf refused {t:?}"));
        }
    }
    // bnf/cases/gen-0107, gen-0111: `( rule ?w ) *` reads no rule from
    // whitespace alone.
    for t in ["\t\n", "\n"] {
        if BnfParser::parse(t).is_ok() {
            wrong.push(format!("bnf accepted {t:?}"));
        }
    }
    assert!(wrong.is_empty(), "{} verdicts differ from the grammar's text:\n{}", wrong.len(), wrong.join("\n"));
}

#[test]
fn the_cases_the_class_does_not_touch_are_read() {
    assert!(MathParser::parse("1.5e3").is_ok());
    assert!(EbnfParser::parse("a = b , 'c' | { d } ;").is_ok());
    assert!(BnfParser::parse("<a> ::= \"x\" | <b>").is_ok());
}
