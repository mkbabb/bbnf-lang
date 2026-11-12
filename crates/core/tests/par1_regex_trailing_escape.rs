//! sk-v27 LW2.PAR-1, DIVERGENCE.tsv D-21/D-23: a regex literal whose body ends in an
//! escaped backslash. docs/bbnf/grammar-syntax.md: "Inside a pattern, `\` escapes the next
//! character: ... so `/a\\/` is the pattern `a\\`." The TS front end reads it so; the Rust
//! bootstrap front end rejected it (grammar/misc/json-commented.bbnf, offset 264); cured on
//! sk-v27-lw3 before 3bdc88cfa, un-ignored at LW3.ORC2 (DIVERGENCE.tsv D-21/D-23 CURED-RUST).

use bbnf::grammar::generated::BbnfBootstrap;

#[test]
fn regex_body_may_end_in_an_escaped_backslash() {
    for src in [r"c = /a\\/ ;", r"c = /\\/ ;", r#"c = /[^"\\]|\\/ ;"#] {
        assert!(BbnfBootstrap::parse(src).is_ok(), "rejected: {src}");
    }
}

#[test]
fn neighbouring_escapes_are_read() {
    for src in [r"c = /a\\b/ ;", r"c = /\\a/ ;", r"c = /\// ;"] {
        assert!(BbnfBootstrap::parse(src).is_ok(), "rejected: {src}");
    }
}
