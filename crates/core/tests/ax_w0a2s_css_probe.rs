//! AX.W0a.2.s characterization probe — minimal CSS inputs reproducing the
//! real-world failure positions flagged by the W0a.close bench:
//!
//! - bootstrap.css  → `Syntax { offset: 18, rule: None }`
//! - normalize.css  → offset 0
//! - tailwind.css   → offset 0
//!
//! Goal: isolate the exact grammar construct + emitter path that fails.

use bbnf_derive::Parser;

#[allow(dead_code)]
mod css_types {
    pub fn parse_hex_color(_: &str) -> u32 {
        0
    }
}

#[derive(Parser)]
#[parser(path = "../../grammar/css/l4/stylesheet.bbnf", skip_recover)]
struct CssL4Parser;

fn try_parse(label: &str, input: &str) {
    match CssL4Parser::parse(input) {
        Ok(_) => eprintln!("  {label}: OK ({} bytes)", input.len()),
        Err(e) => eprintln!("  {label}: ERR {e:?} (input len={} bytes)", input.len()),
    }
}

#[test]
fn probe_bootstrap_head_reduced() {
    eprintln!("=== bootstrap.css head reduction ladder ===");
    // Exactly matches bootstrap.css head, progressively extended.
    try_parse("just @charset directive", "@charset \"UTF-8\";");
    try_parse("@charset + newline", "@charset \"UTF-8\";\n");
    try_parse("@charset + newline + /*!", "@charset \"UTF-8\";\n/*!");
    try_parse(
        "@charset + newline + block comment",
        "@charset \"UTF-8\";\n/*! x */",
    );
    try_parse(
        "@charset + block comment + empty rule",
        "@charset \"UTF-8\";\n/*! x */\na {}",
    );
}

#[test]
fn probe_leading_block_comment() {
    eprintln!("=== normalize/tailwind: leading block comment ===");
    try_parse("empty input", "");
    try_parse("just ws", "   \n  ");
    try_parse("/* hello */ (no !)", "/* hello */");
    try_parse("/*! hello */ (with !)", "/*! hello */");
    try_parse("/**/", "/**/");
    try_parse("/* */ rule", "/* */ a { color: red; }");
    try_parse("rule /* */", "a { color: red; } /* */");
    try_parse("rule /* */ rule", "a{} /* hi */ b{}");
    try_parse("block comment + newline", "/*! hello */\n");
    try_parse("block comment + rule", "/*! hello */\na { color: red; }");
    try_parse(
        "block comment + double newline + rule",
        "/*! hello */\n\na { color: red; }",
    );
}

#[test]
fn probe_charset_quoted_semicolon() {
    eprintln!("=== charset directive constructions ===");
    // Quoted string directive (@charset is a generic @-rule).
    try_parse("generic @-rule with ;", "@charset \"UTF-8\";");
    try_parse("@foo ;", "@foo;");
    try_parse("@foo body ;", "@foo body;");
    try_parse("@foo \"q\" ;", "@foo \"q\";");
    // Rule AFTER @charset
    try_parse("@charset + rule", "@charset \"UTF-8\"; a { color: red; }");
}

#[test]
fn probe_exact_bootstrap_bytes() {
    use std::fs;
    // Read first 60 bytes of bootstrap.css; try progressively longer
    // prefixes to localize the failure position.
    let full = fs::read_to_string("../../data/css/bootstrap.css")
        .expect("read bootstrap.css");
    eprintln!("=== bootstrap.css prefix ladder (bytes) ===");
    for &n in &[17, 18, 19, 25, 40, 60, 80, 120] {
        if n > full.len() {
            continue;
        }
        let slice = &full[..n];
        eprintln!("  [0..{n}] = {slice:?}");
        try_parse(&format!("prefix {n}"), slice);
    }
}

#[test]
fn probe_exact_normalize_bytes() {
    use std::fs;
    let full = fs::read_to_string("../../data/css/normalize.css")
        .expect("read normalize.css");
    eprintln!("=== normalize.css prefix ladder ===");
    for &n in &[20, 50, 80, 120, 200, 400] {
        if n > full.len() {
            continue;
        }
        let slice = &full[..n];
        eprintln!("  [0..{n}] = {:?}", &slice[..slice.len().min(60)]);
        try_parse(&format!("prefix {n}"), slice);
    }
}

#[test]
fn probe_exact_tailwind_bytes() {
    use std::fs;
    let full = fs::read_to_string("../../data/css/tailwind.css")
        .expect("read tailwind.css");
    eprintln!("=== tailwind.css prefix ladder ===");
    for &n in &[20, 50, 80, 120, 200, 400] {
        if n > full.len() {
            continue;
        }
        let slice = &full[..n];
        eprintln!("  [0..{n}] = {:?}", &slice[..slice.len().min(60)]);
        try_parse(&format!("prefix {n}"), slice);
    }
}
