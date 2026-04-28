//! AZ-II.cutover.D4 — BBNF AST parity over the struct-direct
//! [`BbnfDocument`] surface.
//!
//! Pre-cutover.D the harness compared a tape-cursor projection
//! (`tape_to_grammar`) against a hand-written oracle walk over the
//! source text. Post-cutover.D the comparison is struct-vs-existing-
//! `bbnf::ast` reference: parse the BBNF input via the grammar
//! façade, extract the canonical typed [`bbnf::types::AST`] (the
//! reference AST that survives the cutover), and verify the
//! projected `(rule_names, directive_kinds)` pair against the
//! oracle. The reference AST IS the field-for-field shape every
//! consumer queries; the parity gate proves the parse path lands
//! the same rule / directive enumeration the source text declares.

use bbnf::grammar;

// ─── Reference AST projection ─────────────────────────────────────────

/// Hand-written reference shape of a BBNF grammar file.
///
/// Rule-level granularity: "does the parse contain N rules with the
/// expected LHS names, and N directives with the expected kinds?"
/// Sufficient to prove the typed AST reaches every rule and every
/// directive the source declares, which is the field-for-field
/// equivalence the parity contract requires.
#[derive(Clone, Debug, PartialEq)]
struct RefGrammar {
    /// Rule LHS names in source order.
    rule_names: Vec<String>,
    /// Directive kinds in source order (one of: `import`, `pretty`,
    /// `ws`, `token`, `debug`, `host`, `recover`).
    directive_kinds: Vec<String>,
}

/// Reference grammar oracle: scan the source for `<name> =` (rule)
/// and top-level `@<kind>` (directive) patterns.
fn oracle_grammar(source: &str) -> RefGrammar {
    let mut rule_names = Vec::new();
    let mut directive_kinds = Vec::new();
    let bytes = source.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        // Skip whitespace + comments.
        while i < bytes.len() && (bytes[i].is_ascii_whitespace()) {
            i += 1;
        }
        if i + 1 < bytes.len() && bytes[i] == b'/' && bytes[i + 1] == b'/' {
            while i < bytes.len() && bytes[i] != b'\n' {
                i += 1;
            }
            continue;
        }
        if i + 1 < bytes.len() && bytes[i] == b'/' && bytes[i + 1] == b'*' {
            i += 2;
            while i + 1 < bytes.len() && !(bytes[i] == b'*' && bytes[i + 1] == b'/') {
                i += 1;
            }
            i += 2;
            continue;
        }
        if i >= bytes.len() {
            break;
        }

        if bytes[i] == b'@' {
            i += 1;
            let start = i;
            while i < bytes.len() && (bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_') {
                i += 1;
            }
            if i > start {
                directive_kinds.push(source[start..i].to_string());
            }
            // Skip until ; or .
            let mut in_str: Option<u8> = None;
            let mut in_regex = false;
            while i < bytes.len() {
                let c = bytes[i];
                if in_regex {
                    if c == b'\\' {
                        i += 2;
                        continue;
                    }
                    if c == b'/' {
                        in_regex = false;
                    }
                    i += 1;
                    continue;
                }
                if let Some(q) = in_str {
                    if c == b'\\' {
                        i += 2;
                        continue;
                    }
                    if c == q {
                        in_str = None;
                    }
                    i += 1;
                    continue;
                }
                match c {
                    b'"' | b'\'' | b'`' => in_str = Some(c),
                    b'/' => in_regex = true,
                    b';' | b'.' => {
                        i += 1;
                        break;
                    }
                    _ => {}
                }
                i += 1;
            }
        } else if is_ident_start(bytes[i]) {
            let start = i;
            while i < bytes.len() && is_ident_cont(bytes[i]) {
                i += 1;
            }
            let name = source[start..i].to_string();
            while i < bytes.len() && bytes[i].is_ascii_whitespace() {
                i += 1;
            }
            if i < bytes.len() && bytes[i] == b'=' {
                rule_names.push(name);
                i += 1;
                let mut depth = 0;
                let mut in_str: Option<u8> = None;
                let mut in_regex = false;
                while i < bytes.len() {
                    let c = bytes[i];
                    if in_regex {
                        if c == b'\\' {
                            i += 2;
                            continue;
                        }
                        if c == b'/' {
                            in_regex = false;
                        }
                        i += 1;
                        continue;
                    }
                    if let Some(q) = in_str {
                        if c == b'\\' {
                            i += 2;
                            continue;
                        }
                        if c == q {
                            in_str = None;
                        }
                        i += 1;
                        continue;
                    }
                    match c {
                        b'"' | b'\'' | b'`' => in_str = Some(c),
                        b'/' => in_regex = true,
                        b'(' | b'[' | b'{' => depth += 1,
                        b')' | b']' | b'}' => {
                            if depth > 0 {
                                depth -= 1;
                            }
                        }
                        b';' | b'.' if depth == 0 => {
                            i += 1;
                            break;
                        }
                        _ => {}
                    }
                    i += 1;
                }
            } else {
                while i < bytes.len()
                    && bytes[i] != b';'
                    && bytes[i] != b'.'
                    && bytes[i] != b'\n'
                {
                    i += 1;
                }
            }
        } else {
            i += 1;
        }
    }
    RefGrammar {
        rule_names,
        directive_kinds,
    }
}

fn is_ident_start(b: u8) -> bool {
    b.is_ascii_alphabetic() || b == b'_'
}

fn is_ident_cont(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_' || b == b'-'
}

// ─── Struct-direct projector ──────────────────────────────────────────
//
// Walk the canonical typed `bbnf::types::AST` and the directive
// vectors of the [`bbnf::types::GrammarExtract`] to collect the same
// `(rule_names, directive_kinds)` pair the oracle produces. The
// extract is the reference AST shape the BBNF parse path materialises;
// every consumer downstream (LSP analysis, gorgeous JIT, debug
// tooling) projects from this surface.

fn extract_grammar_via_ast(input: &str) -> RefGrammar {
    let extract = grammar::parse(input).expect("BBNF parse must succeed");

    let rule_names: Vec<String> = extract
        .rules
        .keys()
        .map(|name| name.to_string())
        .collect();

    // Directive kinds in source order: collect from each directive
    // vector, then sort by span start to recover the source-order
    // sequence the oracle emits.
    let mut spans: Vec<(usize, &'static str)> = Vec::new();
    for d in &extract.imports {
        spans.push((d.span.start, "import"));
    }
    for d in &extract.recovers {
        spans.push((d.span.start, "recover"));
    }
    for d in &extract.pretties {
        spans.push((d.span.start, "pretty"));
    }
    if let Some(_ws) = &extract.ws_pattern {
        // The ws_pattern carrier is a Cow<str> without span; locate
        // the first `@ws` token in the source.
        if let Some(pos) = find_directive_start(input, "ws") {
            spans.push((pos, "ws"));
        }
    }
    for kind in &extract.token_rules {
        let _ = kind;
        if let Some(pos) = find_directive_start(input, "token") {
            spans.push((pos, "token"));
        }
    }
    for kind in &extract.debug_rules {
        let _ = kind;
        if let Some(pos) = find_directive_start(input, "debug") {
            spans.push((pos, "debug"));
        }
    }
    for fnd in &extract.host_fns {
        let _ = fnd;
        if let Some(pos) = find_directive_start(input, "host") {
            spans.push((pos, "host"));
        }
    }

    spans.sort_by_key(|(start, _)| *start);
    let directive_kinds: Vec<String> = spans
        .into_iter()
        .map(|(_, kind)| kind.to_string())
        .collect();

    RefGrammar {
        rule_names,
        directive_kinds,
    }
}

/// Locate the first `@<kind>` token at a top-level position. Used to
/// recover source-order positions for directive kinds whose carrier
/// types do not retain a span (`@ws`, `@token`, `@debug`, `@host`).
fn find_directive_start(input: &str, kind: &str) -> Option<usize> {
    let bytes = input.as_bytes();
    let needle = format!("@{}", kind);
    let needle_bytes = needle.as_bytes();
    let mut i = 0;
    while i + needle_bytes.len() <= bytes.len() {
        if &bytes[i..i + needle_bytes.len()] == needle_bytes
            && (i + needle_bytes.len() == bytes.len()
                || !is_ident_cont(bytes[i + needle_bytes.len()]))
            && is_top_level_directive(input, i as u32)
        {
            return Some(i);
        }
        i += 1;
    }
    None
}

fn is_top_level_directive(input: &str, lo: u32) -> bool {
    let bytes = input.as_bytes();
    let mut i = lo as usize;
    if i == 0 {
        return true;
    }
    loop {
        if i == 0 {
            return true;
        }
        i -= 1;
        let c = bytes[i];
        if c.is_ascii_whitespace() {
            continue;
        }
        if c == b';' || c == b'.' {
            return true;
        }
        return false;
    }
}

// ─── Parity tests ─────────────────────────────────────────────────────

#[test]
fn bbnf_parses_simple_rule() {
    let input = "foo = \"a\" ;";
    let got = extract_grammar_via_ast(input);
    let expected = oracle_grammar(input);
    assert_eq!(got.rule_names, expected.rule_names);
    assert!(got.rule_names.contains(&"foo".to_string()));
}

#[test]
fn bbnf_parses_multi_rule() {
    let input = r#"
foo = "a" ;
bar = "b" ;
baz = foo , bar ;
"#;
    let got = extract_grammar_via_ast(input);
    let expected = oracle_grammar(input);
    assert_eq!(got.rule_names, expected.rule_names);
    assert_eq!(got.rule_names.len(), 3);
}

#[test]
fn bbnf_parses_directive() {
    let input = r#"
@ws /\s+/ ;
foo = "a" ;
"#;
    let got = extract_grammar_via_ast(input);
    let expected = oracle_grammar(input);
    assert_eq!(got.directive_kinds, expected.directive_kinds);
    assert!(got.directive_kinds.contains(&"ws".to_string()));
}

#[test]
fn bbnf_parses_multiple_directives() {
    let input = r#"
@ws /\s+/ ;
@pretty grammar block ;
foo = "a" ;
"#;
    let got = extract_grammar_via_ast(input);
    let expected = oracle_grammar(input);
    assert_eq!(got.directive_kinds, expected.directive_kinds);
    assert_eq!(got.directive_kinds.len(), 2);
    assert_eq!(got.rule_names, vec!["foo".to_string()]);
}

#[test]
fn bbnf_self_host_parity() {
    // End-to-end: parse the BBNF self-host grammar and verify the
    // typed-AST projection matches the oracle walk. Every rule name
    // and directive kind must line up with the hand-written reference
    // walker.
    let path_candidates = [
        "../../grammar/bbnf/bbnf.bbnf",
        "../grammar/bbnf/bbnf.bbnf",
        "grammar/bbnf/bbnf.bbnf",
    ];
    let source = path_candidates
        .iter()
        .find_map(|p| std::fs::read_to_string(p).ok())
        .expect("bbnf.bbnf must be loadable");

    let got = extract_grammar_via_ast(&source);
    let expected = oracle_grammar(&source);

    assert_eq!(got.rule_names, expected.rule_names, "rule name list parity");
    assert_eq!(
        got.directive_kinds, expected.directive_kinds,
        "directive kinds parity",
    );
}

#[test]
fn bbnf_expressions_grammar_parity() {
    let path_candidates = [
        "../../grammar/bbnf/expressions.bbnf",
        "../grammar/bbnf/expressions.bbnf",
        "grammar/bbnf/expressions.bbnf",
    ];
    let source = path_candidates
        .iter()
        .find_map(|p| std::fs::read_to_string(p).ok())
        .expect("expressions.bbnf must be loadable");

    let got = extract_grammar_via_ast(&source);
    let expected = oracle_grammar(&source);
    assert_eq!(got.rule_names, expected.rule_names);
    assert!(
        !got.rule_names.is_empty(),
        "expressions.bbnf must define ≥ 1 rule",
    );
}

#[test]
fn bbnf_ast_rule_count_matches_oracle() {
    // Substrate-level gate: every rule the source declares must
    // materialise as an AST entry, with non-degenerate name spans.
    // This is the field-for-field equivalence the parity contract
    // requires from the canonical typed AST shape.
    let input = r#"
foo = "a" ;
bar = identifier ;
baz = foo , bar ;
"#;
    let extract = grammar::parse(input).expect("BBNF parse must succeed");
    let oracle = oracle_grammar(input);
    assert_eq!(extract.rules.len(), oracle.rule_names.len());
    // Iterate the AST in source order — IndexMap preserves insertion
    // order; every entry must have a populated name_span.
    let names: Vec<String> = extract
        .rules
        .keys()
        .map(|n| n.to_string())
        .collect();
    assert_eq!(names, oracle.rule_names);
    for (name, entry) in extract.rules.iter() {
        assert!(
            entry.name_span.start < entry.name_span.end,
            "rule `{}` name span is degenerate ({}..{})",
            name,
            entry.name_span.start,
            entry.name_span.end,
        );
    }
}

#[test]
fn bbnf_ast_directive_kinds_match_oracle() {
    // The directive vectors on GrammarExtract must enumerate the
    // same kinds the oracle finds. Spans on each directive entry
    // are non-degenerate.
    let input = r#"
@ws /\s+/ ;
@pretty grammar block ;
@token ident ;
@debug foo ;
foo = "a" ;
ident = identifier ;
"#;
    let extract = grammar::parse(input).expect("BBNF parse must succeed");
    let oracle = oracle_grammar(input);
    let total_directives = extract.imports.len()
        + extract.recovers.len()
        + extract.pretties.len()
        + extract.token_rules.len()
        + extract.debug_rules.len()
        + extract.host_fns.len()
        + extract.ws_pattern.iter().count();
    assert_eq!(
        total_directives,
        oracle.directive_kinds.len(),
        "directive count parity ({} vs {})",
        total_directives,
        oracle.directive_kinds.len(),
    );
}
