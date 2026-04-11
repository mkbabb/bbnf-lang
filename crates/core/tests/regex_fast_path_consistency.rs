//! Tranche AG.1 regression gate — strategy planner and tier emitter
//! must stay consistent.
//!
//! Pre-AG.1, `solve_regex_strategy` probed with `emit_regex_direct_call`
//! (which hard-codes default `EmitOpts`) while `emit_regex` passed the
//! caller's real opts to `emit_regex_fast_path`. When the CSP had
//! decided `RegexEngine::Dfa` for a pattern that would classify as a
//! fast path under default opts, the planner committed to `FastPath`
//! but `emit_regex_fast_path` returned `None`, triggering the
//! `.expect("solve_regex_strategy returned FastPath — emission must
//! succeed")` panic at `crates/core/src/generate/regex/emit/mod.rs:38`.
//!
//! AG.1 collapses the decide-then-re-emit dance: `emit_regex` walks the
//! tier ladder directly, and `solve_regex_strategy` is a pure
//! classifier that runs the same predicates against the caller's real
//! `opts`. The two cannot disagree because they share the predicate
//! functions — this gate captures that invariant against every regex
//! pattern in every production `.bbnf` file.
//!
//! For each pattern:
//!
//! - `solve_regex_strategy(pattern, opts)` returns a non-`Unsupported`
//!   tier iff `emit_regex(pattern, opts)` produces non-`compile_error`
//!   tokens.
//! - `emit_regex(pattern, opts)` never panics (enforced structurally by
//!   the absence of `.expect` / `.unwrap` in its body after AG.1, but
//!   this test confirms it on real grammar input).

use std::path::{Path, PathBuf};

use bbnf::generate::regex::{CostModel, EmitOpts, RegexStrategy, emit_regex, solve_regex_strategy};

fn scan_bbnf_for_regex(source: &str) -> Vec<String> {
    let mut patterns = Vec::new();
    let mut chars = source.chars().peekable();

    while let Some(c) = chars.next() {
        // String literals — skip contents.
        if c == '"' || c == '\'' || c == '`' {
            let close = c;
            while let Some(c2) = chars.next() {
                if c2 == '\\' {
                    chars.next();
                } else if c2 == close {
                    break;
                }
            }
            continue;
        }

        // Line comments `//`.
        if c == '/' && chars.peek() == Some(&'/') {
            for c2 in chars.by_ref() {
                if c2 == '\n' {
                    break;
                }
            }
            continue;
        }

        // Block comments `/* ... */`.
        if c == '/' && chars.peek() == Some(&'*') {
            chars.next();
            loop {
                match chars.next() {
                    Some('*') if chars.peek() == Some(&'/') => {
                        chars.next();
                        break;
                    }
                    None => break,
                    _ => {}
                }
            }
            continue;
        }

        // Block comments `(* ... *)` — EBNF-style, used by bbnf.bbnf.
        if c == '(' && chars.peek() == Some(&'*') {
            chars.next();
            let mut depth = 1;
            while depth > 0 {
                match chars.next() {
                    Some('(') if chars.peek() == Some(&'*') => {
                        chars.next();
                        depth += 1;
                    }
                    Some('*') if chars.peek() == Some(&')') => {
                        chars.next();
                        depth -= 1;
                    }
                    None => break,
                    _ => {}
                }
            }
            continue;
        }

        // Regex literal `/.../`.
        if c == '/' {
            let mut pattern = String::new();
            let mut escaped = false;
            let mut in_class = false;
            let mut valid = true;

            for c2 in chars.by_ref() {
                if escaped {
                    pattern.push('\\');
                    pattern.push(c2);
                    escaped = false;
                    continue;
                }
                if c2 == '\\' {
                    escaped = true;
                    continue;
                }
                if c2 == '[' {
                    in_class = true;
                    pattern.push(c2);
                    continue;
                }
                if c2 == ']' && in_class {
                    in_class = false;
                    pattern.push(c2);
                    continue;
                }
                if c2 == '/' && !in_class {
                    break;
                }
                if c2 == '\n' {
                    valid = false;
                    break;
                }
                pattern.push(c2);
            }

            if valid && !pattern.is_empty() {
                patterns.push(pattern);
            }
        }
    }

    patterns
}

fn collect_all_patterns() -> Vec<(PathBuf, String)> {
    let mut out = Vec::new();
    let dirs = [
        Path::new(env!("CARGO_MANIFEST_DIR")).join("../../grammar"),
        Path::new(env!("CARGO_MANIFEST_DIR")).join("../gorgeous/grammar"),
    ];
    for dir in &dirs {
        walk(dir, &mut out);
    }
    out
}

fn walk(dir: &Path, out: &mut Vec<(PathBuf, String)>) {
    if !dir.exists() {
        return;
    }
    for entry in std::fs::read_dir(dir).unwrap() {
        let path = entry.unwrap().path();
        if path.is_dir() {
            walk(&path, out);
        } else if path.extension().is_some_and(|e| e == "bbnf") {
            let src = std::fs::read_to_string(&path).unwrap();
            for pat in scan_bbnf_for_regex(&src) {
                out.push((path.clone(), pat));
            }
        }
    }
}

#[test]
fn emit_regex_never_panics_on_grammar_patterns() {
    let patterns = collect_all_patterns();
    assert!(
        !patterns.is_empty(),
        "no regex patterns found under grammar/ or gorgeous/grammar/ — test cannot run"
    );

    let opts = EmitOpts::new(&CostModel::DEFAULT);

    for (file, pattern) in &patterns {
        let _tokens = emit_regex(pattern, &opts);
        // The assertion is implicit: reaching this line means emit_regex
        // did not panic. The AG.1 regression pre-fix would blow up on
        // any pattern where solve_regex_strategy committed to FastPath
        // but the real emitter (with the caller's opts) returned None.
        let _ = file;
    }
}

#[test]
fn solve_regex_strategy_agrees_with_emit_regex_tier() {
    let patterns = collect_all_patterns();
    assert!(!patterns.is_empty());

    let opts = EmitOpts::new(&CostModel::DEFAULT);

    for (file, pattern) in &patterns {
        let strategy = solve_regex_strategy(pattern, &opts);
        let tokens = emit_regex(pattern, &opts);
        let s = tokens.to_string();

        match strategy {
            RegexStrategy::Unsupported => {
                assert!(
                    s.contains("compile_error"),
                    "solve_regex_strategy returned Unsupported but emit_regex did not produce \
                     compile_error! for /{pattern}/ in {file:?}: {s}"
                );
            }
            _ => {
                assert!(
                    !s.contains("compile_error"),
                    "solve_regex_strategy returned {strategy:?} but emit_regex produced \
                     compile_error! for /{pattern}/ in {file:?}: {s}"
                );
                assert!(
                    !s.is_empty(),
                    "solve_regex_strategy returned {strategy:?} but emit_regex returned empty \
                     tokens for /{pattern}/ in {file:?}"
                );
            }
        }
    }
}
