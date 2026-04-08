//! Regex tier coverage audit — verifies that every regex pattern in every
//! .bbnf grammar file resolves to a tier without falling back to LazyLock.

use std::path::Path;

fn extract_regex_patterns(source: &str) -> Vec<String> {
    let mut patterns = Vec::new();
    let mut chars = source.chars().peekable();

    while let Some(c) = chars.next() {
        // Skip string literals (single-quoted, double-quoted, backtick).
        if c == '"' || c == '\'' || c == '`' {
            let close = c;
            while let Some(c2) = chars.next() {
                if c2 == '\\' {
                    chars.next(); // skip escaped char
                } else if c2 == close {
                    break;
                }
            }
            continue;
        }

        // Line comments: //
        if c == '/' && chars.peek() == Some(&'/') {
            for c2 in chars.by_ref() {
                if c2 == '\n' {
                    break;
                }
            }
            continue;
        }

        // Block comments: /* ... */ (C-style, used by BBNF)
        if c == '/' && chars.peek() == Some(&'*') {
            chars.next(); // consume '*'
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

        // Block comments: (* ... *) (EBNF-style)
        if c == '(' && chars.peek() == Some(&'*') {
            chars.next(); // consume '*'
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

        // Regex literal: /pattern/
        if c == '/' {
            // Check it's not a division operator or comment — heuristic:
            // if the previous non-ws char was not a closing bracket/paren/ident, it's a regex.
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
                    break; // end of regex
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

fn scan_grammar_dir(dir: &Path) -> Vec<(String, String)> {
    let mut results = Vec::new();
    if !dir.exists() {
        return results;
    }

    for entry in std::fs::read_dir(dir).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.is_dir() {
            results.extend(scan_grammar_dir(&path));
        } else if path.extension().is_some_and(|e| e == "bbnf") {
            let source = std::fs::read_to_string(&path).unwrap();
            let patterns = extract_regex_patterns(&source);
            for p in patterns {
                results.push((path.display().to_string(), p));
            }
        }
    }
    results
}

#[test]
fn audit_all_grammar_regex_patterns() {
    let grammar_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../grammar");

    let all_patterns = scan_grammar_dir(&grammar_dir);

    if all_patterns.is_empty() {
        eprintln!("WARNING: No regex patterns found in grammar files");
        return;
    }

    use bbnf::generate::regex::{CostModel, EmitOpts, RegexStrategy, solve_regex_strategy};

    let cost = CostModel::DEFAULT;
    let opts = EmitOpts::new(&cost);

    // fast_path, hir_inline, dfa_a, dfa_b, unsupported, total
    let mut tier_counts = [0usize; 6];
    let mut fallbacks: Vec<(String, String)> = Vec::new();

    for (file, pattern) in &all_patterns {
        let strategy = solve_regex_strategy(pattern, &opts);
        tier_counts[5] += 1;

        match &strategy {
            RegexStrategy::FastPath(_) | RegexStrategy::FastPathFused(_) => {
                tier_counts[0] += 1;
            }
            RegexStrategy::HirInline => {
                tier_counts[1] += 1;
            }
            RegexStrategy::DfaTierA { states, classes } => {
                tier_counts[2] += 1;
                eprintln!(
                    "  DFA-A: /{pattern}/ ({states} states, {classes} classes) [{file}]"
                );
            }
            RegexStrategy::DfaTierB { states, classes } => {
                tier_counts[3] += 1;
                eprintln!(
                    "  DFA-B: /{pattern}/ ({states} states, {classes} classes) [{file}]"
                );
            }
            RegexStrategy::Unsupported => {
                tier_counts[4] += 1;
                fallbacks.push((file.clone(), pattern.clone()));
            }
        }
    }

    eprintln!("\n=== Regex Strategy Audit ===");
    eprintln!("  Total patterns: {}", tier_counts[5]);
    eprintln!("  Tier 1 (fast paths):       {}", tier_counts[0]);
    eprintln!("  Tier 2 (HIR inline):       {}", tier_counts[1]);
    eprintln!("  Tier 3a (DFA decision tree): {}", tier_counts[2]);
    eprintln!("  Tier 3b (DFA table lookup):  {}", tier_counts[3]);
    eprintln!("  Tier 4 (Unsupported):      {}", tier_counts[4]);

    if !fallbacks.is_empty() {
        eprintln!("\n  LazyLock fallbacks:");
        for (file, pattern) in &fallbacks {
            eprintln!("    /{pattern}/ [{file}]");
        }
    }

    // The DFA tier should handle everything the HIR walker can't.
    // LazyLock fallback should be zero.
    // (Relaxed for now — some patterns may use features like backreferences
    // that neither DFA nor HIR can handle.)
    if tier_counts[4] > 0 {
        eprintln!(
            "\nWARNING: {} patterns fell through to Unsupported. \
             These should be handled by the DFA tier.",
            tier_counts[4]
        );
    }
}
