//! DFA fidelity test: compares bbnf's DFA regex engine against the `regex` crate
//! on the CSS grammar's compiled bytecode patterns, probing for mismatches at
//! critical offsets in tailwind.css (where the VM stalls at byte 3633741/3642321).
//!
//! Root cause: the DFA engine treats `.*?` (non-greedy) as greedy `.*`.
//! The CSS whitespace pattern `(?s)(?:\s|\/\*.*?\*\/)*` relies on non-greedy
//! matching inside comments. The greedy DFA makes `.*` eat through `*/` boundaries,
//! consuming from the first `/*` all the way to the last `*/` in the entire file.

use bbnf::pipeline::{PipelineOptions, compile_grammar};
use bbnf_ir::compiler::compile as compile_bytecode;
use regex::Regex;

/// Known DFA limitation: `.*?` (non-greedy) is treated as greedy `.*`.
/// DFA engines inherently produce leftmost-longest matches — they cannot
/// distinguish greedy from non-greedy quantifiers.
///
/// Grammars must avoid `.*?` in patterns compiled to DFA. The CSS grammar
/// uses `[^*]*(\*+[^/][^*]*)*\*+/` instead of `.*?\*/` for comment matching.
#[test]
fn dfa_greedy_star_is_documented() {
    let pattern = r"(?s)\/\*.*?\*\/";
    let input = "/* c1 */ code; /* c2 */";

    let dfa = parse_that::regex_engine::Dfa::compile(pattern)
        .expect("DFA should compile");

    let dfa_end = dfa.find_at(input.as_bytes(), 0);
    // DFA matches greedily: from first /* to LAST */ = entire string (22 chars + trailing)
    assert!(dfa_end.unwrap() >= 22, "DFA greedy: should consume through last */");
    // regex crate would match non-greedy: first /* to first */ = 8 bytes
    // This divergence is expected — grammar patterns must be DFA-compatible.
}

/// The DFA-compatible CSS comment pattern stops at the first `*/`.
#[test]
fn dfa_compatible_comment_pattern() {
    let pattern = r"(?s)(?:\s|\/\*[^*]*(?:\*+[^/][^*]*)*\*+\/)*";
    let input = "/* c1 */ code; /* c2 */";

    let dfa = parse_that::regex_engine::Dfa::compile(pattern)
        .expect("DFA should compile");
    let re = Regex::new(&format!(r"\A(?:{})", pattern)).unwrap();

    let dfa_end = dfa.find_at(input.as_bytes(), 0);
    let re_end = re.find(input).map(|m| m.end());

    // Both stop after first comment + trailing space
    assert_eq!(dfa_end, re_end, "DFA-compatible pattern agrees with regex crate");
}

/// Full fidelity sweep: every compiled DFA regex in the CSS grammar vs regex crate,
/// tested at strategic offsets in tailwind.css.
#[test]
fn pipeline_css_dfa_fidelity() {
    let grammar = std::fs::read_to_string("../../grammar/css/pretty.bbnf")
        .expect("failed to read pretty.bbnf");
    let ir = compile_grammar(&grammar, &PipelineOptions::default()).unwrap();
    let program = compile_bytecode(&ir);

    let input = std::fs::read_to_string("../../data/css/tailwind.css")
        .expect("failed to read tailwind.css");
    let bytes = input.as_bytes();

    eprintln!("Input size: {} bytes", bytes.len());
    eprintln!(
        "Compiled regexes: {} slots, {} active",
        program.compiled_regexes.len(),
        program
            .compiled_regexes
            .iter()
            .filter(|r| r.is_some())
            .count()
    );

    // Collect all (sid, pattern, dfa) tuples.
    let regexes: Vec<(usize, &str, &parse_that::regex_engine::Dfa)> = program
        .compiled_regexes
        .iter()
        .enumerate()
        .filter_map(|(sid, opt)| {
            opt.as_ref()
                .map(|dfa| (sid, program.strings[sid].as_str(), dfa))
        })
        .collect();

    eprintln!("\nPatterns under test:");
    for &(sid, pattern, _) in &regexes {
        eprintln!("  sid={}: /{}/", sid, pattern);
    }

    // Compile reference regexes (anchored).
    let references: Vec<(usize, &str, &parse_that::regex_engine::Dfa, Regex)> = regexes
        .iter()
        .map(|&(sid, pattern, dfa)| {
            let anchored = format!(r"\A(?:{})", pattern);
            let re = Regex::new(&anchored)
                .unwrap_or_else(|e| panic!("regex crate failed to compile '{}': {}", pattern, e));
            (sid, pattern, dfa, re)
        })
        .collect();

    // Build offset list: failure zone + early probes + midpoint probes.
    let mut offsets: Vec<usize> = Vec::new();

    // Failure zone around byte 3633741 (where VM stalls).
    for o in 3633700..3633750 {
        if o < bytes.len() {
            offsets.push(o);
        }
    }

    // Earlier probes.
    for o in (3633500..3633600).step_by(5) {
        if o < bytes.len() {
            offsets.push(o);
        }
    }

    // Start of file (where the two header comments live).
    offsets.push(0);
    offsets.push(1);

    // Midpoint probes.
    let mid = bytes.len() / 2;
    for o in mid..mid + 20 {
        if o < bytes.len() {
            offsets.push(o);
        }
    }

    // Near-end probes.
    let near_end = bytes.len().saturating_sub(100);
    for o in (near_end..bytes.len()).step_by(5) {
        offsets.push(o);
    }

    offsets.sort_unstable();
    offsets.dedup();

    eprintln!(
        "\nTesting {} offsets across {} patterns...",
        offsets.len(),
        references.len()
    );

    let mut mismatches: Vec<String> = Vec::new();

    for &(sid, pattern, dfa, ref re) in &references {
        for &offset in &offsets {
            let dfa_result = dfa.find_at(bytes, offset);

            let regex_result = if offset <= bytes.len() {
                re.find(&input[offset..]).map(|m| offset + m.end())
            } else {
                None
            };

            if dfa_result != regex_result {
                let ctx_start = offset.saturating_sub(20);
                let ctx_end = (offset + 60).min(bytes.len());
                let context = String::from_utf8_lossy(&bytes[ctx_start..ctx_end]);

                let msg = format!(
                    "MISMATCH sid={} pattern=/{}/  offset={}  dfa={:?}  regex={:?}\n  \
                     context[{}..{}]: {:?}",
                    sid, pattern, offset, dfa_result, regex_result, ctx_start, ctx_end, context
                );
                eprintln!("{}", msg);
                mismatches.push(msg);
            }
        }
    }

    eprintln!("\n=== SUMMARY ===");
    eprintln!("Total offsets tested: {}", offsets.len());
    eprintln!("Total patterns tested: {}", references.len());
    eprintln!(
        "Total comparisons: {}",
        offsets.len() * references.len()
    );
    eprintln!("Mismatches found: {}", mismatches.len());

    if !mismatches.is_empty() {
        eprintln!("\n--- All mismatches ---");
        for m in &mismatches {
            eprintln!("  {}", m);
        }
        eprintln!();
    }

    assert!(
        mismatches.is_empty(),
        "{} DFA/regex mismatches found (see stderr for details)",
        mismatches.len()
    );
}
