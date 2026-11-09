//! SK-V19.W6e — the LIVE Lock-14 self-gate.
//!
//! Lock 14 (the user binding bar): *a grammar is added by a `.bbnf`
//! file + workspace metadata; ZERO grammar-specific code in the generic
//! crates.* The two generic crates that must carry no grammar-name
//! token in CODE are `crates/ir` and `crates/analysis`.
//!
//! This is a live check, not prose: it scans every `src/*.rs` file
//! under both crates for the widened 9-grammar roster and FAILS if any
//! grammar-name token appears in a CODE line. Doc-comment lines
//! (`///` and `//!`) are excluded — historical prose may name a
//! grammar; only executable / data code is gated.
//!
//! A re-inject falsifier (`gate_goes_red_on_reinjected_token`) proves
//! the scanner is load-bearing: it flips RED the instant a grammar-name
//! token lands in a code line.

use std::fs;
use std::path::{Path, PathBuf};

/// The widened 9-grammar roster. A grammar-name token in a generic
/// crate's CODE is a Lock-14 leak. Every production grammar advertises
/// a `<Name>Parser` and/or `<Name>Grammar` parser-struct ident; BBNF
/// additionally uses `BbnfBootstrap`. The roster is the union of all
/// nine grammars' advertised idents.
const ROSTER: &[&str] = &[
    "JsonParser",
    "JsonGrammar",
    "GoogleSheetsParser",
    "GoogleSheetsGrammar",
    "CssL4Parser",
    "BbnfBootstrap",
    "BbnfParser",
    "CsvParser",
    "CsvGrammar",
    "MathParser",
    "MathGrammar",
    "BnfParser",
    "BnfGrammar",
    "EbnfParser",
    "EbnfGrammar",
    "CssPrettyParser",
    "CssPrettyGrammar",
];

/// True when `line` is a Rust doc-comment (`///` or `//!`), after
/// leading whitespace. Doc-comment prose is excluded from the gate;
/// only code lines are scanned.
fn is_doc_comment(line: &str) -> bool {
    let t = line.trim_start();
    t.starts_with("///") || t.starts_with("//!")
}

/// Every roster token that appears on `line`. Empty when clean.
fn roster_hits_on_line(line: &str) -> Vec<&'static str> {
    if is_doc_comment(line) {
        return Vec::new();
    }
    ROSTER
        .iter()
        .copied()
        .filter(|tok| line.contains(tok))
        .collect()
}

/// Recursively collect every `.rs` file under `dir`.
fn collect_rs_files(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect_rs_files(&path, out);
        } else if path.extension().is_some_and(|e| e == "rs") {
            out.push(path);
        }
    }
}

/// The two generic crate `src/` roots the gate covers, resolved
/// relative to this crate's manifest dir (`crates/ir`).
fn generic_crate_src_roots() -> Vec<PathBuf> {
    let ir_crate = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let crates_dir = ir_crate.parent().expect("crates/ir has a parent");
    vec![ir_crate.join("src"), crates_dir.join("analysis").join("src")]
}

#[test]
fn no_grammar_name_token_in_generic_crate_code() {
    let mut leaks: Vec<String> = Vec::new();

    for root in generic_crate_src_roots() {
        assert!(
            root.is_dir(),
            "Lock-14 gate: expected generic crate src root at {} — \
             path resolution is wrong, the gate would scan nothing",
            root.display()
        );

        let mut files = Vec::new();
        collect_rs_files(&root, &mut files);

        for file in files {
            let text = fs::read_to_string(&file)
                .unwrap_or_else(|e| panic!("Lock-14 gate: cannot read {}: {e}", file.display()));
            for (i, line) in text.lines().enumerate() {
                for tok in roster_hits_on_line(line) {
                    leaks.push(format!(
                        "{}:{}: grammar-name token `{tok}` in code: {}",
                        file.display(),
                        i + 1,
                        line.trim()
                    ));
                }
            }
        }
    }

    assert!(
        leaks.is_empty(),
        "Lock-14 self-gate RED: {} grammar-name token(s) leaked into generic crate CODE \
         (crates/ir + crates/analysis). A grammar must be added by a .bbnf file + workspace \
         metadata, with ZERO grammar-specific code in the generic crates. Offending lines:\n{}",
        leaks.len(),
        leaks.join("\n")
    );
}

#[test]
fn gate_goes_red_on_reinjected_token() {
    // Falsifier: the scanner MUST detect a grammar-name token the
    // instant it lands on a code line. If this passed silently the
    // main gate would be a no-op (a green light wired to nothing).
    let injected_code_line = "    let _ = JsonParser::new();";
    assert!(
        !roster_hits_on_line(injected_code_line).is_empty(),
        "Lock-14 gate is NOT load-bearing: a re-injected grammar-name token in a code line \
         did not trip the scanner — the GREEN gate would be meaningless"
    );

    // And the inverse: a doc-comment naming a grammar is NOT a leak,
    // so historical prose does not produce false reds.
    let doc_line = "    //! see JsonParser for the historical shape";
    assert!(
        roster_hits_on_line(doc_line).is_empty(),
        "Lock-14 gate over-fires: a grammar name in a doc-comment must not be a code leak"
    );
}
