//! sk-v27 LW3.RX-1d: the `BBNF-RX-NOU` lint. A regex leaf is read with `u`
//! semantics; the lint names a leaf whose reading differs without `u`.

use bbnf_analysis::analysis::LineIndex;
use bbnf_analysis::state::diagnostics::analyze;
use ls_types::NumberOrString;

fn nou(grammar: &str) -> Vec<String> {
    let info = analyze(grammar, &LineIndex::new(grammar));
    info.diagnostics
        .iter()
        .filter(|d| d.code == Some(NumberOrString::String("BBNF-RX-NOU".into())))
        .map(|d| {
            grammar[..]
                .lines()
                .nth(d.range.start.line as usize)
                .unwrap_or("")
                .to_string()
        })
        .collect()
}

#[test]
fn a_leaf_whose_reading_differs_without_u_is_linted() {
    let grammar =
        "string = /\"(?:\\\\.|[^\"\\\\])*\"/ ;\nany = /./ ;\nwide = /[\\u0080-\\uffff]+/ ;";
    assert_eq!(nou(grammar).len(), 3, "{:?}", nou(grammar));
}

#[test]
fn a_leaf_that_reads_the_same_is_not_linted() {
    let grammar = "number = /-?[0-9]+(\\.[0-9]+)?/ ;\nws = /\\s*/ ;\nname = /[_a-zA-Z][_a-zA-Z0-9-]*/ ;\nlatin = /[é]/ ;";
    assert!(nou(grammar).is_empty(), "{:?}", nou(grammar));
}
