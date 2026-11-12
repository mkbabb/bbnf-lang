//! sk-v27 LW3.ORC2, SK/grammar/DIVERGENCE.tsv J-17 J-18 (R-b, rulings ORC-1a
//! and ORC-2): the TS package reads a string literal's `\n`, `\t`, `\r`, `\f`
//! and `\b` as the characters they name.
//!
//! `grammar/ebnf/ebnf.bbnf`'s `symbol` writes `"\n" | "\t" | "\r" | "\f" |
//! "\b" | "\\"`. Root reads them as U+000A, U+0009, U+000D, U+000C and U+0008
//! (its generated keyword table), and so does the peggy oracle (in custody):
//! docs/bbnf/grammar-syntax.md states it (an ADDED sentence, LW3.ORC2; the
//! spec was silent, and the R-b rule is the compiler where it is silent). The
//! TS 0.3.0 package reads each as a backslash and a letter: a model
//! transcription that reads them so agrees with the package on all 617 ebnf
//! cases (restart/skinny/tranches/sk-v27/evidence/LW3.ORC/ws_model.mjs). The
//! cure is the TS backend over the IR (LW4.COMP-4).

use std::path::PathBuf;

use bbnf_oracle::arm::{Arm, Case};
use bbnf_oracle::arms::ts_package::TsPackage;
use bbnf_oracle::registry;

fn cases(test: &str, texts: &[&str]) -> Vec<Case> {
    // One directory per test: the tests of a binary run in parallel.
    let dir = bbnf_oracle::npm::cache_root().join("rb-ts-literal-escapes").join(test);
    std::fs::create_dir_all(&dir).expect("cache dir");
    texts
        .iter()
        .enumerate()
        .map(|(i, t)| {
            let path: PathBuf = dir.join(format!("c{i}.txt"));
            std::fs::write(&path, t).expect("write case");
            Case { id: format!("rb-ts-literal-escapes:{t:?}"), path }
        })
        .collect()
}

#[test]
#[ignore = "born-RED R-b J-17 J-18 (SK/grammar/DIVERGENCE.tsv): the TS package reads \\n \\t \\r \\f \\b in a string literal as two characters; cure LW4.COMP-4 at LW4"]
fn the_ts_package_reads_literal_escapes_as_the_characters_they_name() {
    const TAG: &str = "escapes";
    let g = registry::find("ebnf").expect("ebnf registered");
    let texts = ["a = \"\n\" ;", "a = \"\t\" ;", "a = \"\r\" ;", "a = \"\u{c}\" ;", "a = \"\u{8}\" ;"];
    let outs = TsPackage::default().run(&g, &cases(TAG, &texts)).expect("run the TS package");
    let refused: Vec<String> = texts
        .iter()
        .zip(&outs)
        .filter(|(_, o)| o.accepted() != Some(true))
        .map(|(t, o)| format!("{t:?}: {}", o.verdict()))
        .collect();
    assert!(refused.is_empty(), "the package refuses {} of 5:\n{}", refused.len(), refused.join("\n"));
}

#[test]
fn the_ts_package_reads_a_backslash_escape_and_plain_characters() {
    const TAG: &str = "plain";
    let g = registry::find("ebnf").expect("ebnf registered");
    let texts = ["a = \"\\\" ;", "a = \"x y\" ;", "a = 'q' , { b } ;"];
    let outs = TsPackage::default().run(&g, &cases(TAG, &texts)).expect("run the TS package");
    for (t, o) in texts.iter().zip(&outs) {
        assert_eq!(o.accepted(), Some(true), "{t:?}: {}", o.verdict());
    }
}
