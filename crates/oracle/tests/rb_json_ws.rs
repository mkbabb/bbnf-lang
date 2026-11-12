//! sk-v27 LW3.ORC1-RB, SK/grammar/DIVERGENCE.tsv J-12 (R-b, rulings A-10,
//! ORC-1 and ORC-1a): the TS package reads `?w` as docs/bbnf says, so it
//! accepts JSON whitespace RFC 8259 refuses.
//!
//! RFC 8259 §2: `ws = *( %x20 / %x09 / %x0A / %x0D )`. JSONTestSuite's
//! `n_structure_whitespace_formfeed` (`[\f]`, in custody) names the row:
//! `grammar/json/json.bbnf` writes its inner whitespace with `?w`, which
//! docs/bbnf/operators.md defines as `ws* >> expr << ws*` "where `ws` matches
//! `\s`", and `\s` reads U+000B and U+000C. The TS package reads the grammar
//! as the spec says and accepts `[\f]`; root rejects it only because its
//! `?w` reads four bytes, the spec's set in neither of the two places the
//! spec states it (operators.md `\s`; observation.md six characters).
//!
//! The cure is the grammar's: every `?w` becomes the grammar's own `ws`
//! (`/[ \t\n\r]*/`), so both arms read RFC 8259 by the grammar's text. That
//! edit, tried at LW3 (sk-v27 LW3.ORC1-WS), breaks 75 root tests: root's JSON
//! shape recognisers (structural projection, struct-direct emission) key on
//! the `?w` idiom and read no explicit-`ws` sequence. The cure is therefore
//! the spine's (LW4.COMP-1, the plan-driven Rust emitter), whose recognisers read the grammar by its text;
//! root is the quarry, retired after the cutover (PLAN CB-14).
//!
//! The falsifier runs the TS-package arm over every whitespace character at
//! every place JSON admits whitespace, against RFC 8259, and checks the
//! grammar's text.

use std::path::PathBuf;

use bbnf_oracle::arm::{Arm, Case};
use bbnf_oracle::arms::ts_package::TsPackage;
use bbnf_oracle::registry;

#[test]
#[ignore = "born-RED R-b J-12 J-13 (SK/grammar/DIVERGENCE.tsv): grammar/json/json.bbnf reads ?w, which is \\s by docs/bbnf/operators.md; cure LW4.COMP-1 at LW4"]
fn the_ts_package_arm_reads_rfc_8259_whitespace_at_every_place() {
    let g = registry::find("json").expect("json registered");
    let dir = bbnf_oracle::npm::cache_root().join("orc1-ws");
    std::fs::create_dir_all(&dir).expect("cache dir");
    let places = ["[W]", "[W1]", "[1W]", "[1W,1]", "[1,W1]", "{W}", "{W\"k\":1}", "{\"k\"W:1}", "{\"k\":W1}", "{\"k\":1W}", "W1", "1W"];
    let chars = [(' ', true), ('\t', true), ('\n', true), ('\r', true), ('\u{c}', false), ('\u{b}', false), ('\u{a0}', false), ('\u{feff}', false), ('\u{2028}', false)];
    let mut cases = Vec::new();
    let mut want = Vec::new();
    for (pi, p) in places.iter().enumerate() {
        for (ci, (c, ok)) in chars.iter().enumerate() {
            let text = p.replace('W', &c.to_string());
            let path: PathBuf = dir.join(format!("p{pi:02}-c{ci}.json"));
            std::fs::write(&path, &text).expect("write");
            cases.push(Case { id: format!("orc1-ws:{text:?}"), path });
            want.push((text, *ok));
        }
    }
    let outs = TsPackage::default().run(&g, &cases).expect("run the TS package");
    let wrong: Vec<String> = want
        .iter()
        .zip(&outs)
        .filter(|((_, ok), o)| o.accepted() != Some(*ok))
        .map(|((t, ok), o)| format!("{t:?}: RFC 8259 {}, arm {}", if *ok { "accepts" } else { "rejects" }, o.verdict()))
        .collect();
    assert!(wrong.is_empty(), "{} of {} diverge:\n{}", wrong.len(), outs.len(), wrong.join("\n"));
}

#[test]
#[ignore = "born-RED R-b J-12 J-13 (SK/grammar/DIVERGENCE.tsv): grammar/json/json.bbnf reads ?w, which is \\s by docs/bbnf/operators.md; cure LW4.COMP-1 at LW4"]
fn the_grammar_writes_its_whitespace_with_its_own_ws_rule() {
    let text = std::fs::read_to_string(registry::repo_root().join("grammar/json/json.bbnf")).expect("json.bbnf");
    let code: String = text.lines().filter(|l| !l.trim_start().starts_with("//")).collect::<Vec<_>>().join("\n");
    assert!(!code.contains("?w"), "grammar/json/json.bbnf still writes ?w, which reads \\s (U+000B, U+000C)");
    assert!(code.contains(r"ws = /[ \t\n\r]*/"), "grammar/json/json.bbnf has no RFC 8259 ws rule");
}
