//! sk-v27 LW3.ORC1-RB, SK/grammar/DIVERGENCE.tsv J-11 (R-b, rulings ORC-1 and
//! ORC-1a): the TS 0.3.0 package refuses JSON nested 500 deep.
//!
//! JSONTestSuite's `i_structure_500_nested_arrays` (in custody) is
//! implementation-defined, so JSONTestSuite has no authority over it; root
//! accepts it. The grammar of record decides: `array` and `value` recurse with
//! no bound, and docs/bbnf/grammar-syntax.md says "References are resolved
//! lazily — rules may appear in any order and mutual recursion is supported."
//! The package's parser recurses on the JavaScript stack and fails at that
//! depth (verdict reject at offset 0). The cure is the TS backend over the IR
//! (LW4.COMP-4); the oracle's pin moves to the release that carries it.

use std::path::PathBuf;

use bbnf_oracle::arm::{Arm, Case};
use bbnf_oracle::arms::ts_package::TsPackage;
use bbnf_oracle::registry;

#[test]
#[ignore = "born-RED R-b J-11 (SK/grammar/DIVERGENCE.tsv): the TS package refuses JSON nested 500 deep; cure LW4.COMP-4 at LW4"]
fn the_ts_package_reads_json_nested_500_deep() {
    let g = registry::find("json").expect("json registered");
    let dir = bbnf_oracle::npm::cache_root().join("rb-j11");
    std::fs::create_dir_all(&dir).expect("cache dir");
    let path: PathBuf = dir.join("nested-500.json");
    std::fs::write(&path, format!("{}{}", "[".repeat(500), "]".repeat(500))).expect("write");
    let outs = TsPackage::default()
        .run(&g, &[Case { id: "rb-j11:nested-500".into(), path }])
        .expect("run the TS package");
    assert_eq!(outs[0].accepted(), Some(true), "{:?}", outs[0]);
}
