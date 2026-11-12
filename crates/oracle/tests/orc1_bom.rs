//! sk-v27 LW3.ORC1-BOM: the JavaScript drivers read an input's bytes as they
//! are; a leading byte-order mark is a character of the input.
//!
//! JSONTestSuite's `i_structure_UTF-8_BOM_empty_object` (in custody) named
//! the defect: the TS-package arm accepted it where root rejects it, because
//! the driver decoded each input with `TextDecoder`'s default, which removes
//! a leading U+FEFF before the package sees it. The grammar of record's `ws`
//! (`/[ \t\n\r]*/`) does not read U+FEFF, so the package must reject it. The
//! same decoding stood in the graphql-js driver; each driver now decodes
//! with `ignoreBOM: true` (the peggy driver of LW3.ORC2 is written so).

use std::path::PathBuf;

use bbnf_oracle::arm::{Arm, Case};
use bbnf_oracle::arms::ts_package::TsPackage;
use bbnf_oracle::registry;

fn case(name: &str, bytes: &[u8]) -> Case {
    let dir = bbnf_oracle::npm::cache_root().join("orc1-bom");
    std::fs::create_dir_all(&dir).expect("cache dir");
    let path: PathBuf = dir.join(name);
    std::fs::write(&path, bytes).expect("write case");
    Case {
        id: format!("orc1-bom:{name}"),
        path,
    }
}

#[test]
fn the_ts_package_arm_reads_a_leading_bom_as_a_character() {
    let g = registry::find("json").expect("json registered");
    let cases = [
        case("bom-empty-object.json", b"\xEF\xBB\xBF{}"),
        case("empty-object.json", b"{}"),
    ];
    let outs = TsPackage::default().run(&g, &cases).expect("run the TS package");
    assert_eq!(outs[1].accepted(), Some(true), "{{}} must be accepted: {:?}", outs[1]);
    assert_eq!(
        outs[0].accepted(),
        Some(false),
        "U+FEFF{{}} is not JSON by grammar/json/json.bbnf (ws = /[ \\t\\n\\r]*/), yet the arm accepted it: {:?}",
        outs[0]
    );
}

#[test]
fn every_driver_decodes_with_the_bom_kept() {
    let root = registry::repo_root();
    for driver in [
        "crates/oracle/ts/driver.mjs",
        "crates/oracle/heldout/graphql-js.mjs",
    ] {
        let text = std::fs::read_to_string(root.join(driver)).expect(driver);
        let decoders = text.matches("new TextDecoder(").count();
        let kept = text.matches("ignoreBOM: true").count();
        assert!(decoders > 0, "{driver}: no TextDecoder");
        assert_eq!(decoders, kept, "{driver}: {decoders} decoders, {kept} keep the BOM");
    }
}
