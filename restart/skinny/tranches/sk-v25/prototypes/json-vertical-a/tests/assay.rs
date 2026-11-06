#[test]
fn receipt_cannot_authorize_movement_or_selection() {
    let receipt: serde_json::Value =
        serde_json::from_str(include_str!("../evidence/STATUS.json")).expect("receipt");
    assert_eq!(receipt["scope"], "prototype-evidence-only");
    assert_eq!(receipt["movement_authorized"], false);
    assert_eq!(receipt["production_transpose_authorized"], false);
    assert_eq!(receipt["architecture_selected"], false);
}

#[test]
fn stage_result_digests_are_reported() {
    use bbnf_json_stages_prototype::vertical::{
        assay,
        budget::Budget,
        source::{SourceLock, SourceUnit},
    };
    let source = SourceUnit::acquire_locked(
        include_str!("fixtures/source/json.bbnf"),
        SourceLock {
            bytes: 537,
            sha256: "ab8d21218b2a5f49d9231af6f71503a9949782fdc0a50d81a845fc69054f2604",
        },
        Budget::default(),
    )
    .expect("locked source");
    let stages = assay(&source, Budget::default()).expect("assay");
    assert_eq!(
        stages.program.revision().hex(),
        "908129a7e097b1070ac1b7ac587cd242999ca8672057c13faf22abf41051ca71"
    );
    assert_eq!(
        stages.normalized.digest().hex(),
        "650f68b18ef9a566ed0857feead3afc4c232fec9c606dec3ca26ffbeab47ccaa"
    );
    assert_eq!(
        stages.graph.digest().hex(),
        "f0e97cd27cb8d67d1b86e310c823bb62a976d6c4fbdb092f24eca4c24e6cd8ac"
    );
    println!(
        "source={} program={} normalized={} graph={}",
        source.digest().hex(),
        stages.program.revision().hex(),
        stages.normalized.digest().hex(),
        stages.graph.digest().hex()
    );
}
