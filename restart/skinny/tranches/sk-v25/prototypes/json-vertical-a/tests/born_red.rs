use bbnf_json_stages_prototype::vertical::{
    assay,
    budget::Budget,
    source::{SourceLock, SourceUnit},
};

#[test]
fn locked_source_rejects_g3_carrier() {
    let result = SourceUnit::acquire_locked(
        include_str!("fixtures/corpus/mutation/g3.bbnf"),
        SourceLock {
            bytes: 537,
            sha256: "ab8d21218b2a5f49d9231af6f71503a9949782fdc0a50d81a845fc69054f2604",
        },
        Budget::default(),
    );
    assert!(result.is_err());
}

#[test]
fn second_module_derives_its_own_rule_names_without_sidecars() {
    let source = SourceUnit::acquire(include_str!("fixtures/source/json.bbnf"), Budget::default())
        .expect("source");
    assert_eq!(
        assay(&source, Budget::default())
            .expect("assay")
            .program
            .rules()
            .len(),
        10
    );
}
