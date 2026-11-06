use bbnf_json_stages_prototype::vertical::{
    VerticalError,
    budget::Budget,
    compile,
    source::{SourceLock, SourceUnit},
};

#[test]
fn value_oracle_is_red_until_module_semantics_are_complete() {
    let source = SourceUnit::acquire_locked(
        include_str!("fixtures/source/json.bbnf"),
        SourceLock {
            bytes: 537,
            sha256: "ab8d21218b2a5f49d9231af6f71503a9949782fdc0a50d81a845fc69054f2604",
        },
        Budget::default(),
    )
    .expect("source");
    assert!(matches!(
        compile(&source, Budget::default()),
        Err(VerticalError::Incomplete(_))
    ));
}
