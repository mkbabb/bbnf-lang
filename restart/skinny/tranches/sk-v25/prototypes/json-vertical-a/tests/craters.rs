use bbnf_json_stages_prototype::vertical::{assay, budget::Budget, source::SourceUnit};

#[test]
fn second_carrier_changes_names_schema_literals_and_delimiters() {
    let budget = Budget::default();
    let source =
        SourceUnit::acquire(include_str!("fixtures/source/tiny.bbnf"), budget).expect("source");
    let stages = assay(&source, budget).expect("generic stages");
    assert_eq!(
        stages
            .program
            .rule(stages.program.entry())
            .expect("entry")
            .name(),
        "root"
    );
    assert_eq!(
        stages
            .program
            .rules()
            .iter()
            .map(|rule| rule.name())
            .collect::<Vec<_>>(),
        ["zero", "truth", "collection", "root"]
    );
}
