use bbnf_json_stages_prototype::vertical::{VerticalError, budget::Budget, source::SourceUnit};

#[test]
fn source_budget_is_typed() {
    let budget = Budget {
        source_bytes: 1,
        ..Budget::default()
    };
    assert!(matches!(
        SourceUnit::acquire(super::SOURCE, budget),
        Err(VerticalError::Budget {
            resource: "source_bytes",
            ..
        })
    ));
}
