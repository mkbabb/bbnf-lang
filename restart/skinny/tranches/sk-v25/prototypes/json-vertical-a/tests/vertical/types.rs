#[path = "types/row.rs"]
mod row;

#[test]
fn type_publication_hard_stops_without_module_derived_semantics() {
    use bbnf_json_stages_prototype::vertical::{
        VerticalError, budget::Budget, types::TypeSolution,
    };
    let (_, _, normalized, graph) = super::stages();
    assert!(matches!(
        TypeSolution::solve(&normalized, &graph, Budget::default()),
        Err(VerticalError::Incomplete(_))
    ));
}
