#[test]
fn graph_has_the_locked_recursive_component() {
    let (_, program, _, graph, ..) = super::stages();
    let mut components = graph
        .components()
        .iter()
        .map(|component| {
            let mut names = component
                .iter()
                .map(|id| program.rule(*id).expect("rule").name())
                .collect::<Vec<_>>();
            names.sort_unstable();
            names
        })
        .collect::<Vec<_>>();
    components.sort();
    assert!(components.contains(&vec!["array", "object", "pair", "value"]));
    assert_eq!(components.len(), 7);
    assert_eq!(graph.edges().len(), 13);
}
