#[test]
fn normalization_is_bijective_and_has_no_rewrites() {
    let (_, program, normalized, ..) = super::stages();
    assert_eq!(normalized.report().semantic_rewrites, 0);
    assert_eq!(normalized.report().deleted_nodes, 0);
    assert_eq!(normalized.report().remap.len(), program.rules().len());
}
