use bbnf_type_kernel_v2::{
    lower_layout, solve, Abi, Budget, Layout, LayoutError, Primitive, RowSpec, StoreBuilder,
};

#[test]
fn layout_is_checked_and_only_created_from_a_solved_semantic_type() {
    let mut builder = StoreBuilder::new();
    let boolean = builder.primitive(Primitive::Bool).unwrap();
    let integer = builder.primitive(Primitive::I64).unwrap();
    let record = builder
        .record(RowSpec::closed([("flag", boolean), ("value", integer)]).unwrap())
        .unwrap();
    let store = builder.finish();
    let solution = solve(&store, &[], Budget::default()).unwrap();
    let plan = lower_layout(&solution, &store, record, Abi::AppleAarch64).unwrap();
    assert_eq!(plan.abi(), Abi::AppleAarch64);
    assert_eq!(
        plan.layout(),
        &Layout::Aggregate(vec![
            Layout::Scalar { bytes: 1, align: 1 },
            Layout::Scalar { bytes: 8, align: 8 }
        ])
    );
    assert_eq!(plan.semantic(), "record{flag:Bool,value:I64|closed}");
}

#[test]
fn unresolved_semantics_cannot_claim_a_layout() {
    let mut builder = StoreBuilder::new();
    let (_, unresolved) = builder.fresh_meta().unwrap();
    let store = builder.finish();
    let solution = solve(&store, &[], Budget::default()).unwrap();
    assert!(matches!(
        lower_layout(&solution, &store, unresolved, Abi::AppleAarch64),
        Err(LayoutError::Unresolved(_))
    ));
}
