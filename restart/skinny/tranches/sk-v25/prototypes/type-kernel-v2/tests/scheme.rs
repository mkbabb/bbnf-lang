use bbnf_type_kernel_v2::{solve, Boundary, Budget, Primitive, Scheme, StoreBuilder};

#[test]
fn generalization_occurs_after_solving_and_instantiation_is_fresh() {
    let mut builder = StoreBuilder::new();
    let (_, parameter) = builder.fresh_meta().unwrap();
    let identity = builder.function([parameter], parameter).unwrap();
    let store = builder.finish();
    let solution = solve(&store, &[], Budget::default()).unwrap();
    let scheme = Scheme::generalize(
        &solution,
        &store,
        identity,
        &[],
        Boundary::new("rule identity").unwrap(),
    )
    .unwrap();
    assert_eq!(scheme.boundary().name(), "rule identity");
    assert_eq!(scheme.quantified_types(), 1);

    let mut uses = StoreBuilder::new();
    let first = scheme.instantiate(&mut uses).unwrap();
    let second = scheme.instantiate(&mut uses).unwrap();
    let uses = uses.finish();
    assert_ne!(
        uses.fingerprint(first).unwrap(),
        uses.fingerprint(second).unwrap()
    );
}

#[test]
fn solved_variables_are_not_generalized() {
    let mut builder = StoreBuilder::new();
    let (_, parameter) = builder.fresh_meta().unwrap();
    let integer = builder.primitive(Primitive::I64).unwrap();
    let identity = builder.function([parameter], parameter).unwrap();
    let expected = builder.function([integer], integer).unwrap();
    let store = builder.finish();
    let solution = solve(
        &store,
        &[bbnf_type_kernel_v2::Constraint::equal(
            identity,
            expected,
            bbnf_type_kernel_v2::Origin::new("scheme", "concrete use"),
        )],
        Budget::default(),
    )
    .unwrap();
    let scheme = Scheme::generalize(
        &solution,
        &store,
        identity,
        &[],
        Boundary::new("rule concrete").unwrap(),
    )
    .unwrap();
    assert_eq!(scheme.quantified_types(), 0);
    let mut uses = StoreBuilder::new();
    let use_ty = scheme.instantiate(&mut uses).unwrap();
    assert_eq!(uses.finish().fingerprint(use_ty).unwrap(), "fn(I64)->I64");
}
