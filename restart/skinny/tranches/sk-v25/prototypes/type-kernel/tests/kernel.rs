use bbnf_type_kernel_prototype::{
    Constraint, ErrorKind, Layout, LayoutPlan, Origin, Primitive, ReuseBoundary, Row, RowSide,
    RowTail, Scheme, Solution, TypeStore,
};

fn origin(fact: &str) -> Origin {
    Origin::new("tests/kernel.rs", fact)
}

#[test]
fn equality_obeys_reflexive_symmetric_and_transitive_laws() {
    let mut store = TypeStore::new();
    let (_, a) = store.fresh_type_variable();
    let (_, b) = store.fresh_type_variable();
    let integer = store.primitive(Primitive::Int);

    let reflexive = Solution::solve(
        &mut store.clone(),
        vec![Constraint::equal(a, a, origin("reflexive"))],
    )
    .unwrap();
    assert!(reflexive.type_substitutions.is_empty());

    let forward = Solution::solve(
        &mut store.clone(),
        vec![Constraint::equal(a, integer, origin("symmetric"))],
    )
    .unwrap();
    let reverse = Solution::solve(
        &mut store.clone(),
        vec![Constraint::equal(integer, a, origin("symmetric"))],
    )
    .unwrap();
    assert_eq!(forward, reverse);

    let transitive = Solution::solve(
        &mut store,
        vec![
            Constraint::equal(a, b, origin("a = b")),
            Constraint::equal(b, integer, origin("b = int")),
        ],
    )
    .unwrap();
    assert_eq!(transitive.fingerprint(&store, a), "Int");
    assert_eq!(transitive.fingerprint(&store, b), "Int");

    let plan = LayoutPlan {
        semantic_type: integer,
        layout: Layout::Scalar { bytes: 8, align: 8 },
    };
    assert_eq!(plan.semantic_type, integer);
}

#[test]
fn composite_terms_keep_stable_ids_and_unify_structurally() {
    let mut store = TypeStore::new();
    let (first_variable, first) = store.fresh_type_variable();
    let (second_variable, second) = store.fresh_type_variable();
    assert!(first.0 < second.0);
    assert!(first_variable.0 < second_variable.0);

    let first_list = store.list(first);
    let first_option = store.option(first_list);
    let first_tuple = store.tuple([first_option, first]);
    let first_function = store.function([first_tuple], first);

    let integer = store.primitive(Primitive::Int);
    let integer_list = store.list(integer);
    let integer_option = store.option(integer_list);
    let integer_tuple = store.tuple([integer_option, integer]);
    let integer_function = store.function([integer_tuple], integer);

    let solution = Solution::solve(
        &mut store,
        vec![Constraint::equal(
            first_function,
            integer_function,
            origin("composite equality"),
        )],
    )
    .unwrap();
    assert_eq!(
        solution.fingerprint(&store, first_function),
        solution.fingerprint(&store, integer_function)
    );
}

#[test]
fn generalization_gives_identity_and_const_principal_shapes_fresh_uses() {
    let mut store = TypeStore::new();
    let (a_variable, a) = store.fresh_type_variable();
    let identity = store.function([a], a);
    let identity_scheme =
        Scheme::generalize_at(ReuseBoundary::new("rule identity"), &store, identity, &[]);
    assert_eq!(identity_scheme.boundary.0, "rule identity");
    assert_eq!(identity_scheme.quantified_types, [a_variable].into());

    let first_identity = identity_scheme.instantiate(&mut store);
    let second_identity = identity_scheme.instantiate(&mut store);
    assert_ne!(
        store.fingerprint(first_identity),
        store.fingerprint(second_identity)
    );

    let integer = store.primitive(Primitive::Int);
    let integer_identity = store.function([integer], integer);
    Solution::solve(
        &mut store,
        vec![Constraint::equal(
            first_identity,
            integer_identity,
            origin("identity use"),
        )],
    )
    .unwrap();

    let (left_variable, left) = store.fresh_type_variable();
    let (right_variable, right) = store.fresh_type_variable();
    let constant = store.function([left, right], left);
    let constant_scheme =
        Scheme::generalize_at(ReuseBoundary::new("rule constant"), &store, constant, &[]);
    assert_eq!(
        constant_scheme.quantified_types,
        [left_variable, right_variable].into()
    );

    let constant_use = constant_scheme.instantiate(&mut store);
    let boolean = store.primitive(Primitive::Bool);
    let string = store.primitive(Primitive::Str);
    let expected = store.function([boolean, string], boolean);
    Solution::solve(
        &mut store,
        vec![Constraint::equal(
            constant_use,
            expected,
            origin("const use"),
        )],
    )
    .unwrap();
}

#[test]
fn open_record_row_absorbs_a_disjoint_extension() {
    let mut store = TypeStore::new();
    let integer = store.primitive(Primitive::Int);
    let boolean = store.primitive(Primitive::Bool);
    let tail = store.fresh_row_variable();
    let open = store.record(Row::open([("a", integer)], tail));
    let extended = store.record(Row::closed([("a", integer), ("b", boolean)]));

    let solution = Solution::solve(
        &mut store,
        vec![Constraint::equal(open, extended, origin("row extension"))],
    )
    .unwrap();

    assert_eq!(
        solution.fingerprint(&store, open),
        solution.fingerprint(&store, extended)
    );
    assert_eq!(solution.row_substitutions[&tail].tail, RowTail::Closed);
    assert_eq!(
        solution.row_substitutions[&tail]
            .fields
            .keys()
            .cloned()
            .collect::<Vec<_>>(),
        ["b"]
    );
}

#[test]
fn closed_row_mismatch_is_typed_and_keeps_provenance() {
    let mut store = TypeStore::new();
    let integer = store.primitive(Primitive::Int);
    let boolean = store.primitive(Primitive::Bool);
    let narrow = store.record(Row::closed([("a", integer)]));
    let wide = store.record(Row::closed([("a", integer), ("b", boolean)]));
    let provenance = Origin::new("grammar.bbnf:8:4", "rule payload has declared record type");

    let error = Solution::solve(
        &mut store,
        vec![Constraint::equal(narrow, wide, provenance.clone())],
    )
    .unwrap_err();

    assert_eq!(error.origin, provenance);
    assert_eq!(
        error.kind,
        ErrorKind::MissingFields {
            closed: RowSide::Left,
            labels: vec!["b".to_owned()]
        }
    );
}

#[test]
fn explicit_mu_accepts_recursion_while_implicit_infinity_fails_occurs_check() {
    let mut store = TypeStore::new();
    let (binder, binder_type) = store.fresh_type_variable();
    let body = store.option(binder_type);
    let recursive = store.mu(binder, body);
    let (_, accepted) = store.fresh_type_variable();

    let accepted_solution = Solution::solve(
        &mut store,
        vec![Constraint::equal(
            accepted,
            recursive,
            origin("explicit recursion"),
        )],
    )
    .unwrap();
    assert!(accepted_solution
        .fingerprint(&store, accepted)
        .starts_with("mu."));

    let (infinite_variable, infinite) = store.fresh_type_variable();
    let list_of_self = store.list(infinite);
    let error = Solution::solve(
        &mut store,
        vec![Constraint::equal(
            infinite,
            list_of_self,
            origin("implicit recursion"),
        )],
    )
    .unwrap_err();
    assert!(matches!(
        error.kind,
        ErrorKind::OccursCheck {
            variable,
            within: _
        } if variable == infinite_variable
    ));
}

#[test]
fn constraint_permutation_produces_byte_equivalent_solution_state() {
    let mut seed = TypeStore::new();
    let (_, a) = seed.fresh_type_variable();
    let (_, b) = seed.fresh_type_variable();
    let integer = seed.primitive(Primitive::Int);
    let boolean = seed.primitive(Primitive::Bool);
    let left_tail = seed.fresh_row_variable();
    let right_tail = seed.fresh_row_variable();
    let left = seed.record(Row::open([("a", a)], left_tail));
    let right = seed.record(Row::open([("a", b), ("flag", boolean)], right_tail));
    let constraints = vec![
        Constraint::equal(a, b, origin("variables agree")),
        Constraint::equal(b, integer, origin("primitive choice")),
        Constraint::equal(left, right, origin("row agreement")),
    ];

    let mut forward_store = seed.clone();
    let forward = Solution::solve(&mut forward_store, constraints.clone()).unwrap();
    let mut reverse_store = seed;
    let reverse =
        Solution::solve(&mut reverse_store, constraints.into_iter().rev().collect()).unwrap();

    assert_eq!(forward, reverse);
    assert_eq!(forward_store, reverse_store);
    assert_eq!(
        forward.fingerprint(&forward_store, left),
        reverse.fingerprint(&reverse_store, left)
    );
}

#[test]
fn open_variant_rows_share_the_same_row_kernel() {
    let mut store = TypeStore::new();
    let unit = store.primitive(Primitive::Unit);
    let tail = store.fresh_row_variable();
    let open = store.variant(Row::open([("Some", unit)], tail));
    let closed = store.variant(Row::closed([("Some", unit), ("None", unit)]));
    let solution = Solution::solve(
        &mut store,
        vec![Constraint::equal(open, closed, origin("variant extension"))],
    )
    .unwrap();
    assert_eq!(
        solution.fingerprint(&store, open),
        solution.fingerprint(&store, closed)
    );
}
