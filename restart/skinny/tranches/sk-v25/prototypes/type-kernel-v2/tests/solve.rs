use bbnf_type_kernel_v2::{
    solve, Budget, Constraint, Exhaustion, Origin, Primitive, RowSpec, SolveError, StoreBuilder,
};

fn origin(fact: &str) -> Origin {
    Origin::new("tests/solve.rs", fact)
}

#[test]
fn equality_is_fully_zonked_without_mutating_the_store() {
    let mut builder = StoreBuilder::new();
    let (_, a) = builder.fresh_meta().unwrap();
    let (_, b) = builder.fresh_meta().unwrap();
    let integer = builder.primitive(Primitive::I64).unwrap();
    let pair = builder.tuple([a, b]).unwrap();
    let store = builder.finish();
    let before = store.fingerprint(pair).unwrap();

    let solution = solve(
        &store,
        &[
            Constraint::equal(a, b, origin("a=b")),
            Constraint::equal(integer, b, origin("b=i64")),
        ],
        Budget::default(),
    )
    .unwrap();
    assert_eq!(solution.fingerprint(&store, pair).unwrap(), "(I64,I64)");
    assert_eq!(store.fingerprint(pair).unwrap(), before);
    assert_eq!(solution.origins().len(), 2);
}

#[test]
fn open_record_and_variant_rows_produce_canonical_residuals() {
    let mut builder = StoreBuilder::new();
    let integer = builder.primitive(Primitive::I64).unwrap();
    let boolean = builder.primitive(Primitive::Bool).unwrap();
    let string = builder.primitive(Primitive::Str).unwrap();
    let left_tail = builder.fresh_row().unwrap();
    let right_tail = builder.fresh_row().unwrap();
    let left = builder
        .record(RowSpec::open([("a", integer), ("left", string)], left_tail).unwrap())
        .unwrap();
    let right = builder
        .record(RowSpec::open([("a", integer), ("right", boolean)], right_tail).unwrap())
        .unwrap();
    let store = builder.finish();

    let solution = solve(
        &store,
        &[Constraint::equal(left, right, origin("row equation"))],
        Budget::default(),
    )
    .unwrap();
    let left_rendered = solution.fingerprint(&store, left).unwrap();
    let right_rendered = solution.fingerprint(&store, right).unwrap();
    assert_eq!(left_rendered, right_rendered);
    assert!(left_rendered.contains("!r0"));
    assert!(left_rendered.contains("left:Str"));
    assert!(left_rendered.contains("right:Bool"));
}

#[test]
fn explicit_and_implicit_lacks_constraints_reject_duplicate_extensions() {
    let mut builder = StoreBuilder::new();
    let integer = builder.primitive(Primitive::I64).unwrap();
    let row = builder.fresh_row().unwrap();
    let open = builder
        .record(RowSpec::open([("name", integer)], row).unwrap())
        .unwrap();
    let closed = builder
        .record(RowSpec::closed([("name", integer), ("age", integer)]).unwrap())
        .unwrap();
    let store = builder.finish();
    let error = solve(
        &store,
        &[
            Constraint::lacks(row, "age", origin("schema excludes age")),
            Constraint::equal(open, closed, origin("extension adds age")),
        ],
        Budget::default(),
    )
    .unwrap_err();
    assert!(
        matches!(error, SolveError::Lacks { label, origins, .. } if label == "age" && origins.len() == 2)
    );
}

#[test]
fn occurs_check_traverses_composites_and_row_tails() {
    let mut builder = StoreBuilder::new();
    let (meta, variable) = builder.fresh_meta().unwrap();
    let list = builder.list(variable).unwrap();
    let store = builder.finish();
    assert!(matches!(
        solve(
            &store,
            &[Constraint::equal(variable, list, origin("implicit infinity"))],
            Budget::default()
        ),
        Err(SolveError::Occurs { meta: found, .. }) if found == meta
    ));

    let mut builder = StoreBuilder::new();
    let integer = builder.primitive(Primitive::I64).unwrap();
    let row = builder.fresh_row().unwrap();
    let open = builder
        .record(RowSpec::open([("x", integer)], row).unwrap())
        .unwrap();
    let wider = builder
        .record(RowSpec::open([("x", integer), ("y", integer)], row).unwrap())
        .unwrap();
    let store = builder.finish();
    assert!(matches!(
        solve(
            &store,
            &[Constraint::equal(open, wider, origin("row infinity"))],
            Budget::default()
        ),
        Err(SolveError::RowOccurs { row: found, .. }) if found == row
    ));
}

#[test]
fn one_thousand_constraint_permutations_are_byte_identical() {
    let mut builder = StoreBuilder::new();
    let (_, a) = builder.fresh_meta().unwrap();
    let (_, b) = builder.fresh_meta().unwrap();
    let (_, c) = builder.fresh_meta().unwrap();
    let integer = builder.primitive(Primitive::I64).unwrap();
    let boolean = builder.primitive(Primitive::Bool).unwrap();
    let left_tail = builder.fresh_row().unwrap();
    let right_tail = builder.fresh_row().unwrap();
    let left = builder
        .record(RowSpec::open([("item", a)], left_tail).unwrap())
        .unwrap();
    let right = builder
        .record(RowSpec::open([("flag", boolean), ("item", c)], right_tail).unwrap())
        .unwrap();
    let store = builder.finish();
    let seed = vec![
        Constraint::equal(a, b, origin("a=b")),
        Constraint::equal(c, b, origin("c=b")),
        Constraint::equal(integer, c, origin("c=i64")),
        Constraint::equal(left, right, origin("rows")),
    ];
    let expected = solve(&store, &seed, Budget::default())
        .unwrap()
        .render_bytes();
    let mut state = 0x9e37_79b9_u64;
    for _ in 0..1_000 {
        let mut shuffled = seed.clone();
        for index in (1..shuffled.len()).rev() {
            state ^= state << 13;
            state ^= state >> 7;
            state ^= state << 17;
            shuffled.swap(index, state as usize % (index + 1));
        }
        let actual = solve(&store, &shuffled, Budget::default())
            .unwrap()
            .render_bytes();
        assert_eq!(actual, expected);
    }
}

#[test]
fn recursive_equality_solves_free_variables_under_alpha_stable_binders() {
    let mut builder = StoreBuilder::new();
    let (_, left_free) = builder.fresh_meta().unwrap();
    let (_, right_free) = builder.fresh_meta().unwrap();
    let left = builder
        .mu(|builder, binder| {
            let recursive = builder.bound(binder)?;
            builder.tuple([recursive, left_free])
        })
        .unwrap();
    let right = builder
        .mu(|builder, binder| {
            let recursive = builder.bound(binder)?;
            builder.tuple([recursive, right_free])
        })
        .unwrap();
    let store = builder.finish();
    let solution = solve(
        &store,
        &[Constraint::equal(left, right, origin("recursive equality"))],
        Budget::default(),
    )
    .unwrap();
    assert_eq!(
        solution.fingerprint(&store, left).unwrap(),
        solution.fingerprint(&store, right).unwrap()
    );
}

#[test]
fn successful_permutations_and_repeat_solve_are_byte_identical() {
    let mut builder = StoreBuilder::new();
    let (_, a) = builder.fresh_meta().unwrap();
    let (_, b) = builder.fresh_meta().unwrap();
    let integer = builder.primitive(Primitive::I64).unwrap();
    let store = builder.finish();
    let forward = vec![
        Constraint::equal(a, b, origin("a=b")),
        Constraint::equal(b, integer, origin("b=i64")),
    ];
    let reverse = forward.iter().cloned().rev().collect::<Vec<_>>();
    let first = solve(&store, &forward, Budget::default()).unwrap();
    let second = solve(&store, &reverse, Budget::default()).unwrap();
    let third = solve(&store, &forward, Budget::default()).unwrap();
    assert_eq!(first.render_bytes(), second.render_bytes());
    assert_eq!(first.render_bytes(), third.render_bytes());
}

#[test]
fn one_thousand_failing_permutations_have_identical_complete_diagnostics() {
    let mut builder = StoreBuilder::new();
    let (_, variable) = builder.fresh_meta().unwrap();
    let integer = builder.primitive(Primitive::I64).unwrap();
    let boolean = builder.primitive(Primitive::Bool).unwrap();
    let store = builder.finish();
    let constraints = vec![
        Constraint::equal(variable, integer, origin("declared integer")),
        Constraint::equal(boolean, variable, origin("observed boolean")),
    ];
    let expected = solve(&store, &constraints, Budget::default()).unwrap_err();
    assert!(matches!(
        &expected,
        SolveError::Mismatch { origins, .. } if origins.len() == 2
    ));
    for iteration in 0..1_000 {
        let ordered = if iteration % 2 == 0 {
            constraints.clone()
        } else {
            constraints.iter().cloned().rev().collect()
        };
        assert_eq!(
            solve(&store, &ordered, Budget::default()).unwrap_err(),
            expected
        );
    }
}

#[test]
fn deep_inputs_fail_with_a_typed_budget_error() {
    let mut builder = StoreBuilder::new();
    let mut deep = builder.primitive(Primitive::I64).unwrap();
    for _ in 0..64 {
        deep = builder.list(deep).unwrap();
    }
    let store = builder.finish();
    assert_eq!(
        solve(
            &store,
            &[Constraint::equal(deep, deep, origin("deep"))],
            Budget::new(10_000, 16)
        ),
        Err(SolveError::Exhausted {
            kind: Exhaustion::Depth,
            limit: 16
        })
    );
}
