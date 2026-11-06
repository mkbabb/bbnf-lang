use bbnf_type_kernel_v3::type_kernel::solve::{Constraint, SolvedType, solve};
use bbnf_type_kernel_v3::type_kernel::store::{Primitive, RowSpec, StoreBuilder};
use bbnf_type_kernel_v3::type_kernel::{OperationContext, Origin, TypeErrorKind};

#[test]
fn row_occurs_traverses_field_types() {
    let mut builder = StoreBuilder::new();
    let i64_ty = builder.primitive(Primitive::I64).unwrap();
    let row = builder.fresh_row(0).unwrap();
    let nested = builder
        .record(RowSpec::open(Vec::<(String, _)>::new(), row).unwrap())
        .unwrap();
    let left = builder
        .record(RowSpec::open([("a", i64_ty)], row).unwrap())
        .unwrap();
    let right = builder
        .record(RowSpec::closed([("a", i64_ty), ("x", nested)]).unwrap())
        .unwrap();
    let store = builder.finish([left, right]).unwrap();
    let constraints = [Constraint::equal(
        left,
        right,
        Origin::new("row-occurs", "field recursion"),
    )];
    let mut operations = OperationContext::unlimited();
    let error = solve(&store, &constraints, [left], &mut operations).unwrap_err();
    assert_eq!(
        error.kind(),
        &TypeErrorKind::RowOccurs { row: row.ordinal() }
    );
    assert_eq!(error.proof().origins().len(), 1);
}

#[test]
fn flexible_open_rows_join_without_rigid_solver_residuals() {
    let mut builder = StoreBuilder::new();
    let i64_ty = builder.primitive(Primitive::I64).unwrap();
    let left_tail = builder.fresh_row(0).unwrap();
    let right_tail = builder.fresh_row(0).unwrap();
    let common_tail = builder.fresh_row(0).unwrap();
    let left = builder
        .record(RowSpec::open([("a", i64_ty)], left_tail).unwrap())
        .unwrap();
    let right = builder
        .record(RowSpec::open([("b", i64_ty)], right_tail).unwrap())
        .unwrap();
    let common = builder
        .record(RowSpec::open([("a", i64_ty), ("b", i64_ty)], common_tail).unwrap())
        .unwrap();
    let store = builder.finish([left, right, common]).unwrap();
    let constraints = [
        Constraint::equal(left, right, Origin::new("join", "first residual")),
        Constraint::equal(left, common, Origin::new("join", "left common")),
        Constraint::equal(right, common, Origin::new("join", "right common")),
    ];
    let mut operations = OperationContext::unlimited();
    let solution = solve(&store, &constraints, [left, right, common], &mut operations).unwrap();
    assert!(matches!(
        solution.type_of(left),
        Some(SolvedType::Record(_))
    ));
    assert_eq!(solution.type_of(left), solution.type_of(right));
    assert_eq!(solution.type_of(left), solution.type_of(common));
}

#[test]
fn lacks_conflict_keeps_both_causes() {
    let mut builder = StoreBuilder::new();
    let i64_ty = builder.primitive(Primitive::I64).unwrap();
    let tail = builder.fresh_row(0).unwrap();
    let open = builder
        .record(RowSpec::open(Vec::<(String, _)>::new(), tail).unwrap())
        .unwrap();
    let closed = builder
        .record(RowSpec::closed([("x", i64_ty)]).unwrap())
        .unwrap();
    let store = builder.finish([open, closed]).unwrap();
    let constraints = [
        Constraint::lacks(tail, "x", Origin::new("predicate", "r lacks x")),
        Constraint::equal(open, closed, Origin::new("equation", "r = {x}")),
    ];
    let mut operations = OperationContext::unlimited();
    let error = solve(&store, &constraints, [open], &mut operations).unwrap_err();
    assert!(matches!(error.kind(), TypeErrorKind::RowLacks { label, .. } if label == "x"));
    let sites = error
        .proof()
        .origins()
        .into_iter()
        .map(|origin| origin.site().to_owned())
        .collect::<Vec<_>>();
    assert!(sites.contains(&"predicate".to_owned()));
    assert!(sites.contains(&"equation".to_owned()));
}

#[test]
fn row_constraint_and_field_permutations_are_canonical() {
    let mut builder = StoreBuilder::new();
    let i64_ty = builder.primitive(Primitive::I64).unwrap();
    let left_tail = builder.fresh_row(0).unwrap();
    let right_tail = builder.fresh_row(0).unwrap();
    let left = builder
        .record(RowSpec::open([("z", i64_ty), ("a", i64_ty)], left_tail).unwrap())
        .unwrap();
    let right = builder
        .record(RowSpec::open([("m", i64_ty), ("a", i64_ty)], right_tail).unwrap())
        .unwrap();
    let store = builder.finish([left, right]).unwrap();
    let forward = vec![
        Constraint::equal(left, right, Origin::new("rows", "join")),
        Constraint::lacks(left_tail, "blocked", Origin::new("rows", "lacks-left")),
        Constraint::lacks(right_tail, "blocked", Origin::new("rows", "lacks-right")),
    ];
    let mut reversed = forward.clone();
    reversed.reverse();
    let mut left_ops = OperationContext::unlimited();
    let mut right_ops = OperationContext::unlimited();
    let first = solve(&store, &forward, [left, right], &mut left_ops).unwrap();
    let second = solve(&store, &reversed, [right, left], &mut right_ops).unwrap();
    assert_eq!(first.identity(), second.identity());
    assert_eq!(first.type_of(left), second.type_of(left));
}

#[test]
fn one_thousand_twenty_four_distinct_row_heavy_permutations_are_identical() {
    let mut builder = StoreBuilder::new();
    let i64_ty = builder.primitive(Primitive::I64).unwrap();
    let left_tail = builder.fresh_row(0).unwrap();
    let right_tail = builder.fresh_row(0).unwrap();
    let common_tail = builder.fresh_row(0).unwrap();
    let left = builder
        .record(RowSpec::open([("a", i64_ty)], left_tail).unwrap())
        .unwrap();
    let right = builder
        .record(RowSpec::open([("b", i64_ty)], right_tail).unwrap())
        .unwrap();
    let common = builder
        .record(RowSpec::open([("a", i64_ty), ("b", i64_ty)], common_tail).unwrap())
        .unwrap();
    let store = builder.finish([left, right, common]).unwrap();
    let constraints = [
        Constraint::equal(left, right, Origin::new("p0", "join")),
        Constraint::equal(left, common, Origin::new("p1", "left common")),
        Constraint::equal(right, common, Origin::new("p2", "right common")),
        Constraint::lacks(left_tail, "blocked", Origin::new("p3", "left lacks")),
        Constraint::lacks(right_tail, "blocked", Origin::new("p4", "right lacks")),
        Constraint::lacks(common_tail, "blocked", Origin::new("p5", "common lacks")),
        Constraint::lacks(common_tail, "reserved", Origin::new("p6", "second lacks")),
    ];
    let mut order = [0_usize, 1, 2, 3, 4, 5, 6];
    let mut expected = None;
    for _ in 0..1_024 {
        let permuted = order
            .iter()
            .map(|index| constraints[*index].clone())
            .collect::<Vec<_>>();
        let mut operations = OperationContext::unlimited();
        let identity = solve(&store, &permuted, [left, right, common], &mut operations)
            .unwrap()
            .identity()
            .to_owned();
        if let Some(expected) = &expected {
            assert_eq!(&identity, expected);
        } else {
            expected = Some(identity);
        }
        assert!(next_permutation(&mut order));
    }
}

#[test]
fn row_equality_closure_obeys_identity_commutativity_associativity_and_idempotence() {
    let mut builder = StoreBuilder::new();
    let i64_ty = builder.primitive(Primitive::I64).unwrap();
    let a_tail = builder.fresh_row(0).unwrap();
    let b_tail = builder.fresh_row(0).unwrap();
    let c_tail = builder.fresh_row(0).unwrap();
    let a = builder
        .record(RowSpec::open([("a", i64_ty)], a_tail).unwrap())
        .unwrap();
    let b = builder
        .record(RowSpec::open([("b", i64_ty)], b_tail).unwrap())
        .unwrap();
    let c = builder
        .record(RowSpec::open([("a", i64_ty), ("b", i64_ty)], c_tail).unwrap())
        .unwrap();
    let store = builder.finish([a, b, c]).unwrap();

    let solve_identity = |constraints: Vec<Constraint>| {
        let mut operations = OperationContext::unlimited();
        solve(&store, &constraints, [a, b, c], &mut operations)
            .unwrap()
            .type_of(a)
            .unwrap()
            .clone()
    };
    let ab = Constraint::equal(a, b, Origin::new("ab", "row equality"));
    let ba = Constraint::equal(b, a, Origin::new("ab", "row equality"));
    let bc = Constraint::equal(b, c, Origin::new("bc", "row equality"));
    let ac = Constraint::equal(a, c, Origin::new("ac", "row equality"));

    assert_eq!(solve_identity(vec![ab.clone()]), solve_identity(vec![ba]));
    assert_eq!(
        solve_identity(vec![ab.clone(), bc.clone()]),
        solve_identity(vec![bc, ac])
    );
    assert_eq!(
        solve_identity(vec![ab.clone()]),
        solve_identity(vec![ab.clone(), ab])
    );
    let mut identity_ops = OperationContext::unlimited();
    let identity = solve(
        &store,
        &[Constraint::equal(a, a, Origin::new("identity", "a = a"))],
        [a],
        &mut identity_ops,
    )
    .unwrap();
    assert_eq!(identity.type_of(a).unwrap(), &solve_identity(Vec::new()));
}

fn next_permutation(values: &mut [usize]) -> bool {
    let Some(pivot) = (0..values.len().saturating_sub(1))
        .rev()
        .find(|index| values[*index] < values[*index + 1])
    else {
        return false;
    };
    let successor = (pivot + 1..values.len())
        .rev()
        .find(|index| values[*index] > values[pivot])
        .expect("a pivot always has a successor");
    values.swap(pivot, successor);
    values[pivot + 1..].reverse();
    true
}
